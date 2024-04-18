////////////////////////////////////////////////////////////////////
// Z80 RetroShield Code for Teensy 3.5
// 2020/01/12
// Version 0.9
// Erturk Kocalar
//
// The MIT License (MIT)
//
// Copyright (c) 2019 Erturk Kocalar, 8Bitforce.com
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// Date         Comments                                            Author
// -----------------------------------------------------------------------------
// 01/12/2019   Initial Release (BASIC).                            E. Kocalar
// 01/11/2024   Added Teensy 4.1 support                            E. Kocalar
//
////////////////////////////////////////////////////////////////////
// Options
//   outputDEBUG: Print memory access debugging messages.
////////////////////////////////////////////////////////////////////
#define outputDEBUG     0

////////////////////////////////////////////////////////////////////
// BOARD DEFINITIONS
////////////////////////////////////////////////////////////////////

#include "memorymap.h"      // Memory Map (ROM, RAM, PERIPHERALS)
#include "portmap.h"        // Pin mapping to cpu
#include "setuphold.h"      // Delays required to meet setup/hold
#include "8251.h"           // 8251 Emulation

unsigned long clock_cycle_count;

word uP_ADDR;
byte uP_DATA;

void uP_init()
{
  // Set directions for ADDR & DATA Bus.
  configure_PINMODE_ADDR();
  configure_PINMODE_DATA();
  
  pinMode(uP_RESET_N, OUTPUT);
  pinMode(uP_MREQ_N,  INPUT_PULLUP);
  pinMode(uP_IORQ_N,  INPUT_PULLUP);
  pinMode(uP_RD_N,    INPUT_PULLUP);
  pinMode(uP_WR_N,    INPUT_PULLUP);
  pinMode(uP_NMI_N,   OUTPUT);
  pinMode(uP_INT_N,   OUTPUT);
  pinMode(uP_CLK,     OUTPUT);
  
  uP_assert_reset();
  digitalWriteFast(uP_CLK, LOW);

  clock_cycle_count = 0;
}

void uP_assert_reset()
{
  // Drive RESET conditions
  digitalWriteFast(uP_RESET_N, LOW);
  digitalWriteFast(uP_INT_N, HIGH);
  digitalWriteFast(uP_NMI_N, HIGH);
}

void uP_release_reset()
{
  // Drive RESET conditions
  digitalWriteFast(uP_RESET_N, HIGH);
}


////////////////////////////////////////////////////////////////////
// Processor Control Loop
////////////////////////////////////////////////////////////////////
// This is where the action is.
// it reads processor control signals and acts accordingly.
//
// Z80 takes multiple cycles to accomplish each rd/write.
// so if we act on IORQ, RD, WR at every clock cycle, then
// we perform erroneous extra IO read/writes.
// FIX WR: perform IO write only on IORQ/WR falling edge.
// FIX RD: perform IO read only on IORQ/RD falling edge.

byte prevIORQ = 0;
byte prevMREQ = 0;
byte prevDATA = 0;

byte DATA_OUT;
byte DATA_IN;

inline __attribute__((always_inline))
void cpu_tick()
{   
  CLK_HIGH;
  DELAY_FACTOR_H();
  
  uP_ADDR = ADDR();
  
  // unlike memory mapped devices in 6502 & 6809,
  // Z80 bus has two modes: Memory (MREQ_N) and IO (IORQ_N)

  //////////////////////////////////////////////////////////////////////
  // Memory Access?
  if (!STATE_MREQ_N)    
  {

    // Memory Read?
    if (!STATE_RD_N)
    {
      // change DATA port to output to uP:
      xDATA_DIR_OUT();
      
      // ROM?
      if ( (ROM_START <= uP_ADDR) && (uP_ADDR <= ROM_END) )
        DATA_OUT = ROM[ (uP_ADDR - ROM_START) ];
      else
      // RAM?
      if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) )
        DATA_OUT = RAM[uP_ADDR - RAM_START];
      else
        DATA_OUT = 0xFF;
      
      // Start driving the databus out
      SET_DATA_OUT( DATA_OUT );

#if outputDEBUG
      char tmp[40];
      sprintf(tmp, "-- A=%0.4X D=%0.2X\n", uP_ADDR, DATA_OUT);
      Serial.write(tmp);
#endif

    } else
    // Write?
    if (!STATE_WR_N)
    {
      xDATA_DIR_IN();
      DATA_IN = xDATA_IN();
      
      // Memory Write
      if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) )
        RAM[uP_ADDR - RAM_START] = DATA_IN;

#if outputDEBUG
      char tmp[40];
      sprintf(tmp, "WR A=%0.4X D=%0.2X\n", uP_ADDR, DATA_IN);
      Serial.write(tmp);
#endif
    }

  } else
  
  //////////////////////////////////////////////////////////////////////
  // IO Access?
  if (!STATE_IORQ_N)
  {    
    // IO Read?
    if (!STATE_RD_N && prevIORQ)    // perform actual read on falling edge
    {
      // change DATA port to output to uP:
      xDATA_DIR_OUT();
      byte ADDR_L = ADDR() & 0xFF;

      // 8251 access

      if (ADDR_L == ADDR_8251_DATA)
      {
        // DATA register access
        prevDATA = reg8251_DATA = toupper( Serial.read() );
        // clear RxRDY bit in 8251
        reg8251_STATUS = reg8251_STATUS & (~STAT_8251_RxRDY);
        // Serial.write("8251 serial read\n");
        
        digitalWriteFast(uP_INT_N, HIGH);
      }
      else

      if ( ADDR_L == ADDR_8251_MODCMD )
      {
        // Mode/Command Register access
        if (reg8251_STATE == STATE_8251_RESET)
          prevDATA = reg8251_MODE;
        else
          prevDATA = reg8251_STATUS;
      }
      
      // output data at this cycle too
      DATA_OUT = prevDATA;
      SET_DATA_OUT( DATA_OUT );
      
    }
    else
    
    // continuing IO Read?
    if (!STATE_RD_N && !prevIORQ)    // continue output same data
    {
      // change DATA port to output to uP:
      xDATA_DIR_OUT();

      DATA_OUT = prevDATA;
      SET_DATA_OUT( DATA_OUT );
    }
    else

    // IO Write?
    if (!STATE_WR_N && prevIORQ)      // perform write on falling edge
    {
      xDATA_DIR_IN();
      DATA_IN = xDATA_IN();

      byte ADDR_L = ADDR() & 0xFF;

      // 8251 access
      if (ADDR_L == ADDR_8251_DATA)
      {
        // write to DATA register
        reg8251_DATA = DATA_IN;
        // TODO: Spit byte out to serial
        Serial.write(reg8251_DATA);        
      }
      else
      if ( ADDR_L == ADDR_8251_MODCMD )
      {
        // write to Mode/Command Register
        if (reg8251_STATE == STATE_8251_RESET)
        {
          // 8251 changes from MODE to COMMAND
          reg8251_STATE = STATE_8251_INITIALIZED;
          // we ignore the mode command for now.
          // reg8251_MODE = DATA_IN
          // Serial.write("8251 reset\n");
          
        } else {
          // Write to 8251 command register
          reg8251_COMMAND = DATA_IN;
          // TODO: process command sent
        }
      }
    } else
  //////////////////////////////////////////////////////////////////////
    // if (STATE_RD_N && STATE_WR_N)        // 1 && 1
    {
      // Interrupt Mode 2 Acknowledge scenario
      // IORQ asserted, RD & WR not asserted
      // Z80 expects interrupt vector on databus

      // change DATA port to output to uP:
      xDATA_DIR_OUT();

      // default to vector 0
      DATA_OUT = 0;
      SET_DATA_OUT( DATA_OUT );
    }

#if (outputDEBUG)
    {
      char tmp[40];
      sprintf(tmp, "IORQ RW=%0.1X A=%0.4X D=%0.2X\n", STATE_WR_N, uP_ADDR, DATA_OUT);
      Serial.write(tmp);
    }
#endif
  }

  prevIORQ = STATE_IORQ_N;
  prevMREQ = STATE_MREQ_N;
  
  //////////////////////////////////////////////////////////////////////
  // start next cycle
  CLK_LOW;    // E goes low
  DELAY_FACTOR_L();  
  
  xDATA_DIR_IN();

#if (outputDEBUG)  
  clock_cycle_count ++;
#endif
}

////////////////////////////////////////////////////////////////////
// Setup
////////////////////////////////////////////////////////////////////

void setup() 
{

  Serial.begin(0);
  while (!Serial);
  
  Serial.write(27);       // ESC command
  Serial.print("[2J");    // clear screen command
  Serial.write(27);
  Serial.print("[H");
  Serial.println("\n");
  Serial.println("Configuration:");
  Serial.println("==============");
  print_teensy_version();
  Serial.print("Debug:      "); Serial.println(outputDEBUG, HEX);
  Serial.print("SRAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("SRAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("SRAM_END:   0x"); Serial.println(RAM_END, HEX); 
  Serial.println("");
  Serial.println("=======================================================");
  Serial.println("> Z80 SBC By Grant Searle");
  Serial.println("> Z80 BASIC Ver 4.7b Copyright (C) 1978 by Microsoft");
  Serial.println("> Modified by Grant Searle");
  Serial.println("> http://searle.hostei.com/grant/index.html");
  Serial.println("=======================================================");
  Serial.println("Try running:");
  Serial.println("");
  Serial.println("10 FOR A=0 TO 6.2 STEP 0.2");
  Serial.println("20 PRINT TAB(40+SIN(A)*20);\"*\"");
  Serial.println("30 NEXT A");
  Serial.println("40 END");
  
  // Initialize processor GPIO's
  uP_init();
  intel8251_init();

  // Reset processor
  //
  uP_assert_reset();
  for(int i=0;i<25;i++) cpu_tick();
  
  // Go, go, go
  uP_release_reset();

  Serial.println("\n");
}

////////////////////////////////////////////////////////////////////
// Loop()
////////////////////////////////////////////////////////////////////

void loop()
{
  byte i = 0;
  
  // Loop forever
  //
  while(1)
  {    
    cpu_tick();

    // Check serial events but not every cycle.
    // Watch out for clock mismatch (cpu tick vs serialEvent counters are /128)
    i++;
    if (i == 0) serialEvent8251();
    if (i == 0)  Serial.flush();
  }
}
