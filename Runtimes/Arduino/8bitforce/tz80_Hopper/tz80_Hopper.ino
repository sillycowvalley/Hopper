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
// Time APIs
////////////////////////////////////////////////////////////////////

#define TICKS_START   0x0028
#define TICKS_END     0x002B

uint32_t ticks;

inline __attribute__((always_inline))
byte getTicks(uint16_t address)
{
    address -= 0x0028; // 0..3
    if (address == 0)
    {
        ticks = millis();
    }
    byte * pTicks = (byte *)&ticks;
    return pTicks[address];
}

////////////////////////////////////////////////////////////////////
// Motorolla 6850 ACIA
////////////////////////////////////////////////////////////////////

#define ACIA_CONTROL 0x001E
#define ACIA_STATUS  0x001E
#define ACIA_DATA    0x001F

byte aciaControl;
byte aciaRxData;
byte aciaTxData;
byte aciaStatus;

const byte ACIA_TDRE = 0b00000010;
const byte ACIA_RDRF = 0b00000001;
const byte ACIA_INTR = 0b10000000;

void init6850ACIA()
{
    aciaControl  = 0;
    aciaRxData   = 0;
    aciaTxData   = 0;
    aciaStatus   = ACIA_TDRE;  // bit 1 set means TDRE is empty and ready - we should always be ready to send data
}



inline __attribute__((always_inline))
void tx6850ACIA(unsigned int address, byte data) 
{
    if (address == ACIA_CONTROL)
    {
        aciaControl = data; // we never use this
    }
    else if (address == ACIA_DATA)
    {
        aciaTxData = data; // we just transmit it immediately, TDRF remains set
        char str[2];
        str[0] = aciaTxData;
        str[1] = 0;
        Serial.print(str);
    }
}

inline __attribute__((always_inline))
byte rx6850ACIA(unsigned int address) 
{
    byte data = 0;
    digitalWriteFast(uP_INT_N, HIGH);
    if (address == ACIA_STATUS)
    {
        data = aciaStatus;
        aciaStatus = aciaStatus & 0b01111111; // reading the status register clears the interrupt flag
    }
    else if (address == ACIA_DATA)
    {
        data = aciaRxData;  
        // reading the data register clears the RDRF bit
        aciaStatus = aciaStatus & 0b11111110;
    }
    return data;
}


inline __attribute__((always_inline))
void service6850ACIA() 
{
    const byte TRANSMIT_DELAY = 10;             // tx_delay to handle fast incoming chars
    static byte txDelay = TRANSMIT_DELAY;
    
    if (txDelay > 0)
    {
        txDelay--;
    }
    else if (digitalReadFast(uP_INT_N) == LOW)
    {
        // If interrupt is already asserted
        // wait for it go high...
        return;
    } else if (Serial.available())
    {
        if ((aciaStatus & ACIA_INTR) == 0)               // read serial byte only if 6850 interrupt is clear 
        {
            if ((aciaStatus & ACIA_RDRF) == 0)           // and receive data register (RDRF) is not full
            {
                int ch = Serial.read();
                aciaRxData   = ch & 0xFF;                             // byte from int?          
                aciaStatus   = aciaStatus | (ACIA_INTR | ACIA_RDRF);  // interrupt and RDRF bits set to indicate that data is ready

                // signal an interrupt:
                digitalWriteFast(uP_INT_N, LOW);

                txDelay = TRANSMIT_DELAY;
            }
        }
    }
    return;
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
byte data     = 0;

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
      {
          data = ROM[ (uP_ADDR - ROM_START) ];
      }
      else
      // RAM?
      if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) )
      {
          data = RAM[uP_ADDR - RAM_START];
      }
      else
      {
          data = 0xFF;
      }
      
      // Start driving the databus out
      SET_DATA_OUT( data );

#if outputDEBUG
      if (uP_ADDR != 0x0008)
      {
          char tmp[40];
          sprintf(tmp, "-- A=%04X D=%02X\n", uP_ADDR, data);
          Serial.write(tmp);
          delay(1);
      }
#endif

    } else
    // Write?
    if (!STATE_WR_N)
    {
        xDATA_DIR_IN();
        data = xDATA_IN();
        
        // Memory Write
        if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) )
        {
          RAM[uP_ADDR - RAM_START] = data;
        }

#if outputDEBUG
        char tmp[40];
        sprintf(tmp, "WR A=%04X D=%02X\n", uP_ADDR, data);
        Serial.write(tmp);
        delay(1);
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

      if ( (ACIA_STATUS == ADDR_L) || (ACIA_DATA == ADDR_L) ) // 6850 ?
      {
          prevDATA = rx6850ACIA(ADDR_L);
      }
      else if ( (ADDR_L <= TICKS_END) && (TICKS_START <= ADDR_L) ) // Time on Zero Page
      {
          prevDATA = getTicks(ADDR_L);
      }
      // output data at this cycle too
      data = prevDATA;
      SET_DATA_OUT( data );
      
    }
    else
    
    // continuing IO Read?
    if (!STATE_RD_N && !prevIORQ)    // continue output same data
    {
      // change DATA port to output to uP:
      xDATA_DIR_OUT();

      data = prevDATA;
      SET_DATA_OUT( data );
    }
    else

    // IO Write?
    if (!STATE_WR_N && prevIORQ)      // perform write on falling edge
    {
      xDATA_DIR_IN();
      data = xDATA_IN();

      byte ADDR_L = ADDR() & 0xFF;

      if ( (ACIA_CONTROL == ADDR_L) || (ACIA_DATA == ADDR_L)  )   // 6850 ?
      {
          tx6850ACIA(ADDR_L, data);
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
      data = 0;
      SET_DATA_OUT( data );
    }

#if (outputDEBUG)
    {
        char tmp[40];
        sprintf(tmp, "IORQ RW=%01X (%s) A=%04X D=%02X\n", STATE_WR_N, (STATE_WR_N == 1 ? "read" : "write"), uP_ADDR, data);
        Serial.write(tmp);
        delay(1);
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
  
  Serial.println("Configuration:");
  Serial.println("==============");
  print_teensy_version();
  Serial.print("Debug:      "); Serial.println(outputDEBUG, HEX);

  Serial.print("ROM Size:  ");   Serial.print(ROM_END - ROM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("ROM_START: 0x"); Serial.println(ROM_START, HEX); 
  Serial.print("ROM_END:   0x"); Serial.println(ROM_END, HEX); 

  Serial.print("RAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("RAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("RAM_END:   0x"); Serial.println(RAM_END, HEX); 
  
  // Initialize processor GPIO's
  uP_init();
  //intel8251_init();
  init6850ACIA();

  // Reset processor
  //
  uP_assert_reset();
  for(int i=0;i<25;i++) cpu_tick();
  
  // Go, go, go
  uP_release_reset();
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
    //if (i == 0) service6850ACIA();
    if (i == 0)  Serial.flush();
  }
}
