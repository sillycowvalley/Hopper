////////////////////////////////////////////////////////////////////
// RetroShield 6502 for Teensy 3.5
// Apple 1
//
// 2019/09/13
// Version 0.1

// The MIT License (MIT)

// Copyright (c) 2019 Erturk Kocalar, 8Bitforce.com

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

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
// 09/13/2019   Bring-up on Teensy 3.5.                             Erturk
// 01/11/2024   Added Teensy 4.1 support.                           Erturk

////////////////////////////////////////////////////////////////////
// Options
//   outputDEBUG: Print memory access debugging messages.
////////////////////////////////////////////////////////////////////
#define outputDEBUG     0


#define ACIA_CONTROL 0x001E
#define ACIA_STATUS  0x001E
#define ACIA_DATA    0x001F

byte aciaControl;
byte aciaRxData;
byte aciaTxData;
byte aciaStatus;
byte aciaIRQTicks;

const byte ACIA_TDRE = 0b00000010;
const byte ACIA_RDRF = 0b00000001;
const byte ACIA_INTR = 0b10000000;

void init6850ACIA()
{
    aciaControl  = 0;
    aciaRxData   = 0;
    aciaTxData   = 0;
    aciaStatus   = ACIA_TDRE;  // bit 1 set means TDRE is empty and ready - we should always be ready to send data
    aciaIRQTicks = 0;
}

inline __attribute__((always_inline))
void service6850ACIA() 
{
    if (Serial.available())
    {
        if ((aciaStatus & ACIA_INTR) == 0)               // read serial byte only if 6850 interrupt is clear 
        {
            if ((aciaStatus & ACIA_RDRF) == 0)           // and receive data register (RDRF) is not full
            {
                int ch = Serial.read();
                aciaRxData   = ch & 0xFF;                             // byte from int?          
                aciaStatus   = aciaStatus | (ACIA_INTR | ACIA_RDRF);  // interrupt and RDRF bits set to indicate that data is ready

                //digitalWrite(uP_IRQ_N, LOW);
                //aciaIRQTicks = 2;
            }
        }
    }
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
    byte data;
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


////////////////////////////////////////////////////////////////////
// BOARD DEFINITIONS
////////////////////////////////////////////////////////////////////

#include "memorymap.h"      // Memory Map (ROM, RAM, PERIPHERALS)
#include "portmap.h"        // Pin mapping to cpu
#include "setuphold.h"      // Delays required to meet setup/hold
#include "6821.h"           // 6821 Emulation

unsigned long clock_cycle_count;
unsigned long clock_cycle_last;

word          uP_ADDR;
byte          uP_DATA;

void uP_assert_reset()
{
  // Drive RESET conditions
  digitalWriteFast(uP_RESET_N,  LOW);
  digitalWriteFast(uP_IRQ_N,    HIGH);
  digitalWriteFast(uP_NMI_N,    HIGH);
  digitalWriteFast(uP_RDY,      HIGH);
  digitalWriteFast(uP_SO_N,     HIGH);
}

void uP_release_reset()
{
  // Drive RESET conditions
  digitalWriteFast(uP_RESET_N,  HIGH);
}

void uP_init()
{
  // Set directions for ADDR & DATA Bus.
 
  configure_PINMODE_ADDR();
  configure_PINMODE_DATA();

  pinMode(uP_RESET_N, OUTPUT);
  pinMode(uP_RW_N,    INPUT_PULLUP);
  pinMode(uP_RDY,     OUTPUT);
  pinMode(uP_SO_N,    OUTPUT);
  pinMode(uP_IRQ_N,   OUTPUT);
  pinMode(uP_NMI_N,   OUTPUT);
  pinMode(uP_CLK_E,   OUTPUT);
  pinMode(uP_GPIO,    INPUT_PULLUP);
    
  digitalWriteFast(uP_CLK_E, LOW); 
  uP_assert_reset();

  clock_cycle_count = 0;

}


////////////////////////////////////////////////////////////////////
// Processor Control Loop
////////////////////////////////////////////////////////////////////
// This is where the action is.
// it reads processor control signals and acts accordingly.
//

inline __attribute__((always_inline))
void cpu_tick()
{ 
  
    CLK_HIGH;    // E goes high

    DELAY_FACTOR_H();
    
    uP_ADDR = ADDR();
    
    byte data = (uP_ADDR % 2 == 0) ? 0xAA : 0x55; // good test pattern to know when we are reading from dead space
    
    if (STATE_RW_N)	   // HIGH = READ
    {
        xDATA_DIR_OUT();  // change DATA port to output to uP:
            
        if ( (ROM_START <= uP_ADDR) && (uP_ADDR <= ROM_END) ) // ROM?
        {
          data = rom_bin[ (uP_ADDR - ROM_START) ];
        }
        else
        // RAM?
        if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) )
          // Use Arduino RAM for stack/important stuff
          data = ( RAM[uP_ADDR - RAM_START] );
        else
        // 6821?
        if ( KBD <=uP_ADDR && uP_ADDR <= DSPCR )   
        {      
          // KBD?
          if (uP_ADDR == KBD)
          {
            if (regKBDCR & 0x02)
              // KBD register  
              {
                data = regKBD;
                regKBDCR = regKBDCR & 0x7F;    // clear IRQA bit upon read
              }
            else
              data = regKBDDIR;
          }
          else
          // KBDCR?
          if (uP_ADDR == KBDCR)
          {
            // KBDCR register
            data = regKBDCR;  
          }
          else
          // DSP?
          if (uP_ADDR == DSP)
          {
            if (regDSPCR & 0x02) 
              // DSP register  
              {
                data = regDSP;
                regDSPCR = regDSPCR & 0x7F;    // clear IRQA bit upon read
              }
            else
              data = regDSPDIR;
          }
          else
          // DSPCR?
          if (uP_ADDR == DSPCR)
          {
            // DSPCR register
            data = regDSPCR;  
          }   
          
        }

        // Start driving the databus out
        SET_DATA_OUT( data );
          
    #if outputDEBUG
        // if (clock_cycle_count > 0x3065A)
        {
          char tmp[50];
          sprintf(tmp, "-- A=%0.4X D=%0.2X\n", uP_ADDR, data);
          Serial.write(tmp);
        }
    #endif

    } 
    else 
    //////////////////////////////////////////////////////////////////
    // R/W = LOW = WRITE
    {
      data = xDATA_IN();
      
      // RAM?
      if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) )
        // Use Arduino RAM for stack/important stuff
        RAM[uP_ADDR - RAM_START] = data;
      else
      // 6821?
      if ( KBD <=uP_ADDR && uP_ADDR <= DSPCR )
      {
        // KBD?
        if (uP_ADDR == KBD)
        {
          if (regKBDCR & 0x02)
            // KBD register
            {  
              regKBD = data;
            }
          else
            regKBDDIR = data;
        }
        else
        // KBDCR?
        if (uP_ADDR == KBDCR)
        {
          // KBDCR register
          regKBDCR = data & 0X7F;  
        }
        else
        // DSP?
        if (uP_ADDR == DSP)
        {
          if (regDSPCR & 0x02)
          {
            // DSP register
            regDSP = data;
            Serial.write(regDSP);
          }
          else
            regDSPDIR = data;  
        }
        else
        // DSPCR?
        if (uP_ADDR == DSPCR)
        {
          // DSPCR register
          regDSPCR = data;  
        }
      }
      
  #if outputDEBUG
      if (1) // (clock_cycle_count > 0x3065A)
      {
        char tmp[50];
        sprintf(tmp, "WR A=%0.4X D=%0.2X\n", uP_ADDR, data);
        Serial.write(tmp);
      }
  #endif
    }

    
    //////////////////////////////////////////////////////////////////
      
    // start next cycle
    CLK_LOW;    // E goes low
    DELAY_FACTOR_L();  

    xDATA_DIR_IN();
    
#if outputDEBUG
    clock_cycle_count++;
#endif
}

////////////////////////////////////////////////////////////////////
// Serial Event
////////////////////////////////////////////////////////////////////

/*
  SerialEvent occurs whenever a new data comes in the
 hardware serial RX.  This routine is run between each
 time loop() runs, so using delay inside loop can delay
 response.  Multiple bytes of data may be available.
 */

inline __attribute__((always_inline))
void serialEvent0() 
{
  if (Serial.available())
    if ((regKBDCR & 0x80) == 0x00)      // read serial byte only if we can set 6821 interrupt
    {
      cli();                            // stop interrupts while changing 6821 guts.
      // 6821 portA is available      
      int ch = Serial.read();
      regKBD = byte(ch);                // apple1 expects bit 7 set for incoming characters.
      regKBDCR = regKBDCR | 0x80;       // set 6821 interrupt
      sei();
    }
  return;
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
  Serial.println("> WOZ Monitor, Integer BASIC, Apple Cassette Interface");
  Serial.println("> by Steve Wozniak");
  Serial.println("=======================================================");
  Serial.println("Notes:");
  Serial.println("1) Enter E000R to start Apple BASIC.");  
  Serial.println("2) Cassette Interface added to RetroShield 6502 by Lorenz Born.");  
  Serial.println("   ACI Audio Out is available on GPIO output."); 
  Serial.println("   Somebody should write code to handle incoming audio :)"); 
   

  // Initialize processor GPIO's
  uP_init();
  m6821_init();

  // Reset processor for 25 cycles
  uP_assert_reset();
  for (int i=0; i<25; i++) cpu_tick();
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
    if (i == 0) serialEvent0();
    if (i == 0) Serial.flush();
  }
}
