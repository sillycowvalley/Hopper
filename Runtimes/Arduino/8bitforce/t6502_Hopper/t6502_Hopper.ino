///////////////////////////////////////////////////////////////////
// RetroShield 6502 for Teensy 3.5
//
// This work for Hopper is a derivative of the work by Erturk Kocalar, 8Bitforce.com:

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

#define outputDEBUG     0

////////////////////////////////////////////////////////////////////
// Time APIs on Zero Page
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


inline __attribute__((always_inline))
void service6850ACIA() 
{
    if (Serial.available())
    {
        if ((aciaStatus & ACIA_INTR) == 0)               // read serial byte only if 6850 interrupt is clear 
        {
            cli();
            if ((aciaStatus & ACIA_RDRF) == 0)           // and receive data register (RDRF) is not full
            {
                int ch = Serial.read();
                aciaRxData   = ch & 0xFF;                             // byte from int?          
                aciaStatus   = aciaStatus | (ACIA_INTR | ACIA_RDRF);  // interrupt and RDRF bits set to indicate that data is ready

                //digitalWrite(uP_IRQ_N, LOW);
                //aciaIRQTicks = 2;
            }
            sei();
        }
    }
}

////////////////////////////////////////////////////////////////////
// BOARD DEFINITIONS
////////////////////////////////////////////////////////////////////

#include "memorymap.h"      // Memory Map (ROM, RAM, PERIPHERALS)
#include "portmap.h"        // Pin mapping to cpu
#include "setuphold.h"      // Delays required to meet setup/hold

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
        else if ( (ACIA_STATUS == uP_ADDR) || (ACIA_DATA == uP_ADDR) ) // 6850 ?
        {
            data = rx6850ACIA(uP_ADDR);
        }
        else if ( (uP_ADDR <= TICKS_END) && (TICKS_START <= uP_ADDR) ) // Time on Zero Page
        {
            data = getTicks(uP_ADDR);
        }
        else if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) ) // RAM?
        {
            data = ( RAM[uP_ADDR - RAM_START] );
        }
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
    else // R/W = LOW = WRITE
    {
        data = xDATA_IN();
        if ( (ACIA_CONTROL == uP_ADDR) || (ACIA_DATA == uP_ADDR)  )   // 6850 ?
        {
            tx6850ACIA(uP_ADDR, data);
        } 
        else if ( (RAM_START <= uP_ADDR) && (uP_ADDR <= RAM_END) ) // RAM?
        {
            RAM[uP_ADDR - RAM_START] = data;
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
  Serial.print("SRAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("SRAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("SRAM_END:   0x"); Serial.println(RAM_END, HEX); 
  Serial.println("");

  // Initialize processor GPIO's
  uP_init();
  init6850ACIA();

  // Reset processor for 25 cycles
  uP_assert_reset();
  for (int i=0; i<25; i++) cpu_tick();
  uP_release_reset();

  Serial.println("\n");
}


////////////////////////////////////////////////////////////////////
// Loop()
////////////////////////////////////////////////////////////////////

byte i = 0;
void loop()
{
    cpu_tick();
    i++;
    if (i == 0) service6850ACIA();
    if (i == 0) Serial.flush();
}
