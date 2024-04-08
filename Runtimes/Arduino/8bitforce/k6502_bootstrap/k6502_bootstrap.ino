////////////////////////////////////////////////////////////////////
// RetroShield 6502 for Arduino Mega
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

#include "pins2_arduino.h"

////////////////////////////////////////////////////////////////////
// 65C02 DEFINITIONS
////////////////////////////////////////////////////////////////////

// 65C02 HW CONSTRAINTS
// 1- RESET_N must be asserted at least 2 clock cycles.
// 2- CLK can not be low more than 5 microseconds.  Can be high indefinitely.

// MEMORY LAYOUT
// 6K RAM
#define RAM_START   0x0000
#define RAM_END     0x17FF
byte    RAM[RAM_END-RAM_START+1];

#include "romImage.h"

////////////////////////////////////////////////////////////////////
// 65c02 Processor Control
////////////////////////////////////////////////////////////////////

// Digital Pin Assignments
#define DATA_OUT PORTL
#define DATA_IN  PINL
#define ADDR_H   PINC
#define ADDR_L   PINA
#define ADDR     ((unsigned int) (ADDR_H << 8 | ADDR_L))

#define uP_RESET_N  38
#define uP_RW_N     40
#define uP_RDY      39
#define uP_SO_N     41
#define uP_IRQ_N    50
#define uP_NMI_N    51
#define uP_E        52
#define uP_GPIO     53

// Fast routines to drive clock signals high/low; faster than digitalWrite
// required to meet >100kHz clock freq for 6809e.
// 6502 & z80 do not have this requirement.
//
#define CLK_Q_HIGH  (PORTB = PORTB | 0x01)
#define CLK_Q_LOW   (PORTB = PORTB & 0xFE)
#define CLK_E_HIGH  (PORTB = PORTB | 0x02)
#define CLK_E_LOW   (PORTB = PORTB & 0xFD)
#define STATE_RW_N  (PING & 0x02)

#define DIR_IN  0x00
#define DIR_OUT 0xFF
#define DATA_DIR   DDRL
#define ADDR_H_DIR DDRC
#define ADDR_L_DIR DDRA

uint16_t  uP_ADDR;
unsigned int  clockCycle;

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

#define ACIA6850
#ifdef ACIA6850

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

#endif

////////////////////////////////////////////////////////////////////
// 6821 PIA
////////////////////////////////////////////////////////////////////

//#define PIA6821

#ifdef PIA6821

#define PIA_START 0xD010
#define PIA_END   0xD013

#define PIA_PRA  0xD010 // (when CRA-2 == 1)
#define PIA_DDRA 0xD010 // (when CRA-2 == 0)
#define PIA_CRA  0xD011 // CRA
#define PIA_PRB  0xD012 // (when CRB-2 == 1)
#define PIA_DDRB 0xD012 // (when CRB-2 == 0)
#define PIA_CRB  0xD013 // CRA
byte rxData;
byte rxDirection;
byte rxControl;
byte txData;
byte txDirection;
byte txControl;

void init6821PIA()
{
    rxData      = 0x00;
    rxDirection = 0x00;
    rxControl   = 0x00;
    txData      = 0x00;
    txDirection = 0x00;
    txControl   = 0x00;
}

inline __attribute__((always_inline))
void service6821PIA() 
{
    if (Serial.available())
    {
        if ((rxControl & 0x80) == 0x00)        // read serial byte only if we can set 6821 interrupt
        {
            int ch = Serial.read();
            cli();                              // stop interrupts while messing with 6821 registers
            // 6821 portA is available      
            rxData   = ch & 0xFF;               // byte from int?          
            rxControl = rxControl | 0x80;       // set 6821 interrupt
            sei();
        }
    }
}

inline __attribute__((always_inline))
void tx6821PIA(uint16_t address, byte data) 
{
    if (address == PIA_PRA)
    {
        if (rxControl & 0x04)
        {  
            rxData = data;
        }
        else
        {
            rxDirection = data;
        }
    }
    else if (address == PIA_CRA)
    {
        rxControl = data & 0X7F;
    }
    else if (address == PIA_PRB)
    {
        if (txControl & 0x04)
        { 
            txData = data;
            char str[2];
            str[0] = txData;
            str[1] = 0;
            Serial.print(str);
        }
        else
        {
            txDirection = data;  
        }
    }
    else if (address == PIA_CRB)
    {
        txControl = data;
    }
}

inline __attribute__((always_inline))
byte rx6821PIA(uint16_t address) 
{
    byte data;
    if (address == PIA_PRA)
    {
        if (rxControl & 0x04)
        {
            data = rxData;
            rxControl = rxControl & 0x7F;    // clear IRQA bit upon read
        }
        else
        {
            data = rxDirection;
        }
    }
    else if (address == PIA_CRA)
    {
        data = rxControl;
    }
    else if (address == PIA_PRB)
    {
        if (txControl & 0x04)
        {
            data = txData;
            txControl = txControl & 0x7F;    // clear IRQA bit upon read
        }
        else
        {
            data = txDirection;
        }
    }
    else if (address == PIA_CRB) // DSPCR?
    {
        data = txControl;
    }  
    return data;
}
#endif



////////////////////////////////////////////////////////////////////
// Processor Control Loop
////////////////////////////////////////////////////////////////////
// This is where the action is.
// it reads processor control signals and acts accordingly.


inline __attribute__((always_inline))
void cpu_tick()
{ 
  CLK_E_HIGH;    // E goes high   // digitalWrite(uP_E, HIGH);
  uP_ADDR = ADDR;
  byte data = (uP_ADDR % 2 == 0) ? 0xAA : 0x55; // good test pattern to know when we are reading from dead space
  if (STATE_RW_N)	 // HIGH = READ
  {
      DATA_DIR = DIR_OUT;  // change DATA port to output to uP
      if ( (ROM_START <= uP_ADDR) && (uP_ADDR <= ROM_END) ) // ROM?
      {
          data = pgm_read_byte_near(rom_bin + (uP_ADDR - ROM_START));
      }
#ifdef PIA6821      
      else if ( PIA_START <=uP_ADDR && uP_ADDR <= PIA_END )   // 6821?
      {
          data = rx6821PIA(uP_ADDR);
      }
#endif
#ifdef ACIA6850      
      else if ( (ACIA_STATUS == uP_ADDR) || (ACIA_DATA == uP_ADDR) )   // 6850 ?
      {
          data = rx6850ACIA(uP_ADDR);
      }
#endif
      else if ( (uP_ADDR <= TICKS_END) && (TICKS_START <= uP_ADDR) ) // Time on Zero Page
      {
          data = getTicks(uP_ADDR);
      }
      else if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
      {
          data = RAM[uP_ADDR - RAM_START];
      }
      DATA_OUT = data;
  } 

  else  // R/W = LOW = WRITE
  {
#ifdef ACIA6850      
      if ( (ACIA_CONTROL == uP_ADDR) || (ACIA_DATA == uP_ADDR)  )   // 6850 ?
      {
          data = DATA_IN;
          tx6850ACIA(uP_ADDR, data);
      } else 
#endif
      if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
      {
          data = DATA_IN;
          RAM[uP_ADDR - RAM_START] = data;
      }
#ifdef PIA6821
      else if ( PIA_START <=uP_ADDR && uP_ADDR <= PIA_END ) // 6821?
      {
          data = DATA_IN;
          tx6821PIA(uP_ADDR, data);
      }
#endif

  }

  //////////////////////////////////////////////////////////////////
  
  
  clockCycle++;
  
  if ((uP_ADDR > 0xFF) && (uP_ADDR <= 0x1FF))
  {
      // don't show stack access
  }
  else if ( false
            //|| ((uP_ADDR >= 0x00)    && (uP_ADDR <= 0xFF))   // zero page?
            //|| ((PIA_START <=uP_ADDR && uP_ADDR <= PIA_END))  // 6821?
            //|| ((0x0200  <= uP_ADDR) && (uP_ADDR <= 0x02FF)) // serial buffer
          )
  {
      char tmp[40];
      sprintf(tmp, "%0.6d: %c  0x%0.4X %s %s0x%0.2X\n", clockCycle, (STATE_RW_N ? 'R' : 'W'), uP_ADDR, (uP_ADDR < 0x8000 ? "        " : ""), (STATE_RW_N ? " <- " : " -> "), data);
      Serial.write(tmp);
      delay(100); // the wheels fall off at 250 ms (unless we show less like just zeropage access)
  }
  

  // start next cycle
  CLK_E_LOW;    // E goes low

  // natural delay for DATA Hold time (t_HR)
  DATA_DIR = DIR_IN;

  

  //CLK_E_HIGH; 
}



////////////////////////////////////////////////////////////////////
// Setup
////////////////////////////////////////////////////////////////////

void setup() 
{
  Serial.begin(19200);

/*
  Serial.println("\n");
  Serial.println("Configuration:");
  Serial.println("==============");
  Serial.print("SRAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("SRAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("SRAM_END:   0x"); Serial.println(RAM_END, HEX); 
*/
  clockCycle = 0;

#ifdef PIA6821
  init6821PIA();
#endif
#ifdef ACIA6850
  init6850ACIA();
#endif  
  
  // pin directions:
  DATA_DIR = DIR_IN;
  ADDR_H_DIR = DIR_IN;
  ADDR_L_DIR = DIR_IN;
  pinMode(uP_RESET_N, OUTPUT);
  pinMode(uP_RW_N,    INPUT);
  pinMode(uP_RDY,     OUTPUT);
  pinMode(uP_SO_N,    OUTPUT);
  pinMode(uP_IRQ_N,   OUTPUT);
  pinMode(uP_NMI_N,   OUTPUT);
  pinMode(uP_E,       OUTPUT);
  pinMode(uP_GPIO,    OUTPUT);
  
  // Drive RESET conditions
  digitalWrite(uP_RESET_N, LOW);
  digitalWrite(uP_IRQ_N, HIGH);
  digitalWrite(uP_NMI_N, HIGH);
  digitalWrite(uP_RDY, HIGH);
  digitalWrite(uP_SO_N, HIGH);

  digitalWrite(uP_E, LOW);
  digitalWrite(uP_GPIO, HIGH);
  
  // stay in RESET for 25 cycles
  for(int i=0; i<25; i++) { cpu_tick(); }
  
  // Release RESET
  digitalWrite(uP_RESET_N, HIGH);

}


////////////////////////////////////////////////////////////////////
// Loop()
////////////////////////////////////////////////////////////////////

byte clicks = 0;
void loop()
{
#ifdef PIA6821
    if (clicks == 100)
    {
        service6821PIA();
        clicks = 0;
    }
#endif
#ifdef ACIA6850
    //if (aciaIRQTicks != 0)
    //{
    //    aciaIRQTicks--;
    //    if (aciaIRQTicks == 0)
    //    {
    //        digitalWrite(uP_IRQ_N, HIGH);    
    //    }
    //}
    if (clicks == 24)
    {
        service6850ACIA();
        clicks = 0;
    }
#endif
    cpu_tick();
    clicks++;
}
