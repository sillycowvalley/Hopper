////////////////////////////////////////////////////////////////////
// RetroShield 6502 for Arduino Mega
// Apple I
//
// 2019/01/28
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
// 5/31/2019    Added Apple cassette interface (ACI) to the sketch. Lorenz Born
// 4/05/2024    Reduced to bare minimum WozMon support              Michael Cartwright

////////////////////////////////////////////////////////////////////
// Options
//   outputDEBUG: Print memory access debugging messages.
////////////////////////////////////////////////////////////////////
//#define outputDEBUG

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

// ROMs (Monitor)
#define ROM_START   0xFF00
#define ROM_END     0xFFFF

////////////////////////////////////////////////////////////////////
// Woz Monitor Code
////////////////////////////////////////////////////////////////////
// static const unsigned char 
PROGMEM const unsigned char rom_bin[] = {
// static const unsigned char rom_bin[] = {
    0xd8, 0x58, 0xa0, 0x7f, 0x8c, 0x12, 0xd0, 0xa9, 0xa7, 0x8d, 0x11, 0xd0, 
    0x8d, 0x13, 0xd0, 0xc9, 0xdf, 0xf0, 0x13, 0xc9, 0x9b, 0xf0, 0x03, 0xc8, 
    0x10, 0x0f, 0xa9, 0xdc, 0x20, 0xef, 0xff, 0xa9, 0x8d, 0x20, 0xef, 0xff, 
    0xa0, 0x01, 0x88, 0x30, 0xf6, 0xad, 0x11, 0xd0, 0x10, 0xfb, 0xad, 0x10, 
    0xd0, 0x99, 0x00, 0x02, 0x20, 0xef, 0xff, 0xc9, 0x8d, 0xd0, 0xd4, 0xa0, 
    0xff, 0xa9, 0x00, 0xaa, 0x0a, 0x85, 0x2b, 0xc8, 0xb9, 0x00, 0x02, 0xc9, 
    0x8d, 0xf0, 0xd4, 0xc9, 0xae, 0x90, 0xf4, 0xf0, 0xf0, 0xc9, 0xba, 0xf0, 
    0xeb, 0xc9, 0xd2, 0xf0, 0x3b, 0x86, 0x28, 0x86, 0x29, 0x84, 0x2a, 0xb9, 
    0x00, 0x02, 0x49, 0xb0, 0xc9, 0x0a, 0x90, 0x06, 0x69, 0x88, 0xc9, 0xfa, 
    0x90, 0x11, 0x0a, 0x0a, 0x0a, 0x0a, 0xa2, 0x04, 0x0a, 0x26, 0x28, 0x26, 
    0x29, 0xca, 0xd0, 0xf8, 0xc8, 0xd0, 0xe0, 0xc4, 0x2a, 0xf0, 0x97, 0x24, 
    0x2b, 0x50, 0x10, 0xa5, 0x28, 0x81, 0x26, 0xe6, 0x26, 0xd0, 0xb5, 0xe6, 
    0x27, 0x4c, 0x44, 0xff, 0x6c, 0x24, 0x00, 0x30, 0x2b, 0xa2, 0x02, 0xb5, 
    0x27, 0x95, 0x25, 0x95, 0x23, 0xca, 0xd0, 0xf7, 0xd0, 0x14, 0xa9, 0x8d, 
    0x20, 0xef, 0xff, 0xa5, 0x25, 0x20, 0xdc, 0xff, 0xa5, 0x24, 0x20, 0xdc, 
    0xff, 0xa9, 0xba, 0x20, 0xef, 0xff, 0xa9, 0xa0, 0x20, 0xef, 0xff, 0xa1, 
    0x24, 0x20, 0xdc, 0xff, 0x86, 0x2b, 0xa5, 0x24, 0xc5, 0x28, 0xa5, 0x25, 
    0xe5, 0x29, 0xb0, 0xc1, 0xe6, 0x24, 0xd0, 0x02, 0xe6, 0x25, 0xa5, 0x24, 
    0x29, 0x07, 0x10, 0xc8, 0x48, 0x4a, 0x4a, 0x4a, 0x4a, 0x20, 0xe5, 0xff, 
    0x68, 0x29, 0x0f, 0x09, 0xb0, 0xc9, 0xba, 0x90, 0x02, 0x69, 0x06, 0x2c, 
    0x12, 0xd0, 0x30, 0xfb, 0x8d, 0x12, 0xd0, 0x60, 0x00, 0x00, 0x00, 0x0f, 
    0x00, 0xff, 0x00, 0x00
};

////////////////////////////////////////////////////////////////////
// 6821 Peripheral
// emulate just enough so keyboard/display works thru serial port.
////////////////////////////////////////////////////////////////////

#define KBD   0xd010
#define KBDCR 0xd011
#define DSP   0xd012
#define DSPCR 0xd013
byte regKBD;
byte regKBDDIR;    // Dir register when KBDCR.bit2 == 0
byte regKBDCR;
byte regDSP;
byte regDSPDIR;    // Dir register when DSPCR.bit2 == 0
byte regDSPCR;

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

unsigned int  uP_ADDR;

////////////////////////////////////////////////////////////////////
// Processor Control Loop
////////////////////////////////////////////////////////////////////
// This is where the action is.
// it reads processor control signals and acts accordingly.

inline __attribute__((always_inline))
void cpu_tick()
{ 
  int ch;
  
  CLK_E_HIGH;    // E goes high   // digitalWrite(uP_E, HIGH);

  uP_ADDR = ADDR;
    
  if (STATE_RW_N)	 // HIGH = READ
  {
    // change DATA port to output to uP:
    DATA_DIR = DIR_OUT;
    
    if ( (ROM_START <= uP_ADDR) && (uP_ADDR <= ROM_END) ) // ROM?
    {
      DATA_OUT = pgm_read_byte_near(rom_bin + (uP_ADDR - ROM_START));
    }
    else if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
    {
      DATA_OUT = RAM[uP_ADDR - RAM_START];
    }
    else if ( KBD <=uP_ADDR && uP_ADDR <= DSPCR )   // 6821?
    {      
      if (uP_ADDR == KBD)  // KBD?
      {
        if (regKBDCR & 0x02) // KBD register  
        {
          DATA_OUT = regKBD;
          regKBDCR = regKBDCR & 0x7F;    // clear IRQA bit upon read
        }
        else
        {
          DATA_OUT = regKBDDIR;
        }
      }
      else if (uP_ADDR == KBDCR) // KBDCR?
      {
        // KBDCR register
        DATA_OUT = regKBDCR;  
      }
      else if (uP_ADDR == DSP) // DSP?
      {
        if (regDSPCR & 0x02) // DSP register  
        {
          DATA_OUT = regDSP;
          regDSPCR = regDSPCR & 0x7F;    // clear IRQA bit upon read
        }
        else
        {
          DATA_OUT = regDSPDIR;
        }
      }
      else if (uP_ADDR == DSPCR) // DSPCR?
      {
        DATA_OUT = regDSPCR; // DSPCR register
      }   
    }
    else // nothing here
    {
      DATA_OUT = (uP_ADDR % 2 == 0) ? 0xAA : 0x55; // good test pattern for reading from the ether
    }
#ifdef outputDEBUG
    char tmp[20];
    //sprintf(tmp, "-- A=%0.4X D=%0.2X\n", uP_ADDR, DATA_OUT);
    sprintf(tmp, "RD A=%0.4X\n", uP_ADDR);
    Serial.write(tmp);
#endif
  } 

  else  // R/W = LOW = WRITE
  {
    if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
    {
      RAM[uP_ADDR - RAM_START] = DATA_IN;
    }
    else if ( KBD <=uP_ADDR && uP_ADDR <= DSPCR ) // 6821?
    {
      if (uP_ADDR == KBD) // KBD?
      {
        if (regKBDCR & 0x02) // KBD register
        {  
          regKBD = DATA_IN;
        }
        else
        {
          regKBDDIR = DATA_IN;
        }
      }
      else if (uP_ADDR == KBDCR) // KBDCR?
      {
        regKBDCR = DATA_IN & 0X7F;   // KBDCR register
      }
      else if (uP_ADDR == DSP) // DSP?
      {
        if (regDSPCR & 0x02) // DSP register
        { 
          if (DATA_IN == 0x8D)
          {
            Serial.write("\n");    // send LF (not CR LF: \r\n)
          }
          else
          {
            regDSP = DATA_IN & 0x7F;
            Serial.write(regDSP);
          }
        }
        else
        {
          regDSPDIR = DATA_IN;  
        }
      }
      else if (uP_ADDR == DSPCR) // DSPCR?
      {
        regDSPCR = DATA_IN; // DSPCR register
      }
    }
#ifdef outputDEBUG
    char tmp[20];
    //sprintf(tmp, "WR A=%0.4X D=%0.2X\n", uP_ADDR, DATA_IN);
    sprintf(tmp, "WR A=%0.4X\n", uP_ADDR);
    Serial.write(tmp);
#endif
  }

  //////////////////////////////////////////////////////////////////
  
  // start next cycle
  CLK_E_LOW;    // E goes low

  // natural delay for DATA Hold time (t_HR)
  DATA_DIR = DIR_IN;

  CLK_E_HIGH; 
}

////////////////////////////////////////////////////////////////////
// Serial Event
////////////////////////////////////////////////////////////////////

//  SerialEvent occurs whenever a new data comes in the
//  hardware serial RX.  This routine is run between each
//  time loop() runs, so using delay inside loop can delay
//  response.  Multiple bytes of data may be available.
 
inline __attribute__((always_inline))
void serialEvent0() 
{
  if (Serial.available())
  {
    if ((regKBDCR & 0x80) == 0x00)        // read serial byte only if we can set 6821 interrupt
    {
      cli();                              // stop interrupts while changing 6821 guts.
      // 6821 portA is available      
      int ch = toupper( Serial.read() );  // apple1 expects upper case
      regKBD = ch | 0x80;                 // apple1 expects bit 7 set for incoming characters.
      regKBDCR = regKBDCR | 0x80;         // set 6821 interrupt
      sei();
    }
  }
  return;
}

////////////////////////////////////////////////////////////////////
// Setup
////////////////////////////////////////////////////////////////////

void setup() 
{
  Serial.begin(115200);

  Serial.println("\n");
  Serial.println("Configuration:");
  Serial.println("==============");
  Serial.print("SRAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("SRAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("SRAM_END:   0x"); Serial.println(RAM_END, HEX); 
  Serial.println("");
  Serial.println("=======================================================");
  Serial.println("> WOZ Monitor");
  Serial.println("> by Steve Wozniak");
  Serial.println("=======================================================");

   // 6821 init:
  regKBD    = 0x00;
  regKBDDIR = 0x00;
  regKBDCR  = 0x00;
  regDSP    = 0x00;
  regDSPDIR = 0x00;
  regDSPCR  = 0x00;
  
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

  Serial.println("\n");
}


////////////////////////////////////////////////////////////////////
// Loop()
////////////////////////////////////////////////////////////////////

void loop()
{
    serialEvent0();
    cpu_tick();
}
