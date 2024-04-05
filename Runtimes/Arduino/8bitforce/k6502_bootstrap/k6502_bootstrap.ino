
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

unsigned int  uP_ADDR;
unsigned int  clockCycle;

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

  byte data = 0; //(uP_ADDR % 2 == 0) ? 0xAA : 0x55; // good test pattern for reading from the ether
    
  if (STATE_RW_N)	 // HIGH = READ
  {
    // change DATA port to output to uP:
    DATA_DIR = DIR_OUT;
    
    if ( (ROM_START <= uP_ADDR) && (uP_ADDR <= ROM_END) ) // ROM?
    {
      data = pgm_read_byte_near(rom_bin + (uP_ADDR - ROM_START));
      
    }
    else if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
    {
      data = RAM[uP_ADDR - RAM_START];
    }
    DATA_OUT = data;
  } 

  else  // R/W = LOW = WRITE
  {
      if ( (uP_ADDR <= RAM_END) && (RAM_START <= uP_ADDR) ) // RAM?
      {
          data = DATA_IN;
          RAM[uP_ADDR - RAM_START] = data;
      }
  }

  //////////////////////////////////////////////////////////////////
  
  
  clockCycle++;

  if (uP_ADDR <= 0xFF)
  {
      char tmp[40];
      sprintf(tmp, "%0.6d: %c  0x%0.4X %s 0x%0.2X\n", clockCycle, (STATE_RW_N ? 'R' : 'W'), uP_ADDR, (uP_ADDR < 0x8000 ? "        " : ""), data);
      Serial.write(tmp);
      delay(100);
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
  Serial.begin(115200);

  Serial.println("\n");
  Serial.println("Configuration:");
  Serial.println("==============");
  Serial.print("SRAM Size:  "); Serial.print(RAM_END - RAM_START + 1, DEC); Serial.println(" Bytes");
  Serial.print("SRAM_START: 0x"); Serial.println(RAM_START, HEX); 
  Serial.print("SRAM_END:   0x"); Serial.println(RAM_END, HEX); 
  Serial.println("");
  
  clockCycle = 0;

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
    cpu_tick();
}
