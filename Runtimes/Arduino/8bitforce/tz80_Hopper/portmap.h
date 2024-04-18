#ifndef _PORTMAP_H
#define _PORTMAP_H

////////////////////////////////////////////////////////////////////
// Arduino Port Mapping to Teensy GPIO Numbers.
// we can use arduino naming for teensy to make our life easier.
////////////////////////////////////////////////////////////////////
#define MEGA_PD7  (24)
#define MEGA_PG0  (13)
#define MEGA_PG1  (16)
#define MEGA_PG2  (17)
#define MEGA_PB0  (28)
#define MEGA_PB1  (39)
#define MEGA_PB2  (29)
#define MEGA_PB3  (30)

#define MEGA_PC7  (27)
#define MEGA_PC6  (26)
#define MEGA_PC5  (4)
#define MEGA_PC4  (3)
#define MEGA_PC3  (38)
#define MEGA_PC2  (37)
#define MEGA_PC1  (36)
#define MEGA_PC0  (35)

#define MEGA_PL7  (5)
#define MEGA_PL6  (21)
#define MEGA_PL5  (20)
#define MEGA_PL4  (6)
#define MEGA_PL3  (8)
#define MEGA_PL2  (7)
#define MEGA_PL1  (14)
#define MEGA_PL0  (2)

#define MEGA_PA7  (12)
#define MEGA_PA6  (11)
#define MEGA_PA5  (25)
#define MEGA_PA4  (10)
#define MEGA_PA3  (9)
#define MEGA_PA2  (23)
#define MEGA_PA1  (22)
#define MEGA_PA0  (15)


////////////////////////////////////////////////////////////////////
// Z80 Processor Control
////////////////////////////////////////////////////////////////////

#define uP_RESET_N  MEGA_PD7
#define uP_MREQ_N   MEGA_PG0
#define uP_IORQ_N   MEGA_PG2
#define uP_RD_N     MEGA_PB0
#define uP_WR_N     MEGA_PG1
#define uP_NMI_N    MEGA_PB2
#define uP_INT_N    MEGA_PB3
#define uP_CLK      MEGA_PB1

////////////////////////////////////////////////////////////////////
// MACROS
////////////////////////////////////////////////////////////////////

#define CLK_HIGH      digitalWriteFast(uP_CLK, HIGH)
#define CLK_LOW       digitalWriteFast(uP_CLK, LOW)
#define STATE_RD_N    digitalReadFast(uP_RD_N)
#define STATE_WR_N    digitalReadFast(uP_WR_N)
#define STATE_MREQ_N  digitalReadFast(uP_MREQ_N)
#define STATE_IORQ_N  digitalReadFast(uP_IORQ_N)

byte ADDRpinTable[] = {
  // ADDR0..ADDR15
  MEGA_PA0,MEGA_PA1,MEGA_PA2,MEGA_PA3,MEGA_PA4,MEGA_PA5,MEGA_PA6,MEGA_PA7,
  MEGA_PC0,MEGA_PC1,MEGA_PC2,MEGA_PC3,MEGA_PC4,MEGA_PC5,MEGA_PC6,MEGA_PC7
};

byte DATApinTable[] = {
  // D0..D7
  MEGA_PL0,MEGA_PL1,MEGA_PL2,MEGA_PL3,MEGA_PL4,MEGA_PL5,MEGA_PL6,MEGA_PL7
};

void configure_PINMODE_ADDR()
{
  for (unsigned int i=0; i<sizeof(ADDRpinTable); i++)
  {
    pinMode(ADDRpinTable[i],INPUT);
  } 
}

void configure_PINMODE_DATA()
{
  for (unsigned int i=0; i<sizeof(DATApinTable); i++)
  {
    pinMode(DATApinTable[i],INPUT);
  } 
}

// ##################################################
#if (ARDUINO_TEENSY35 || ARDUINO_TEENSY36)
// ##################################################

// read bits raw
#define xDATA_DIR_IN()    (GPIOD_PDDR = (GPIOD_PDDR & 0xFFFFFF00))
#define xDATA_DIR_OUT()   (GPIOD_PDDR = (GPIOD_PDDR | 0x000000FF))
#define SET_DATA_OUT(D)   (GPIOD_PDOR = (GPIOD_PDOR & 0xFFFFFF00) | ( (byte) D))
#define xDATA_IN()        ((byte) (GPIOD_PDIR & 0xFF))

// Teensy has an LED on its digital pin13 (PTC5). which interferes w/
// level shifters.  So we instead pick-up A5 from PTA5 port and use
// PTC5 for PG0 purposes.
//
#define ADDR_H_RAW        ((word) (GPIOA_PDIR & 0b1111000000100000))
#define ADDR_L_RAW        ((word) (GPIOC_PDIR & 0b0000111111011111))
#define ADDR()            ((word) (ADDR_H_RAW | ADDR_L_RAW))

// ##################################################
#elif (ARDUINO_TEENSY41)
// ##################################################

// Teensy 4.1 has different port/pin assignments compared to Teeny 3.5 & 3.6
// so we have to do bit shuffling to construct address and data buses. On
// Teensy 3.5/3.6 we can read the whole ports in one access due to rerouting
// on the board.

// Teensy 4.1's 600Mhz seems to compensate for these delays by executing rest
// of the code faster.

inline __attribute__((always_inline))
void xDATA_DIR_IN()
{
  // for (int i=7; i>=0; i--)
  // {
  //   pinMode(DATApinTable[i],INPUT);
  // } 

  // Unroll the loop
  pinMode(MEGA_PL0, INPUT);
  pinMode(MEGA_PL1, INPUT);
  pinMode(MEGA_PL2, INPUT);
  pinMode(MEGA_PL3, INPUT);
  pinMode(MEGA_PL4, INPUT);
  pinMode(MEGA_PL5, INPUT);
  pinMode(MEGA_PL6, INPUT);
  pinMode(MEGA_PL7, INPUT);

}


inline __attribute__((always_inline))
void xDATA_DIR_OUT()
{
  // for (int i=7; i>=0; i--)
  // {
  //   pinMode(DATApinTable[i],OUTPUT);
  // } 

  // Unroll the loop
  pinMode(MEGA_PL0, OUTPUT);
  pinMode(MEGA_PL1, OUTPUT);
  pinMode(MEGA_PL2, OUTPUT);
  pinMode(MEGA_PL3, OUTPUT);
  pinMode(MEGA_PL4, OUTPUT);
  pinMode(MEGA_PL5, OUTPUT);
  pinMode(MEGA_PL6, OUTPUT);
  pinMode(MEGA_PL7, OUTPUT);

}

inline __attribute__((always_inline))
void SET_DATA_OUT(byte b)
{
  // for (int i=0; i<sizeof(DATApinTable); i++)
  // {
  //   digitalWrite(DATApinTable[i], (b & 1));
  //   b = b >> 1;
  // } 

  // Unroll the loop

  // MEGA_PL0,MEGA_PL1,MEGA_PL2,MEGA_PL3,MEGA_PL4,MEGA_PL5,MEGA_PL6,MEGA_PL7

  digitalWriteFast(MEGA_PL0, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL1, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL2, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL3, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL4, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL5, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL6, b & 1);
  b = b >> 1;
  digitalWriteFast(MEGA_PL7, b & 1);

}

inline __attribute__((always_inline))
byte xDATA_IN()
{
  byte b = 0;

  // for (int i=7; i>=0; i--)
  // {
  //   b = b << 1;
  //   b = b | digitalRead(DATApinTable[i]);
  // } 

  // Unroll the loop

  // MEGA_PL0,MEGA_PL1,MEGA_PL2,MEGA_PL3,MEGA_PL4,MEGA_PL5,MEGA_PL6,MEGA_PL7

  b = b | digitalReadFast(MEGA_PL7);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL6);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL5);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL4);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL3);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL2);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL1);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PL0);

  return b;
}


inline __attribute__((always_inline))
word ADDR()
{
  word b = 0;

  // for (int i=15; i>=0; i--)
  // {
  //   b = b << 1;
  //   b = b | digitalRead(ADDRpinTable[i]);
  // } 

  // Unroll the loop

  // A0..A15
  // MEGA_PA0,MEGA_PA1,MEGA_PA2,MEGA_PA3,MEGA_PA4,MEGA_PA5,MEGA_PA6,MEGA_PA7,
  // MEGA_PC0,MEGA_PC1,MEGA_PC2,MEGA_PC3,MEGA_PC4,MEGA_PC5,MEGA_PC6,MEGA_PC7

  b = b | digitalReadFast(MEGA_PC7);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC6);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC5);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC4);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC3);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC2);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC1);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PC0);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA7);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA6);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA5);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA4);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA3);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA2);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA1);
  b = b << 1;
  b = b | digitalReadFast(MEGA_PA0);

  return b;
}

// ##################################################
#endif
// ##################################################

void print_teensy_version()
{
#if (ARDUINO_TEENSY35)
  Serial.println("Teensy:     3.5");
#elif (ARDUINO_TEENSY36)
  Serial.println("Teensy:     3.6");
#elif (ARDUINO_TEENSY41)
  Serial.println("Teensy:     4.1");
#endif
}

#endif    // _PORTMAP_H



// Reference
//
// #define GPIO?_PDOR    (*(volatile uint32_t *)0x400FF0C0) // Port Data Output Register
// #define GPIO?_PSOR    (*(volatile uint32_t *)0x400FF0C4) // Port Set Output Register
// #define GPIO?_PCOR    (*(volatile uint32_t *)0x400FF0C8) // Port Clear Output Register
// #define GPIO?_PTOR    (*(volatile uint32_t *)0x400FF0CC) // Port Toggle Output Register
// #define GPIO?_PDIR    (*(volatile uint32_t *)0x400FF0D0) // Port Data Input Register
// #define GPIO?_PDDR    (*(volatile uint32_t *)0x400FF0D4) // Port Data Direction Register
//