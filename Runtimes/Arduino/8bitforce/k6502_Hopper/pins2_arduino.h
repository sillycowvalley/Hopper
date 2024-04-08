/*
 * pins2_arduino.h
 *
 *  Created on: 12. 2. 2014
 *      Author: Jan Dolinay
 *  Adapted by: Thierry Paris
 *
 *  Alternate version of digital input/output for Arduino.
 *  This file is for Arduino Mega.
 *
 *  Howto port this file to another Arduino variant:
 *  1) Define GPIO_pin_t enum with pin codes
 *  2) Set the number of pins to proper value in GPIO_PINS_NUMBER 
 *  3) Check/modify the macros for getting addresses of I/O registers
 *  4) Define the gpio_pins_progmem array with the values from GPIO_pin_t enum
 *  
 * Notes:
 *  Step 1) you need the datasheet of the AVR MCU used in your variant. Check the
 *  addresses of the registers for controlling GPIO ports (typically there is
 *  "Register summary" chapter in the datasheet with the addresses. 
 *  See the definition below for Atmega328 as an example.
 *  Step 3) if the addresses of all the GPIO registers are lower than 0xFF, you 
 *  can use the simple macros as defined here.
 *  If there are some registers with higher address (such as the case in 
 *  Atmega 2560 used in Arduino Mega), we need to encode the address into
 *  single byte or use a different approach. Use the macros defined in 
 *  pins2_arduino.h for Arduino mega.
 *  
 *  
  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

 */

#ifndef PINS2_ARDUINO_MEGA_H_
#define PINS2_ARDUINO_MEGA_H_

// ===========================================================================
// Configuration
// ===========================================================================
//
// GPIO2_PREFER_SPEED - User can define this value before including arduino2.h
// or this default value is used.
// Set to 1 to make all digital I/O functions inline.
// This will make them work faster but the resulting code will be bigger.
// In general, you can start with 1 and if you are running out of program memory
// switch to 0.
//
#ifndef GPIO2_PREFER_SPEED
	#define		GPIO2_PREFER_SPEED	1
#endif

// GPIO2_IOREGS_ABOVEFF - set to one if the Atmega used for this Arduino variant has
// some I/O registers at address higher than 0xff (e.g. Atmega2560 in Arduino Mega).
// In this case we must always disable interrupts in digitalWrite and pinMode,
// because the registers will not be manipulated by single instruction (SBI, CBI),
// because these instructions only work on locations below 0xFF.
//
// For Arduino Standard (ATmega328) set to 0
// For Arduino Mega (ATmega1280 or ATmega2560) set to 1
// If not sure, set to 1 (this is always safe option)
//
#define	  GPIO2_IOREGS_ABOVEFF	1


// Unfortunately, we cannot use the PORTA etc. definitions from avr/io.h in the
// GPIO_MAKE_PINCODE macro, so I define the port register addresses here to make
// it more comfortable to define the pins
// This is the address of the port register, e.g. PORTA or PORTB, from the datasheet.
#define		MYPORTA		(0x22)
#define		MYPORTB		(0x25)
#define		MYPORTC		(0x28)
#define		MYPORTD		(0x2B)
#define		MYPORTE		(0x2E)
#define		MYPORTF		(0x31)
#define		MYPORTG		(0x34)
#define		MYPORTH		(0x0102)
#define		MYPORTJ		(0x0105)
#define		MYPORTK		(0x0108)
#define		MYPORTL		(0x010B)

// Helper macro to create the pin code from port register address and pin number.
// If the port address is above 0xFF, we set the bit 7 in the lower byte of pin code.
#define	GPIO_MAKE_PINCODE(port, pin)  ((uint16_t)port > 0xFF ) ? \
		(((uint16_t)port & 0x00FF) | ((1<<pin) << 8) | 0x0080 ) : \
		(((uint16_t)port & 0x00FF) | ((1<<pin) << 8))

/*
 * GPIO_pin_t
 * Define the type for digital I/O pin.
 * We will not use simple integer (int) to identify a pin.
 * Instead we use special code, which contains the address of the I/O register
 * for given pin (lower byte) together with its bit mask (upper byte).
 * For this code we create our special data type (GPIO_pin_t) which will prevent
 * the user from calling our digitalRead/Write with invalid pin numbers.
 *
 * Arduino Mega note: The registers for port H and above have addresss bigger than
 * 1 byte, so we have to work with the pin code more than in standard Arduino.
 * Adress ranges:
 * PINA  	0 (+0x20) = 0x20
 * DDRA  	1
 * PORTA 	2
 * PINB		3
 * ...
 * PORTG    0x14 (+0x20) = 0x34
 * not used
 * PINH		0x100
 * DDRH		0x101
 * PORTH	0x102
 * ...
 * PORTL	0x10B
 *
 * We will use the upper bit of the address byte in our pin code (0x10) to indicate
 * that the address should be computed like this: AND this bit and add 0x100
 *
*/
enum GPIO_pin_enum
{
	// Note: The invalid value can be any valid port register
	// - as long as the bit mask in upper byte is 0, the
	// operation on this register will have no effect.
	DP_INVALID = 0x0020,
	DP0 = GPIO_MAKE_PINCODE(MYPORTE,0),		// PE0
	DP1 = GPIO_MAKE_PINCODE(MYPORTE,1),		// PE1
	DP2 = GPIO_MAKE_PINCODE(MYPORTE,4),		// PE4
	DP3 = GPIO_MAKE_PINCODE(MYPORTE,5),
	DP4 = GPIO_MAKE_PINCODE(MYPORTG,5),
	DP5 = GPIO_MAKE_PINCODE(MYPORTE,3),
	DP6 = GPIO_MAKE_PINCODE(MYPORTH,3),
	DP7 = GPIO_MAKE_PINCODE(MYPORTH,4),
	DP8 = GPIO_MAKE_PINCODE(MYPORTH,5),
	DP9 = GPIO_MAKE_PINCODE(MYPORTH,6),
	DP10 = GPIO_MAKE_PINCODE(MYPORTB,4),
	DP11 = GPIO_MAKE_PINCODE(MYPORTB,5),

	DP12 = GPIO_MAKE_PINCODE(MYPORTB,6),
	DP13 = GPIO_MAKE_PINCODE(MYPORTB,7),
	DP14 = GPIO_MAKE_PINCODE(MYPORTJ,1),
	DP15 = GPIO_MAKE_PINCODE(MYPORTJ,0),
	DP16 = GPIO_MAKE_PINCODE(MYPORTH,1),
	DP17 = GPIO_MAKE_PINCODE(MYPORTH,0),
	DP18 = GPIO_MAKE_PINCODE(MYPORTD,3),
	DP19 = GPIO_MAKE_PINCODE(MYPORTD,2),
	DP20 = GPIO_MAKE_PINCODE(MYPORTD,1),

	DP21 = GPIO_MAKE_PINCODE(MYPORTD,0),
	DP22 = GPIO_MAKE_PINCODE(MYPORTA,0),
	DP23 = GPIO_MAKE_PINCODE(MYPORTA,1),
	DP24 = GPIO_MAKE_PINCODE(MYPORTA,2),
	DP25 = GPIO_MAKE_PINCODE(MYPORTA,3),
	DP26 = GPIO_MAKE_PINCODE(MYPORTA,4),
	DP27 = GPIO_MAKE_PINCODE(MYPORTA,5),
	DP28 = GPIO_MAKE_PINCODE(MYPORTA,6),
	DP29 = GPIO_MAKE_PINCODE(MYPORTA,7),
	DP30 = GPIO_MAKE_PINCODE(MYPORTC,7),

	DP31 = GPIO_MAKE_PINCODE(MYPORTC,6),
	DP32 = GPIO_MAKE_PINCODE(MYPORTC,5),
	DP33 = GPIO_MAKE_PINCODE(MYPORTC,4),
	DP34 = GPIO_MAKE_PINCODE(MYPORTC,3),
	DP35 = GPIO_MAKE_PINCODE(MYPORTC,2),
	DP36 = GPIO_MAKE_PINCODE(MYPORTC,1),
	DP37 = GPIO_MAKE_PINCODE(MYPORTC,0),
	DP38 = GPIO_MAKE_PINCODE(MYPORTD,7),
	DP39 = GPIO_MAKE_PINCODE(MYPORTG,2),
	DP40 = GPIO_MAKE_PINCODE(MYPORTG,1),
	DP41 = GPIO_MAKE_PINCODE(MYPORTG,0),

	DP42 = GPIO_MAKE_PINCODE(MYPORTL,7),
	DP43 = GPIO_MAKE_PINCODE(MYPORTL,6),
	DP44 = GPIO_MAKE_PINCODE(MYPORTL,5),
	DP45 = GPIO_MAKE_PINCODE(MYPORTL,4),
	DP46 = GPIO_MAKE_PINCODE(MYPORTL,3),
	DP47 = GPIO_MAKE_PINCODE(MYPORTL,2),
	DP48 = GPIO_MAKE_PINCODE(MYPORTL,1),
	DP49 = GPIO_MAKE_PINCODE(MYPORTL,0),

	DP50 = GPIO_MAKE_PINCODE(MYPORTB,3),
	DP51 = GPIO_MAKE_PINCODE(MYPORTB,2),
	DP52 = GPIO_MAKE_PINCODE(MYPORTB,1),
	DP53 = GPIO_MAKE_PINCODE(MYPORTB,0),

	DP54 = GPIO_MAKE_PINCODE(MYPORTF,0),
	DP55 = GPIO_MAKE_PINCODE(MYPORTF,1),
	DP56 = GPIO_MAKE_PINCODE(MYPORTF,2),
	DP57 = GPIO_MAKE_PINCODE(MYPORTF,3),
	DP58 = GPIO_MAKE_PINCODE(MYPORTF,4),
	DP59 = GPIO_MAKE_PINCODE(MYPORTF,5),
	DP60 = GPIO_MAKE_PINCODE(MYPORTF,6),
	DP61 = GPIO_MAKE_PINCODE(MYPORTF,7),

	DP62 = GPIO_MAKE_PINCODE(MYPORTK,0),
	DP63 = GPIO_MAKE_PINCODE(MYPORTK,1),
	DP64 = GPIO_MAKE_PINCODE(MYPORTK,2),
	DP65 = GPIO_MAKE_PINCODE(MYPORTK,3),
	DP66 = GPIO_MAKE_PINCODE(MYPORTK,4),
	DP67 = GPIO_MAKE_PINCODE(MYPORTK,5),
	DP68 = GPIO_MAKE_PINCODE(MYPORTK,6),
	DP69 = GPIO_MAKE_PINCODE(MYPORTK,7),

};

#define DPA0	DP54
#define DPA1	DP55
#define DPA2	DP56
#define DPA3	DP57
#define DPA4	DP58
#define DPA5	DP59
#define DPA6	DP60
#define DPA7	DP61
#define DPA8	DP62
#define DPA9	DP63
#define DPA10	DP64
#define DPA11	DP65
#define DPA12	DP66
#define DPA13	DP67
#define DPA14	DP68
#define DPA15	DP69

#define A0		54
#define A1		55
#define A2		56
#define A3		57
#define A4		58
#define A5		59
#define A6		60
#define A7		61
#define A8		62
#define A9		63
#define A10		64
#define A11		65
#define A12		66
#define A13		67
#define A14		68
#define A15		69

typedef	enum GPIO_pin_enum  GPIO_pin_t;

// Number of GPIO pins.
// Used in Arduino_to_GPIO_pin
#define		GPIO_PINS_NUMBER		(70)

// Macro to obtain bit mask of a pin from its code
#define		GPIO_PIN_MASK(pin)		((uint8_t)((uint16_t)pin >> 8))

// Macros to obtain the addresses of various I/O registers from pin code
// Macro to obtain port register address with support for addresses above 0xff
// NOTE: do not use for devices which have all GPIO registers under 0xFF as it would
// add unnecessary test (and) to the code for non-const pins.
#define		GET_PORT_REG_ADR(pin)	((((pin) & 0x0080) == 0) ? \
			((volatile uint8_t*)((pin) & 0x00FF)) : \
			((volatile uint8_t*)(((pin) & 0x007F) | 0x0100)))

#define		GET_PIN_REG_ADR(pin)		(GET_PORT_REG_ADR(pin)-2)
#define		GET_DDR_REG_ADR(pin)		(GET_PORT_REG_ADR(pin)-1)

// Macros which allow us to refer to the I/O registers directly
#define   GPIO_PIN_REG(pin)    (*(volatile uint8_t*)GET_PIN_REG_ADR(pin) )
#define   GPIO_PORT_REG(pin)    (*(volatile uint8_t*)GET_PORT_REG_ADR(pin))
#define   GPIO_DDR_REG(pin)    (*(volatile uint8_t*)GET_DDR_REG_ADR(pin) )



// ARDUINO2_MAIN should be defined only once in the program so that the
// gpio_pins_progmem is not duplicated. This is done in digital2.c
#ifdef ARDUINO2_MAIN

// This array in program memory (FLASH) maps Arduino pin number (simple integer)
// to our GPIO pin code.
// The Arduino pin number is used as index into this array. Value at given index
// N is the pin code for the Arduino pin N.
const GPIO_pin_t PROGMEM gpio_pins_progmem[] = {
		DP0, DP1, DP2, DP3,	DP4,
		DP5, DP6, DP7, DP8, DP9,
		DP10, DP11, DP12, DP13, DP14,
		DP15, DP16, DP17, DP18, DP19,
		DP20, DP21, DP22, DP23, DP24,
		DP25, DP26, DP27, DP28,	DP29,
		DP30, DP31, DP32, DP33,	DP34,
		DP35, DP36, DP37, DP38,	DP39,
		DP40, DP41, DP42, DP43,	DP44,
		DP45, DP46, DP47, DP48,	DP49,
		DP50, DP51, DP52, DP53,	DP54,
		DP55, DP56, DP57, DP58,	DP59,
		DP60, DP61, DP62, DP63,	DP64,
		DP65, DP66, DP67, DP68,	DP69,
};
#else
	extern const GPIO_pin_t PROGMEM gpio_pins_progmem[];
#endif

#define GPIO_TO_ARDUINO			case (uint16_t)DP1:  return 1; \
								case (uint16_t)DP2:  return 2;	\
								case (uint16_t)DP3:  return 3;	\
								case (uint16_t)DP4:  return 4;	\
								case (uint16_t)DP5:  return 5;	\
								case (uint16_t)DP6:  return 6;	\
								case (uint16_t)DP7:  return 7;	\
								case (uint16_t)DP8:  return 8;	\
								case (uint16_t)DP9:  return 9;	\
								case (uint16_t)DP10: return 10;	\
								case (uint16_t)DP11: return 11;	\
								case (uint16_t)DP12: return 12;	\
								case (uint16_t)DP13: return 13;	\
								case (uint16_t)DP14: return 14;	\
								case (uint16_t)DP15: return 15;	\
								case (uint16_t)DP16: return 16;	\
								case (uint16_t)DP17: return 17;	\
								case (uint16_t)DP18: return 18;	\
								case (uint16_t)DP19: return 19; \
								case (uint16_t)DP20: return 20; \
								case (uint16_t)DP21: return 21; \
								case (uint16_t)DP22: return 22; \
								case (uint16_t)DP23: return 23; \
								case (uint16_t)DP24: return 24; \
								case (uint16_t)DP25: return 25; \
								case (uint16_t)DP26: return 26; \
								case (uint16_t)DP27: return 27; \
								case (uint16_t)DP28: return 28; \
								case (uint16_t)DP29: return 29; \
								case (uint16_t)DP30: return 30; \
								case (uint16_t)DP31: return 31; \
								case (uint16_t)DP32: return 32; \
								case (uint16_t)DP33: return 33; \
								case (uint16_t)DP34: return 34; \
								case (uint16_t)DP35: return 35; \
								case (uint16_t)DP36: return 36; \
								case (uint16_t)DP37: return 37; \
								case (uint16_t)DP38: return 38; \
								case (uint16_t)DP39: return 39; \
								case (uint16_t)DP40: return 40; \
								case (uint16_t)DP41: return 41; \
								case (uint16_t)DP42: return 42; \
								case (uint16_t)DP43: return 43; \
								case (uint16_t)DP44: return 44; \
								case (uint16_t)DP45: return 45; \
								case (uint16_t)DP46: return 46; \
								case (uint16_t)DP47: return 47; \
								case (uint16_t)DP48: return 48; \
								case (uint16_t)DP49: return 49; \
								case (uint16_t)DP50: return 50; \
								case (uint16_t)DP51: return 51; \
								case (uint16_t)DP52: return 52; \
								case (uint16_t)DP53: return 53; \
								case (uint16_t)DP54: return 54; \
								case (uint16_t)DP55: return 55; \
								case (uint16_t)DP56: return 56; \
								case (uint16_t)DP57: return 57; \
								case (uint16_t)DP58: return 58; \
								case (uint16_t)DP59: return 59; \
								case (uint16_t)DP60: return 60; \
								case (uint16_t)DP61: return 61; \
								case (uint16_t)DP62: return 62; \
								case (uint16_t)DP63: return 63; \
								case (uint16_t)DP64: return 64; \
								case (uint16_t)DP65: return 65; \
								case (uint16_t)DP66: return 66; \
								case (uint16_t)DP67: return 67; \
								case (uint16_t)DP68: return 68; \
								case (uint16_t)DP69: return 69;

#endif /* PINS2_ARDUINO_MEGA_H_ */
