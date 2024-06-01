# nanoFORTH Documentation

## Overview

nanoFORTH is a simple and useful FORTH for Arduino Nano, designed to be an interactive and minimalist system. It aims to provide an easy-to-use development environment with a small set of core words and functionality that meet the needs of most Arduino projects.

## Assumptions

- More than 80% of Arduino makers use UNO or Nano.
- Most makers do not need the full-blown FORTH vocabularies.
- Abbreviation for words is acceptable as most users are not familiar with standard FORTH words.
- A meta-compiler is unnecessary, as creating a new type of FORTH within nanoFORTH is not a common requirement.
- A small set of core primitive words (50+) should suffice for most Arduino projects.

## Requirements

- Simple usage akin to example sketches from the Arduino IDE.
- REPL development/operating environment for Arduino.
- Core Arduino functions: `pinMode`, `digitalRead/Write`, `analogRead/Write`, `millis`, `delay`.
- Arduino library for easy inclusion by developers.
- At least 1K RAM dictionary for reasonable project sizes.
- Persistent storage for user-defined words using EEPROM.
- C API for integrating user-defined functions/components.
- Timer interrupt handler for multi-tasking.
- Pin change interrupt handler for hardware triggers.
- Assembly trace for beginners to understand FORTH internals.
- Execution trace for debugging and single-stepping.
- Autorun capability after reboot from a saved EEPROM image.

## Use Cases - Interaction Examples

### Turn on LED (Red) on Digital Pin 5

```
1 5 OUT ⏎
```

### Turn off LED (Blue) on Digital Pin 6

```
0 6 OUT ⏎
```

### Define a Function (Word) to Control LEDs

```forth
: red 1 5 OUT 0 6 OUT ; ⏎
: blu 0 5 OUT 1 6 OUT ; ⏎
```

### Execute the Defined Words

```forth
blu ⏎
```

### Define a Word to Blink LEDs

```forth
: xy FOR red 500 DLY blu 500 DLY NXT ; ⏎
```

### Run the Blink Function for 10 Cycles

```forth
10 xy ⏎
```

### Redefine the Blink Function with Different Timing

```forth
FGT xy ⏎
: xy FOR red 200 DLY blu 300 DLY I . NXT ; ⏎
20 xy ⏎
```

### Read an Analog Value from Pin 1

```forth
1 AIN ⏎
```

### Clean Data Stack

```forth
DRP ⏎
```

### Define a Word to Read and Compare Analog Value

```forth
: lit 1 AIN 200 > ; ⏎
lit ⏎
```

### Define a Conditional Word to Control LEDs

```forth
: ?z IF red ELS blu THN ; ⏎
1 ?z ⏎
0 ?z ⏎
```

### Continuously Check Analog Value and Control LEDs Based on the Value

```forth
: xyz BGN lit ?z 7 IN UTL ; ⏎
xyz ⏎
```

### List All Defined Words

```forth
WRD ⏎
```

## Installation

### From Arduino IDE's Library Manager

1. Hook up an Arduino Nano/Uno or compatible development board.
2. Open Arduino IDE, navigate to `Tools > Manage Libraries`.
3. Search for `nanoFORTH` and install the latest version.
4. Load an example sketch from `Files > Examples > nanoForth`.
5. Open Serial Monitor, set baud rate to 115200, and line ending to 'Both NL & CR'.
6. Compile and upload the sketch. You should see the 'ok' prompt, and the built-in LED should blink.
7. In Serial Monitor, type `WRD` and press return to see nanoFORTH's response.

### From GitHub

1. Clone the repository: `git clone https://github.com/chochain/nanoFORTH`.
2. Copy `examples/0_blink/0_blink.ino` to your local sketch directory and rename it to `nanoFORTH.ino`.
3. Open `nanoFORTH.ino` with Arduino IDE and setup your development board.
4. Change `#include <nanoFORTH.h>` to `#include "./src/nanoFORTH.h"`.
5. Open Serial Monitor, set baud rate to 115200, and line ending to 'Both NL & CR'.
6. Compile and upload the sketch. You should see the 'ok' prompt, and the built-in LED should blink.
7. In Serial Monitor, type `WRD` and press return to see nanoFORTH's response.

## Built-in Words

### Stack Operations

| Opcode | Stack    | Description                        |
|--------|----------|------------------------------------|
| DRP    | ( w -- ) | Drop                               |
| DUP    | ( w -- w w ) | Duplicate                        |
| SWP    | ( a b -- b a ) | Swap                           |
| OVR    | ( a b -- a b a ) | Over                         |
| ROT    | ( a b c -- b c a ) | Rotate                      |

### Arithmetic Operations

| Opcode | Stack       | Description                  |
|--------|-------------|------------------------------|
| +      | ( a b -- a+b ) | Add                         |
| -      | ( a b -- a-b ) | Subtract                    |
| *      | ( a b -- a*b ) | Multiply                    |
| /      | ( a b -- a/b ) | Divide                      |
| MOD    | ( a b -- a%b ) | Modulo                      |
| NEG    | ( a -- -a )    | Negate                      |
| ABS    | ( a -- abs(a) )| Absolute value of a        |
| MIN    | ( a b -- min(a, b) ) | Minimum value between a and b |
| MAX    | ( a b -- max(a, b) ) | Maximum value between a and b |

### Binary and Logical Operations

| Opcode | Stack              | Description             |
|--------|--------------------|-------------------------|
| AND    | ( a b -- a&b )     | Binary and              |
| OR     | ( a b -- a\|b )    | Binary or               |
| XOR    | ( a b -- a^b )     | Binary xor              |
| NOT    | ( a -- ^a )        | Binary not              |
| LSH    | ( n i -- n<<=i )   | Left shift              |
| RSH    | ( n i -- n>>=i )   | Right shift             |
| =      | ( a b -- a==b )    | Equal                   |
| <      | ( a b -- a<b )     | Less than               |
| >      | ( a b -- a>b )     | Greater than            |
| <>     | ( a b -- a!=b )    | Not equal               |

### Word Definition and Dictionary Operations (Interactive Mode Only)

| Opcode | Stack      | Description                       |
|--------|------------|-----------------------------------|
| :      | ( -- )     | Start defining a new word         |
| ;      | ( -- )     | End of word definition            |
| WRD    | ( -- )     | List all words in the dictionary  |
| HRE    | ( -- w )   | Get current user dictionary pointer |
| FGT    | ( -- )     | Forget/remove functions           |

### Flow Control (Compiler Mode Only)

| Branching Ops      | Description                   |
|--------------------|-------------------------------|
| f IF xxx THN       | Conditional branch            |
| f IF xxx ELS yyy THN | Conditional branch with else |
| BGN xxx f UTL      | Begin-Until loop              |
| BGN xxx f WHL yyy RPT | Begin-While-Repeat loop     |
| n FOR xxx NXT      | For loop, index value I counts down from n to 1 |

### Return Stack Operations

| Opcode | Stack    | Description                            |
|--------|----------|----------------------------------------|
| I      | ( -- w ) | Fetch word from top of return stack (R@) |
| >R     | ( w -- ) | Push word from data stack onto return stack |
| R>     | ( -- w ) | Pop top of return stack value onto data stack |

### Memory Access Operations

| Opcode | Stack    | Description                            |
|--------|----------|----------------------------------------|
| @      | ( a -- w ) | Fetch 16-bit value from memory address `a` |
| !      | ( a w -- ) | Store 16-bit value to memory address `a`   |
| C@     | ( a -- w ) | Fetch byte from memory address `a`         |
| C!     | ( a w -- ) | Store byte (or lower byte of word) to memory address `a` |

### Variable, Constant, and Array Operations

| Opcode | Stack    | Description                            |
|--------|----------|----------------------------------------|
| VAR    | ( -- )   | Define a 16-bit variable               |
| VAL    | ( w -- ) | Define a 16-bit value (constant)       |
| ALO    | ( w -- ) | Allocate extra `w` bytes on user dictionary (for array) |

### Console I/O

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| KEY    | ( -- c )  | Get a byte from input console         

 |
| EMT    | ( c -- )  | Write a byte to output console         |
| CR     | ( -- )    | Send a <return> to console             |
| .      | ( w -- )  | Print value from data stack to output  |
| ."     | ( -- )    | Send following string to console       |
| S"     | ( -- a n )| Put string address and length on TOS   |
| TYP    | ( a n -- )| Type string at address with length `n` |

### Reset, Debug, and Tracing

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| BYE    | ( -- )    | Reset nanoFORTH on Arduino, exit to OS on other platform |
| DMP    | ( a w -- )| Dump nanoFORTH user dictionary from address `a` for `w` bytes |
| TRC    | ( t -- )  | Enable/disable execution tracing      |

### EEPROM Access

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| SAV    | ( -- )    | Save user dictionary into Arduino Flash Memory |
| LD     | ( -- )    | Restore user dictionary from Arduino Flash Memory |
| SEX    | ( -- )    | Save with autorun flag set in EEPROM for reboot/execution |

### Arduino Specific Operations

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| CLK    | ( -- d )  | Fetch Arduino millis() value onto data stack as double number |
| DLY    | ( w -- )  | Wait milliseconds (yield to hardware tasks) |
| PIN    | ( w p -- )| Set pinMode(p, w)                      |
| IN     | ( p -- w )| digitalRead(p)                         |
| OUT    | ( w p -- )| if p=0x0xx, digitalWrite(xx, w), if p=0x1xx, multi-port write to PORTD, if p=0x2xx, multi-port write to PORTB, if p=0x3xx, multi-port write to PORTC |
| AIN    | ( p -- w )| analogRead(p)                          |
| PWM    | ( w p -- )| analogWrite(p, w)                      |

### C API Function Call

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| API    | ( n -- )  | Call API by number registered via ef_api(n, func) in Arduino sketch |

### Interrupt Operations

| Opcode | Stack     | Description                            |
|--------|-----------|----------------------------------------|
| TMI    | ( n i -- )| Set timer ISR with period `n` microseconds |
| PCI    | ( p -- )  | Capture pin `p` change (HIGH to LOW or LOW to HIGH) |
| TME    | ( f -- )  | Enable/disable timer interrupt, 0:disable, 1:enable |
| PCE    | ( f -- )  | Enable/disable pin change interrupt, 0:disable, 1:enable |

### Double Precision Arithmetic (for Arduino Clock)

| Opcode | Stack          | Description                     |
|--------|----------------|---------------------------------|
| D+     | ( d1 d0 -- d1+d0 ) | Add two doubles              |
| D-     | ( d1 d0 -- d1-d0 ) | Subtract two doubles         |
| DNG    | ( d0 -- -d0 )     | Negate a double number       |

## Meta Programming (Available only via source recompilation with N4_META set to 1)

| Opcode | Stack      | Description                            |
|--------|------------|----------------------------------------|
| CRE    | ( -- )     | Create a word with link and name field |
| ,      | ( n -- )   | Comma, add 16-bit value onto dictionary |
| C,     | ( n -- )   | Byte comma, add 8-bit value onto dictionary |
| '      | ( -- xt )  | Tick, fetch xt (parameter field address) of a word |
| EXE    | ( xt -- )  | Execute an xt address                  |

## Function/Word Structure

| Size      | Field                          |
|-----------|--------------------------------|
| Fixed 16-bit | Address to previous word, 0xffff is terminator |
| Fixed 3-byte | Function name               |
| n-byte      | Compiled opcodes or address of user-defined word |

## Opcode Memory Formats

| Opcode       | Stack           | Description                       |
|--------------|-----------------|-----------------------------------|
| 1-byte literal | 0nnn nnnn        | 0..127, often used, speeds up core |
| 3-byte literal | 1011 1111 snnn nnnn nnnn nnnn | 16-bit signed integer |
| 1-byte primitive | 10oo oooo        | 6-bit opcode i.e. 64 primitives |
| Branching opcodes | 11BB aaaa aaaa aaaa | 12-bit address i.e. 4K space |
| n-byte string | len, byte, byte, ... | 256 bytes max, used in print string |

---

This documentation covers the key aspects of nanoFORTH, including its use cases, installation steps, built-in words, and operations.
nanoFORTH provides a compact, interactive environment for Arduino projects, enabling easy and efficient development without the need for frequent recompilation.