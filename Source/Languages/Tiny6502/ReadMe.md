# Tiny6502 Language Documentation

## Introduction

Tiny6502 is a minimal programming language designed for a specific 6502 system configuration like the Hopper 6502 SBC, including a VIA, an ACIA, and a serial I2C EEPROM. It combines the simplicity and performance of low-level programming with higher code density and type safety, tailored for embedded systems. The language syntax is familiar to C programmers, making it easy to learn and use.

## Key Features

1. **Low-Level Access:**
   - Direct access to registers, memory locations, and I/O ports.
   - Inline assembly support for critical performance sections.

2. **Basic Data Types:**
   - `byte` (8-bit unsigned integer)
   - `word` (16-bit unsigned integer)
   - `char` (8-bit unsigned character)
   - `bool` (boolean type, stored as a single bit)

3. **Control Structures:**
   - Simple, C-like control structures: `if`, `while`, `for`, and `switch`, with strict type expectations for boolean expressions.
   - Minimal syntactic sugar to keep parsing simple.

4. **Function Calls:**
   - Support for basic functions with a fixed number of parameters (no overloading).
   - Limited stack-based function call mechanism to keep overhead low.

5. **Memory Management:**
   - Simple manual memory management using `malloc` and `free`.
   - No automatic garbage collection to save on overhead.

6. **I/O Operations:**
   - Built-in functions for serial communication, EEPROM read/write, GPIO manipulation, and I2C communication.
   - Access to timers for millisecond system tick and delay operations.

7. **Const Strings:**
   - Use `const char[]` for defining string literals for use with writeString.

8. **Boolean Arrays:**
   - `bool[]` for efficient bit arrays (8 `bool` values per byte).

## Syntax and Semantics

### Variable Declarations

```c
byte a = 0x10;       // 8-bit unsigned integer
word b = 0x1234;     // 16-bit unsigned integer
char c = 'A';        // 8-bit unsigned character
bool flag = true;    // Boolean value
```

### Const Strings

```c
const char[] message = "Hello, 6502!";
```

### Memory Access

```c
a = [0x2000];        // Load from memory
[0x2000] = a;        // Store to memory
```

### Memory Management

```c
byte[] ptr = malloc(10); // Allocate 10 bytes
free(ptr);               // Free allocated memory
```

### Functions

```c
func add(byte x, byte y) -> byte {
    return x + y;
}
```

### Control Structures

```c
if (flag) {
    [0x2000] = 0xFF;
}

while (a != 0) {
    a--;
}

for (byte i = 0; i < 10; i++) {
    [0x2000 + i] = i;
}
```

### Type Casting

Explicit type casting using `as`:

```c
word wide_value = 0x1234;
byte narrow_value = wide_value as byte; // Cast word to byte, keeping the least significant 8 bits

byte narrow_value2 = 0x34;
word wide_value2 = narrow_value2 as word; // Cast byte to word, zero-extends the 8-bit value

byte value = 1;
bool flag = value as bool; // Cast byte to boolean
value = flag as byte; // Cast boolean to byte
```

## Built-in Functions and Constants

### Serial Communication

```c
func write(byte data);
func byte read();
func writeString(const char[] str);
```

### EEPROM

```c
func writePage(word address, const byte[] data);
func readPage(word address, byte[] buffer);
```

### GPIO (Pin) Control

```c
func pinSet(byte pin, bool value);
func bool pinRead(byte pin);
func pinMode(byte pin, byte mode);
```

### I2C

```c
func I2CWrite(byte address, byte data);
func byte I2CRead(byte address);
```

### Timing

```c
func word millis();
func delay(word milliseconds);
```

### Memory Management

```c
func byte[] malloc(word size);
func free(byte[] ptr);
```

### Constants for Pin Modes

```c
const byte INPUT = 0;
const byte OUTPUT = 1;
const byte INPUT_PULLUP = 2;
```

## Including Other Source Files

Use the `import` keyword to include other source files. This allows modular organization and reuse of common definitions and functions.

### Example System API File

**system.tc**

```c
// Pin Modes
const byte INPUT = 0;
const byte OUTPUT = 1;
const byte INPUT_PULLUP = 2;

// Serial Communication
func write(byte data);
func byte read();
func writeString(const char[] str);

// EEPROM
func writePage(word address, const byte[] data);
func readPage(word address, byte[] buffer);

// GPIO (Pin) Control
func pinSet(byte pin, bool value);
func bool pinRead(byte pin);
func pinMode(byte pin, byte mode);

// I2C
func I2CWrite(byte address, byte data);
func byte I2CRead(byte address);

// Timing
func word millis();
func delay(word milliseconds);

// Memory Management
func byte[] malloc(word size);
func free(byte[] ptr);
```

### Example Main Program File

**main.tc**

```c
import "system.tc"

// Blink an LED on GPIO pin 0 every second and send a message over serial
func main() {
    byte pin = 0;
    const char[] welcome_message = "Hello, 6502!";
    bool led_on = false;
    
    pinMode(pin, OUTPUT); // Set pin mode to output
    pinSet(pin, false); // Ensure LED is off
    writeString(welcome_message); // Send welcome message

    // Allocate memory for a boolean array
    bool[] flags = malloc(1); // 8 boolean flags

    // Set the first flag to true
    flags[0] = true;

    // Main loop
    while (true) {
        if (led_on) {
            pinSet(pin, false); // Turn LED off
            led_on = false;
        } else {
            pinSet(pin, true); // Turn LED on
            led_on = true;
        }
        delay(1000); // Delay for 1 second
    }
}
```

## Example EEPROM Page Read/Write Program

```c
func main() {
    word eeprom_address = 0x100;
    byte[] data_to_write = malloc(16);
    byte[] read_buffer = malloc(16);
    
    // Initialize data to write
    for (byte i = 0; i < 16; i++) {
        data_to_write[i] = i;
    }
    
    // Write a page to EEPROM
    writePage(eeprom_address, data_to_write);
    
    // Read a page from EEPROM
    readPage(eeprom_address, read_buffer);
    
    // Verify the read data
    for (byte i = 0; i < 16; i++) {
        if (read_buffer[i] != i) {
            writeString("Read verification failed.");
            return;
        }
    }
    
    writeString("Read verification successful.");
}
```
