# Tiny6502 Language Documentation

## Introduction

Tiny6502 is a minimal programming language designed for a specific 6502 system configuration like the Hopper 6502 SBC, including a VIA, an ACIA, and a serial I2C EEPROM. It combines the simplicity and performance of low-level programming with higher code density and type safety, tailored for embedded systems. The language syntax is familiar to C programmers, making it easy to learn and use.

### Key Differences Between Tiny6502 and C

- **Pointers:** Pointers in Tiny6502 can only originate from the `malloc()` API, which defaults to returning a byte array pointer written as `byte[]`.
- **Local Arrays:** Local arrays can be declared without calling `malloc` by giving them a dimension, e.g., `byte[100] hundredBytes;`. These arrays are automatically freed when the scope exits.
- **Stricter Type Safety:** Tiny6502 requires explicit casts between incompatible types. Implicit castless conversions are only allowed from narrower numeric types to wider ones. Casting between `char` and any other type, `bool` and any other type, and between `int` and `word` is required. However, `byte` can be converted to `int` or `word` without a cast.
- **Pointer Casting:** Explicit casting for pointers is required, e.g., `char[] = malloc(10) as char[]`.
- **Function Declarations:** Use the `func` keyword for function declarations.
- **No `void` Keyword:** There is no `void` keyword in Tiny6502.
- **Switch Case:** The `done` keyword is used to prevent fall-through in switch cases, instead of `break`, which is only used for exiting loops.
- **Preprocessor Symbols:** Preprocessor symbols can only be defined or undefined with `#define` and `#undef`. They cannot have values or macro content associated with them.
- **Initialization:** All variables and memory allocation blocks are zero-initialized.
- **Boolean Expressions:** `if`, `while`, and `for` require their boolean expressions to be of `bool` type.
- **Include Directive:** `#include` directive inline source into one large source file in the preprocessor. A second `#include` directive for the same source file is ignored. There are no header files, only source files with the `.tc` extension.

## Key Features

1. **Low-Level Access:**
   - Direct access to registers, memory locations, and I/O ports.
   - Inline assembly support for critical performance sections.

2. **Basic Data Types:**
   - `byte` (8-bit unsigned integer)
   - `word` (16-bit unsigned integer)
   - `int` (16-bit signed integer)
   - `char` (8-bit unsigned character)
   - `bool` (boolean type, stored as a single bit)

3. **Control Structures:**
   - Simple, C-like control structures: `if`, `while`, `for`, and `switch`, with strict type expectations for boolean expressions.
   - Distinct keywords for breaking out of loops and preventing fall-through in switch cases (`break` and `done` respectively).
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
   - Use `const char[]` for defining string literals for use with `writeString`. Note: This syntax is essentially a read-only fixed-size array declaration.

8. **Boolean Arrays:**
   - `bool[]` for efficient bit arrays (8 `bool` values per byte).

9. **Type Casting:**
   - No explicit casting required between types of the same size (e.g., `char` and `byte`).
   - Automatic zero extension when casting from a narrower unsigned type to a wider type.
   - Warnings are issued for potentially unsafe casts (e.g., `int` to `uint` and `uint` to `int`) unless an explicit cast is provided.

10. **Function Pointers:**
   - Use the `func` keyword to define function pointers, assign functions to them, and invoke them.

11. **Preprocessor Support:**
   - `#include "filename"` for including other source files.
   - `#define` for defining constants and macros.
   - `#ifdef`, `#ifndef`, `#else`, `#endif` for conditional compilation.
   - `#if`, `#elif`, `#else`, `#endif` for more complex conditional compilation.
   - Logical operators `||`, `&&`, `!` and parentheses `()` in preprocessor directives.
   - `#undef` for undefining macros.
   - `#pragma` for compiler-specific directives.

## Syntax and Semantics

### Preprocessor Directives

```c
#include "file.tc"                // Include another source file
#define MAX_SIZE 10               // Define a constant
#undef MAX_SIZE                   // Undefine a constant
#ifdef DEBUG                      // Conditional compilation if DEBUG is defined
    // Debug code
#else
    // Non-debug code
#endif
#if defined(DEBUG) || defined(TEST) // More complex conditional compilation
    // Code for DEBUG or TEST build
#elif defined(RELEASE) && !defined(FAST)
    // Code for RELEASE build without FAST
#else
    // Default code
#endif
#pragma optimize(on)              // Compiler-specific directive
```

### Variable Declarations

```c
byte a = 0x10;       // 8-bit unsigned integer
word b = 0x1234;     // 16-bit unsigned integer
int d = -1234;       // 16-bit signed integer
char c = 'A';        // 8-bit unsigned character
bool flag = true;    // Boolean value
```

### Const Strings

```c
const char[] message = "Hello, 6502!"; // Read-only fixed-size array
```

### Memory Access

```c
byte b = mem[42];      // Load from memory
mem[42] = b;           // Store to memory
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
    mem[0x2000] = 0xFF;
}

while (a != 0) {
    a--;
}

for (byte i = 0; i < 10; i++) {
    mem[0x2000 + i] = i;
}

switch (value) {
    case 1:
        // Code for case 1
        done; // Prevents fall-through
    case 2:
        // Code for case 2
        done; // Prevents fall-through
    default:
        // Code for default case
        done; // Prevents fall-through
}
```

### Type Casting

Explicit type casting using `as`:

```c
word wide_value = 0x1234;
byte narrow_value = wide_value as byte; // Cast word to byte, keeping the least significant 8 bits

byte narrow_value2 = 0x34;
word wide_value2 = narrow_value2; // Automatically cast byte to word, zero-extends the 8-bit value

int signed_value = -1234;
word unsigned_value = signed_value as word; // Cast int to word without warning

byte value;
bool flag = false;
value = flag as byte; // Cast boolean to byte
flag = value as bool; // Cast byte to bool without warning
```

### Fixed-Size Array Declarations

**Global and Local Variable Example:**

```c
const word SIZEPL = 8191;
bool[SIZEPL] flagsGlobal;  // Fixed-size array, compiler-managed
char[5] digits;            // Fixed-size array, compiler-managed
```

### Dynamic Allocation with `malloc`

**Syntax:**

```c
bool[] dynamicFlags = malloc(SIZEPL);  // Dynamic array, manually managed
char[] dynamicDigits = malloc(5);      // Dynamic array, manually managed
```

### Rules for Fixed-Size Arrays

1. **Memory Management**: The compiler is responsible for allocating and deallocating memory for these arrays.
2. **Range Checking**: The compiler can enforce range checking since it knows the size of the array.
3. **Assignment Restrictions**: These arrays cannot be reassigned or freed manually to prevent memory management issues.

### Function Pointers

**Defining a Function Pointer**:
- Use the `func` keyword as the type for function pointers.

**Syntax**:
```c
func functionPointerName = functionName;
```

**Example**:
```c
func fiboDelegate = Fibo;
```

**Calling a Function via a Function Pointer**:
- Invoke the function pointer directly by using it as a function.

**Syntax**:
```c
returnType result = functionPointerName(arguments);
```

**Example**:
```c
uint result = fiboDelegate(arg);
```

### Built-in Functions and Constants

### Serial Communication

```c
func writeChar(char c);       // System function to write a single character
func writeString(const char[] str) {
    // Function to write a null-terminated string to the serial output using writeChar
    byte i = 0;
    while (str[i] != 0) {
        writeChar(str[i]);
        i++;
    }
}
func writeWord(word num) {
    // Function to write a word as a string to the serial output buffer using writeChar
    if (num == 0) {
        writeChar('0');
        return;
    }

    byte i = 0;
    char[5] digits; // Maximum 5 digits for a word

    while (num > 0) {
        digits[i++] = (num % 10) + '0';
        num /= 10;
    }

    // Write the digits in reverse order
    while (i > 0) {
        writeChar(digits[--i]);
    }
}
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
func word[] millis(); // System function to return the address of the zero-page location where the 4-byte millis count is stored as an array of two words
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

Use the `#include` preprocessor directive to include other source files. This allows modular organization and reuse of common definitions and functions.

### Example System API File

**system.tc**

```c
// Pin Modes
const byte INPUT = 0;
const byte OUTPUT = 1;
const byte INPUT_PULLUP = 2;

// Serial Communication
func writeChar(char c); // System function to write a single character

func writeString(const char[] str) {
    // Function to write a null-terminated string to the serial output using writeChar
    byte i = 0;
    while (str[i] != 0) {
        writeChar(str[i]);
        i++;
    }
}

func writeWord(word num) {
    // Function to write a word as a string to the serial output buffer using writeChar
    if (num == 0) {
        writeChar('0');
        return;
    }

    byte i = 0;
    char[] digits = malloc(5); // Maximum 5 digits for a word

    while (num > 0) {
        digits[i++] = (num % 10) + '0';
        num /= 10;
    }

    // Write the digits in reverse order
    while (i > 0) {
        writeChar(digits[--i]);
    }

    free(digits); // Free allocated memory
}

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
func word[] millis(); // System function to return the address of the zero-page location where the 4-byte millis count is stored as an array of two words
func delay(word milliseconds);

// Memory Management
func byte[] malloc(word size);
func free(byte[] ptr);
```

### Example Main Program File

**main.tc**

```c
#include "system.tc"

func main() {
    const char[] welcome_message = "Hello, 6502!";
    
    writeString(welcome_message);
}
```

### Example EEPROM Page Read/Write Program

```c
func main() {
    word eeprom_address = 0x100;
    byte[16] write;
    byte[16] read;

    // Initialize data to write
    for (byte i = 0; i < 16; i++) {
        write[i] = i;
    }
    
    // Write a page to EEPROM
    writePage(eeprom_address, write);
    
    // Read a page from EEPROM
    readPage(eeprom_address, read);
    
    // Verify the read data
    for (byte i = 0; i < 16; i++) {
        if (read[i] != write[i]) {
            writeString("Read verification failed.");
            return;
        }
    }
    
    writeString("Read verification successful.");
}
```

## Operator Precedence

The following table shows the precedence and associativity of Tiny6502 operators, modeled after the C standard:

| Precedence Level | Operator(s)     | Description                           | Associativity    |
|------------------|-----------------|---------------------------------------|------------------|
| 1                | `()`            | Parentheses (expression grouping)     | Left to right    |
|                  | `[]`            | Array subscript                       | Left to right    |
|                  | `.`             | Member access                         | Left to right    |
| 2                | `!`             | Logical NOT                           | Right to left    |
|                  | `~`             | Bitwise NOT                           | Right to left    |
|                  | `-`             | Unary minus                           | Right to left    |
|                  | `+`             | Unary plus                            | Right to left    |
| 3                | `*`             | Multiplication                        | Left to right    |
|                  | `/`             | Division                              | Left to right    |
|                  | `%`             | Modulus                               | Left to right    |
| 4                | `+`             | Addition                              | Left to right    |
|                  | `-`             | Subtraction                           | Left to right    |
| 5                | `<<`            | Bitwise left shift                    | Left to right    |
|                  | `>>`            | Bitwise right shift                   | Left to right    |
| 6                | `<`             | Less than                             | Left to right    |
|                  | `<=`            | Less than or equal to                 | Left to right    |
|                  | `>`             | Greater than                          | Left to right    |
|                  | `>=`            | Greater than or equal to              | Left to right    |
| 7                | `==`            | Equal to                              | Left to right    |
|                  | `!=`            | Not equal to                          | Left to right    |
| 8                | `&`             | Bitwise AND                           | Left to right    |
| 9                | `^`             | Bitwise XOR                           | Left to right    |
| 10               | `|`             | Bitwise OR                            | Left to right    |
| 11               | `&&`            | Logical AND                           | Left to right    |
| 12               | `||`            | Logical OR                            | Left to right    |
| 13               | `=`             | Assignment                            | Right to left    |
| 14               | `,`             | Comma (sequence operator)             | Left to right    |

This table helps clarify the order in which operations are performed in Tiny6502, ensuring that expressions are evaluated as intended.