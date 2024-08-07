# Tigger C Language Documentation

## Introduction

Tigger C is a minimal programming language designed for the Hopper 6502 SBC, including a VIA, an ACIA, and a serial I2C EEPROM. It combines the simplicity and performance of low-level programming with higher code density and type safety, tailored for embedded systems. The language syntax is familiar to C programmers, making it easy to learn and use.

### Key Differences Between Tigger C and C

- **Pointers:** Only originate from the `malloc()` API, returning a byte array pointer (`byte[]`).
- **Local Arrays:** Declared with dimensions, e.g., `byte[100] hundredBytes;`, and automatically freed when the scope exits.
- **Stricter Type Safety:** Explicit casts are required for incompatible types. Implicit conversions are allowed only from narrower to wider numeric types. Explicit casts are required for `char` to any other type, `bool` to any other type, and between `int` and `word`. `byte` can be converted to `int` or `word` without a cast.
- **Pointer Casting:** Explicit casting required, e.g., `char[] = malloc(10) as char[]`.
- **Function Declarations:** Use the `func` keyword.
- **No `void` Keyword:** `void` keyword is not used.
- **Switch Case:** Like Hopper, switch cases never fall through, so `break` is only for loops.
- **Preprocessor Symbols:** Can only be defined or undefined with `#define` and `#undef`.
- **Initialization:** All variables and memory blocks are zero-initialized.
- **Boolean Expressions:** `if`, `while`, and `for` require `bool` type expressions.
- **Include Directive:** `#include` inlines source into one large source file. A second `#include` for the same file is ignored. No header files, only source files with the `.tc` extension.

## Key Features

1. **Low-Level Access:**
   - Direct access to registers, memory locations, and I/O ports.
   - Inline assembly support.

2. **Basic Data Types:**
   - `byte` (8-bit unsigned integer)
   - `word` (16-bit unsigned integer)
   - `int` (16-bit signed integer)
   - `char` (8-bit unsigned character)
   - `bool` (boolean type, stored as a single bit)

3. **Control Structures:**
   - C-like control structures: `if`, `while`, `for`, and `switch`.
   - Minimal syntactic sugar.

4. **Function Calls:**
   - Support for basic functions with fixed parameters.
   - Limited stack-based function call mechanism.
   - No nested/local functions.

5. **Memory Management:**
   - Manual memory management using `malloc` and `free`.
   - No garbage collection.
   - `malloc` zeroes memory.

6. **I/O Operations:**
   - Built-in functions for serial communication, EEPROM read/write, GPIO, and I2C.
   - Access to timers for system ticks and delays.

7. **Const Strings:**
   - Use `const char[]` for string literals.

8. **Boolean Arrays:**
   - `bool[]` for efficient bit arrays.

9. **Type Casting:**
   - No explicit casting required for same-size types.
   - Automatic zero extension for narrower to wider unsigned types.
   - Warnings for potentially unsafe casts unless explicitly cast.

10. **Preprocessor Support:**
    - `#include "filename"` for including files.
    - `#define` for constants and macros.
    - Conditional compilation with `#ifdef`, `#ifndef`, `#else`, `#endif`.
    - More complex conditional compilation with `#if`, `#elif`, `#else`, `#endif`.
    - Logical operators and parentheses in preprocessor directives.
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

Direct access to all 64K of memory is provided via a pseudo `byte[]` called `mem`.

```c
byte b = mem[42];      // Load from memory
mem[42] = b;           // Store to memory
```

### Inline Assembly

Tigger C supports inline assembly, allowing direct 6502 assembly instructions within your code. Two underscores before a function name indicates a "naked" function, meaning it has no entry or exit code (beyond RTS).
This means no stack frame (local variables) and no arguments. Inline assembly can be inserted anywhere in any method.

#### Sample Program: Inline 6502 Assembly

This sample program illustrates how to use inline 6502 assembly to control an LED on GPIO pin 0.

```c
#include "../system.tc"

func Setup() {
    asm("SMB0 0xF3 // DDRA");
    writeString("Initialized.");
}

func __LEDOn() {
    asm("SMB0 0xF1 // PORTA");
}

func __LEDOff() {
    asm("RMB0 0xF1 // PORTA");
}

// Blink an LED on GPIO pin 0 every second
func main() {
    bool ledState;

    Setup();
    __LEDOff();

    while (true) {
        if (ledState) {
            __LEDOff();
            ledState = false;
        } else {
            __LEDOn();
            ledState = true;
        }
        delay(1000); // Delay for 1 second
    }
}
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

#### `if`, `while`, and `for`

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
```

#### `switch`

In Tigger C, the `switch` statement has two key differences from C:

1. **No Fall-Through**: Cases in Tigger C's `switch` statements do not fall through to the next case. There is no need for a `break` statement to prevent fall-through.
2. **Single Statement Blocks**: Even single statement blocks in `switch` cases require curly braces.

```c
switch (value) {
    case 1: {
        // Code for case 1
    }
    case 2: {
        // Code for case 2
    }
    default: {
        // Code for default case
    }
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

1. **Memory Management**: The compiler allocates and deallocates memory for these arrays.
2. **Range Checking**: The compiler enforces range checking since it knows the array size.
3. **Assignment Restrictions**: These arrays cannot be reassigned or freed manually.

## Example Programs

### Including Other Source Files

Use the `#include` preprocessor directive to include other source files. This allows modular organization and reuse of common definitions and functions.

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

### Example Blink Program

```c
#include "system.tc"

func main() {
    const char[] welcome_message = "Blink program running...";
    byte pin = 1; // Example pin number
    bool led_on = false;

    // Set pin mode to output
    pinMode(pin, OUTPUT);

    // Initial pin state
    pinSet(pin, false); // Ensure LED is off
    writeString(welcome_message); // Send welcome message

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

### Example Mandelbrot Program

```c
#define EXPERIMENTAL
#include "system.tc"

func main() {
    writeString("\n Mandelbrot - ported from Gordon's TinyBasic - Integers\n");
    writeString("    Ported to Tigger C.\n\n");
    writeChar(' ');

    byte[] start = millis();
    const char[] palette = ".,'~=+:;*%&$OXB#@ ";
    int a; int b; int c; int d;
    int q; int p; int t; int s; byte i;
    int y; int x;
    int f = 50;

    for (y = -12; y <= 12; y++) {
        for (x = -49; x <= 29; x++) {
            c = x * 229 / 100;
            d = y * 416 / 100;
            a = c; b = d; i = 0;
            while (true) {
                q = b / f; s = b - (q * f);
                t = ((a * a) - (b * b)) / f + c;
                b = 2 * ((a * q) + (a * s / f)) + d;
                a = t; p = a / f; q = b / f;
                if (((p * p) + (q * q)) >= 5) {
                    writeChar(palette[i]);
                    break;
                } else {
                    i++;
                    if (i < 16) {
                        continue;
                    }
                    writeChar(' ');
                    break;
                }
            }
        } // next x
        writeString("\n "); 
    } // next y

    word ms = elapsedMillis(start);
    word sec = elapsedSeconds(start);
    free(start);

    writeString("\n"); 
    writeWord(ms); writeString(" ms\n");        
    writeWord(sec); writeString(" s\n");        
}
```

## Operator Precedence

The following table shows the precedence and associativity of Tigger C operators, modeled after the C standard:

| Precedence Level | Operator(s)       | Description                           | Associativity    |
|------------------|-------------------|---------------------------------------|------------------|
| 1                | `()`              | Parentheses (expression grouping)     | Left to right    |
|                  | `[]`              | Array subscript                       | Left to right    |
|                  | `.`               | Member access                         | Left to right    |
| 2                | `!`               | Logical NOT                           | Right to left    |
|                  | `~`               | Bitwise NOT                           | Right to left    |
|                  | `-`               | Unary minus                           | Right to left    |
|                  | `+`               | Unary plus                            | Right to left    |
| 3                | `*`               | Multiplication                        | Left to right    |
|                  | `/`               | Division                              | Left to right    |
|                  | `%`               | Modulus                               | Left to right    |
| 4                | `+`               | Addition                              | Left to right    |
|                  | `-`               | Subtraction                           | Left to right    |
| 5                | `<<`              | Bitwise left shift                    | Left to right    |
|                  | `>>`              | Bitwise right shift                   | Left to right    |
| 6                | `<`               | Less than                             | Left to right    |
|                  | `<=`              | Less than or equal to                 | Left to right    |
|                  | `>`               | Greater than                          | Left to right    |
|                  | `>=`              | Greater than or equal to              | Left to right    |
| 7                | `==`              | Equal to                              | Left to right    |
|                  | `!=`              | Not equal to                          | Left to right    |
| 8                | `&`               | Bitwise AND                           | Left to right    |
| 9                | `^`               | Bitwise XOR                           | Left to right    |
| 10               | `\|`               | Bitwise OR                            | Left to right    |
| 11               | `&&`              | Logical AND                           | Left to right    |
| 12               | `\|\|`              | Logical OR                            | Left to right    |
| 13               | `? :`             | Ternary conditional                   | Right to left    |
| 14               | `=`               | Assignment                            | Right to left    |
| 15               | `,`               | Comma (sequence operator)             | Left to right    |

This table helps clarify the order in which operations are performed in Tigger C, ensuring that expressions are evaluated as intended.
