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


## System/Library Functions

This section describes the available system/library functions in Tigger C, including their arguments and return values.

### Serial Communication

#### `writeChar`

**Description**: Writes a single character to the serial output.

**Prototype**: 
```c
func writeChar(char c);
```

**Arguments**:
- `c`: Character to be written.

**Return Value**: None.

#### `writeString`

**Description**: Writes a null-terminated string to the serial output.

**Prototype**: 
```c
func writeString(const char[] str);
```

**Arguments**:
- `str`: Null-terminated string to be written.

**Return Value**: None.

#### `writeWord`

**Description**: Writes a 16-bit word as a string to the serial output.

**Prototype**: 
```c
func writeWord(word num);
```

**Arguments**:
- `num`: 16-bit word to be written.

**Return Value**: None.

#### `writeInt`

**Description**: Writes a 16-bit signed integer as a string to the serial output.

**Prototype**: 
```c
func writeInt(int num);
```

**Arguments**:
- `num`: 16-bit signed integer to be written.

**Return Value**: None.

#### `writeHex`

**Description**: Writes a byte as two hexadecimal characters to the serial output.

**Prototype**: 
```c
func writeHex(byte value);
```

**Arguments**:
- `value`: Byte to be written as hexadecimal.

**Return Value**: None.

### EEPROM

#### `writePage`

**Description**: Writes a page of data to serial EEPROM.

**Prototype**: 
```c
func writePage(word address, const byte[] data);
```

**Arguments**:
- `address`: EEPROM address to write to.
- `data`: Data to be written.

**Return Value**: None.

#### `readPage`

**Description**: Reads a page of data from serial EEPROM.

**Prototype**: 
```c
func readPage(word address, byte[] buffer);
```

**Arguments**:
- `address`: EEPROM address to read from.
- `buffer`: Buffer to store the read data.

**Return Value**: None.

### GPIO (Pin) Control

#### `pinSet`

**Description**: Sets the value of a pin.

**Prototype**: 
```c
func pinSet(byte pin, bool value);
```

**Arguments**:
- `pin`: Pin number to set.
- `value`: Value to set (true for high, false for low).

**Return Value**: None.

#### `pinRead`

**Description**: Reads the value of a pin.

**Prototype**: 
```c
func bool pinRead(byte pin);
```

**Arguments**:
- `pin`: Pin number to read.

**Return Value**: Boolean value of the pin (true for high, false for low).

#### `pinMode`

**Description**: Sets the mode of a pin.

**Prototype**: 
```c
func pinMode(byte pin, byte mode);
```

**Arguments**:
- `pin`: Pin number to set the mode for.
- `mode`: Mode to set (INPUT, OUTPUT, INPUT_PULLUP).

**Return Value**: None.

### I2C

#### `I2CWrite`

**Description**: Writes data to an I2C device.

**Prototype**: 
```c
func I2CWrite(byte address, byte data);
```

**Arguments**:
- `address`: I2C address of the device.
- `data`: Data to be written.

**Return Value**: None.

#### `I2CRead`

**Description**: Reads data from an I2C device.

**Prototype**: 
```c
func byte I2CRead(byte address);
```

**Arguments**:
- `address`: I2C address of the device.

**Return Value**: Data read from the device.

### Timing

#### `millis`

**Description**: Gets the current system tick count.

**Prototype**: 
```c
func byte[] millis();
```

**Arguments**: None.

**Return Value**: Byte array containing the current tick count.

#### `elapsedMillis`

**Description**: Calculates the elapsed milliseconds since the start.

**Prototype**: 
```c
func word elapsedMillis(byte[] start);
```

**Arguments**:
- `start`: Byte array containing the start tick count.

**Return Value**: Elapsed milliseconds since the start.

#### `elapsedSeconds`

**Description**: Calculates the elapsed seconds since the start.

**Prototype**: 
```c
func word elapsedSeconds(byte[] start);
```

**Arguments**:
- `start`: Byte array containing the start tick count.

**Return Value**: Elapsed seconds since the start.

#### `delay`

**Description**: Delays execution for a specified number of milliseconds.

**Prototype**: 
```c
func delay(word milliseconds);
```

**Arguments**:
- `milliseconds`: Number of milliseconds to delay.

**Return Value**: None.

### Memory Management

#### `malloc`

**Description**: Allocates memory and returns a pointer to the allocated memory.

**Prototype**: 
```c
func byte[] malloc(word size);
```

**Arguments**:
- `size`: Number of bytes to allocate.

**Return Value**: Pointer to the allocated memory.

#### `free`

**Description**: Frees previously allocated memory.

**Prototype**: 
```c
func free(byte[] ptr);
```

**Arguments**:
- `ptr`: Pointer to the memory to be freed.

**Return Value**: None.

### String Functions

#### `strlen`

**Description**: Calculates the length of a null-terminated string.

**Prototype**:
```c
func word strlen(const char[] str);
```

**Arguments**:
- `str`: Null-terminated string.

**Return Value**: Length of the string.

#### `strcpy`

**Description**: Copies a null-terminated string from source to destination.

**Prototype**:
```c
func char[] strcpy(char[] dest, const char[] src);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.

**Return Value**: Destination array.

#### `strncpy`

**Description**: Copies up to `n` characters from source to destination.

**Prototype**:
```c
func char[] strncpy(char[] dest, const char[] src, word n);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.
- `n`: Maximum number of characters to copy.

**Return Value**: Destination array.

#### `strcat`

**Description**: Concatenates source string to the destination string.

**Prototype**:
```c
func char[] strcat(char[] dest, const char[] src);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.

**Return Value**: Destination array.

#### `strncat`

**Description**: Concatenates up to `n` characters from source string to the destination string.

**Prototype**:
```c
func char[] strncat(char[] dest, const char[] src, word n);
```

**Arguments**:
- `dest`: Destination array.
- `src`: Source string.
- `n`: Maximum number of characters to concatenate.

**Return Value**: Destination array.

#### `strcmp`

**Description**: Compares two strings lexicographically.

**Prototype**:
```c
func int strcmp(const char[] str1, const char[] str2);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.

**Return Value**: Integer indicating the result of the comparison.

#### `strncmp`

**Description**: Compares up to `n` characters of two strings lexicographically.

**Prototype**:
```c
func int strncmp(const char[] str1, const char[] str2, word n);
```

#### `stricmp`

**Description**: Compares two strings lexicographically in a case-insensitive manner.

**Prototype**:
```c
func int stricmp(const char[] str1, const char[] str2);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.

**Return Value**: Integer indicating the result of the comparison.

#### `strnicmp`

**Description**: Compares up to `n` characters of two strings lexicographically in a case-insensitive manner.

**Prototype**:
```c
func int strnicmp(const char[] str1, const char[] str2, word n);
```

**Arguments**:
- `str1`: First string.
- `str2`: Second string.
- `n`: Maximum number of characters to compare.

**Return Value**: Integer indicating the result of the comparison.

**Arguments**:
- `str1`: First string.
- `str2`: Second string.
- `n`: Maximum number of characters to compare.

**Return Value**: Integer indicating the result of the comparison.

#### `strchr`

**Description**: Finds the first occurrence of a character in a string.

**Prototype**:
```c
func char[] strchr(const char[] str, char c);
```

**Arguments**:
- `str`: String to search.
- `c`: Character to find.

**Return Value**: Pointer to the first occurrence of the character or null if not found.

#### `strrchr`

**Description**: Finds the last occurrence of a character in a string.

**Prototype**:
```c
func char[] strrchr(const char[] str, char c);
```

**Arguments**:
- `str`: String to search.
- `c`: Character to find.

**Return Value**: Pointer to the last occurrence of the character or null if not found.

#### `strstr`

**Description**: Finds the first occurrence of a substring in a string.

**Prototype**:
```c
func char[] strstr(const char[] haystack, const char[] needle);
```

**Arguments**:
- `haystack`: String to search in.
- `needle`: Substring to find.

**Return Value**: Pointer to the first occurrence of the substring or null if not found.

#### `strdup`

**Description**: Duplicates a string.

**Prototype**:
```c
func char[] strdup(const char[] str);
```

**Arguments**:
- `str`: String to duplicate.

**Return Value**: Pointer to the duplicated string.

#### `strtok`

**Description**: Tokenizes a string.

**Prototype**:
```c
func char[] strtok(char[] str, const char[] delimiters);
```

**Arguments**:
- `str`: String to tokenize.
- `delimiters`: Delimiters to use for tokenizing.

**Return Value**: Pointer to the next token or null if no more tokens.

### Character Functions

#### `isdigit`

**Description**: Checks if a character is a digit.

**Prototype**:
```c
func bool isdigit(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is a digit.

#### `isalpha`

**Description**: Checks if a character is alphabetic.

**Prototype**:
```c
func bool isalpha(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is alphabet

ic.

#### `isalnum`

**Description**: Checks if a character is alphanumeric.

**Prototype**:
```c
func bool isalnum(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is alphanumeric.

#### `isspace`

**Description**: Checks if a character is whitespace.

**Prototype**:
```c
func bool isspace(char c);
```

**Arguments**:
- `c`: Character to check.

**Return Value**: Boolean value indicating if the character is whitespace.

#### `toupper`

**Description**: Converts a character to uppercase.

**Prototype**:
```c
func char toupper(char c);
```

**Arguments**:
- `c`: Character to convert.

**Return Value**: Uppercase character.

#### `tolower`

**Description**: Converts a character to lowercase.

**Prototype**:
```c
func char tolower(char c);
```

**Arguments**:
- `c`: Character to convert.

**Return Value**: Lowercase character.


### File System Functions

#### fopen

**Description**: Opens a file or directory.

**Prototype**:
```c
func byte[] fopen(const char[] filename, const char[] mode);
```

**Arguments**:
- `filename`: Name of the file or directory to open.
- `mode`: Mode in which to open the file (e.g., "r" for read, "w" for write, etc.).

**Return Value**: A file handle if successful, or null if an error occurs.

#### fclose

**Description**: Closes an open file or directory.

**Prototype**:
```c
func int fclose(byte[] fileHandle);
```

**Arguments**:
- `fileHandle`: The handle of the file or directory to close.

**Return Value**: 0 on success, or -1 on error.

#### fread

**Description**: Reads data from an open file.

**Prototype**:
```c
func word fread(byte[] buffer, word size, word count, byte[] fileHandle);
```

**Arguments**:
- `buffer`: Buffer to store the read data.
- `size`: Size of each element to read.
- `count`: Number of elements to read.
- `fileHandle`: The handle of the file to read from.

**Return Value**: Number of elements successfully read.

#### fwrite

**Description**: Writes data to an open file.

**Prototype**:
```c
func word fwrite(const byte[] buffer, word size, word count, byte[] fileHandle);
```

**Arguments**:
- `buffer`: Buffer containing the data to write.
- `size`: Size of each element to write.
- `count`: Number of elements to write.
- `fileHandle`: The handle of the file to write to.

**Return Value**: Number of elements successfully written.

#### fseek

**Description**: Sets the file position indicator for the file.

**Prototype**:
```c
func int fseek(byte[] fileHandle, int offset, byte whence);
```

**Arguments**:
- `fileHandle`: The handle of the file.
- `offset`: Number of bytes to offset from `whence`.
- `whence`: Position from where the offset is applied (0: beginning, 1: current position, 2: end of file).

**Return Value**: 0 on success, or -1 on error.

#### ftell

**Description**: Returns the current file position indicator for the file.

**Prototype**:
```c
func word ftell(byte[] fileHandle);
```

**Arguments**:
- `fileHandle`: The handle of the file.

**Return Value**: Current file position as a word, or 0 on error.

#### remove

**Description**: Removes a file.

**Prototype**:
```c
func int remove(const char[] filename);
```

**Arguments**:
- `filename`: Name of the file to remove.

**Return Value**: 0 on success, or -1 on error.

#### rename

**Description**: Renames a file or directory.

**Prototype**:
```c
func int rename(const char[] oldname, const char[] newname);
```

**Arguments**:
- `oldname`: Current name of the file or directory.
- `newname`: New name of the file or directory.

**Return Value**: 0 on success, or -1 on error.

#### mkdir

**Description**: Creates a new directory.

**Prototype**:
```c
func int mkdir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to create.

**Return Value**: 0 on success, or -1 on error.

#### rmdir

**Description**: Removes a directory.

**Prototype**:
```c
func int rmdir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to remove.

**Return Value**: 0 on success, or -1 on error.

#### opendir

**Description**: Opens a directory for reading.

**Prototype**:
```c
func byte[] opendir(const char[] dirname);
```

**Arguments**:
- `dirname`: Name of the directory to open.

**Return Value**: Directory handle if successful, or null if an error occurs.

#### readdir

**Description**: Reads the next entry in an open directory.

**Prototype**:
```c
func byte[] readdir(byte[] dirHandle);
```

**Arguments**:
- `dirHandle`: The handle of the directory to read from.

**Return Value**: The next directory entry, or null if no more entries are available.

#### closedir

**Description**: Closes an open directory.

**Prototype**:
```c
func int closedir(byte[] dirHandle);
```

**Arguments**:
- `dirHandle`: The handle of the directory to close.

**Return Value**: 0 on success, or -1 on error.

#### chdir

**Description**: Changes the current working directory.

**Prototype**:
```c
func int chdir(const char[] path);
```

**Arguments**:
- `path`: Path to the new working directory.

**Return Value**: 0 on success, or -1 on error.


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
