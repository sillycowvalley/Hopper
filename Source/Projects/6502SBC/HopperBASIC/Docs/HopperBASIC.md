# HopperBASIC User Manual and Reference Guide
*Version 3.3 - A Modern BASIC for 6502 Systems*

---

## Introduction

### What is HopperBASIC?

HopperBASIC is a modern BASIC interpreter designed specifically for 6502 microprocessor systems. It combines the simplicity and immediacy of classic BASIC with modern language features, fitting comfortably within a 32K ROM while providing a rich, interactive development environment with hardware I/O capabilities including I2C communication.

### Design Philosophy

HopperBASIC was created with four core principles:

1. **Immediate Feedback** - Interactive REPL environment for instant results
2. **Incremental Development** - Encourage small, testable functions with easy persistence
3. **Small Footprint** - Target 32K ROM with minimal RAM usage
4. **Hardware Integration** - Direct support for GPIO, I2C, and other peripherals

### Comparison with Classic BASICs

| Feature | HopperBASIC | Apple II BASIC | Commodore 64 BASIC | BBC BASIC |
|---------|------------|----------------|-------------------|-----------|
| **Numeric Type** | 32-bit LONG | 16-bit INT + REAL | 16-bit INT + REAL | INT + REAL |
| **Type System** | Strict typing | Weak typing | Weak typing | Weak typing |
| **Functions** | Named with locals | Line numbers only | Line numbers only | Named PROC/FN |
| **Arrays** | Multiple types | REAL only | REAL only | Multiple types |
| **Strings** | Immutable | Mutable | Limited (255 char) | Full support |
| **Bitwise Ops** | Full support | None | None | AND/OR/EOR |
| **Compilation** | JIT to opcodes | Interpreted | Interpreted | Semi-compiled |
| **Storage** | EEPROM files | Tape/Disk | Tape/Disk | Tape/Disk |
| **Hardware I/O** | GPIO, I2C built-in | PEEK/POKE only | PEEK/POKE only | Built-in |

### Key Innovations

- **Single Numeric Type**: LONG (32-bit signed integer) simplifies the type system while providing ample range
- **Strict Type Safety**: No implicit conversions prevent subtle bugs
- **Modern Precedence**: Bitwise operators bind tighter than arithmetic, matching assembly expectations
- **Interactive Functions**: Define and test functions interactively without line numbers
- **JIT Compilation**: Expressions compile to stack-based opcodes for fast execution
- **I2C Support**: Native commands for I2C device communication and control

---

## User Guide

### Getting Started

When you start HopperBASIC, you'll see:

```
Hopper BASIC v3.3
MEMORY: 27390 BYTES AVAILABLE
EEPROM: 64K, 64512 BYTES AVAILABLE
READY
>
```

The `>` prompt indicates the system is ready for your commands. HopperBASIC operates in two modes:

1. **Immediate Mode**: Commands execute instantly at the prompt
2. **Program Mode**: Build programs with functions and a main block

### The REPL Environment

The Read-Eval-Print Loop (REPL) provides instant feedback for experimentation:

```basic
> PRINT 2 + 3
5
> VAR x = 10
OK
> PRINT x * x
100
> x = x + 5
OK
> PRINT x
15
```

Multiple statements can be combined with colons:

```basic
> VAR a = 5 : VAR b = 10 : PRINT a + b
15
```

Comments help document your code:

```basic
> VAR radius = 5  ! Circle radius in pixels
OK
> REM This is a full-line comment
OK
```

### Working with Variables

#### Variable Declaration

Variables are dynamically typed based on their initial value:

```basic
> VAR count = 0           ! LONG (32-bit integer)
OK
> VAR name = "Alice"      ! STRING
OK
> VAR flag = TRUE         ! BIT (boolean)
OK
> VAR letter = 'A'        ! CHAR
OK
```

Multiple variables can be declared at once:

```basic
> VAR x, y, z            ! All default to LONG = 0
OK
> VAR a = 1, b = 2, c = 3  ! Multiple with initialization
OK
```

#### Constants

Constants are immutable values:

```basic
> CONST PI = 3
OK
> CONST MESSAGE = "Hello, World!"
OK
> PI = 4
RUNTIME ERROR (Assignment to constant)
```

#### Arrays

Arrays provide indexed storage with explicit types for memory efficiency:

```basic
> BIT flags[10]          ! Boolean array (packed 8 per byte)
OK
> CHAR name[20]          ! Character array
OK
> INT scores[100]        ! 16-bit signed integer array
OK
> WORD data[50]          ! 16-bit unsigned array
OK

> scores[0] = 95
OK
> scores[1] = 87
OK
> PRINT scores[0]
95
```

### Functions

#### Defining Functions

Functions encapsulate reusable code with parameters and local variables:

```basic
> FUNC Add(a, b)
*   VAR sum
*   sum = a + b
*   RETURN sum
* ENDFUNC
OK

> PRINT Add(3, 4)
7
```

The `*` prompt indicates multi-line input mode during function definition.

#### Function Arguments

Arguments are passed by value (or by reference for arrays):

```basic
> FUNC Max(x, y)
*   IF x > y THEN RETURN x ENDIF
*   RETURN y
* ENDFUNC
OK

> PRINT Max(10, 20)
20
```

Arrays can be passed as arguments:

```basic
> INT data[5]
OK
> FUNC SumArray(arr)
*   VAR total = 0
*   FOR i = 0 TO LEN(arr) - 1
*     total = total + arr[i]
*   NEXT i
*   RETURN total
* ENDFUNC
OK

> FOR i = 0 TO 4
*   data[i] = i + 1
* NEXT i
OK
> PRINT SumArray(data)
15
```

#### Local Variables

Variables declared inside functions are local:

```basic
> VAR global = 100
OK
> FUNC Test()
*   VAR local = 50      ! Local to Test
*   PRINT local
*   PRINT global        ! Can access globals
* ENDFUNC
OK

> Test()
50
100
> PRINT local
RUNTIME ERROR (Undefined variable)
```

#### Recursion

Functions support full recursion:

```basic
> FUNC Factorial(n)
*   IF n <= 1 THEN RETURN 1 ENDIF
*   RETURN n * Factorial(n - 1)
* ENDFUNC
OK

> PRINT Factorial(5)
120

> FUNC Fibo(n)
*   IF n <= 1 THEN RETURN n ENDIF
*   RETURN Fibo(n-1) + Fibo(n-2)
* ENDFUNC
OK

> PRINT Fibo(10)
55
```

### Main Program

The main program is defined with BEGIN/END:

```basic
> BEGIN
*   VAR x = 10
*   VAR y = 20
*   PRINT "The sum is: ", x + y
*   FOR i = 1 TO 5
*     PRINT i
*   NEXT i
* END
OK

> RUN
The sum is: 30
1
2
3
4
5
READY
```

### Control Flow

#### IF/THEN/ELSE

Conditional execution:

```basic
> VAR age = 18
OK
> IF age >= 18 THEN PRINT "Adult" ELSE PRINT "Minor" ENDIF
Adult
```

#### Loops

Three types of loops are available:

```basic
! FOR loop - counted iteration
> FOR i = 1 TO 10 STEP 2
*   PRINT i;
* NEXT i
13579

! WHILE loop - pre-test condition
> VAR n = 1
> WHILE n < 100
*   n = n * 2
* WEND
> PRINT n
128

! DO/UNTIL loop - post-test condition  
> VAR count = 0
> DO
*   count = count + 1
*   PRINT count;
* UNTIL count = 5
12345
```

### Input and Output

#### PRINT Statement

PRINT provides flexible output formatting:

```basic
> PRINT "Hello"           ! With newline
Hello
> PRINT "Hello",          ! Comma suppresses newline with space
Hello > PRINT "World"
World
> PRINT "A"; "B"; "C"     ! Semicolon for no spacing
ABC
> PRINT 1, 2, 3          ! Comma for spaced values
1 2 3
```

#### INPUT Function

INPUT reads user input:

```basic
> PRINT "Enter your age: ";
Enter your age: > VAR age = INPUT()
*   [User types: 25]
OK
> PRINT "You are ", age, " years old"
You are 25 years old

! Single character input returns ASCII value
> PRINT "Press a key: ";
Press a key: > VAR key = INPUT()
*   [User types: A]
OK
> PRINT "You pressed: ", CHR(key)
You pressed: A
```

### Hardware I/O

#### Digital I/O

Control GPIO pins directly:

```basic
CONST LED = 13
PINMODE(LED, 1)     ! Set as output
WRITE(LED, TRUE)    ! Turn on
DELAY(1000)         ! Wait 1 second
WRITE(LED, FALSE)   ! Turn off

VAR button = READ(2)  ! Read input pin
IF button THEN PRINT "Pressed" ENDIF
```

#### I2C Communication

HopperBASIC provides comprehensive I2C support for communicating with peripherals:

```basic
! Scan for I2C devices
FOR addr = 8 TO 119
    IF I2CFIND(addr) THEN
        PRINT "Device found at address"; addr
    ENDIF
NEXT addr

! Write to device
CONST DISPLAY = 60   ! OLED at 0x3C
I2CBEGIN(DISPLAY)
I2CPUT(0)           ! Command mode
I2CPUT(175)         ! Display on command
IF I2CEND() THEN
    PRINT "Display configured"
ENDIF

! Read from device  
VAR bytes = I2CGET(80, 4)  ! Read 4 bytes from EEPROM
FOR i = 1 TO bytes
    PRINT I2CNEXT();
NEXT i
```

### Storage Management

Save and load programs to EEPROM:

```basic
> SAVE MYPROG
OK
> NEW
OK
> LOAD MYPROG
OK
> DIR
MYPROG      (256 bytes)
HELLO       (128 bytes)
2 files, 384 bytes used, 64128 bytes free
OK
> DEL HELLO
OK
```

Auto-load feature - type a filename to load and run:

```basic
> MYPROG              ! Automatically loads and runs MYPROG
[Program executes]
READY
```

---

## Reference Manual

### Grammar Specification

#### Program Structure

```ebnf
program := { statement }*

statement := variable_decl
           | constant_decl
           | array_decl
           | assignment
           | print_statement
           | if_statement
           | while_statement
           | do_until_statement
           | for_statement
           | function_definition
           | main_program
           | return_statement
           | comment_statement
           | function_call
```

#### Declarations

```ebnf
variable_decl := VAR variable_list
variable_list := variable_item [ "," variable_item ]*
variable_item := identifier [ "=" expression ]

constant_decl := CONST identifier "=" expression

array_decl := array_type identifier "[" expression "]"
array_type := BIT | CHAR | BYTE | WORD | INT
```

#### Control Structures

```ebnf
if_statement := IF expression THEN statement_block 
                [ ELSE statement_block ] ENDIF

while_statement := WHILE expression 
                   { statement }* 
                   WEND

do_until_statement := DO 
                      { statement }* 
                      UNTIL expression

for_statement := FOR identifier "=" expression TO expression 
                 [ STEP expression ]
                 { statement }*
                 NEXT [ identifier ]

statement_block := statement [ ":" statement ]*
```

#### Functions

```ebnf
function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { local_declaration | statement }*
                      ENDFUNC

main_program := BEGIN 
                { local_declaration | statement }* 
                END

local_declaration := VAR variable_list
parameter_list := identifier [ "," identifier ]*
function_call := identifier "(" [ argument_list ] ")"
argument_list := expression [ "," expression ]*
return_statement := RETURN [ expression ]
```

#### Expressions

```ebnf
expression := logical_or_expr

logical_or_expr := logical_and_expr [ OR logical_and_expr ]*
logical_and_expr := logical_not_expr [ AND logical_not_expr ]*
logical_not_expr := [ NOT ] comparison_expr
comparison_expr := additive_expr [ comparison_op additive_expr ]
comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="
additive_expr := bitwise_or_expr [ ("+" | "-") bitwise_or_expr ]*
bitwise_or_expr := bitwise_and_expr [ "|" bitwise_and_expr ]*
bitwise_and_expr := multiplicative_expr [ "&" multiplicative_expr ]*
multiplicative_expr := unary_expr [ ("*" | "/" | MOD) unary_expr ]*
unary_expr := [ "-" | "~" ] primary_expr
primary_expr := number | identifier | string_literal | char_literal 
              | TRUE | FALSE | "(" expression ")" 
              | function_call | built_in_function
              | identifier "[" expression "]"
```

#### Input/Output

```ebnf
print_statement := PRINT [ print_list ]
print_list := print_item [ separator print_item ]* [ separator ]
print_item := expression
separator := "," | ";"

comment_statement := REM [ any_text ] 
                   | "!" [ any_text ]
```

### Console Commands

#### Program Management

| Command | Description | Example |
|---------|-------------|---------|
| `NEW` | Clear all program code and variables | `> NEW` |
| `LIST` | Display the complete program | `> LIST` |
| `RUN` | Execute the main program (BEGIN/END) | `> RUN` |
| `CLEAR` | Reset all variables to default values | `> CLEAR` |

#### Inspection Commands

| Command | Description | Example |
|---------|-------------|---------|
| `VARS` | Show all variables and constants | `> VARS` |
| `FUNCS` | Show all defined functions | `> FUNCS` |
| `MEM` | Display available memory | `> MEM` |
| `FORGET name` | Remove a variable or function | `> FORGET x` |

#### Storage Commands

| Command | Description | Example |
|---------|-------------|---------|
| `SAVE name` | Save program to EEPROM | `> SAVE GAME` |
| `LOAD name` | Load program from EEPROM | `> LOAD GAME` |
| `DIR` | List all saved programs | `> DIR` |
| `DEL name` | Delete a saved program | `> DEL OLD` |
| `FORMAT` | Erase all stored programs | `> FORMAT` |

#### System Commands

| Command | Description | Example |
|---------|-------------|---------|
| `BYE` | Exit the interpreter | `> BYE` |

#### Debug Commands (DEBUG build only)

| Command | Description | Example |
|---------|-------------|---------|
| `HEAP` | Show heap memory layout | `> HEAP` |
| `BUFFERS` | Display buffer contents | `> BUFFERS` |
| `DUMP [page]` | Hex dump of memory page | `> DUMP 0` |

### Data Types

#### Type System Overview

HopperBASIC uses a strict type system with four fundamental types:

| Type | Description | Size | Range/Values |
|------|-------------|------|--------------|
| `LONG` | 32-bit signed integer | 4 bytes | -2,147,483,648 to 2,147,483,647 |
| `CHAR` | Single character | 1 byte | ASCII 0-255 |
| `BIT` | Boolean value | 1 bit | TRUE or FALSE |
| `STRING` | Immutable text | Variable | Any text |

#### Type Compatibility Rules

- **No implicit conversions** - Types must match exactly
- **LONG** only compatible with LONG
- **CHAR** only compatible with CHAR  
- **BIT** only compatible with BIT
- **STRING** only compatible with STRING

#### Array Types

Arrays use compact storage types for memory efficiency:

| Type | Element Size | Description |
|------|--------------|-------------|
| `BIT[n]` | 1 bit | Packed boolean array (8 per byte) |
| `CHAR[n]` | 1 byte | Character array |
| `BYTE[n]` | 1 byte | Unsigned 8-bit array |
| `WORD[n]` | 2 bytes | Unsigned 16-bit array |
| `INT[n]` | 2 bytes | Signed 16-bit array |

### Operators

#### Operator Precedence (Highest to Lowest)

1. **Primary**: `()` Function calls
2. **Unary**: `-` `~` `[]` (negation, bitwise NOT, array/string accessor)
3. **Multiplicative**: `*` `/` `MOD`
4. **Bitwise AND**: `&`
5. **Bitwise OR**: `|`
6. **Additive**: `+` `-`
7. **Comparison**: `=` `<>` `<` `>` `<=` `>=`
8. **Logical NOT**: `NOT`
9. **Logical AND**: `AND`
10. **Logical OR**: `OR`

#### Arithmetic Operators (LONG only)

| Operator | Operation | Example | Result |
|----------|-----------|---------|--------|
| `+` | Addition | `5 + 3` | `8` |
| `-` | Subtraction | `10 - 4` | `6` |
| `*` | Multiplication | `6 * 7` | `42` |
| `/` | Integer division | `15 / 4` | `3` |
| `MOD` | Modulo (remainder) | `15 MOD 4` | `3` |
| `-` (unary) | Negation | `-5` | `-5` |

#### Bitwise Operators (LONG only)

| Operator | Operation | Example | Result |
|----------|-----------|---------|--------|
| `&` | Bitwise AND | `12 & 7` | `4` |
| `|` | Bitwise OR | `8 | 4` | `12` |
| `~` | Bitwise NOT | `~0` | `-1` |

#### Logical Operators (BIT only)

| Operator | Operation | Example | Result |
|----------|-----------|---------|--------|
| `AND` | Logical AND | `TRUE AND FALSE` | `FALSE` |
| `OR` | Logical OR | `TRUE OR FALSE` | `TRUE` |
| `NOT` | Logical NOT | `NOT TRUE` | `FALSE` |

#### Comparison Operators

| Operator | Operation | Types Supporting | Example |
|----------|-----------|-----------------|---------|
| `=` | Equal | All types | `x = 5` |
| `<>` | Not equal | All types | `name <> "Alice"` |
| `<` | Less than | LONG, CHAR only | `x < 10` |
| `>` | Greater than | LONG, CHAR only | `ch > 'A'` |
| `<=` | Less than or equal | LONG, CHAR only | `age <= 18` |
| `>=` | Greater than or equal | LONG, CHAR only | `score >= 90` |

### Built-in Functions

#### Type Conversion Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `ASC(char)` | Character to ASCII value | CHAR → LONG | `ASC('A')` returns `65` |
| `CHR(value)` | ASCII value to character | LONG → CHAR | `CHR(65)` returns `'A'` |

#### Mathematical Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `ABS(x)` | Absolute value | LONG → LONG | `ABS(-42)` returns `42` |

#### String/Array Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `LEN(x)` | Length of string or array | STRING/Array → LONG | `LEN("Hello")` returns `5` |

#### Time Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `MILLIS()` | Milliseconds since startup | → LONG | `start = MILLIS()` |
| `SECONDS()` | Seconds since startup | → LONG | `elapsed = SECONDS()` |
| `DELAY(ms)` | Pause execution | LONG → | `DELAY(1000)` |

#### Memory Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `PEEK(addr)` | Read byte from memory | LONG → LONG | `value = PEEK(0x5000)` |
| `POKE(addr, val)` | Write byte to memory | LONG, LONG → | `POKE(0x5000, 65)` |

#### GPIO Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `INPUT()` | Read user input | → LONG | `age = INPUT()` |
| `READ(pin)` | Read digital pin | LONG → BIT | `state = READ(13)` |
| `WRITE(pin, val)` | Write digital pin | LONG, BIT → | `WRITE(13, TRUE)` |
| `PINMODE(pin, mode)` | Configure pin | LONG, LONG → | `PINMODE(13, 1)` |

#### I2C Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `I2CFIND(addr)` | Test if device responds | LONG → BIT | `IF I2CFIND(60) THEN...` |
| `I2CBEGIN(addr)` | Start write transaction | LONG → | `I2CBEGIN(60)` |
| `I2CPUT(byte)` | Send byte in transaction | LONG → | `I2CPUT(175)` |
| `I2CEND()` | Complete transaction | → BIT | `IF I2CEND() THEN...` |
| `I2CGET(addr, count)` | Read bytes from device | LONG, LONG → LONG | `bytes = I2CGET(80, 4)` |
| `I2CNEXT()` | Get next buffered byte | → LONG | `data = I2CNEXT()` |

### Error Messages

#### Syntax Errors
- `SYNTAX ERROR` - Invalid statement or expression
- `UNDEFINED VARIABLE` - Reference to non-existent variable
- `UNDEFINED FUNCTION` - Call to non-existent function

#### Runtime Errors
- `TYPE MISMATCH` - Incompatible types in operation
- `ARRAY BOUNDS` - Array index out of range
- `DIVISION BY ZERO` - Attempt to divide by zero
- `STACK OVERFLOW` - Too many nested calls
- `OUT OF MEMORY` - Insufficient memory
- `RANGE ERROR` - Value out of valid range (e.g., I2C address > 127)

#### Storage Errors
- `FILE NOT FOUND` - Requested file doesn't exist
- `STORAGE FULL` - EEPROM capacity exceeded
- `FILE EXISTS` - Duplicate filename

### Special Features

#### Auto-Load
Type any saved program name at the prompt to automatically load and run it:

```basic
> MYPROG          ! If MYPROG exists, loads and runs it
[Program executes]
READY
```

#### AUTOEXEC
Programs named "AUTOEXEC" run automatically at system startup:

```basic
> BEGIN
*   PRINT "System initialized!"
* END
OK
> SAVE AUTOEXEC
OK
! Next startup will run this automatically
```

#### Statement Separators
Use colons to combine multiple statements:

```basic
> VAR x = 5 : VAR y = 10 : PRINT x + y
15
```

#### Comments
Two comment styles are supported:

```basic
> REM This is a traditional BASIC comment
OK
> VAR count = 0  ! This is a modern inline comment
OK
```

### Memory Layout

HopperBASIC manages memory efficiently:

- **Zero Page**: System variables and pointers
- **Stack**: Expression evaluation and function calls
- **Heap**: Dynamic allocation for strings, arrays, and symbol tables
- **Buffers**: Fixed areas for input, tokens, opcodes, and I2C data

### Performance Notes

#### JIT Compilation
Expressions are compiled to stack-based opcodes for efficient execution, providing performance comparable to classic 6502 BASICs.

#### Optimization
- FOR loops with STEP 1 use optimized FORITF opcode
- String comparisons use pointer comparison when possible
- Local variables use BP-relative addressing
- I2C operations use buffered reads for efficiency

#### Memory Efficiency
- Arrays use compact storage types
- BIT arrays pack 8 bits per byte
- Strings are immutable to enable sharing
- I2C read buffer minimizes memory overhead

---

## Appendix: Sample Programs

### Sieve of Eratosthenes

```basic
! Classic benchmark for finding prime numbers
CONST size = 8191
BIT flags[size]

BEGIN
    VAR count, prime, k
    
    ! Initialize flags to TRUE
    FOR i = 0 TO size - 1
        flags[i] = TRUE
    NEXT i
    
    ! Sieve algorithm
    FOR i = 0 TO size - 1
        IF flags[i] THEN
            prime = i + i + 3
            k = i + prime
            WHILE k < size
                flags[k] = FALSE
                k = k + prime
            WEND
            count = count + 1
        ENDIF
    NEXT i
    
    PRINT count, " primes found"
END
```

### Number Guessing Game

```basic
FUNC Game()
    VAR target = 42  ! In real version, use RND
    VAR guess, tries = 0
    
    PRINT "Guess the number (1-100)"
    
    DO
        PRINT "Your guess: ";
        guess = INPUT()
        tries = tries + 1
        
        IF guess < target THEN
            PRINT "Too low!"
        ELSE
            IF guess > target THEN
                PRINT "Too high!"
            ENDIF
        ENDIF
    UNTIL guess = target
    
    PRINT "Correct! It took ", tries, " tries"
ENDFUNC

BEGIN
    Game()
END
```

### LED Blinker (Hardware I/O)

```basic
CONST LED_PIN = 13

BEGIN
    VAR state = FALSE
    
    PINMODE(LED_PIN, 1)  ! Output mode
    
    FOR i = 1 TO 10
        state = NOT state
        WRITE(LED_PIN, state)
        DELAY(500)  ! 500ms delay
    NEXT i
    
    WRITE(LED_PIN, FALSE)  ! Turn off
END
```

### I2C Device Scanner

```basic
! Scan I2C bus for connected devices
BEGIN
    PRINT "Scanning I2C bus..."
    VAR found = 0
    
    FOR addr = 8 TO 119
        IF I2CFIND(addr) THEN
            PRINT "Device found at address ", addr, " (0x";
            IF addr < 16 THEN PRINT "0" ENDIF
            ! Would print hex here in full version
            PRINT ")"
            found = found + 1
        ENDIF
    NEXT addr
    
    PRINT found, " devices found"
END
```

### OLED Display Control

```basic
! Initialize and write to SSD1306 OLED display
CONST OLED = 60  ! I2C address 0x3C

FUNC InitOLED()
    IF NOT I2CFIND(OLED) THEN
        PRINT "OLED not found!"
        RETURN FALSE
    ENDIF
    
    ! Send initialization sequence
    I2CBEGIN(OLED)
    I2CPUT(0)       ! Command mode
    I2CPUT(174)     ! Display off
    I2CPUT(213)     ! Set display clock
    I2CPUT(128)     ! Suggested ratio
    I2CPUT(168)     ! Set multiplex
    I2CPUT(63)      ! 1/64 duty
    I2CPUT(211)     ! Set display offset
    I2CPUT(0)       ! No offset
    I2CPUT(64)      ! Set start line
    I2CPUT(175)     ! Display on
    
    IF I2CEND() THEN
        PRINT "OLED initialized"
        RETURN TRUE
    ELSE
        PRINT "OLED init failed"
        RETURN FALSE
    ENDIF
ENDFUNC

BEGIN
    IF InitOLED() THEN
        PRINT "Display ready for use"
    ENDIF
END
```

### EEPROM Read/Write

```basic
! Read and write to 24C256 EEPROM via I2C
CONST EEPROM = 80  ! I2C address 0x50

FUNC WriteEEPROM(addr, value)
    I2CBEGIN(EEPROM)
    I2CPUT(addr / 256)    ! Address high byte
    I2CPUT(addr MOD 256)  ! Address low byte
    I2CPUT(value)         ! Data byte
    RETURN I2CEND()
ENDFUNC

FUNC ReadEEPROM(addr)
    ! Set read address
    I2CBEGIN(EEPROM)
    I2CPUT(addr / 256)    ! Address high byte
    I2CPUT(addr MOD 256)  ! Address low byte
    IF NOT I2CEND() THEN
        PRINT "Failed to set address"
        RETURN 0
    ENDIF
    
    ! Read single byte
    VAR count = I2CGET(EEPROM, 1)
    IF count = 1 THEN
        RETURN I2CNEXT()
    ELSE
        PRINT "Read failed"
        RETURN 0
    ENDIF
ENDFUNC

BEGIN
    ! Write test data
    IF WriteEEPROM(0, 42) THEN
        PRINT "Wrote 42 to address 0"
    ENDIF
    
    DELAY(10)  ! Write cycle time
    
    ! Read it back
    VAR data = ReadEEPROM(0)
    PRINT "Read back: ", data
END
```

---

## Quick Reference Card

### Essential Commands
```
NEW         - Clear program          SAVE name   - Save to EEPROM
LIST        - Show program           LOAD name   - Load from EEPROM  
RUN         - Execute main           DIR         - List files
VARS        - Show variables         BYE         - Exit
FUNCS       - Show functions         MEM         - Show memory
```

### Variable Declaration
```basic
VAR x = 42              ! Single variable
VAR a, b, c            ! Multiple (default 0)
VAR x = 1, y = 2       ! Multiple with init
CONST PI = 3           ! Constant
INT data[100]          ! Array
```

### Control Flow
```basic
IF x > 0 THEN ... ELSE ... ENDIF
WHILE condition ... WEND
DO ... UNTIL condition
FOR i = 1 TO 10 [STEP 2] ... NEXT i
```

### Functions
```basic
FUNC Name(param1, param2)
    VAR local
    ! code here
    RETURN result
ENDFUNC
```

### Main Program
```basic
BEGIN
    ! main code here
END
```

### Built-in Functions
```basic
! Math & Type Conversion
ABS(x)          CHR(n)          LEN(s)
ASC(c)          INPUT()         

! Time & Memory
MILLIS()        SECONDS()       DELAY(ms)
PEEK(addr)      POKE(addr,val)  

! GPIO
READ(pin)       WRITE(pin,val)  PINMODE(pin,mode)

! I2C Communication
I2CFIND(addr)   I2CBEGIN(addr)  I2CPUT(byte)
I2CEND()        I2CGET(addr,n)  I2CNEXT()
```

### Operators (by precedence)
```
() functions       Primary
- ~ []             Unary (negation, bitwise NOT, array/string accessor)
* / MOD            Multiplicative
&                  Bitwise AND
|                  Bitwise OR
+ -                Additive
= <> < > <= >=     Comparison
NOT                Logical NOT
AND                Logical AND
OR                 Logical OR
```

### Common I2C Device Addresses
```basic
CONST OLED = 60      ! SSD1306 OLED (0x3C)
CONST EEPROM = 80    ! 24C256 EEPROM (0x50)
CONST RTC = 104      ! DS3231 RTC (0x68)
CONST TEMP = 72      ! LM75 Temp (0x48)
```

---

*HopperBASIC v3.3 - A Modern BASIC for 6502 Systems*  
*© 2025 Hopper Development Team*