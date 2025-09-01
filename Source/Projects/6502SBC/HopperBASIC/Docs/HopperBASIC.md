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

### Key Innovations

- **Single Numeric Type**: LONG (32-bit signed integer) simplifies the type system while providing ample range
- **Strict Type Safety**: No implicit conversions prevent subtle bugs
- **Modern Precedence**: Bitwise operators bind tighter than arithmetic, matching assembly expectations
- **Interactive Functions**: Define and test functions interactively without line numbers
- **JIT Compilation**: Expressions compile to stack-based opcodes for fast execution
- **Peephole Optimization**: Automatic bytecode optimizations for better performance
- **I2C Support**: Native commands for I2C device communication and control
- **Array Persistence**: IMPORT/EXPORT functions for saving and loading array data

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
> ' This is also a comment (alternative syntax)
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

The main program is defined with BEGIN/END and is stored as the hidden function `$MAIN`:

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
> PRINT                  ! Empty PRINT for newline only

```

### Array Storage

#### EXPORT - Save Arrays to Files

The EXPORT function saves array data to EEPROM files for persistent storage:

```basic
> INT scores[10]
OK
> scores[0] = 100
OK
> scores[1] = 95
OK
> scores[2] = 87
OK
> EXPORT(scores, "HISCORE")
OK
```

#### IMPORT - Load Arrays from Files

The IMPORT function loads previously saved array data. The array type determines how the raw data is interpreted, and the array is automatically resized to fit all the data:

```basic
> CHAR data[0]  ! Declare zero-sized array
OK
> VAR count = IMPORT(data, "MYFILE")
OK
> PRINT count, " elements loaded"
256 elements loaded

! Or with a pre-sized array that will be resized if needed
> INT scores[10]
OK
> VAR count = IMPORT(scores, "HISCORE")
OK
> IF count > 0 THEN
*   PRINT count, " elements loaded"
*   FOR i = 0 TO count - 1
*     PRINT "Score ", i, ": ", scores[i]
*   NEXT i
* ELSE
*   PRINT "Failed to load scores"
* ENDIF
3 elements loaded
Score 0: 100
Score 1: 95
Score 2: 87
```

#### Storage Notes

- The array type passed to IMPORT determines how the data is interpreted (e.g., BIT arrays read 8 bits per byte, WORD arrays read 2 bytes per element)
- Arrays are automatically resized to accommodate all imported data
- You can declare zero-sized arrays for importing: `CHAR data[0]`
- File names follow standard HopperBASIC naming rules (1-13 characters)
- Files created with EXPORT appear in DIR listings
- Use DEL to remove exported array files
- IMPORT returns the number of elements loaded, or 0 on error

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
HISCORE     (20 bytes)     ! Array data file
3 files, 404 bytes used, 64108 bytes free
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
                   | "'" [ any_text ]
```

### Literals and Data Formats

#### Numeric Literals

HopperBASIC supports multiple numeric formats:

| Format | Example | Description |
|--------|---------|-------------|
| Decimal | `42`, `1000` | Standard decimal numbers |
| Hexadecimal | `0xFF`, `0x1234` | C-style hex with `0x` prefix |

#### Character Literals

Character literals support C-style escape sequences:

```basic
> VAR ch = 'A'           ! Regular character
OK
> VAR tab = '\t'        ! Tab character
OK
> VAR newline = '\n'     ! Newline character
OK
> VAR quote = '\''       ! Single quote
OK
```

#### String Literals

String literals also support escape sequences:

```basic
> VAR msg = "Hello\nWorld"       ! Newline in string
OK
> VAR path = "C:\\Users\\Me"     ! Backslashes
OK
> VAR quoted = "He said \"Hi\""  ! Embedded quotes
OK
```

#### Escape Sequences

| Sequence | Character | ASCII Value |
|----------|-----------|-------------|
| `\b` | Backspace | 0x08 |
| `\t` | Tab | 0x09 |
| `\n` | Newline | 0x0A |
| `\f` | Form feed | 0x0C |
| `\r` | Carriage return | 0x0D |
| `\\` | Backslash | 0x5C |
| `\"` | Double quote | 0x22 |
| `\'` | Single quote | 0x27 |

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
| `FUNCS` | Show all functions  | `> FUNCS` |
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
| `TRON` | Enable execution trace | `> TRON` |
| `TROFF` | Disable execution trace | `> TROFF` |

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
| `RND(max)` | Random number 1 to max | LONG → LONG | `RND(100)` returns 1-100 |

#### String/Array Functions

| Function | Description | Type Signature | Example |
|----------|-------------|----------------|---------|
| `LEN(x)` | Length of string or array | STRING/Array → LONG | `LEN("Hello")` returns `5` |
| `IMPORT(array, "file")` | Load array data from file | Array, STRING → LONG | `count = IMPORT(data, "MYDATA")` |
| `EXPORT(array, "file")` | Save array data to file | Array, STRING → | `EXPORT(scores, "HISCORE")` |

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
- `ILLEGAL CHARACTER` - Invalid character or escape sequence

#### Runtime Errors
- `TYPE MISMATCH` - Incompatible types in operation
- `ARRAY BOUNDS` - Array index out of range
- `DIVISION BY ZERO` - Attempt to divide by zero
- `STACK OVERFLOW` - Too many nested calls
- `OUT OF MEMORY` - Insufficient memory
- `RANGE ERROR` - Value out of valid range (e.g., I2C address > 127)
- `NUMERIC OVERFLOW` - Number too large for LONG type

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

#### Peephole Optimization
The compiler automatically performs bytecode optimizations:
- **INCGLOBAL/INCLOCAL**: Optimizes increment operations
- **ADDLOCALS/ADDGLOBALS**: Optimizes addition of two variables
- **GETITEM/SETITEM optimizations**: Optimizes array access patterns
- FOR loops with STEP 1 use optimized FORITF opcode
- String comparisons use pointer comparison when possible
- Local variables use BP-relative addressing

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

### High Score Table

```basic
! Persistent high score table
INT highscores[5]
VAR newscore

FUNC ShowScores()
    PRINT "HIGH SCORES:"
    FOR i = 0 TO 4
        PRINT i + 1, ". ", highscores[i]
    NEXT i
ENDFUNC

FUNC AddScore(score)
    VAR pos = 5
    ! Find position for new score
    FOR i = 0 TO 4
        IF score > highscores[i] THEN
            pos = i
            i = 5  ! Exit loop
        ENDIF
    NEXT i
    
    ! Insert if in top 5
    IF pos < 5 THEN
        ! Shift scores down
        FOR i = 4 TO pos + 1 STEP -1
            highscores[i] = highscores[i - 1]
        NEXT i
        highscores[pos] = score
        PRINT "New high score #", pos + 1
        EXPORT(highscores, "HISCORES")
    ENDIF
ENDFUNC

BEGIN
    ! Load existing scores
    IF IMPORT(highscores, "HISCORES") = 0 THEN
        PRINT "No saved scores, initializing..."
        FOR i = 0 TO 4
            highscores[i] = 0
        NEXT i
    ENDIF
    
    ShowScores()
    
    ! Simulate adding a new score
    newscore = RND(1000)
    PRINT "Your score: ", newscore
    AddScore(newscore)
    ShowScores()
END
```

### Temperature Data Logger

```basic
! Temperature data logger with automatic array resizing
WORD temps[100]
VAR count = 0

FUNC LogTemp()
    IF count < 100 THEN
        ! Simulate temperature reading
        temps[count] = 200 + RND(100)  ! 20.0-29.9°C
        count = count + 1
        RETURN TRUE
    ELSE
        PRINT "Buffer full!"
        RETURN FALSE
    ENDIF
ENDFUNC

FUNC SaveLog()
    ! Only export the elements we've logged
    WORD toSave[100]
    FOR i = 0 TO count - 1
        toSave[i] = temps[i]
    NEXT i
    EXPORT(toSave, "TEMPLOG")
    PRINT "Saved ", count, " readings"
ENDFUNC

FUNC LoadLog()
    ! Use zero-sized array - will be resized automatically
    WORD loaded[0]
    count = IMPORT(loaded, "TEMPLOG")
    IF count > 0 THEN
        PRINT "Loaded ", count, " readings"
        ! Copy to working array
        FOR i = 0 TO count - 1
            temps[i] = loaded[i]
        NEXT i
    ELSE
        PRINT "No saved data"
    ENDIF
ENDFUNC

FUNC ShowStats()
    IF count = 0 THEN
        PRINT "No data"
        RETURN
    ENDIF
    
    VAR min = temps[0]
    VAR max = temps[0]
    VAR sum = 0
    
    FOR i = 0 TO count - 1
        IF temps[i] < min THEN min = temps[i] ENDIF
        IF temps[i] > max THEN max = temps[i] ENDIF
        sum = sum + temps[i]
    NEXT i
    
    PRINT "Readings: ", count
    PRINT "Min: ", min / 10, ".", min MOD 10, "°C"
    PRINT "Max: ", max / 10, ".", max MOD 10, "°C"
    PRINT "Avg: ", (sum / count) / 10, "°C"
ENDFUNC

BEGIN
    LoadLog()
    
    ! Take some readings
    FOR i = 1 TO 10
        IF LogTemp() THEN
            PRINT "Reading ", count, " logged"
        ENDIF
        DELAY(100)
    NEXT i
    
    ShowStats()
    SaveLog()
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
            PRINT "Device found at address ", addr
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

### Using Hexadecimal Numbers

```basic
! Demonstrate hexadecimal number support
BEGIN
    VAR mask = 0xFF         ! 255 in decimal
    VAR addr = 0x8000       ! Memory address
    
    PRINT "Mask: ", mask
    PRINT "Address: ", addr
    
    ! Bitwise operations with hex
    VAR result = addr & 0xFF00
    PRINT "High byte: ", result / 256
END
```

### Dynamic Array Import

```basic
! Demonstrate automatic array resizing with IMPORT
BEGIN
    ! Save some data first
    BYTE original[5]
    FOR i = 0 TO 4
        original[i] = i * 10
    NEXT i
    EXPORT(original, "TESTDATA")
    
    ! Now import into different array types to show interpretation
    
    ! Import as BYTE array (5 elements)
    BYTE asBytes[0]  ! Zero-sized, will be resized
    VAR count = IMPORT(asBytes, "TESTDATA")
    PRINT "As BYTE array: ", count, " elements"
    FOR i = 0 TO count - 1
        PRINT asBytes[i];
    NEXT i
    
    ! Import same data as WORD array (2 or 3 elements)
    WORD asWords[0]  ! Zero-sized, will be resized
    count = IMPORT(asWords, "TESTDATA")  
    PRINT "As WORD array: ", count, " elements"
    FOR i = 0 TO count - 1
        PRINT asWords[i];
    NEXT i
END
```

### Escape Sequences Demo

```basic
! Demonstrate character and string escape sequences
BEGIN
    PRINT "Line 1\nLine 2\nLine 3"
    PRINT "Tab\tSeparated\tValues"
    PRINT "Path: C:\\Users\\Me\\Documents"
    PRINT "He said \"Hello!\""
    
    VAR bell = '\b'  ! Backspace character
    VAR tab = '\t'   ! Tab character
    PRINT "Alert", bell, "Tab", tab, "Done"
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

### Debug Commands (DEBUG builds)
```
HEAP        - Show heap dump         TRON        - Enable trace
BUFFERS     - Show buffers          TROFF       - Disable trace
DUMP [n]    - Dump memory page n
```

### Variable Declaration
```basic
VAR x = 42              ! Single variable
VAR a, b, c            ! Multiple (default 0)
VAR x = 1, y = 2       ! Multiple with init
CONST PI = 3           ! Constant
INT data[100]          ! Array
```

### Numeric Formats
```basic
42                     ! Decimal
0xFF                   ! Hexadecimal
0x1234                 ! Hex (any size)
```

### Character & String Escapes
```basic
'\n'  '\r'  '\t'      ! Newline, Return, Tab
'\b'  '\f'            ! Backspace, Form feed
'\\'  '\''  '\"'      ! Backslash, Quotes
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
ASC(c)          RND(max)

! Array Storage
IMPORT(arr,"file")              EXPORT(arr,"file")

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
CONST OLED = 0x3C      ! SSD1306 OLED
CONST EEPROM = 0x50    ! 24C256 EEPROM
CONST RTC = 0x68       ! DS3231 RTC
CONST TEMP = 0x48      ! LM75 Temp
```

### Comment Styles
```basic
REM Traditional BASIC comment
VAR x = 5  ! Modern inline comment
```

---

*HopperBASIC v3.3 - A Modern BASIC for 6502 Systems*  
*© 2025 Hopper Development Team*