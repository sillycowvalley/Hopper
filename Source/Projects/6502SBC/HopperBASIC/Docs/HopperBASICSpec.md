# Hopper BASIC Specification v3.1
**Document Type: Language Specification for Hopper BASIC**
**Last Updated: Fixed Operator Precedence Grammar + INPUT + Multiple VAR**

## Project Objectives

**Primary Goal**: Create a simple, elegant BASIC interpreter for 6502 systems that fits in 32K ROM and provides an interactive development environment.

**Design Principles**:
- Simplicity and reliability over large feature set
- Immediate feedback - Interactive development with instant results (REPL ala Python)
- Incremental development - easy to persist environment, encourage breakdown into small functions (FORTH-like development)
- Small footprint - Target 32K ROM, minimal RAM usage
- Fast - competitive with the Classic 6502 BASIC's of the day (Apple II+ / Commodore 64 era)
- **Simplified type system** - LONG as single numeric type with strict compatibility rules

## Fundamental Type System Changes

### Core Types
- **LONG**: 32-bit signed integer (-2,147,483,648 to 2,147,483,647) - **The only numeric type for variables and expressions**
- **CHAR**: 8-bit character (ASCII 0-255) - Compatible only with CHAR
- **BIT**: Pure boolean (TRUE or FALSE only) - Compatible only with BIT  
- **STRING**: Immutable strings with mutable references - Compatible only with STRING

### Strict Type Compatibility
- **CHAR** is only compatible with **CHAR**
- **BIT** is only compatible with **BIT**
- **STRING** is only compatible with **STRING**
- **LONG** is only compatible with **LONG**

### Ordering Comparisons
- **CHAR types**: Full ordering support (`< > <= >= = <>`)
- **LONG types**: Full ordering support (`< > <= >= = <>`)
- **BIT and STRING types**: Equality only (`= <>`)

---

## Target Benchmarks ✅ READY TO RUN

Both benchmark programs should now run successfully with the simplified type system!

### Sieve of Eratosthenes
```basic
! Sieve of Eratosthenes - Byte Magazine benchmark
CONST sizepl = 8191
BIT flags[sizepl]
BEGIN
    VAR i
    VAR prime
    VAR k
    VAR count
    VAR iter
    VAR start
    VAR elapsed
    VAR avgMs
    
    PRINT "10 iterations"
    start = MILLIS()
    
    FOR iter = 1 TO 10
        count = 0
        
        ! Initialize flags array to true
        FOR i = 0 TO sizepl-1
            flags[i] = TRUE
        NEXT i
        
        ! Sieve algorithm
        FOR i = 0 TO sizepl-1
            IF flags[i] THEN
                prime = i + i + 3
                k = i + prime
                WHILE k < sizepl
                    flags[k] = FALSE
                    k = k + prime
                WEND
                count = count + 1
            ENDIF
        NEXT i
    NEXT iter
    
    elapsed = MILLIS() - start
    avgMs = elapsed / 10
    
    PRINT "Done."
    PRINT count
    PRINT " primes"
    PRINT avgMs
    PRINT " ms average"
END
```

### Fibonacci Benchmark
```basic
FUNC Fibo(n)
    IF n <= 1 THEN RETURN n ENDIF
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

FUNC Benchmark(name, arg, loops)
    VAR start
    VAR result
    VAR count
    VAR elapsed
    VAR avgMs
    
    start = MILLIS()
    
    FOR count = 0 TO loops-1
        result = Fibo(arg)
    NEXT count
    
    elapsed = MILLIS() - start
    avgMs = elapsed / loops
    
    PRINT name; "("; arg; ") = "; result; " in "; avgMs; " ms average"
ENDFUNC

BEGIN
    Benchmark("Fibo", 10, 1)
END
```

---

## Phase 1: Core Functionality ✅ COMPLETE

### Console Commands
- **`NEW`** - Clear everything (program, variables, functions)
- **`LIST`** - Display complete program in creation order
- **`RUN`** - Execute the main program (BEGIN/END block)
- **`CLEAR`** - Reset all variables to default values
- **`VARS`** - Show constants and variables in creation order
- **`FUNCS`** - Show all functions including main program
- **`FORGET name`** - Remove variable or function
- **`MEM`** - Show available memory
- **`BYE`** - Exit interpreter
- **`SAVE "name"`** - Save program to EEPROM
- **`LOAD "name"`** - Load program from EEPROM
- **`DIR`** - List saved programs
- **`DEL "name"`** - Delete saved program
- **`FORMAT`** - Format storage system

### Debug Commands (DEBUG build only)
- **`HEAP`** - Show heap memory layout
- **`BUFFERS`** - Show tokenizer and opcode buffer contents
- **`DUMP [page]`** - Hex dump of memory page (default: page 0)

### Variable Declaration Commands
- **`VAR name [= value] [, name [= value]]*`** - Runtime-typed variables with implicit type inference and multiple declarations
  - **Single**: `VAR n = 42` (LONG)
  - **Multiple uninitialized**: `VAR A, B, C, D` (all default to LONG with value 0)
  - **Multiple with mixed initialization**: `VAR T = "TEXT", B = FALSE, X = 100`
  - **Type inference from literals**:
    - Numeric literals become LONG: `VAR n = 42`
    - Character literals become CHAR: `VAR c = 'A'`  
    - Boolean literals become BIT: `VAR flag = TRUE`
    - String literals become STRING: `VAR text = "Hello"`

### Constant Declaration Commands (Type Inference)
- **`CONST name = value`** - Immutable constant with type inferred from RHS
  - `CONST N = 10` → LONG constant
  - `CONST C = 'A'` → CHAR constant  
  - `CONST B = TRUE` → BIT constant
  - `CONST S = "STR"` → STRING constant

### Array Declaration Commands (Explicit Typing)
Arrays retain explicit type specifiers for memory efficiency:
- **`BIT name[size]`** - Packed boolean array (8 bits per byte)
- **`CHAR name[size]`** - Character array (1 byte per element)  
- **`BYTE name[size]`** - 8-bit unsigned array (1 byte per element)
- **`WORD name[size]`** - 16-bit unsigned array (2 bytes per element)
- **`INT name[size]`** - 16-bit signed array (2 bytes per element)

### Definition Commands
- **`FUNC name(params)`** - Function definition (multi-line capture)
- **`BEGIN`** - Main program definition (multi-line capture)

### Core Language Features

#### Basic I/O
- **`PRINT expr`** - Output single value with newline
- **`PRINT`** - Output empty line
- **`PRINT "string"`** - Direct string literal output
- **`PRINT expr[,expr...]`** - Comma-separated values with spacing
- **`PRINT expr[;expr...]`** - Semicolon-separated values without spacing
- **`PRINT expr,`** - Trailing comma suppresses newline with spacing
- **`PRINT expr;`** - Trailing semicolon suppresses newline
- ✅ **`INPUT`** - Read user input as LONG value
  - Returns 0 for empty line or parsing error
  - Single character returns ASCII value (A → 65)
  - Numeric input parsed to LONG value

#### Statement Separators
- **Colon (`:`) separator** - Multiple statements per line

#### Comments
- **`REM [comment]`** - Full-form comment
- **`! [comment]`** - Short-form comment

#### Expressions & Operators
- **Arithmetic**: `+ - * / MOD` (LONG types only)
- **Bitwise**: `& | ~` (AND, OR, complement - LONG types only)
- **Unary**: `-` (negation - LONG only), `NOT` (logical - BIT only), `~` (bitwise complement - LONG only)
- **Comparison**: `= <> < > <= >=` (returns BIT type)
  - **LONG comparisons**: ✅ Full support for all comparison operators
  - **CHAR comparisons**: ✅ Full support for all comparison operators  
  - **BIT comparisons**: ✅ Equality only (`=` and `<>`)
  - **STRING comparisons**: ✅ Equality only (`=` and `<>`)
- **Logical**: `AND OR NOT` (BIT operands only)
- **Bitwise Complement**: `~` (LONG operand only)
- **Parentheses**: `( )` for precedence

#### Simplified Type System
- **LONG**: 32-bit signed integer - The single numeric type for all calculations
- **CHAR**: 8-bit character - Independent type for character operations
- **BIT**: Pure boolean - Independent type for logical operations  
- **STRING**: Immutable strings - Independent type for text operations
- **No automatic type promotion** - Strict type compatibility enforced
- **Type safety**: Comprehensive checking with runtime errors for mismatched types

#### String Operations
- **String variables**: ✅ IMPLEMENTED
- **String constants**: ✅ IMPLEMENTED  
- **String comparison**: ✅ IMPLEMENTED (equality only: `=` and `<>`)
- **String indexing**: ✅ IMPLEMENTED - `char = string[index]` for 0-based character access
- **Bounds checking**: ✅ IMPLEMENTED - Runtime error on out-of-bounds access

#### Control Flow
- **`IF expr THEN statements [ELSE statements] ENDIF`** - Conditional execution
- **`WHILE expr`...`WEND`** - Pre-test conditional loops
- **`DO`...`UNTIL expr`** - Post-test conditional loops
- **`FOR var = start TO end [STEP increment]`...`NEXT [var]`** - Counted loops
- **`RETURN [expr]`** - Return from function
- **`END`** - End main program

#### Assignment
- **`var = expr`** - Assignment with strict type checking
- **`array[index] = expr`** - Array element assignment with type checking

#### JIT Compilation System
- **Expression compilation** - Infix to postfix opcodes
- **Stack-based execution** - Using Hopper VM stacks with LONG support
- **Buffer management** - 512-byte opcode buffer
- **Opcode dispatch** - Complete instruction set including FORCHK/FORIT/GETITEM/SETITEM
- **Jump patching** - Forward and backward jump resolution
- **Local variable management** - BP-relative addressing for locals and arguments
- **LONG arithmetic** - 32-bit operations on stack

#### Function System
- **Function declaration** - FUNC/ENDFUNC syntax
- **Parameter parsing** - Comma-separated parameters including arrays
- **Local variables** - VAR declarations within functions (positive BP offset)
- **Arguments** - Function parameters (negative BP offset)
- **Array parameters** - ✅ Arrays can be passed to functions
- **Multi-line capture** - Interactive definition
- **BEGIN/END blocks** - Main program as function
- **Function storage** - Token stream storage
- **Arguments system** - Full parameter management with BP-relative access
- **Function listing** - FUNCS command display
- **Symbol table** - Functions, variables, locals, and arguments
- **Function calls** - Parse and execute with argument passing
- **Function execution** - JIT compilation with stack frame management
- **Call resolution** - CALL→CALLF optimization
- **Return values** - RETURN statement handling
- **Call stack** - Full recursion support with BP/SP management

#### Built-in Functions
- **`ABS(x)`** - Absolute value (LONG → LONG)
- **`ASC(char)`** - Convert CHAR to LONG value
- **`CHR(numeric)`** - Convert LONG to CHAR
  - Range check: Value must be 0-255, runtime error otherwise
  - Returns: CHAR value
- **`LEN(string|array)`** - Return length as LONG
- **`MILLIS()`** - Milliseconds since startup (LONG)
- **`SECONDS()`** - Seconds since startup (LONG)
- **`DELAY(ms)`** - Pause for a delay in milliseconds (LONG parameter)
- **`PEEK(addr)`** - Read byte from memory (LONG → LONG)
- **`POKE(addr, value)`** - Write byte to memory (LONG, LONG)
- ✅ **`INPUT()`** - Read user input and parse as LONG
  - Returns 0 for empty line or parsing error
  - Single character returns ASCII value (A → 65, '0' → 48)
  - Numeric input parsed to LONG value (65 → 65)

---

## Phase 2: Loop Constructs & Array Support ✅ COMPLETE

### FOR/NEXT Loops ✅ COMPLETE
- **`FOR var = start TO end [STEP increment]`** - Counted loops with LONG iterator as local variable
- **`NEXT [var]`** - End of FOR loop (var name optional)
- **Nested loops** - Full support with proper iterator management
- **Optimization** - FORITF opcode for positive-only ranges with STEP 1

### Array Support ✅ COMPLETE
- **Global Arrays** - Single-dimensional arrays with dynamic allocation
- **Array Declaration** - Explicit type syntax: `BIT name[size]`, `CHAR name[size]`, etc.
- **Array Indexing** - Zero-based access with `array[index]` syntax (LONG index)
- **Array Assignment** - `array[index] = value` with type checking
- **Array Parameters** - Arrays can be passed to functions
- **Array Types** - Support for BIT, CHAR, BYTE, WORD, INT arrays (not LONG arrays for memory efficiency)
- **Bounds Checking** - Runtime validation with error reporting
- **Memory Management** - Automatic allocation and cleanup
- **Redimensioning** - Dynamic resizing of arrays
- **GETITEM/SETITEM** - Opcodes for array element access
- **LEN(array)** - Get array element count as LONG

---

## Phase 3: Storage and File Management ✅ COMPLETE

### Storage Commands
- ✅ **`SAVE "name"`** - Save program to EEPROM
- ✅ **`LOAD "name"`** - Load program from EEPROM
- ✅ **`DIR`** - List saved programs
- ✅ **`DEL "name"`** - Delete saved program
- ✅ **`FORMAT`** - Format storage system

---

## Phase 4: Hardware I/O ✅ COMPLETE

### Hardware Commands
- **`READ(pin)`** - Digital input (LONG → BIT) - Returns TRUE or FALSE
- **`WRITE(pin, value)`** - Digital output (LONG, BIT) - Value is TRUE or FALSE
- **`PINMODE(pin, mode)`** - Configure pins (LONG, LONG)

**Note**: All hardware I/O functions are complete with full validation (pin 0-15, mode 0-1). PIN values are BIT type for TRUE/FALSE logic.

---

## Phase 5: Extended Functions ✅ PARTIALLY COMPLETE

### Additional Functions
- ✅ **`INPUT()`** - User input parsing (no prompt) - **IMPLEMENTED**
  - Returns LONG: 0 for empty line, 0 for parse errors
  - Single character returns ASCII: 'A' → 65
  - Numbers parse to LONG: "123" → 123
- ❌ **`RND(max)`** - Random number generation

---

## Phase 6: Advanced Features (Future Consideration)

### Advanced Control Flow
- ❌ **`BREAK`** - Exit loops early
- ❌ **`CONTINUE`** - Skip to next iteration

### String Manipulation
- ❌ **String functions** - Basic string operations beyond indexing

### Character Iteration Support
- ✅ **`FOR char_var = 'start' TO 'end'`** - Character range iteration NOW WORKS
  - CHAR variables work as FOR loop iterators
  - CHAR ordering comparisons enable natural iteration
  - Example: `FOR c = 'A' TO 'Z'` to iterate through alphabet

---

## Grammar - **CORRECTED OPERATOR PRECEDENCE**

### Console Commands
```
console_command := NEW | LIST | RUN | CLEAR | VARS | FUNCS | MEM | BYE
                 | HEAP | BUFFERS | DUMP [number]
                 | FORGET identifier
                 | SAVE string_literal | LOAD string_literal 
                 | DIR | DEL string_literal | FORMAT
```

### Variable and Constant Declarations - **UPDATED FOR MULTIPLE VARS**
```
variable_decl := VAR variable_list
variable_list := variable_item [ "," variable_item ]*
variable_item := identifier [ "=" expression ]
constant_decl := CONST identifier "=" expression
array_decl := array_type identifier "[" expression "]"
array_type := BIT | CHAR | BYTE | WORD | INT
```

### Program Structure
```
program := { statement }*

statement := variable_decl
           | constant_decl
           | array_decl
           | assignment
           | print_statement
           | input_statement
           | if_statement
           | while_statement
           | do_until_statement
           | for_statement
           | function_definition
           | main_program
           | return_statement
           | comment_statement
           | function_call

assignment := identifier "=" expression
            | identifier "[" expression "]" "=" expression

print_statement := PRINT [ print_list ]
print_list := print_item [ separator print_item ]* [ separator ]
print_item := expression
separator := "," | ";"

input_statement := INPUT "(" ")"

if_statement := IF expression THEN statement_block [ ELSE statement_block ] ENDIF

while_statement := WHILE expression { statement }* WEND

do_until_statement := DO { statement }* UNTIL expression

for_statement := FOR identifier "=" expression TO expression [ STEP expression ]
                { statement }*
                NEXT [ identifier ]

statement_block := statement [ ":" statement ]*

comment_statement := REM [ comment_text ] | "!" [ comment_text ]

function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { local_declaration | statement }*
                      ENDFUNC

local_declaration := VAR variable_list

main_program := BEGIN 
                { local_declaration | statement }* 
                END

return_statement := RETURN [ expression ]

parameter_list := identifier [ "," identifier ]*

function_call := identifier "(" [ argument_list ] ")"
argument_list := expression [ "," expression ]*
```

### Expressions - **FIXED OPERATOR PRECEDENCE**
```
expression := logical_or_expr
logical_or_expr := logical_and_expr [ OR logical_and_expr ]
logical_and_expr := comparison_expr [ AND comparison_expr ]
comparison_expr := additive_expr [ comparison_op additive_expr ]
comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="
additive_expr := bitwise_or_expr [ ("+" | "-") bitwise_or_expr ]
bitwise_or_expr := bitwise_and_expr [ "|" bitwise_and_expr ]
bitwise_and_expr := multiplicative_expr [ "&" multiplicative_expr ]
multiplicative_expr := unary_expr [ ("*" | "/" | MOD) unary_expr ]
unary_expr := [ "-" | NOT | "~" ] primary_expr
primary_expr := number | identifier | string_literal | char_literal | TRUE | FALSE
              | "(" expression ")" | function_call | built_in_function
              | identifier "[" expression "]"

built_in_function := ABS "(" expression ")"
                  | ASC "(" expression ")"
                  | CHR "(" expression ")"
                  | LEN "(" expression ")"
                  | PEEK "(" expression ")"
                  | POKE "(" expression "," expression ")"
                  | DELAY "(" expression ")"
                  | MILLIS "(" ")"
                  | SECONDS "(" ")"
                  | INPUT "(" ")"
                  | READ "(" expression ")"
                  | WRITE "(" expression "," expression ")"
                  | PINMODE "(" expression "," expression ")"
```

### Lexical Elements
```
number := decimal_digits | hex_number
hex_number := "0" ("x" | "X") hex_digits
identifier := letter [ letter | digit ]*
string_literal := '"' { character }* '"'
char_literal := "'" character "'"
```

### Operator Precedence (Highest to Lowest) - **CORRECTED**
1. Function calls, parentheses, indexing ([])
2. Unary minus (-), NOT, ~ (bitwise complement)
3. Multiplication (*), Division (/), Modulo (MOD)
4. **Bitwise AND (&)** - **MOVED HIGHER**
5. **Bitwise OR (|)** - **MOVED HIGHER**
6. **Addition (+), Subtraction (-)** - **MOVED LOWER**
7. Comparison (=, <>, <, >, <=, >=)
8. Logical AND
9. Logical OR

**Key Fix**: Bitwise operators (&, |) now have higher precedence than arithmetic (+, -), which makes sense for low-level programming and matches assembly language expectations.

---

## Technical Architecture

### Simplified Type System Architecture
**Core Principle**: Single numeric type (LONG) with strict type boundaries

**Type Compatibility Matrix**:
- LONG ↔ LONG only
- CHAR ↔ CHAR only
- BIT ↔ BIT only  
- STRING ↔ STRING only

**No automatic promotion** - Clean, predictable type behavior

### JIT Compilation System
**Compilation Phase:**
1. Infix to Postfix conversion via recursive descent with **corrected precedence**
2. Opcode generation to 512-byte buffer with LONG arithmetic support
3. Strict type checking at compile time - no implicit conversions
4. Literal optimization
5. Jump offset calculation and patching
6. Local variable and argument allocation with BP-relative addressing

**Execution Phase:**
1. Stack machine using Hopper VM stacks with 32-bit LONG operations
2. Fast opcode dispatch including FOR loop and array opcodes
3. Runtime type checking with strict compatibility enforcement
4. Stack frame management with BP/SP
5. Clean API with register preservation

**Enhanced Opcode Set for LONG Support**:
- Arithmetic operations extended for 32-bit LONG values
- Type-specific opcodes for each core type (LONG, CHAR, BIT, STRING)
- Array access opcodes with proper type checking
- FOR loop opcodes optimized for LONG iterators

### Memory Management
- **Symbol Table**: 4-layer architecture for variables/functions/locals/arguments
- **String Architecture**: Immutable strings with pointer comparisons
- **Array Architecture**: Dynamic allocation with header (count, type, elements)
  - Arrays use compact storage types (BIT, CHAR, BYTE, WORD, INT)
  - Variable expressions use LONG for all numeric operations
- **Buffer Management**: Fixed buffers for input, tokens, opcodes
- **Zero Page**: Dedicated allocations for BASIC and symbol tables with LONG support
- **Stack Frame**: BP-relative addressing for locals (positive offset) and arguments (negative offset)

### Array Implementation
- **Memory Layout**: 2-byte count + 1-byte type + element data
- **Element Storage**:
  - BIT: Packed 8 bits per byte
  - CHAR: One byte per element
  - BYTE: One byte per element  
  - WORD: Two bytes per element (LSB first)
  - INT: Two bytes per element (LSB first)
- **Index Operations**: All array indices are LONG values
- **Dynamic Management**: Allocation, redimensioning, automatic cleanup
- **Type Safety**: Runtime type checking for assignments with strict compatibility
- **Bounds Checking**: Runtime validation with error reporting
- **Array Parameters**: Arrays passed by reference to functions

### Built on Hopper VM
- Reuses Serial I/O, memory management, stack operations
- Extended with 32-bit LONG arithmetic support
- Leverages proven runtime code
- Maintains compatibility with memory layout
- Well-defined external API contracts

---

## Current Implementation Status Summary

### ✅ Benchmark-Ready Status
**Both target benchmark programs should now run successfully with simplified type system!**

The interpreter has achieved its primary goal with:
- Simplified LONG-based numeric system with **corrected operator precedence**
- Complete array implementation including array parameters
- Full character comparison support (equality and ordering)
- Robust function system with recursion
- All required control structures
- Strict type system with clear compatibility rules
- JIT compilation for performance with LONG support
- **Multi-variable declarations** for convenience
- **INPUT function** for interactive programs

### Feature Completeness by Phase:
- **Phase 1**: ✅ COMPLETE - Core functionality with simplified types
- **Phase 2**: ✅ COMPLETE - Loops and arrays
- **Phase 3**: ✅ COMPLETE - Storage and file management
- **Phase 4**: ✅ COMPLETE - Hardware I/O
- **Phase 5**: 🔧 **PARTIALLY COMPLETE** - INPUT implemented, RND pending
- **Phase 6**: ❌ Not started - Advanced features

---

## Usage Examples

### Multiple Variable Declarations - **NEW FEATURE**
```basic
! Multiple variables without initialization (default to LONG = 0)
> VAR A, B, C, D
OK
> PRINT A, B, C, D
0 0 0 0

! Multiple variables with mixed initialization and type inference
> VAR text = "Hello", flag = TRUE, count = 42
OK
> PRINT text, flag, count
Hello TRUE 42

! Mix of initialized and uninitialized
> VAR X = 100, Y, Z = 200  
OK
> PRINT X, Y, Z
100 0 200
```

### INPUT Function Usage - **NEW FEATURE**
```basic
! Interactive number input
> PRINT "Enter number: ";
Enter number: > VAR n = INPUT()
*   [User enters: 123]
OK  
> PRINT n
123

! Character input returns ASCII value
> VAR ch = INPUT()
*   [User enters: A]
OK
> PRINT ch          ! Shows ASCII value
65
> PRINT CHR(ch)     ! Convert back to character
A

! Empty input or errors return 0
> VAR empty = INPUT()
*   [User presses Enter]
OK
> PRINT empty
0

! Interactive program example
> BEGIN
*   VAR guess, target = 42
*   PRINT "Guess the number (1-100): ";
*   guess = INPUT()
*   IF guess = target THEN
*     PRINT "Correct!"
*   ELSE
*     PRINT "Wrong, it was "; target
*   ENDIF
* END
```

### Corrected Operator Precedence Examples
```basic
! Bitwise operations now bind tighter than arithmetic
> PRINT 2 + 3 & 4     ! Now: 2 + (3 & 4) = 2 + 0 = 2
2

> PRINT 8 | 1 + 2     ! Now: (8 | 1) + 2 = 9 + 2 = 11  
11

! Bitwise complement operator
> PRINT ~0            ! All bits flipped: ~0 = -1 (0xFFFFFFFF)
-1

> PRINT ~(-1)         ! All bits flipped: ~(-1) = 0
0

> VAR mask = ~(1 << 3)  ! Create bit mask (if we had << operator)
> VAR mask = ~8         ! Invert bit 3: ~8 = -9 (0xFFFFFFF7)
> PRINT mask
-9

! Use parentheses to override precedence when needed
> PRINT (2 + 3) & 4   ! Force addition first: 5 & 4 = 4
4

! Complex expression showing all precedence levels
> VAR result = 10 + 5 & 3 | 1 * 2 > 15
> ! Evaluates as: 10 + ((5 & 3) | (1 * 2)) > 15
> ! = 10 + (1 | 2) > 15 = 10 + 3 > 15 = 13 > 15 = FALSE
> PRINT result
FALSE

! Bitwise complement with arithmetic
> PRINT ~5 + 1        ! (~5) + 1 = -6 + 1 = -5
-5

> PRINT ~(5 + 1)      ! ~(6) = -7
-7
```

### Basic Operations with Simplified Types
```basic
> VAR x = 10          ! LONG variable
OK
> VAR count = 0       ! LONG variable  
OK
> CONST name = "Test" ! STRING constant
OK
> CONST flag = TRUE   ! BIT constant
OK
> PRINT x * 2 + 5
25
> x = x + 1
OK
> PRINT x
11
```

### Array Operations
```basic
> BIT flags[100]      ! BIT array for memory efficiency
OK
> INT numbers[10]     ! INT array (16-bit elements)
OK
> VAR index = 5       ! LONG index variable
OK
> flags[0] = TRUE
OK
> numbers[index] = 42
OK
> PRINT numbers[5]
42

! Array with dimension expression
> CONST size = 50
OK
> WORD data[size * 2] ! Expression evaluation uses LONG arithmetic
OK
```

### Array Parameters to Functions
```basic
> BIT flags[20]
OK
> FUNC CountTrue(arr)
*   VAR total = 0           ! LONG local variable
*   FOR i = 0 TO LEN(arr)-1 ! LONG iterator
*     IF arr[i] THEN total = total + 1 ENDIF
*   NEXT i
*   RETURN total
* ENDFUNC
OK
> FOR i = 0 TO 19 STEP 2
*   flags[i] = TRUE
* NEXT i
OK
> PRINT CountTrue(flags)
10
```

### Character and String Operations
```basic
> VAR letter = 'A'        ! CHAR variable
OK
> VAR name = "HOPPER"     ! STRING variable
OK
> VAR first = name[0]     ! Gets 'H' as CHAR
OK
> PRINT first
H
> PRINT ASC(first)        ! Returns LONG value
72

! Character comparisons work fully:
> IF letter >= 'A' AND letter <= 'Z' THEN
*   PRINT "Uppercase letter"
* ENDIF
Uppercase letter

! Character iteration
> FOR ch = 'A' TO 'Z'     ! CHAR iteration variable
*   PRINT CHR(ch);
* NEXT ch
ABCDEFGHIJKLMNOPQRSTUVWXYZ
```

### Functions with LONG Arithmetic
```basic
> FUNC Add(a, b)
*   VAR sum         ! LONG local variable
*   sum = a + b     ! LONG arithmetic
*   RETURN sum
* ENDFUNC
OK
> PRINT Add(5, 3)
8

> FUNC Factorial(n)
*   IF n <= 1 THEN RETURN 1 ENDIF    ! LONG comparison
*   RETURN n * Factorial(n - 1)      ! LONG arithmetic
* ENDFUNC
OK
> PRINT Factorial(5)
120

! Large number support with LONG
> PRINT Factorial(10)
3628800
```

### Control Flow
```basic
> IF x > 10 THEN PRINT "Big" ELSE PRINT "Small" ENDIF
Big

> VAR i = 0
> WHILE i < 5
*   PRINT i
*   i = i + 1
* WEND

> DO
*   PRINT "At least once"
*   i = i + 1
* UNTIL i > 10
```

### Enhanced PRINT with Separators
```basic
! Comma separator adds spacing
> PRINT "X =", x, "Y =", count
X = 11 Y = 0

! Semicolon separator for compact output
> PRINT "Value"; x; "Count"; count
Value11Count0

! Trailing separators control newlines
> FOR i = 1 TO 5
*   PRINT i;        ! Compact: 12345
* NEXT i
12345

> FOR i = 1 TO 5  
*   PRINT i,        ! Spaced: 1 2 3 4 5
* NEXT i
1 2 3 4 5
```

### Hardware I/O
```basic
> PINMODE(13, 1)    ! Set pin 13 as output (LONG parameters)
OK
> WRITE(13, TRUE)   ! Turn on LED (BIT value)
OK
> PRINT READ(12)    ! Read pin 12, returns BIT (TRUE or FALSE)
FALSE
> WRITE(13, FALSE)  ! Turn off LED (BIT value)
OK
```

### Memory Access
```basic
> POKE(0x5000, 65)  ! LONG address and value
OK
> PRINT PEEK(0x5000) ! Returns LONG
65
```

### Built-in Functions with LONG Support
```basic
> PRINT ABS(-42)     ! LONG → LONG
42
> PRINT MILLIS()     ! Returns LONG
12345
> PRINT LEN("HELLO") ! Returns LONG
5
> PRINT CHR(65)      ! LONG → CHAR
A
> PRINT ASC('Z')     ! CHAR → LONG
90
> DELAY(1000)        ! LONG parameter - Wait 1 second
OK

! New INPUT function
> VAR userInput = INPUT()  ! No prompt - waits for input
*   [User enters: 456]
OK
> PRINT userInput
456

! Bitwise operations including complement
> PRINT 15 & 7       ! Bitwise AND: 15 & 7 = 7
7
> PRINT 8 | 4        ! Bitwise OR: 8 | 4 = 12
12  
> PRINT ~15          ! Bitwise complement: ~15 = -16
-16
```

---

## Development Guidelines Compliance

- **Rule #0**: Project knowledge current and authoritative
- **Rule #1**: No silent failures - proper error handling throughout
- **Rule #2**: Stack preferred over zero page for temporaries
- **Rule #3**: Clear flag comments (Set Z/Set NZ)
- **Rule #4**: Complete methods always generated
- **Rule #5**: Analysis-first debugging approach
- **Rule #6**: X-indexed zero page indirect addressing forbidden
- **Rule #7**: C/NC flags for success/failure status
- **Rule #8**: CamelCase identifiers used consistently
- **Rule #9**: Direct enum syntax without qualification
- **Rule #10**: Proper switch statement usage without break
- **Rule #11**: "What changed?" approach for debugging regressions

### Code Quality
- Comprehensive error handling with C/NC status
- Memory leak prevention with proper cleanup
- Strict type safety with no implicit conversions
- Clear documentation and interfaces
- Debugging support via Tools.Dump* methods
- Clean APIs with register preservation
- Nested structure support with proper state management
- 32-bit LONG arithmetic support throughout

---

## Grammar Changes Summary

### **🔧 Critical Fix: Operator Precedence**
**Problem**: Arithmetic operators (+, -) were binding tighter than bitwise operators (&, |)
**Solution**: Reordered grammar so bitwise operations parse before arithmetic

**Old (Incorrect)**:
```
bitwise_and_expr := additive_expr [ "&" additive_expr ]      ! + binds tighter than &
additive_expr := multiplicative_expr [ ("+" | "-") multiplicative_expr ]
```

**New (Correct)**:
```
additive_expr := bitwise_or_expr [ ("+" | "-") bitwise_or_expr ]    ! & binds tighter than +
bitwise_or_expr := bitwise_and_expr [ "|" bitwise_and_expr ]
bitwise_and_expr := multiplicative_expr [ "&" multiplicative_expr ]
```

**Expression Impact**:
- `2 + 3 & 4` now correctly evaluates as `2 + (3 & 4)` instead of `(2 + 3) & 4`
- `8 | 1 + 2` now correctly evaluates as `(8 | 1) + 2` instead of `8 | (1 + 2)`

### **✅ Added Multiple Variable Declaration Support**
```
variable_decl := VAR variable_list
variable_list := variable_item [ "," variable_item ]*
variable_item := identifier [ "=" expression ]
```

### **✅ Added INPUT Function Support**
```
input_statement := INPUT "(" ")"
built_in_function := ... | INPUT "(" ")" | ...
```

---

## Next Steps

The core interpreter is **feature-complete for the benchmark programs** with the **corrected operator precedence**. The fixed grammar ensures:

1. **Assembly-like precedence** - Bitwise operations bind tighter than arithmetic
2. **Predictable evaluation** - Complex expressions evaluate as expected in low-level context  
3. **Compatibility** - Maintains existing functionality while fixing precedence
4. **Enhanced usability** - Multiple VAR declarations and INPUT for interactive programs

With the precedence fix and new features, the implementation achieves its goal of providing a clean, efficient BASIC interpreter suitable for 6502 systems with proper low-level operation precedence.