while_statement := WHILE expression
                  { statement }*
                  WEND# Hopper BASIC Specification v2.11
**Document Type: Language Specification for Hopper BASIC**

## Project Objectives

**Primary Goal**: Create a simple, elegant BASIC interpreter for 6502 systems that fits in 16K ROM and provides an interactive development environment.

**Design Principles**:
- Simplicity over features - Classic BASIC functionality only
- Direct execution - No bytecode, no complex compilation
- Immediate feedback - Interactive development with instant results
- Small footprint - Target 16K ROM, minimal RAM usage

## Target Benchmarks

**Milestone Goal**: Successfully run these two classic BASIC benchmark programs:

### Sieve of Eratosthenes
```basic
' Sieve of Eratosthenes - Byte Magazine benchmark
CONST sizepl = 8191
BIT flags[sizepl]
BEGIN
    WORD i
    WORD prime
    WORD k
    WORD count
    WORD iter
    WORD start
    WORD elapsed
    WORD avgMs
    
    PRINT "10 iterations"
    start = MILLIS()
    
    FOR iter = 1 TO 10
        count = 0
        
        ' Initialize flags array to true
        FOR i = 0 TO sizepl-1
            flags[i] = TRUE
        NEXT i
        
        ' Sieve algorithm
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
    WORD start
    WORD result
    WORD count
    WORD elapsed
    WORD avgMs
    
    start = MILLIS()
    
    FOR count = 0 TO loops-1
        result = Fibo(arg)
    NEXT count
    
    elapsed = MILLIS() - start
    avgMs = elapsed / loops
    
    PRINT name
    PRINT "("
    PRINT arg
    PRINT ") = "
    PRINT result
    PRINT " in "
    PRINT avgMs
    PRINT " ms average"
ENDFUNC

BEGIN
    Benchmark("Fibo", 10, 1)
END
```

---

## Phase 1: Core Functionality (Current Implementation)

### Console Commands
- ✅ **`NEW`** - Clear everything (program, variables, functions)
- ✅ **`LIST`** - Display complete program in creation order
- ✅ **`RUN`** - Execute the main program (BEGIN/END block)
- ✅ **`CLEAR`** - Reset all variables to default values
- ✅ **`VARS`** - Show constants and variables in creation order
- ✅ **`FUNCS`** - Show all functions including main program
- ✅ **`FORGET name`** - Remove variable or function
- ✅ **`MEM`** - Show available memory
- ✅ **`BYE`** - Exit interpreter

### Debug Commands (DEBUG build only)
- ✅ **`HEAP`** - Show heap memory layout
- ✅ **`BUFFERS`** - Show tokenizer and opcode buffer contents
- ✅ **`DUMP [page]`** - Hex dump of memory page (default: page 0)

### Variable Declaration Commands
- ✅ **`INT name [= value]`** - Signed integer (-32768 to 32767)
- ✅ **`WORD name [= value]`** - Unsigned integer (0 to 65535)
- ✅ **`BIT name [= value]`** - Boolean (TRUE or FALSE only)
- ✅ **`BYTE name [= value]`** - 8-bit unsigned (0 to 255)
- ✅ **`STRING name = "value"`** - Mutable string variable

### Constant Declaration Commands
- ✅ **`CONST INT name = value`** - Immutable signed integer
- ✅ **`CONST WORD name = value`** - Immutable unsigned integer
- ✅ **`CONST BIT name = value`** - Immutable boolean
- ✅ **`CONST BYTE name = value`** - Immutable 8-bit unsigned
- ✅ **`CONST STRING name = "value"`** - Immutable string

### Definition Commands
- ✅ **`FUNC name(params)`** - Function definition (multi-line capture)
- ✅ **`BEGIN`** - Main program definition (multi-line capture)

### Core Language Features

#### Basic I/O
- ✅ **`PRINT expr`** - Output single value with newline
- ✅ **`PRINT`** - Output empty line
- ✅ **`PRINT "string"`** - Direct string literal output
- ❌ **Multiple PRINT arguments** - Not implemented

#### Statement Separators
- ✅ **Colon (`:`) separator** - Multiple statements per line

#### Comments
- ✅ **`REM [comment]`** - Full-form comment
- ✅ **`' [comment]`** - Short-form comment

#### Expressions & Operators
- ✅ **Arithmetic**: `+ - * / MOD`
- ✅ **Bitwise**: `& |` (AND, OR)
- ✅ **Unary**: `-` (negation), `NOT` (logical)
- ✅ **Comparison**: `= <> < > <= >=` (returns BIT type)
- ✅ **Logical**: `AND OR NOT` (BIT operands only)
- ✅ **Parentheses**: `( )` for precedence

#### Type System
- ✅ **INT**: 16-bit signed (-32768 to 32767)
- ✅ **WORD**: 16-bit unsigned (0 to 65535)
- ✅ **BIT**: Pure boolean (TRUE or FALSE only)
- ✅ **BYTE**: 8-bit unsigned (0 to 255)
- ✅ **STRING**: Immutable strings with mutable references
- ✅ **Type promotion**: Automatic between compatible types
- ✅ **Type safety**: Comprehensive checking with errors

#### Control Flow
- ✅ **`IF expr THEN statements ENDIF`** - Conditional execution
- ✅ **`IF expr THEN statements ELSE statements ENDIF`** - With alternative
- ✅ **`WHILE expr`...`WEND`** - Conditional loops  
- ✅ **`RETURN [expr]`** - Return from function
- ✅ **`END`** - End main program

#### Assignment
- ✅ **`var = expr`** - Assignment with type checking

#### JIT Compilation System
- ✅ **Expression compilation** - Infix to postfix opcodes
- ✅ **Stack-based execution** - Using Hopper VM stacks
- ✅ **Buffer management** - 512-byte opcode buffer
- ✅ **Opcode dispatch** - Complete instruction set

#### Function System
- ✅ **Function declaration** - FUNC/ENDFUNC syntax
- ✅ **Parameter parsing** - Comma-separated parameters
- ✅ **Multi-line capture** - Interactive definition
- ✅ **BEGIN/END blocks** - Main program as function
- ✅ **Function storage** - Token stream storage
- ✅ **Arguments system** - Parameter management
- ✅ **Function listing** - FUNCS command display
- ✅ **Symbol table** - Functions and variables
- ✅ **Function calls** - Parse and execute
- ✅ **Function execution** - JIT compilation
- ✅ **Call resolution** - CALL→CALLF optimization
- ✅ **Return values** - RETURN statement handling
- ✅ **Call stack** - Recursion support

#### Built-in Functions
- ✅ **`ABS(x)`** - Absolute value
- ❌ **`RND(max)`** - Random number (TODO - returns NotImplemented)
- ✅ **`MILLIS()`** - Milliseconds since startup
- ✅ **`SECONDS()`** - Seconds since startup

#### Memory Access
- ✅ **`PEEK(addr)`** - Read byte from memory
- ✅ **`POKE(addr, value)`** - Write byte to memory

---

## Phase 2: Loop Constructs & Array Support (Next Priority)

### FOR/NEXT Loops
- ❌ **`FOR var = start TO end [STEP increment]`** - Counted loops
- ❌ **`NEXT var`** - End of FOR loop

### Additional Loops
- ✅ **`WHILE expr`...`WEND`** - Conditional loops  
- ❌ **`DO`...`UNTIL expr`** - Post-test loops
- ❌ **`BREAK`** - Exit loops early
- ❌ **`CONTINUE`** - Skip to next iteration

### Array Support
- ❌ **Global Arrays** - Single-dimensional arrays
- ❌ **Array Indexing** - Zero-based access
- ❌ **Array Parameters** - Pass arrays to functions

---

## Phase 3: Enhanced I/O

### Enhanced PRINT
- ❌ **`PRINT expr[,expr...]`** - Comma-separated values
- ❌ **`PRINT expr[;expr...]`** - No separation
- ❌ **`PRINT expr;`** - No newline after
- ❌ **`PRINT expr,`** - Space, no newline

---

## Phase 4: Storage and File Management

### Storage Commands
- ❌ **`SAVE "name"`** - Save to EEPROM
- ❌ **`LOAD "name"`** - Load from EEPROM
- ❌ **`DIR`** - List saved programs
- ❌ **`DEL "name"`** - Delete program

---

## Phase 5: Enhanced Features

### Additional Functions
- ❌ **Conversion functions** - Type conversions
- ❌ **String functions** - Basic manipulation

### Hardware I/O
- ❌ **`READ(pin)`** - Digital input
- ❌ **`WRITE(pin, value)`** - Digital output
- ❌ **`PWM(pin, value)`** - Analog output
- ❌ **`DELAY(ms)`** - Pause execution
- ❌ **`PINMODE(pin, mode)`** - Configure pins

---

## Grammar

### Console Commands
```
console_command := NEW | LIST | RUN | CLEAR | VARS | FUNCS | MEM | BYE
                 | HEAP | BUFFERS | DUMP [number]
                 | FORGET identifier
```

### Variable and Constant Declarations
```
variable_decl := type_keyword identifier [ "=" expression ]
constant_decl := CONST type_keyword identifier "=" expression
type_keyword := INT | WORD | BIT | BYTE | STRING
```

### Program Structure
```
program := { statement }*

statement := variable_decl
           | constant_decl
           | assignment
           | print_statement
           | if_statement
           | while_statement
           | function_definition
           | main_program
           | return_statement
           | comment_statement
           | function_call

assignment := identifier "=" expression

print_statement := PRINT [ expression ]

if_statement := IF expression THEN statement_block [ ELSE statement_block ] ENDIF

while_statement := WHILE expression { statement }* WEND

statement_block := statement [ ":" statement ]*

comment_statement := REM [ comment_text ] | "'" [ comment_text ]

function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { statement }*
                      ENDFUNC

main_program := BEGIN { statement }* END

return_statement := RETURN [ expression ]

parameter_list := identifier [ "," identifier ]*

function_call := identifier "(" [ argument_list ] ")"
argument_list := expression [ "," expression ]*
```

### Expressions
```
expression := logical_or_expr
logical_or_expr := logical_and_expr [ OR logical_and_expr ]
logical_and_expr := comparison_expr [ AND comparison_expr ]
comparison_expr := bitwise_or_expr [ comparison_op bitwise_or_expr ]
comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="
bitwise_or_expr := bitwise_and_expr [ "|" bitwise_and_expr ]
bitwise_and_expr := additive_expr [ "&" additive_expr ]
additive_expr := multiplicative_expr [ ("+" | "-") multiplicative_expr ]
multiplicative_expr := unary_expr [ ("*" | "/" | MOD) unary_expr ]
unary_expr := [ "-" | NOT ] primary_expr
primary_expr := number | identifier | string_literal | TRUE | FALSE
              | "(" expression ")" | function_call | built_in_function

built_in_function := ABS "(" expression ")"
                  | RND "(" expression ")"
                  | PEEK "(" expression ")"
                  | POKE "(" expression "," expression ")"
                  | MILLIS "(" ")"
                  | SECONDS "(" ")"
```

### Lexical Elements
```
number := decimal_digits | hex_number
hex_number := "0" ("x" | "X") hex_digits
identifier := letter [ letter | digit ]*
string_literal := '"' { character }* '"'
```

### Operator Precedence (Highest to Lowest)
1. Function calls, parentheses
2. Unary minus (-), NOT
3. Multiplication (*), Division (/), Modulo (MOD)
4. Addition (+), Subtraction (-)
5. Bitwise AND (&)
6. Bitwise OR (|)
7. Comparison (=, <>, <, >, <=, >=)
8. Logical AND
9. Logical OR

---

## Technical Architecture

### JIT Compilation System
**Compilation Phase:**
1. Infix to Postfix conversion via recursive descent
2. Opcode generation to 512-byte buffer
3. Type checking at compile time
4. Literal optimization

**Execution Phase:**
1. Stack machine using Hopper VM stacks
2. Fast opcode dispatch
3. Runtime type checking and overflow detection
4. Clean API with register preservation

**Opcode Set:**
- No Operands: Arithmetic, logical, comparison operations
- One Byte: PUSHBIT, PUSHBYTE, CALL, CALLF, SYSCALL
- Two Bytes: PUSHINT, PUSHWORD, PUSHCSTRING, PUSHGLOBAL, jumps

### Memory Management
- **Symbol Table**: 4-layer architecture for variables/functions
- **String Architecture**: Immutable strings with pointer comparisons
- **Buffer Management**: Fixed buffers for input, tokens, opcodes
- **Zero Page**: Dedicated allocations for BASIC and symbol tables

### Built on Hopper VM
- Reuses Serial I/O, memory management, stack operations
- Leverages proven runtime code
- Maintains compatibility with memory layout
- Well-defined external API contracts

---

## Current Implementation Status

### ✅ Completed
- Symbol table system with 4-layer architecture
- Complete expression evaluation with JIT compilation
- Type system with promotion and safety
- Variable and constant management
- Function declaration, storage, and execution
- Multi-line capture for functions and main program
- Console commands (NEW, CLEAR, VARS, FUNCS, LIST, FORGET, MEM, BYE)
- Statement processing with colon separators
- IF/THEN/ELSE/ENDIF statements
- WHILE/WEND loops
- RETURN statements with optional expression
- END statement for main program
- Assignment with type checking
- BIT type with TRUE/FALSE literals
- STRING type with literals and comparison
- BYTE type with 8-bit arithmetic
- Function execution with recursion
- RUN command for main program
- Built-in functions (ABS, MILLIS, SECONDS)
- Memory access (PEEK, POKE)
- RND function (declared but TODO)
- BEGIN/END main program definition
- FUNC/ENDFUNC function definition

### ❌ Missing for Benchmarks
1. FOR/NEXT loops - Basic iteration
2. Global array declarations - Array support
3. Array indexing - Array access
4. Multiple PRINT arguments - Complex output

---

## Development Guidelines Compliance

- **Rule #0**: ✅ Project knowledge current and authoritative
- **Rule #1**: ✅ No silent failures - proper error handling
- **Rule #4**: ✅ Complete methods generated
- **Rule #5**: ✅ Analysis-first debugging approach
- **Rule #7**: ✅ C/NC flags for success/failure
- **Rule #8**: ✅ CamelCase identifiers
- **Rule #9**: ✅ Direct enum syntax
- **Rule #10**: ✅ Proper switch statement usage

### Code Quality
- Comprehensive error handling with C/NC status
- Memory leak prevention with proper cleanup
- Type safety throughout operations
- Clear documentation and interfaces
- Debugging support via Tools.Dump* methods
- Clean APIs with register preservation

---

## Usage Examples

### Basic Operations
```basic
> INT x = 10
OK
> WORD count = 0
OK
> PRINT x * 2 + 5
25
> x = x + 1
OK
> PRINT x
11
```

### Functions
```basic
> FUNC Add(a, b)
* RETURN a + b
* ENDFUNC
OK
> PRINT Add(5, 3)
8
```

### Control Flow
```basic
> IF x > 10 THEN PRINT "Big" ELSE PRINT "Small" ENDIF
Big
```

### Memory Access
```basic
> POKE(0x5000, 65)
OK
> PRINT PEEK(0x5000)
65
```

### Built-in Functions
```basic
> PRINT ABS(-42)
42
> PRINT RND(10)
7
```

The implementation provides a solid foundation with complete function execution, JIT compilation, and all core data types. The main missing components for the benchmarks are loop constructs and array support.