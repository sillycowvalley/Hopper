# Hopper BASIC Specification v2.5

## Project Objectives

**Primary Goal**: Create a simple, elegant BASIC interpreter for 6502 systems that fits in 16K ROM and provides an interactive development environment (self-hosted).

**Design Principles**:
- **Simplicity over features** - Classic BASIC functionality only
- **Direct execution** - No bytecode, no complex compilation
- **Immediate feedback** - Interactive development with instant results
- **Small footprint** - Target 16K ROM, minimal RAM usage

## Target Benchmarks

**Milestone Goal**: Successfully run these two classic BASIC benchmark programs:

### Sieve of Eratosthenes (Byte Magazine Benchmark)
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
        
        ' Sieve algorithm
        FOR i = 0 TO sizepl-1
            IF flags[i] THEN
                prime = i + i + 3
                k = i + prime
                WHILE k < sizepl
                    flags[k] = FALSE
                    k = k + prime
                count = count + 1
    
    elapsed = MILLIS() - start
    avgMs = elapsed / 10
    
    PRINT "Done."
    PRINT count
    PRINT " primes"
    PRINT avgMs
    PRINT " ms average"
END
```

### Fibonacci Benchmark with Functions
```basic
FUNC Fibo(n)
    IF n <= 1 THEN RETURN n
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

**Usage Examples:**
- `RUN` - Execute the main program (BEGIN/END block)
- `Benchmark("Fibo", 10, 1)` - Call function directly from REPL
- `LIST` - Show complete program structure (constants, variables, functions, main)

---

## Phase 1: Core Functionality (Current Implementation Status)

### Console Commands
- ✅ **`NEW`** - Clear everything (program, variables, functions)
- ❌ **`LIST`** - Display complete program: constants, variables, functions, main program (creation order)
- ❌ **`RUN`** - Execute the main program (BEGIN/END block)
- ✅ **`CLEAR`** - Reset all variables to default values, keep definitions
- ✅ **`VARS`** - Show constants first, then variables (creation order)
- ❌ **`FUNCS`** - Show all functions in creation order (main program listed last as "__main")
- ❌ **`FORGET name`** - Remove variable or function
- ✅ **`MEM`** - Show available memory
- ✅ **`BYE`** - Exit interpreter

### Debug Commands (DEBUG build only)
- ✅ **`HEAP`** - Show heap memory layout
- ✅ **`BUFFERS`** - Show tokenizer and opcode buffer contents
- ✅ **`DUMP [page]`** - Hex dump of memory page (default: page 0)

### Variable Declaration Commands
- ✅ **`INT name [= value]`** - Create signed integer variable (-32768 to 32767)
- ✅ **`WORD name [= value]`** - Create unsigned integer variable (0 to 65535)
- ✅ **`BIT name [= value]`** - Create boolean variable (0 or 1)

### Constant Declaration Commands
- ✅ **`CONST INT name = value`** - Define immutable signed integer constant
- ✅ **`CONST WORD name = value`** - Define immutable unsigned integer constant
- ✅ **`CONST BIT name = value`** - Define immutable boolean constant

### Definition Commands
- ❌ **`FUNC name(params)`** - Start function definition (ends with `ENDFUNC`)
- ❌ **`BEGIN`** - Start main program definition (ends with `END`)

### Core Language Features

#### Basic I/O
- ✅ **`PRINT expr`** - Output single value followed by newline
- ✅ **`PRINT`** - Output empty line (no arguments)

#### Statement Separators
- ✅ **Colon (`:`) separator** - Multiple statements on one line
  - **Usage**: `PRINT 10 : PRINT 20` executes both print statements
  - **Classic BASIC compatibility** - Standard behavior for multiple commands
  - **Tokenizer support** - Colon recognized as statement boundary token
  - **Parser integration** - Line processor splits on colons and executes each statement

#### Comments
- ✅ **`REM [comment]`** - Full-form comment (traditional BASIC)
- ✅ **`' [comment]`** - Short-form comment (modern convenience)

#### Expressions & Operators (Complete Implementation)
- ✅ **Arithmetic**: `+ - * / MOD` (basic math operations)
- ✅ **Bitwise**: `& |` (bitwise AND, bitwise OR)
- ✅ **Unary**: `-` (negation), `NOT` (logical negation)
- ✅ **Comparison**: `= <> < > <= >=` (all comparison operators - returns BIT type)
- ✅ **Logical**: `AND OR NOT` (BIT operands only)
- ✅ **Parentheses**: `( )` for precedence grouping

#### Type System
- ✅ **INT**: 16-bit signed integer (-32768 to 32767)
- ✅ **WORD**: 16-bit unsigned integer (0 to 65535)
- ✅ **BIT**: Boolean value (0 or 1)
- ✅ **Type promotion**: Automatic promotion between compatible types
- ✅ **Type safety**: Proper type checking with meaningful error messages
- ✅ **Type compatibility checking**: Comprehensive validation for all operations
- ✅ **Mixed-type operations**: INT↔WORD (when safe), with runtime validation

#### Control Flow
- ✅ **`IF expr THEN statement`** - Basic conditional execution
- ❌ **`RETURN [expr]`** - Return from function (stub implemented)
- ❌ **`END`** - End main program (stub implemented)

#### Assignment
- ✅ **`var = expr`** - Assignment to existing variables with type checking

#### JIT Compilation System
- ✅ **Expression compilation** - Infix expressions compiled to postfix opcodes
- ✅ **Stack-based execution** - Efficient opcode execution using value stack
- ✅ **Buffer management** - 512-byte opcode buffer with bounds checking
- ✅ **Opcode dispatch** - Fast execution with comprehensive instruction set

---

## Phase 2: Storage and File Management

### Storage Commands
- ❌ **`SAVE "name"`** - Save complete session to EEPROM (tokenized form)
- ❌ **`LOAD "name"`** - Load complete session from EEPROM
- ❌ **`DIR`** - List saved programs
- ❌ **`DEL "name"`** - Delete saved program

### Technical Implementation
- **Tokenized storage**: Programs saved in tokenized form to save space
- **Complete sessions**: Variables, functions, and main program all saved together
- **EEPROM integration**: Use existing Hopper 6502 SBC I2C library for storage
- **Efficient format**: Minimal overhead for maximum program storage

---

## Phase 3: Extended Features

### Additional Types
- ❌ **`BYTE name [= value]`** - 8-bit unsigned (0 to 255) for hardware I/O
- ❌ **`STRING name [= "value"]`** - Text handling with string operations (maximum length of 255 characters)
- ❌ **Arrays**: `INT numbers[10]` - single-dimensional arrays of integral types (no string arrays)

### Enhanced I/O
- ❌ **`INPUT var`** - Read value from serial console into variable
- ❌ **`INPUT "prompt",var`** - Read with prompt
- ❌ **`INPUT var1, var2, ...`** - Read multiple values in one statement
- ❌ **`PRINT expr[,expr...]`** - Multiple values separated by spaces, newline at end
- ❌ **`PRINT expr[;expr...]`** - Multiple values with no separation, newline at end
- ❌ **`PRINT expr;`** - Output value with no newline (cursor stays on line)
- ❌ **`PRINT expr,`** - Output value followed by space, no newline

### Extended Control Flow
- ❌ **`FOR var = start TO end [STEP increment]`** - Counted loops (required for benchmarks)
- ❌ **`NEXT var`** - End of FOR loop (required for benchmarks)
- ❌ **`WHILE expr`...`WEND`** - Conditional loops (required for benchmarks)
- ❌ **`DO`...`UNTIL expr`** - Post-test conditional loops
- ❌ **`BREAK`** - Exit from loops early
- ❌ **`CONTINUE`** - Skip to next loop iteration
- ❌ **`CONT`** - Continue execution after break (console command)

### Built-in Functions
- ❌ **Math functions**: `ABS(x)`, `RND(x)`
- ❌ **String functions**: `LEN()`, `MID()`, `LEFT()`, `RIGHT()` (if strings added)
- ❌ **Conversion functions**: Type conversion between INT/WORD/BYTE

### Hardware I/O (If space permits)
- ❌ **`READ(pin)`** - Digital pin input
- ❌ **`WRITE(pin, value)`** - Digital pin output
- ❌ **`PWM(pin, value)`** - Analog output
- ❌ **`DELAY(milliseconds)`** - Pause execution
- ❌ **`PINMODE(pin, mode)`** - Configure pin as input/output
- ❌ **`MILLIS()`** - Get milliseconds since startup (returns WORD, required for benchmarks)

---

## Grammar

### Console Commands
```
console_command := NEW | LIST | RUN | CLEAR | VARS | FUNCS | MEM | BYE
                 | HEAP | BUFFERS | DUMP [number]
                 | FORGET identifier
                 | SAVE string_literal
                 | LOAD string_literal  
                 | DIR
                 | DEL string_literal
```

### Variable and Constant Declarations
```
variable_decl := type_keyword identifier [ "=" expression ]
              | type_keyword identifier "[" number "]" [ "=" array_initializer ]
constant_decl := CONST type_keyword identifier "=" expression
type_keyword := INT | WORD | BIT | BYTE | STRING
array_initializer := "{" expression [ "," expression ]* "}"
```

### Program Structure
```
program := { statement }*

statement := variable_decl
           | constant_decl
           | assignment
           | print_statement
           | if_statement
           | for_statement
           | while_statement
           | function_definition
           | main_program
           | return_statement
           | comment_statement
           | function_call

statement_line := statement [ ":" statement ]*

assignment := identifier "=" expression
            | identifier "[" expression "]" "=" expression

for_statement := FOR identifier "=" expression TO expression [ STEP expression ]
                { statement }*
                NEXT identifier

while_statement := WHILE expression
                  { statement }*
                  WEND

print_statement := PRINT [ expression ]

if_statement := IF expression THEN statement

comment_statement := REM [ comment_text ]
                   | "'" [ comment_text ]

comment_text := any_characters_to_end_of_line

function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { statement }*
                      ENDFUNC

main_program := BEGIN
               { statement }*
               END

return_statement := RETURN [ expression ]

parameter_list := identifier [ "," identifier ]*
```

### Expressions (Phase 1 - Complete Implementation)
```
expression := logical_or_expr

logical_or_expr := logical_and_expr [ OR logical_and_expr ]

logical_and_expr := comparison_expr [ AND comparison_expr ]

comparison_expr := bitwise_or_expr [ comparison_op bitwise_or_expr ]
comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="

bitwise_or_expr := bitwise_and_expr [ "|" bitwise_and_expr ]

bitwise_and_expr := additive_expr [ "&" additive_expr ]

additive_expr := multiplicative_expr [ additive_op multiplicative_expr ]
additive_op := "+" | "-"

multiplicative_expr := unary_expr [ multiplicative_op unary_expr ]
multiplicative_op := "*" | "/" | MOD

unary_expr := [ "-" | NOT ] primary_expr

primary_expr := number
              | identifier
              | identifier "[" expression "]"
              | TRUE
              | FALSE
              | "(" expression ")"
              | function_call
              | built_in_function

built_in_function := ABS "(" expression ")"
                  | RND "(" expression ")"
                  | LEN "(" expression ")"
                  | MILLIS "(" ")"
                  | hardware_function

hardware_function := READ "(" expression ")"
                  | WRITE "(" expression "," expression ")"
                  | PWM "(" expression "," expression ")"
                  | DELAY "(" expression ")"
                  | PINMODE "(" expression "," expression ")"

function_call := identifier "(" [ argument_list ] ")"
argument_list := expression [ "," expression ]*

number := decimal_digits | hex_number
hex_number := "0" ("x" | "X") hex_digits
decimal_digits := digit { digit }*
hex_digits := hex_digit { hex_digit }*
identifier := letter [ letter | digit ]*
string_literal := '"' { character }* '"'
```

### Lexical Elements
```
letter := 'A'..'Z' | 'a'..'z'
digit := '0'..'9'
hex_digit := '0'..'9' | 'A'..'F' | 'a'..'f'
character := any printable ASCII character except '"'
whitespace := ' ' | '\t'
statement_separator := ":"
comment := REM any_characters_to_end_of_line
         | "'" any_characters_to_end_of_line
```

### Token Types
```
NUMBER := decimal_digits | hex_number
IDENTIFIER := letter [ letter | digit ]*
KEYWORD := predefined language keywords (PRINT, IF, THEN, CONST, FOR, WHILE, etc.)
OPERATOR := "+" | "-" | "*" | "/" | "=" | "<>" | "<" | ">" | "<=" | ">=" | "&" | "|" | "(" | ")" | "[" | "]" | MOD | AND | OR | NOT | TO | STEP
SEPARATOR := ":" | "," | ";"
COMMENT := REM | "'"
LITERAL := TRUE | FALSE
```

### Type System
- **INT**: 16-bit signed integer (-32768 to 32767)
- **WORD**: 16-bit unsigned integer (0 to 65535)  
- **BIT**: Boolean value (0 or 1)
- **BYTE**: 8-bit unsigned integer (0 to 255) [Phase 3]
- **STRING**: Null-terminated character array [Phase 3]

### Type Promotion and Compatibility Rules
- **INT → WORD**: Compatible only when INT ≥ 0 (runtime check)
- **WORD → INT**: Compatible only when WORD ≤ 32767 (runtime check)
- **All types → BIT**: Only values 0 and 1 are compatible
- **BIT operations**: Logical AND/OR/NOT for BIT types only
- **Bitwise operations**: AND (&), OR (|) for all numeric types (INT, WORD, BIT)
- **Comparison results**: All comparisons return BIT type
- **Operation modes**:
  - **Arithmetic** (rejects BIT): +, -, *, /, MOD
  - **Equality** (allows all): =, <>
  - **Ordering** (allows all): <, >, <=, >=
  - **Bitwise** (allows all): &, |
  - **Logical** (BIT only): AND, OR, NOT

### Operator Precedence (Highest to Lowest)
1. Function calls, parentheses
2. Unary minus (-), Logical NOT
3. Multiplication (*), Division (/), Modulo (MOD)
4. Addition (+), Subtraction (-)
5. Bitwise AND (&)
6. Bitwise OR (|)
7. Comparison (=, <>, <, >, <=, >=)
8. Logical AND (BIT operands only)
9. Logical OR (BIT operands only)

## Console Command Display Order

### VARS Command Output Format
**Display Order**: Constants first (creation order), then variables (creation order)
```
CONST INT SIZE = 100
CONST BIT DEBUG = TRUE  
INT counter = 5
WORD buffer = 200
```

### FUNCS Command Output Format  
**Display Order**: Functions in creation order, main program (__main) listed last
```
FUNC Fibo(n)
FUNC Benchmark(name, arg, loops)
FUNC __main()  // BEGIN/END block shown as special function
```

### LIST Command Output Format
**Complete Program Listing**: VARS output + FUNCS output (maintains creation order)
```
CONST INT SIZE = 100
CONST BIT DEBUG = TRUE  
INT counter = 5
WORD buffer = 200

FUNC Fibo(n)
    IF n <= 1 THEN RETURN n
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

FUNC Benchmark(name, arg, loops)
    [function body...]
ENDFUNC

BEGIN
    [main program body...]
END
```

**Implementation Note**: LIST = VARS + FUNCS, maintaining the symbol table creation order for predictable program structure display.

### Statement Separator Rules

**Colon Separator (`:`):**
- **Multiple statements per line**: `PRINT 10 : PRINT 20`
- **Classic BASIC compatibility**: Standard behavior across many BASIC dialects
- **Execution order**: Left to right, each statement executes completely before next
- **Error handling**: If any statement fails, execution stops at that point
- **Whitespace**: Spaces around colons are optional: `PRINT 10:PRINT 20` works

**Usage Examples:**
```basic
PRINT 10 : PRINT 20
INT x = 5 : PRINT x : x = x + 1 : PRINT x
IF x > 0 THEN PRINT "positive" : PRINT "done"
CONST INT SIZE = 10 : INT buffer = SIZE * 2
```

### Literal Values
- **Decimal numbers**: `123`, `0`, `32767`
- **Hexadecimal numbers**: `0x1F`, `0xFF00`, `0xA0` (case insensitive)
- **Boolean literals**: `TRUE` (value 1), `FALSE` (value 0)
- **Type determination**: Numbers typed as INT (0-32767) or WORD (32768-65535)

---

## Technical Architecture

### JIT Compilation System
The expression evaluation system uses a Just-In-Time compilation approach:

**Compilation Phase:**
1. **Infix to Postfix**: Expressions compiled using recursive descent parser
2. **Opcode Generation**: Stack-based opcodes emitted to 512-byte buffer
3. **Type Checking**: Compile-time type compatibility validation
4. **Optimization**: Efficient literal handling (PUSHBIT, PUSHBYTE, PUSHINT, PUSHWORD)

**Execution Phase:**
1. **Stack Machine**: Opcodes executed using Hopper VM value/type stacks
2. **Fast Dispatch**: Single-byte opcode determines operand count and handler
3. **Error Handling**: Runtime type checking and overflow detection
4. **Register Preservation**: Clean API with documented side effects only

**Opcode Set:**
```
No Operands (0x00-0x3F):
  ADD, SUB, MUL, DIV, MOD, NEG
  BITWISE_AND, BITWISE_OR
  LOGICAL_AND, LOGICAL_OR, LOGICAL_NOT
  EQ, NE, LT, GT, LE, GE
  RETURN, RETURNVAL, DECSP, DUP, NOP

One Byte Operand (0x40-0x7F):
  PUSHBIT, PUSHBYTE
  PUSHLOCAL, POPLOCAL
  JUMPB, JUMPZB, JUMPNZB
  CALL, SYSCALL

Two Byte Operands (0x80-0xBF):
  PUSHINT, PUSHWORD
  PUSHGLOBAL, POPGLOBAL
  JUMPW, JUMPZW, JUMPNZW
```

### Memory Management

#### Symbol Table Architecture (4-Layer System)
**Layer 1: Table.asm** - Generic linked list operations
**Layer 2: Objects.asm** - Symbol node management with type information
**Layer 3: Variables/Functions.asm** - Domain-specific symbol operations
**Layer 4: Statement.asm** - Parser integration and type checking

#### Node Structure
**Variable/Constant Node (Objects layer):**
```
Offset 0-1: next pointer (managed by Table layer)
Offset 2:   symbolType|dataType (packed byte)
            High nibble: SymbolType (VARIABLE=1, CONSTANT=2)
            Low nibble: BasicType (INT=2, WORD=4, BIT=6)
Offset 3-4: tokens pointer (16-bit pointer to initialization token stream)
Offset 5-6: value (16-bit current value)
Offset 7+:  null-terminated name string
```

**Function Node (Objects layer):**
```
Offset 0-1: next pointer (managed by Table layer)
Offset 2:   unused in functions
Offset 3-4: function body tokens pointer (16-bit)
Offset 5-6: arguments list head pointer (16-bit, points directly to first argument)
Offset 7+:  null-terminated function name string
```

**Argument Node (Arguments layer):**
```
Offset 0-1: next pointer (managed by Arguments layer)
Offset 2+:  null-terminated argument name string
```

#### Memory Allocation Strategy
- **Exact-size allocation**: Nodes allocated to exact size needed (overhead + name length)
- **Automatic cleanup**: Memory.Free() called when symbols are removed
- **Token stream management**: Initialization and function body tokens stored separately and freed with symbols
- **Argument integration**: Function arguments stored as separate linked list, automatically cleaned up with function

### Buffer Management
**BasicInputBuffer**: 128 bytes - Raw user input (0x0900-0x097F)
**BasicTokenizerBuffer**: 512 bytes - Tokenized line storage (0x0A00-0x0BFF)
**BasicOpcodeBuffer**: 512 bytes - JIT compiled opcodes (0x0C00-0x0DFF)

**Working Buffers:**
- **BasicProcessBuffer1**: 64 bytes - General workspace (0x0980-0x09BF)
- **BasicProcessBuffer2**: 32 bytes - Statement layer storage (0x09C0-0x09DF)
- **BasicProcessBuffer3**: 32 bytes - Compiler/Executor storage (0x09E0-0x09FF)

### Zero Page Allocation
**BASIC Project Allocation (0x30-0x4F, 32 bytes):**
- **Console Input**: BasicInputLength (0x30)
- **Tokenizer State**: TokenBufferLength, TokenizerPos (0x31-0x34)
- **Error Handling**: LastError pointer (0x35-0x36)
- **Token Cache**: CurrentToken, TokenLiteralPos (0x37-0x39)
- **JIT Compiler**: OpcodeBuffer state, CompilerFlags (0x3A-0x3F)
- **Available**: 16 bytes for future features (0x40-0x4F)

**Symbol Table Allocation (0x70-0x7F, 16 bytes):**
- **Table Heads**: VariablesList, FunctionsList (0x70-0x73)
- **Symbol Working Storage**: SymbolType, SymbolValue, SymbolName, SymbolTokens (0x74-0x7A)
- **Iterator State**: SymbolIteratorFilter (0x7B)
- **Temporary Storage**: SymbolLength, SymbolTemp0-2 (0x7C-0x7F)

### Built on Hopper VM Foundation
- **Reuse existing runtime**: Serial I/O, memory management, stack operations
- **Leverage proven code**: Use existing helper functions and utilities
- **Maintain compatibility**: Work within established memory layout
- **External API contracts**: Well-defined interfaces to Hopper VM services

---

## Current Implementation Status

### Phase 1 Progress: ~90% Complete

**✅ Completed Components:**
- **Symbol Table System**: Complete 4-layer architecture with comprehensive testing
- **Tokenization**: Full lexical analysis with keyword recognition, hex numbers, comments
- **Expression Evaluation**: Complete JIT compilation system with all operators
- **Type System**: Comprehensive type checking and promotion rules
- **Variable Management**: Declaration, assignment, constant enforcement
- **Console Commands**: NEW, CLEAR, VARS, MEM, BYE, debug commands
- **Statement Processing**: Multi-statement lines with colon separators
- **IF/THEN Statements**: Basic conditional execution
- **Assignment**: Variable assignment with type checking
- **Error Handling**: Proper error messages and recovery

**❌ Remaining for Phase 1:**
- **Function System Integration**: FUNC/ENDFUNC definitions and RETURN statements
- **Program Structure**: BEGIN/END main program blocks  
- **Management Commands**: LIST, RUN, FUNCS, FORGET integration

### Next Implementation Priority
**Function System Integration** - The symbol table foundation supports functions completely, but the parser needs:
1. **FUNC/ENDFUNC parsing** - Function definition blocks with parameter lists
2. **RETURN statement handling** - Function exit with optional return values
3. **Function call execution** - Parameter passing and local variable scoping
4. **BEGIN/END blocks** - Main program structure (special case of functions)

### Testing Status
All core systems have comprehensive test coverage:
- **Symbol table operations**: Creation, lookup, type checking, memory management
- **Expression evaluation**: All operators, type promotion, error conditions
- **Tokenization**: All token types, edge cases, buffer management
- **Memory management**: Allocation, deallocation, leak detection
- **Type system**: Compatibility rules, runtime validation, error messages

---

## Development Guidelines Compliance

### Rule Compliance Status
- **Rule #0**: ✅ Project knowledge prioritized for current implementation status
- **Rule #1**: ✅ Silent failures replaced with proper error messages and BRK patterns
- **Rule #4**: ✅ Complete methods generated without "rest of function" shortcuts
- **Rule #5**: ✅ Analysis-first approach for debugging rather than immediate code generation
- **Rule #7**: ✅ C/NC flags used for success/failure status returns
- **Rule #8**: ✅ CamelCase identifiers preferred over SCREAMING_SNAKE_CASE
- **Rule #9**: ✅ Direct enum syntax used (SymbolType.VARIABLE vs Objects.SymbolType.VARIABLE)
- **Rule #10**: ✅ Switch statements use proper break semantics

### Code Quality Measures
- **Comprehensive error handling**: All operations return proper C/NC status
- **Memory leak prevention**: All allocations paired with proper cleanup
- **Type safety**: Strict type checking throughout symbol table operations
- **Clear documentation**: Each layer has defined interfaces and responsibilities
- **Debugging support**: Tools.Dump* methods available for system state inspection
- **Clean API standards**: Register preservation and documented side effects only

---

## Usage Examples

### Basic Variable Operations
```basic
INT count = 5
WORD max = 65000
BIT flag = TRUE
PRINT count : PRINT max : PRINT flag
count = count + 1 : PRINT count
```

### Constant Definitions
```basic
CONST INT SIZE = 100
CONST BIT DEBUG = TRUE
CONST WORD MAX_VALUE = 0xFFFF
INT buffer = SIZE * 2
IF DEBUG THEN PRINT buffer
```

### Expression Evaluation
```basic
INT a = 10 : INT b = 20
PRINT a + b * 2        ' Prints 50 (precedence: * before +)
PRINT (a + b) * 2      ' Prints 60 (parentheses override precedence)
BIT result = a > b
PRINT result           ' Prints 0 (false)
```

### Multi-Statement Lines
```basic
INT x = 5 : INT y = 10 : PRINT x + y : PRINT x * y
IF x < y THEN PRINT "x is smaller" : x = y
```

### Hexadecimal Numbers
```basic
WORD addr = 0x8000
INT offset = 0x100
PRINT addr + offset    ' Prints 32768 + 256 = 33024
```

### Type Mixing
```basic
INT signed = -100
WORD unsigned = 200
' This works because signed value promoted to unsigned safely
PRINT unsigned + 300   ' Prints 500
```

---

## Future Roadmap

### Phase 2: Storage System
- **Tokenized SAVE/LOAD**: Preserve programs in compact binary format
- **EEPROM Integration**: Use I2C library for persistent storage
- **File Management**: DIR, DEL commands for program organization

### Phase 3: Extended Language Features
- **Additional Types**: BYTE, STRING, single-dimensional arrays
- **Control Structures**: FOR/NEXT, WHILE/WEND, DO/UNTIL loops
- **Enhanced I/O**: INPUT statements, formatted PRINT output
- **Built-in Functions**: Math, string manipulation, hardware I/O

### Phase 4: Advanced Features (If ROM space permits)
- **Program Structure**: Multi-line functions and main programs
- **Local Variables**: Function-scoped variables with automatic cleanup
- **Error Recovery**: Structured exception handling
- **Interactive Debugging**: Breakpoints, single-step execution

The current implementation provides a solid foundation that can be extended incrementally while maintaining the core design principles of simplicity and direct execution.