# Hopper BASIC Specification v2.9
**Document Type: Language Specification**

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

**Usage Examples:**
- `RUN` - Execute the main program (BEGIN/END block)
- `Benchmark("Fibo", 10, 1)` - Call function directly from REPL
- `LIST` - Show complete program structure (constants, variables, functions, main)

---

## Phase 1: Core Functionality (Current Implementation Status)

### Console Commands
- ✅ **`NEW`** - Clear everything (program, variables, functions)
- ✅ **`LIST`** - Display complete program: constants, variables, functions, main program (creation order)
- ❌ **`RUN`** - Execute the main program (BEGIN/END block) - **stub implemented**
- ✅ **`CLEAR`** - Reset all variables to default values, keep definitions
- ✅ **`VARS`** - Show constants first, then variables (creation order)
- ✅ **`FUNCS`** - Show all functions in creation order (main program listed last as "BEGIN")
- ✅ **`FORGET name`** - Remove variable or function
- ✅ **`MEM`** - Show available memory
- ✅ **`BYE`** - Exit interpreter

### Debug Commands (DEBUG build only)
- ✅ **`HEAP`** - Show heap memory layout
- ✅ **`BUFFERS`** - Show tokenizer and opcode buffer contents
- ✅ **`DUMP [page]`** - Hex dump of memory page (default: page 0)

### Variable Declaration Commands
- ✅ **`INT name [= value]`** - Create signed integer variable (-32768 to 32767)
- ✅ **`WORD name [= value]`** - Create unsigned integer variable (0 to 65535)
- ✅ **`BIT name [= value]`** - Create boolean variable (TRUE or FALSE only)
- ✅ **`BYTE name [= value]`** - Create 8-bit unsigned variable (0 to 255)
- ✅ **`STRING name = "value"`** - Create mutable string variable (contents are immutable, reference can change)

### Constant Declaration Commands
- ✅ **`CONST INT name = value`** - Define immutable signed integer constant
- ✅ **`CONST WORD name = value`** - Define immutable unsigned integer constant
- ✅ **`CONST BIT name = value`** - Define immutable boolean constant
- ✅ **`CONST BYTE name = value`** - Define immutable 8-bit unsigned constant  
- ✅ **`CONST STRING name = "value"`** - Define immutable string constant

### Definition Commands
- ✅ **`FUNC name(params)`** - Start function definition (ends with `ENDFUNC`) - **multi-line capture mode implemented**
- ✅ **`BEGIN`** - Start main program definition (ends with `END`) - **multi-line capture mode implemented**

### Core Language Features

#### Basic I/O
- ✅ **`PRINT expr`** - Output single value followed by newline
- ✅ **`PRINT`** - Output empty line (no arguments)
- ✅ **`PRINT "string"`** - Direct string literal output
- ✅ **Function arguments**: Direct string literals as function arguments (`MyFunc("Hello")`)

#### Statement Separators
- ✅ **Colon (`:`) separator** - Multiple statements on one line
  - **Usage**: `PRINT 10 : PRINT 20` executes both print statements
  - **Classic BASIC compatibility** - Standard behavior for multiple commands
  - **Tokenizer support** - Colon recognized as statement boundary token
  - **Parser integration** - Line processor splits on colons and executes each statement

#### Comments
- ✅ **`REM [comment]`** - Full-form comment (traditional BASIC)
- ✅ **`' [comment]`** - Short-form comment (modern convenience)

#### Expressions & Operators
- ✅ **Arithmetic**: `+ - * / MOD` (basic math operations)
- ✅ **Bitwise**: `& |` (bitwise AND, bitwise OR)
- ✅ **Unary**: `-` (negation), `NOT` (logical negation)
- ✅ **Comparison**: `= <> < > <= >=` (all comparison operators - returns BIT type)
- ✅ **Logical**: `AND OR NOT` (BIT operands only)
- ✅ **Parentheses**: `( )` for precedence grouping

#### Type System
- ✅ **INT**: 16-bit signed integer (-32768 to 32767)
- ✅ **WORD**: 16-bit unsigned integer (0 to 65535)
- ✅ **BIT**: Pure boolean type (TRUE or FALSE only)
  - **Values**: Only TRUE (1) and FALSE (0) literals allowed
  - **Type Isolation**: Incompatible with all numeric types (INT, WORD, BYTE)
  - **Comparisons**: Only BIT = BIT and BIT <> BIT supported
  - **No Ordering**: BIT < BIT, BIT > BIT, etc. → TYPE MISMATCH
  - **No Arithmetic**: BIT + anything → TYPE MISMATCH
  - **Logical Only**: AND, OR, NOT operations exclusively for BIT types
- ✅ **BYTE**: 8-bit unsigned integer (0 to 255)
- ✅ **STRING**: Immutable string literals with mutable variables
- ✅ **Type promotion**: Automatic promotion between compatible types
- ✅ **Type safety**: Proper type checking with meaningful error messages
- ✅ **Type compatibility checking**: Comprehensive validation for all operations
- ✅ **Mixed-type operations**: INT↔WORD (when safe), with runtime validation

### BIT Type Design Philosophy

The BIT type is designed as a **pure boolean type** that is intentionally isolated from numeric types to prevent logical errors and enforce type safety:

#### ✅ **Allowed BIT Operations:**
```basic
BIT a = TRUE
BIT b = FALSE
BIT result1 = a AND b      ' Logical operations with other BIT types
BIT result2 = a OR b       ' Logical operations 
BIT result3 = NOT a        ' Logical negation
BIT result4 = a = b        ' BIT equality comparison
BIT result5 = a <> b       ' BIT inequality comparison
BIT result6 = a & b        ' Bitwise AND (since BIT is 0 or 1)
BIT result7 = a | b        ' Bitwise OR (since BIT is 0 or 1)
```

#### ❌ **Rejected BIT Operations (TYPE MISMATCH):**
```basic
BIT flag = TRUE
INT number = 5
WORD count = 10

' Cross-type comparisons
PRINT flag = number        ' TYPE MISMATCH - BIT cannot compare with INT
PRINT flag <> count        ' TYPE MISMATCH - BIT cannot compare with WORD
PRINT number = flag        ' TYPE MISMATCH - INT cannot compare with BIT

' Ordering operations between BIT types
PRINT flag < TRUE          ' TYPE MISMATCH - BIT has no ordering concept
PRINT flag > FALSE         ' TYPE MISMATCH - BIT ordering is meaningless
PRINT flag <= TRUE         ' TYPE MISMATCH - No BIT ordering supported
PRINT flag >= FALSE        ' TYPE MISMATCH - No BIT ordering supported

' Arithmetic operations
PRINT flag + 1             ' TYPE MISMATCH - BIT is not numeric
PRINT number * flag        ' TYPE MISMATCH - BIT cannot participate in arithmetic

' Assignment type mismatches
flag = number              ' TYPE MISMATCH - Cannot assign INT to BIT
number = flag              ' TYPE MISMATCH - Cannot assign BIT to INT
```

#### **Design Rationale:**
1. **Type Safety**: Prevents logical errors like `if (count = true)` where assignment was intended
2. **Clear Semantics**: BIT represents true/false states, not numeric values 0/1
3. **Self-Documenting Code**: Forces explicit conversion when mixing boolean and numeric concepts
4. **Error Prevention**: Catches common programming mistakes at compile time

#### **Working with BIT Values:**
```basic
' Correct patterns for BIT usage
BIT isReady = FALSE
INT count = 0

' Conditional logic
IF isReady = TRUE THEN count = count + 1

' BIT-only operations
BIT result = isReady AND (count > 5)   ' Error: cannot mix BIT and numeric in expression
BIT result = isReady AND TRUE          ' Correct: BIT-only expression

' Proper flag usage
IF isReady = TRUE THEN PRINT "System ready"
IF isReady <> FALSE THEN PRINT "Not false"

' BIT assignment from expressions  
BIT comparison = (count > 10)          ' Comparison result is BIT type
BIT combined = comparison AND isReady  ' BIT-only logical operation
```

#### **TRUE/FALSE Literal Behavior:**
- `TRUE` and `FALSE` are **BIT literals** (not numeric values)
- `TRUE` has BIT type with value 1
- `FALSE` has BIT type with value 0  
- Cannot be used in numeric expressions
- Only compatible with other BIT values

**Correct Usage:**
```basic
BIT flag1 = TRUE           ' BIT literal assignment
BIT flag2 = FALSE          ' BIT literal assignment
BIT result = flag1 = TRUE  ' BIT comparison
```

**Incorrect Usage:**
```basic
INT number = TRUE          ' TYPE MISMATCH - TRUE is BIT, not INT
WORD count = FALSE         ' TYPE MISMATCH - FALSE is BIT, not WORD
BIT flag = 1               ' TYPE MISMATCH - 1 is INT, not BIT
BIT flag = 0               ' TYPE MISMATCH - 0 is INT, not BIT
```

### Type Promotion and Compatibility Rules
- ✅ **INT → WORD**: Compatible only when INT ≥ 0 (runtime check)
- ✅ **WORD → INT**: Compatible only when WORD ≤ 32767 (runtime check)
- ✅ **BYTE → INT/WORD**: Always compatible (promotes to larger type)
- ✅ **INT/WORD → BYTE**: Compatible only when value ≤ 255 (runtime check)
- ✅ **BIT Type Isolation**: BIT is incompatible with all other types (INT, WORD, BYTE, STRING)
- ✅ **STRING operations**: Only equality comparison supported (`=`, `<>`)
- ✅ **BIT operations**: Logical AND/OR/NOT for BIT types only
- ✅ **Bitwise operations**: AND (&), OR (|) for numeric types only (INT, WORD, BYTE) - **excludes BIT**
- ✅ **Comparison results**: All comparisons return BIT type
- ✅ **Operation modes**:
  - **Arithmetic** (numeric only): +, -, *, /, MOD
    - **Allowed**: INT, WORD, BYTE (with appropriate promotion)
    - **Rejected**: BIT, STRING → TYPE MISMATCH
  - **Equality** (same type groups): =, <>
    - **BIT group**: BIT = BIT, BIT <> BIT
    - **Numeric group**: INT/WORD/BYTE cross-comparisons (with promotion)
    - **STRING group**: STRING = STRING, STRING <> STRING  
    - **Cross-group**: TYPE MISMATCH (BIT vs numeric, STRING vs numeric, BIT vs STRING)
  - **Ordering** (numeric only): <, >, <=, >=
    - **Allowed**: INT, WORD, BYTE (with appropriate promotion)
    - **Rejected**: BIT, STRING → TYPE MISMATCH
  - **Bitwise** (numeric only): &, |
    - **Allowed**: INT, WORD, BYTE
    - **Rejected**: BIT, STRING → TYPE MISMATCH
  - **Logical** (BIT only): AND, OR, NOT
    - **Allowed**: BIT AND BIT, BIT OR BIT, NOT BIT
    - **Rejected**: All other types → TYPE MISMATCH

#### Control Flow
- ✅ **`IF expr THEN statement`** - Basic conditional execution
- ✅ **`RETURN [expr]`** - Return from function (parsing and execution complete)
- ✅ **`END`** - End main program (parsing and execution complete)

#### Assignment
- ✅ **`var = expr`** - Assignment to existing variables with type checking

#### JIT Compilation System
- ✅ **Expression compilation** - Infix expressions compiled to postfix opcodes
- ✅ **Stack-based execution** - Efficient opcode execution using value stack
- ✅ **Buffer management** - 512-byte opcode buffer with bounds checking
- ✅ **Opcode dispatch** - Fast execution with comprehensive instruction set

#### Function System Architecture
- ✅ **Function declaration**: Complete FUNC/ENDFUNC syntax parsing
- ✅ **Parameter parsing**: Function parameter lists with comma separation
- ✅ **Multi-line capture**: Interactive function definition across multiple lines
- ✅ **BEGIN/END blocks**: Main program definition with same capture system
- ✅ **Function storage**: Token stream storage for function bodies
- ✅ **Arguments system**: Parameter storage and management
- ✅ **Function listing**: FUNCS command displays all defined functions with formatted output
- ✅ **Symbol table integration**: Functions stored alongside variables with proper cleanup
- ✅ **Function call parsing**: Parse function calls in expressions and statements
- ✅ **Function execution**: Execute stored function bodies with JIT compilation
- ✅ **Function call resolution**: CALL→CALLF opcode patching for performance
- ✅ **Return value handling**: Process RETURN statements and pass values
- ✅ **Call stack management**: Support for recursion and nested calls

---

## Phase 2: String Support and Enhanced I/O

### String Constants and Variables
- ✅ **`CONST STRING name = "value"`** - Immutable string constants (name cannot be reassigned)
- ✅ **`STRING name = "value"`** - Mutable string variables (name can be reassigned to different string literals)
- ✅ **String literals**: `"Hello World"` in expressions, PRINT statements, and function arguments
- ✅ **String comparison**: `IF name = "admin" THEN ...`
- ✅ **Direct string printing**: `PRINT "Hello"`
- ✅ **Function arguments**: `MyFunction("Hello", "World")`

### Enhanced I/O
- ✅ **`PRINT "string"`** - Direct string literal output
- ❌ **`PRINT expr[,expr...]`** - Multiple values separated by spaces, newline at end
- ❌ **`PRINT expr[;expr...]`** - Multiple values with no separation, newline at end
- ❌ **`PRINT expr;`** - Output value with no newline (cursor stays on line)
- ❌ **`PRINT expr,`** - Output value followed by space, no newline

### String Type Behavior (By Design)
**Supported:**
```basic
CONST STRING greeting = "Hello World"    // Immutable string constant
STRING message = "Initial"               // Mutable string variable
message = "New Value"                    // String reassignment allowed
PRINT greeting                           // Direct string printing
PRINT "Direct literal"                   // Direct string literals
IF message = "OK" THEN PRINT "Success"   // String comparison
MyFunction("Hello", message)             // String literals and variables as function arguments
```

**Deliberately Unsupported:**
```basic
message[0] = 'H'                         // Character modification (not supported)
message = greeting + " World"            // String concatenation (not supported)
INPUT message                            // String input (not supported)
LEN(message)                             // String functions (not supported)
MID(message, 1, 3)                       // String manipulation (not supported)
```

**Design Rationale:** STRING type supports both constant and variable string references, but string **contents** are always immutable. Variables can be reassigned to point to different string literals, but individual characters cannot be modified. This avoids complex string memory management while supporting essential string functionality for benchmarks and basic programs.

---

## Phase 3: Storage and File Management

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

## Phase 4: Extended Features

### Additional Types
- ✅ **`BYTE name [= value]`** - 8-bit unsigned (0 to 255) for hardware I/O
- ❌ **Arrays**: `INT numbers[10]` - single-dimensional arrays of integral types

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
- ❌ **Memory functions**: `PEEK(address)`, `POKE(address, value)`
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
              | type_keyword identifier "[" number "]"
              | STRING identifier "=" string_literal
constant_decl := CONST type_keyword identifier "=" expression
              | CONST STRING identifier "=" string_literal
type_keyword := INT | WORD | BIT | BYTE
```

**Array Declaration Rules:**
- **Global scope only**: Array declarations are only allowed at global scope (not inside functions)
- **Fixed size**: Array size must be a compile-time constant
- **Zero initialization**: Arrays are automatically initialized to zero values (0 for numeric types, FALSE for BIT arrays)
- **No initialization syntax**: Array initialization lists are not supported
- **Function parameters**: Global arrays can be passed as arguments to functions
- **No local arrays**: Functions cannot declare local array variables
- **BIT array optimization**: BIT arrays store 8 elements per byte for efficient memory usage

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

print_statement := PRINT [ print_list ]
print_list := print_item [ print_separator print_item ]*
print_item := expression | string_literal
print_separator := ";" | ","

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

### Expressions
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
              | string_literal
              | TRUE
              | FALSE
              | "(" expression ")"
              | function_call
              | built_in_function

built_in_function := ABS "(" expression ")"
                  | RND "(" expression ")"
                  | PEEK "(" expression ")"
                  | MILLIS "(" ")"
                  | hardware_function

hardware_function := READ "(" expression ")"
                  | WRITE "(" expression "," expression ")"
                  | PWM "(" expression "," expression ")"
                  | DELAY "(" expression ")"
                  | PINMODE "(" expression "," expression ")"
                  | POKE "(" expression "," expression ")"

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
STRING := '"' { character }* '"'
```

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

### Comment Rules
- `REM [comment]` - Traditional BASIC comment (consumes rest of line)
- `' [comment]` - Modern shorthand comment (consumes rest of line)
- Comments can appear on their own line or at end of statements
- Comment text is preserved in token stream for REM/COMMENT processing

**Usage Examples:**
```basic
REM This is a full comment line
PRINT "Hello"  REM This is an end-of-line comment
' Short form comment
INT count = 0  ' Initialize counter
CONST INT MAX = 100  ' Define constant
CONST STRING msg = "Ready"  ' String constant
```

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
IF x < y THEN PRINT "x is smaller" : x = y
CONST STRING greeting = "Hello" : PRINT greeting
```

### Literal Values
- **Decimal numbers**: `123`, `0`, `32767`
- **Hexadecimal numbers**: `0x1F`, `0xFF00`, `0xA0` (case insensitive)
- ✅ **Boolean literals**: `TRUE` (value 1), `FALSE` (value 0)
- **String literals**: `"Hello World"`, `""` (empty string)
- **Type determination**: Numbers typed as INT (0-32767) or WORD (32768-65535)

---

## Technical Architecture

### JIT Compilation System
The expression evaluation system uses a Just-In-Time compilation approach:

**Compilation Phase:**
1. **Infix to Postfix**: Expressions compiled using recursive descent parser
2. **Opcode Generation**: Stack-based opcodes emitted to 512-byte buffer
3. **Type Checking**: Compile-time type compatibility validation
4. **Optimization**: Efficient literal handling (PUSHBIT, PUSHBYTE, PUSHINT, PUSHWORD, PUSHCSTRING)

**Execution Phase:**
1. **Stack Machine**: Opcodes executed using Hopper VM value/type stacks
2. **Fast Dispatch**: Single-byte opcode determines operand count and handler
3. **Error Handling**: Runtime type checking and overflow detection
4. **Register Preservation**: Clean API with documented side effects only

**Opcode Set:**
```
No Operands (0x00-0x3F):
  ✅ ADD, SUB, MUL, DIV, MOD, NEG
  ✅ BITWISE_AND, BITWISE_OR
  ✅ LOGICAL_AND, LOGICAL_OR, LOGICAL_NOT
  ✅ EQ, NE, LT, GT, LE, GE
  ✅ RETURN, RETURNVAL, DECSP, DUP, NOP, ENTER

One Byte Operand (0x40-0x7F):
  ✅ PUSHBIT, PUSHBYTE
  ❌ PUSHLOCAL, POPLOCAL
  ❌ JUMPB, JUMPZB, JUMPNZB
  ✅ CALL (with JIT resolution), CALLF, SYSCALL

Two Byte Operands (0x80-0xBF):
  ✅ PUSHINT, PUSHWORD, PUSHCSTRING
  ✅ PUSHGLOBAL, ❌ POPGLOBAL
  ❌ JUMPW, JUMPZW, JUMPNZW
  ❌ PEEK, POKE
```

### String Architecture

#### STRING Type Implementation
- **BasicType.STRING = 0x0F**: Immutable string constant type
- **Token buffer storage**: String literals stored in tokenized form
- **16-bit pointers**: STRING values are pointers to null-terminated strings
- **PUSHCSTRING opcode**: Emit string literals as 16-bit pointers to token buffer

#### String Operations
- **Equality comparison**: `=` and `<>` operators supported
- **Smart comparison**: Pointer equality first, then content comparison
- **PRINT support**: Direct string output via PRINT statement
- **Expression integration**: String literals work in all expression contexts

#### String Memory Management
- **Dynamic allocation**: Mutable STRING variables allocate memory for string copies
- **STRING Memory Management**: Dynamic string allocation and cleanup for mutable variables
- **String constants**: Stored in token buffer, immutable string literal pool
- **Efficient storage**: Identical string literals share same memory location

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
            Low nibble: BasicType (INT=2, WORD=4, BIT=6, BYTE=3, STRING=0x0F)
Offset 3-4: tokens pointer (16-bit pointer to initialization token stream)
Offset 5-6: value (16-bit current value or string pointer)
Offset 7+:  null-terminated name string
```

**Function Node (Objects layer):**
```
Offset 0-1: next pointer (managed by Table layer)
Offset 2:   function flags byte (was unused)
Offset 3-4: function body tokens pointer / compiled opcodes pointer (dual purpose)
Offset 5-6: arguments list head pointer (points directly to first argument node)
Offset 7-8: opcode stream pointer (16-bit - for functions, unused for variables/constants)
Offset 9+:  null-terminated name string
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
- **BasicCompilerWorkspace**: 32 bytes - Compiler state (0x0980-0x099F)
- **BasicStatementWorkspace**: 32 bytes - Statement layer storage (0x09A0-0x09BF)
- **BasicExecutorWorkspace**: 32 bytes - Executor state (0x09C0-0x09DF)
- **BasicProcessBuffer**: 32 bytes - General workspace (0x09E0-0x09FF)

### Zero Page Allocation
**BASIC Project Allocation (0x30-0x4F, 32 bytes):**
- **Console Input**: BasicInputLength (0x30)
- **Tokenizer State**: TokenBufferLength, TokenizerPos (0x31-0x34)
- **Error Handling**: LastError pointer (0x35-0x36)
- **Token Cache**: CurrentToken, TokenLiteralPos (0x37-0x39)
- **JIT Compiler**: OpcodeBuffer state, CompilerFlags (0x3A-0x3F)
- **TokenIterator**: Iterator state for function display (0x40-0x43)
- **Available**: 12 bytes for future features (0x44-0x4F)

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

### ✅ Completed Foundation
- **Symbol Table System**: Complete 4-layer architecture with comprehensive testing
- **String Tokenization**: Full string literal parsing with quote handling
- **Expression Evaluation**: Complete JIT compilation system with all operators
- **Type System**: Comprehensive type checking and promotion rules
- **Variable Management**: Declaration, assignment, constant enforcement
- **Function Declaration**: Complete FUNC/ENDFUNC and BEGIN/END syntax
- **Function Storage**: Token stream capture and storage for function bodies
- **Parameter Lists**: Argument parsing and storage in Functions system
- **Multi-line Capture**: Interactive function definition across multiple input lines
- **Function Display**: FUNCS and LIST commands with formatted token stream rendering
- **Function Calls**: Complete function call parsing in expressions and statements
- **Console Commands**: NEW, CLEAR, VARS, FUNCS, LIST, FORGET, MEM, BYE, debug commands
- **Statement Processing**: Multi-statement lines with colon separators
- **IF/THEN Statements**: Basic conditional execution
- **RETURN/END Statements**: Function and program termination (parsing and execution complete)
- **Assignment**: Variable assignment with type checking
- **Error Handling**: Proper error messages and recovery
- **Clean API Standards**: All units follow register preservation and documented contracts
- **BIT Type**: Complete implementation with TRUE/FALSE literals and logical operations
- **STRING Type**: Complete implementation with literals, variables, constants, and comparison operations
- **BYTE Type**: Complete implementation with 8-bit unsigned arithmetic and type promotion
- **Function Execution**: Complete JIT compilation and execution with call stack management
- **Function Call Resolution**: CALL→CALLF opcode patching for performance optimization
- **Recursion Support**: Proper stack frame management enables recursive function calls

### 🎯 Current Status: Core Function System Complete

**Major Achievement**: Function system with JIT compilation fully operational, enabling recursive function calls and complex program execution.

### ❌ Missing Components for Full Benchmark Support:
1. **FOR/NEXT loops** - Basic iteration support
2. **WHILE/WEND loops** - Conditional iteration  
3. **Multiple PRINT arguments** - `PRINT name; "("; arg; ")"`
4. **MILLIS() function** - System timer access
5. **Nested loop support** - FOR within WHILE constructs
6. **Global array declarations** - `BIT flags[8191]` (global scope only)
7. **Array indexing** - `flags[i] = TRUE`
8. **Array function parameters** - Pass global arrays to functions
9. **RUN Command** - Execute stored main program (BEGIN/END block)

### ❌ Missing Components for Memory Functions:
1. **PEEK Function**: Built-in function to read memory bytes
2. **POKE Statement**: Built-in statement to write memory bytes

### ❌ Missing Components for Enhanced I/O:
1. **Multiple PRINT Arguments**: Enhanced PRINT with string and value combinations
2. **Print separators**: Comma and semicolon handling in PRINT statements

---

## Testing Status
All implemented systems have comprehensive test coverage:
- **Symbol table operations**: Creation, lookup, type checking, memory management
- **Expression evaluation**: All operators, type promotion, error conditions
- **Tokenization**: All token types, edge cases, buffer management
- **Memory management**: Allocation, deallocation, leak detection
- **Type system**: Compatibility rules, runtime validation, error messages
- **Function declaration**: Complete function definition and storage
- **Function display**: Token stream rendering and formatting
- **Multi-line capture**: Function definition across multiple input lines
- **BIT Type**: Complete test suite passed with correct type isolation behavior
- **STRING Type**: Complete test suite passed with string literal parsing, memory management, and comparison operations
- **BYTE Type**: Complete test suite passed with 8-bit arithmetic and type promotion rules
- **Function execution**: Recursive function calls validated with FOO()→BAR() test case
- **JIT compilation**: Expression compilation and opcode execution verified

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

## Console Command Display Order

### VARS Command Output Format
**Display Order**: Constants first (creation order), then variables (creation order)
```
CONST INT SIZE = 100
CONST BIT DEBUG = TRUE  
CONST BYTE ACIA = 80
CONST STRING msg = "Ready"
INT counter = 5
WORD buffer = 200
BYTE port = 255
STRING status = "Active"
```

### FUNCS Command Output Format  
**Display Order**: Functions in creation order, main program (BEGIN block) listed with special handling
```
FUNC Fibo(n)
    IF n <= 1 THEN RETURN n
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

FUNC Benchmark(name, arg, loops)
    [function body with proper indentation...]
ENDFUNC

BEGIN
    [main program body...]
END
```

### LIST Command Output Format
**Complete Program Listing**: VARS output + FUNCS output (maintains creation order)
```
CONST INT SIZE = 100
CONST BIT DEBUG = TRUE  
CONST BYTE ACIA = 80
CONST STRING msg = "Ready"
INT counter = 5
WORD buffer = 200
BYTE port = 255
STRING status = "Active"

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

**Implementation Details:**
- LIST = VARS + FUNCS output combined
- Function bodies rendered with 4-space indentation
- Token streams converted back to readable BASIC syntax
- BEGIN blocks displayed without FUNC keyword
- Parameters shown in function signatures
- String constants displayed with quotes

---

## Usage Examples

### Function Execution and Recursion
```basic
> FUNC BAR()
*     PRINT 2 + 2 + 2
* ENDFUNC
OK

> FUNC FOO()
*     PRINT 21 * 2
*     BAR()
* ENDFUNC
OK

> FOO()
42
6
READY
```

### BIT Type Usage
```basic
> CONST BIT debugMode = TRUE
OK

> BIT flag1 = FALSE
OK

> BIT result = flag1 OR debugMode
OK

> PRINT result
TRUE

> BIT complex = (TRUE OR FALSE) AND NOT (FALSE AND TRUE)
OK

> PRINT complex
TRUE

> VARS
CONST BIT DEBUGMODE = TRUE
BIT FLAG1 = FALSE
BIT RESULT = TRUE
BIT COMPLEX = TRUE
```

### String Constants and Variables  
```basic
> CONST STRING greeting = "Hello World"
OK

> STRING message = "Initial Value"
OK

> message = "Updated Value"
OK

> PRINT greeting
Hello World

> PRINT message  
Updated Value

> IF message = "Updated Value" THEN PRINT "Match"
Match

> VARS
CONST STRING greeting = "Hello World"
STRING message = "Updated Value"
```

### BYTE Type and Memory Access
```basic
> BYTE port = 0x80
OK

> CONST BYTE ACIA = 0x50
OK

> POKE(0x5000, 0xFF)    ' Write 255 to address $5000
OK

> BYTE value = PEEK(0x5000)    ' Read byte from address $5000
OK

> PRINT value
255

> port = PEEK(ACIA)     ' Read from I/O port
OK

> POKE(ACIA + 1, port | 0x01)   ' Set bit 0 in next port
OK

> VARS
CONST BYTE ACIA = 80
BYTE port = 193
BYTE value = 255
```

### String Literals in Function Arguments
```basic
> FUNC DisplayMessage(title, text)
* PRINT title
* PRINT ": "
* PRINT text
* ENDFUNC
OK

> DisplayMessage("Status", "Ready")
Status
: 
Ready

> STRING status = "Running"
OK

> DisplayMessage("Current", status)
Current
: 
Running
```

### Function Definition and Display
```basic
> FUNC Add(a, b)
* RETURN a + b
* ENDFUNC
OK

> FUNCS
FUNC Add(a, b)
    RETURN a + b
ENDFUNC

> LIST
FUNC Add(a, b)
    RETURN a + b
ENDFUNC
```

### Multi-line Function Capture
```basic
> FUNC Complex(x)
* INT temp = x * 2
* IF temp > 10 THEN temp = temp - 5
* RETURN temp
* ENDFUNC
OK

> FUNCS Complex
FUNC Complex(x)
    INT temp = x * 2
    IF temp > 10 THEN temp = temp - 5
    RETURN temp
ENDFUNC
```

### BEGIN/END Main Program with Strings
```basic
> BEGIN
* CONST STRING msg = "Starting program"
* PRINT msg
* INT result = 42
* PRINT result
* END
OK

> FUNCS
BEGIN
    CONST STRING msg = "Starting program"
    PRINT msg
    INT result = 42
    PRINT result
END
```

### Basic Variable Operations
```basic
INT count = 5
WORD max = 65000
BIT flag = TRUE
BYTE port = 0x80
CONST STRING status = "Ready"
STRING current = "Active"
PRINT count : PRINT max : PRINT flag : PRINT port : PRINT status : PRINT current
count = count + 1 : PRINT count
current = "Updated" : PRINT current
port = PEEK(0x5000) : PRINT port
```

### Array Usage Examples
```basic
' Global array declarations (allowed)
INT numbers[10]  ' 20 bytes (2 bytes per INT)
BIT flags[100]   ' 13 bytes (8 bits per byte, so 100 bits = 12.5 bytes rounded up to 13)
BYTE buffer[256] ' 256 bytes (1 byte per BYTE)

' Array usage in main program  
BEGIN
    FOR i = 0 TO 9
        numbers[i] = i * 2
    NEXT i
    
    ProcessArray(numbers, 10)   ' Pass array to function
END

' Function with array parameter
FUNC ProcessArray(arr, size)
    FOR i = 0 TO size-1
        PRINT arr[i]
    NEXT i
ENDFUNC

' Function with local array (NOT ALLOWED)
FUNC BadExample()
    INT localArray[5]    ' ERROR: Local arrays not supported
ENDFUNC
```

### Array Scope Rules
```basic
' ALLOWED: Global array declarations
INT globalData[100]

FUNC WorkWithArrays()
    ' ALLOWED: Access global arrays
    globalData[0] = 42
    
    ' ALLOWED: Global arrays as parameters (passed by reference)
    ' (array parameter receives reference to global array)
    
    ' NOT ALLOWED: Local array declarations
    ' INT localBuffer[50]    ' Compile error
ENDFUNC

' ALLOWED: Pass global arrays to functions
ProcessData(globalData, 100)
```

### Expression Evaluation with BIT Types
```basic
BIT a = TRUE : BIT b = FALSE
BIT result1 = a AND b         ' FALSE
BIT result2 = a OR b          ' TRUE  
BIT result3 = NOT a           ' FALSE
BIT result4 = a = b           ' FALSE (TRUE != FALSE)
BIT complex = (a OR b) AND NOT (b AND a)  ' TRUE
PRINT result1 : PRINT result2 : PRINT result3 : PRINT result4 : PRINT complex
```

### Multi-Statement Lines with BIT Types
```basic
BIT flag1 = TRUE : BIT flag2 = FALSE : PRINT flag1 : PRINT flag2
IF flag1 = TRUE THEN flag2 = TRUE : PRINT flag2 : PRINT "Done"
```

### Hexadecimal Numbers
```basic
WORD addr = 0x8000
INT offset = 0x100
CONST STRING label = "Address"
PRINT label : PRINT addr + offset    ' Prints Address, then 33024
```

---

## Future Roadmap

### Phase 2: Loop Constructs & Array Support (Next Priority)
- **FOR/NEXT loops**: Counted iteration with STEP support
- **WHILE/WEND loops**: Conditional iteration
- **MILLIS() function**: System timer for benchmarks
- **Global Arrays**: Single-dimensional arrays for all types (global scope only)
- **Array Indexing**: Zero-based array access with bounds checking
- **Array Parameters**: Pass global arrays as function arguments
- **RUN Command**: Execute stored main program (BEGIN/END block)

### Phase 3: Memory Functions and Enhanced I/O
- **PEEK Function**: Built-in memory read function
- **POKE Statement**: Built-in memory write statement
- **Multiple PRINT Arguments**: Enhanced PRINT with separators

### Phase 4: Enhanced Features (After Benchmarks)
- **Storage System**: SAVE/LOAD with EEPROM integration
- **Enhanced I/O**: INPUT statements, formatted PRINT output
- **Built-in Functions**: Math (ABS, RND), string manipulation
- **Advanced Control**: DO/UNTIL loops, BREAK/CONTINUE statements

### Phase 5: Hardware Integration (If ROM space permits)
- **Hardware I/O**: Pin control, PWM, timing functions
- **Embedded Features**: Real-time capabilities for microcontroller applications
- **Optimization**: Performance tuning for 6502 constraints

The current implementation provides a robust foundation with complete function execution, JIT compilation, and all core data types (INT, WORD, BIT, BYTE, STRING) fully implemented with proper type checking, promotion rules, and memory management. The function system breakthrough enables complex recursive programming and validates the core interpreter architecture before adding loop constructs and other advanced features needed for the benchmark programs.