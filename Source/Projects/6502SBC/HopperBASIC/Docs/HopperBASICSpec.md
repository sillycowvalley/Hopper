# Hopper BASIC Specification v2.14
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

## Phase 1: Core Functionality (Completed)

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

### Debug Commands (DEBUG build only)
- **`HEAP`** - Show heap memory layout
- **`BUFFERS`** - Show tokenizer and opcode buffer contents
- **`DUMP [page]`** - Hex dump of memory page (default: page 0)

### Variable Declaration Commands
- **`INT name [= value]`** - Signed integer (-32768 to 32767)
- **`WORD name [= value]`** - Unsigned integer (0 to 65535)
- **`BIT name [= value]`** - Boolean (TRUE or FALSE only)
- **`BYTE name [= value]`** - 8-bit unsigned numeric (0 to 255)
- **`CHAR name [= value]`** - 8-bit character (ASCII 0-255)
- **`STRING name = "value"`** - Mutable string variable
- **`VAR name [= value]`** - Runtime-typed variable (duck typing)

### Constant Declaration Commands
- **`CONST INT name = value`** - Immutable signed integer
- **`CONST WORD name = value`** - Immutable unsigned integer
- **`CONST BIT name = value`** - Immutable boolean
- **`CONST BYTE name = value`** - Immutable 8-bit unsigned numeric
- **`CONST CHAR name = 'c'`** - Immutable character
- **`CONST STRING name = "value"`** - Immutable string

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

#### Statement Separators
- **Colon (`:`) separator** - Multiple statements per line

#### Comments
- **`REM [comment]`** - Full-form comment
- **`' [comment]`** - Short-form comment

#### Expressions & Operators
- **Arithmetic**: `+ - * / MOD` (numeric types only)
- **Bitwise**: `& |` (AND, OR)
- **Unary**: `-` (negation), `NOT` (logical)
- **Comparison**: `= <> < > <= >=` (returns BIT type, CHAR values use ASCII ordering)
- **Logical**: `AND OR NOT` (BIT operands only)
- **Parentheses**: `( )` for precedence

#### Type System
- **INT**: 16-bit signed (-32768 to 32767)
- **WORD**: 16-bit unsigned (0 to 65535)
- **BIT**: Pure boolean (TRUE or FALSE only)
- **BYTE**: 8-bit unsigned numeric (0 to 255)
- **CHAR**: 8-bit character (ASCII 0-255)
- **STRING**: Immutable strings with mutable references
- **Type promotion**: Automatic between compatible numeric types only
- **Type safety**: Comprehensive checking with errors

#### String Operations
- **String indexing**: `char = string[index]` - 0-based character access
- **Bounds checking**: Runtime error on out-of-bounds access

#### Control Flow
- **`IF expr THEN statements [ELSE statements] ENDIF`** - Conditional execution
- **`WHILE expr`...`WEND`** - Pre-test conditional loops
- **`DO`...`UNTIL expr`** - Post-test conditional loops
- **`FOR var = start TO end [STEP increment]`...`NEXT [var]`** - Counted loops ✅
- **`RETURN [expr]`** - Return from function
- **`END`** - End main program

#### Assignment
- **`var = expr`** - Assignment with type checking

#### JIT Compilation System
- **Expression compilation** - Infix to postfix opcodes
- **Stack-based execution** - Using Hopper VM stacks
- **Buffer management** - 512-byte opcode buffer
- **Opcode dispatch** - Complete instruction set including FORCHK/FORIT
- **Jump patching** - Forward and backward jump resolution
- **Local variable management** - BP-relative addressing for locals and arguments

#### Function System
- **Function declaration** - FUNC/ENDFUNC syntax
- **Parameter parsing** - Comma-separated parameters ✅
- **Local variables** - Declarations within functions (positive BP offset) ✅
- **Arguments** - Function parameters (negative BP offset) ✅
- **Multi-line capture** - Interactive definition
- **BEGIN/END blocks** - Main program as function
- **Function storage** - Token stream storage
- **Arguments system** - Full parameter management with BP-relative access ✅
- **Function listing** - FUNCS command display
- **Symbol table** - Functions, variables, locals, and arguments
- **Function calls** - Parse and execute with argument passing
- **Function execution** - JIT compilation with stack frame management
- **Call resolution** - CALL→CALLF optimization
- **Return values** - RETURN statement handling
- **Call stack** - Full recursion support with BP/SP management

#### Built-in Functions
- **`ABS(x)`** - Absolute value
- **`ASC(char)`** - Convert CHAR to BYTE value
- **`CHR(numeric)`** - Convert any numeric value (BYTE/WORD/INT) to CHAR
  - Accepts: BYTE (0-255), WORD (0-65535), INT (-32768 to 32767)  
  - Range check: Value must be 0-255, runtime error otherwise
  - Returns: CHAR value
- **`LEN(string)`** - Return length of string
- **`MILLIS()`** - Milliseconds since startup
- **`SECONDS()`** - Seconds since startup
- **`DELAY(ms)`** - Pause for a delay in milliseconds
- **`PEEK(addr)`** - Read byte from memory
- **`POKE(addr, value)`** - Write byte to memory

---

## Phase 2: Loop Constructs & Array Support (Partially Complete)

### FOR/NEXT Loops ✅ COMPLETE
- **`FOR var = start TO end [STEP increment]`** - Counted loops with automatic iterator as local variable
- **`NEXT [var]`** - End of FOR loop (var name optional)
- **Nested loops** - Full support with proper iterator management
- **Optimization** - FORITF opcode for positive-only ranges with STEP 1

### Array Support (Not Started)
- ❌ **Global Arrays** - Single-dimensional arrays
- ❌ **Array Indexing** - Zero-based access
- ❌ **Array Parameters** - Pass arrays to functions

---

## Phase 3: Storage and File Management

### Storage Commands
- ❌ **`SAVE "name"`** - Save to EEPROM
- ❌ **`LOAD "name"`** - Load from EEPROM
- ❌ **`DIR`** - List saved programs
- ❌ **`DEL "name"`** - Delete program

---

## Phase 4: Hardware I/O

### Hardware Commands
- ❌ **`READ(pin)`** - Digital input
- ❌ **`WRITE(pin, value)`** - Digital output
- ❌ **`PINMODE(pin, mode)`** - Configure pins

---

## Phase 5: Extended Functions

### Additional Functions
- ❌ **`RND(max)`** - Random number generation
- ❌ **`INPUT prompt`** - User input with prompt

---

## Phase 6: Advanced Features (Future Consideration)

### Advanced Control Flow
- ❌ **`BREAK`** - Exit loops early
- ❌ **`CONTINUE`** - Skip to next iteration

### String Manipulation
- ❌ **String functions** - Basic string operations beyond indexing

### Type Conversion
- ❌ **Additional conversion functions** - Beyond ASC/CHR

Here's a Phase 6 addition for the spec:

### Character Iteration Support
- ❌ **`FOR char_var = 'start' TO 'end'`** - Character range iteration
  - Allow CHAR variables as FOR loop iterators
  - Support character literals in FOR range specification
  - STEP would work with character ASCII values
  - Example: `FOR c = 'A' TO 'Z'` to iterate through alphabet
  - Implicit type inference from character literals
  - Natural syntax for character sequence generation
  - *Note: Workaround exists using ASC/CHR with numeric FOR loops*


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
type_keyword := INT | WORD | BIT | BYTE | CHAR | STRING | VAR
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
           | do_until_statement
           | for_statement
           | function_definition
           | main_program
           | return_statement
           | comment_statement
           | function_call

assignment := identifier "=" expression
            | identifier "[" expression "]" "=" expression  // Future: array element assignment

print_statement := PRINT [ print_list ]
print_list := print_item [ separator print_item ]* [ separator ]
print_item := expression
separator := "," | ";"

if_statement := IF expression THEN statement_block [ ELSE statement_block ] ENDIF

while_statement := WHILE expression { statement }* WEND

do_until_statement := DO { statement }* UNTIL expression

for_statement := FOR identifier "=" expression TO expression [ STEP expression ]
                { statement }*
                NEXT [ identifier ]

statement_block := statement [ ":" statement ]*

comment_statement := REM [ comment_text ] | "'" [ comment_text ]

function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { local_declaration | statement }*
                      ENDFUNC

local_declaration := type_keyword identifier [ "=" expression ]

main_program := BEGIN 
                { local_declaration | statement }* 
                END

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
primary_expr := number | identifier | string_literal | char_literal | TRUE | FALSE
              | "(" expression ")" | function_call | built_in_function
              | identifier "[" expression "]"    // String/Array indexing

built_in_function := ABS "(" expression ")"
                  | ASC "(" expression ")"
                  | CHR "(" expression ")"
                  | LEN "(" expression ")"
                  | PEEK "(" expression ")"
                  | POKE "(" expression "," expression ")"
                  | DELAY "(" expression ")"
                  | MILLIS "(" ")"
                  | SECONDS "(" ")"
```

### Lexical Elements
```
number := decimal_digits | hex_number
hex_number := "0" ("x" | "X") hex_digits
identifier := letter [ letter | digit ]*
string_literal := '"' { character }* '"'
char_literal := "'" character "'"
```

### Operator Precedence (Highest to Lowest)
1. Function calls, parentheses, indexing ([])
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
5. Jump offset calculation and patching
6. Local variable and argument allocation with BP-relative addressing

**Execution Phase:**
1. Stack machine using Hopper VM stacks
2. Fast opcode dispatch including FOR loop opcodes
3. Runtime type checking and overflow detection
4. Stack frame management with BP/SP
5. Clean API with register preservation

**Opcode Set:**
- No Operands: Arithmetic, logical, comparison operations
- One Byte: PUSHBIT, PUSHBYTE, PUSHLOCAL, POPLOCAL, CALL, CALLF, SYSCALL, JUMPB series
- Two Bytes: PUSHINT, PUSHWORD, PUSHCSTRING, PUSHGLOBAL, JUMPW series
- Three Bytes: FORCHK, FORIT, FORITF (FOR loop management)

### Memory Management
- **Symbol Table**: 4-layer architecture for variables/functions/locals/arguments
- **String Architecture**: Immutable strings with pointer comparisons
- **Buffer Management**: Fixed buffers for input, tokens, opcodes
- **Zero Page**: Dedicated allocations for BASIC and symbol tables
- **Stack Frame**: BP-relative addressing for locals (positive offset) and arguments (negative offset)

### Built on Hopper VM
- Reuses Serial I/O, memory management, stack operations
- Leverages proven runtime code
- Maintains compatibility with memory layout
- Well-defined external API contracts

---

## Current Implementation Status

### Phase 1: Core Functionality - COMPLETE ✅
All features listed in Phase 1 are fully implemented and tested, including:
- Complete symbol table system with 4-layer architecture
- Full expression evaluation with JIT compilation
- Type system with automatic promotion and comprehensive safety
- All control flow structures (IF/THEN/ELSE/ENDIF, WHILE/WEND, DO/UNTIL)
- Function system with recursion, parameter passing, and local variables
- PRINT statement with all formatting options
- All console and debug commands
- Built-in functions except RND (moved to Phase 5)

### Phase 2: Loop Constructs & Array Support - PARTIALLY COMPLETE
**Completed:**
- ✅ FOR/NEXT loops with STEP support
- ✅ Nested FOR loops with proper iterator management
- ✅ Optimized FORITF opcode for positive ranges
- ✅ Local variables within functions
- ✅ Function arguments with BP-relative addressing

**Missing components needed to run benchmark programs:**
1. ❌ CHAR type implementation
2. ❌ String indexing for character access
3. ❌ Global array declarations
4. ❌ Array indexing and access
5. ❌ Array parameters for functions

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
- Type safety throughout all operations
- Clear documentation and interfaces
- Debugging support via Tools.Dump* methods
- Clean APIs with register preservation
- Nested structure support with proper state management

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

### Character and String Operations
```basic
> CHAR letter = 'A'
OK
> CHAR digit = '9'
OK
> STRING name = "HOPPER"
OK
> CHAR first = name[0]      ' Gets 'H'
OK
> CHAR third = name[2]      ' Gets 'P'
OK
> PRINT first
H
> PRINT ASC(first)
72

' Character comparisons
> IF first = 'H' THEN PRINT "Starts with H" ENDIF
Starts with H
> IF third > 'A' THEN PRINT "After A in ASCII" ENDIF
After A in ASCII

' Process string characters
> STRING text = "HELLO"
OK
> FOR i = 0 TO LEN(text) - 1
*   CHAR ch = text[i]
*   PRINT ASC(ch);
* NEXT i
72 69 76 76 79

' Type safety
> BYTE b = 65
OK
> CHAR ch = CHR(b)      ' Explicit conversion required
OK
> PRINT ch
A
```

### Functions with Arguments and Local Variables
```basic
> FUNC Add(a, b)
*   INT sum
*   sum = a + b
*   RETURN sum
* ENDFUNC
OK
> PRINT Add(5, 3)
8

> FUNC Factorial(n)
*   IF n <= 1 THEN RETURN 1 ENDIF
*   RETURN n * Factorial(n - 1)
* ENDFUNC
OK
> PRINT Factorial(5)
120

> FUNC CountChars(str, target)
*   WORD count = 0
*   FOR i = 0 TO LEN(str) - 1
*     CHAR ch = str[i]
*     IF ch = target THEN count = count + 1 ENDIF
*   NEXT i
*   RETURN count
* ENDFUNC
OK
> PRINT CountChars("HELLO WORLD", 'L')
3
```

### FOR/NEXT Loops
```basic
> FOR i = 1 TO 5
*   PRINT i
* NEXT i
1
2
3
4
5

> FOR x = 10 TO 1 STEP -2
*   PRINT x
* NEXT
10
8
6
4
2

> ' Nested loops
> FOR i = 1 TO 3
*   FOR j = 1 TO 3
*     PRINT i * j;
*   NEXT j
*   PRINT
* NEXT i
1 2 3
2 4 6
3 6 9
```

### Control Flow
```basic
> IF x > 10 THEN PRINT "Big" ELSE PRINT "Small" ENDIF
Big

> INT i = 0
> WHILE i < 5
*   PRINT i
*   i = i + 1
* WEND

> DO
*   PRINT "At least once"
*   i = i + 1
* UNTIL i > 10
```

### Advanced PRINT
```basic
> PRINT "Value:", x, "Count:", count
Value: 11 Count: 0

> PRINT "Compact";x;"text"
Compact11text

> PRINT "No newline",
No newline > 
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
> PRINT MILLIS()
12345
> PRINT LEN("HELLO")
5
> CONST CHAR newline = CHR(10)
OK
```

The implementation now provides a robust foundation with complete function execution including arguments and local variables, JIT compilation with stack frame management, comprehensive control flow including FOR/NEXT loops, and all core data types including the new CHAR type for character manipulation. The primary remaining work for full benchmark compatibility is the implementation of array support (declaration, indexing, and passing as parameters).