# Hopper BASIC Specification v2.0

## Project Objectives

**Primary Goal**: Create a simple, elegant BASIC interpreter for 6502 systems that fits in 8K ROM and provides an interactive development environment.

**Design Principles**:
- **Simplicity over features** - Classic BASIC functionality only
- **Direct execution** - No bytecode, no complex compilation
- **Immediate feedback** - Interactive development with instant results
- **Small footprint** - Target 8K ROM, minimal RAM usage
- **Educational value** - Code should be readable and understandable

---

## Phase 1: Core Console Commands (13 Commands)

### Program Management
- **`NEW`** - Clear everything (program, variables, functions)
- **`LIST`** - Display complete program (main + functions)
- **`RUN`** - Execute the main program

### Variable Declaration Commands
- **`INT name [= value]`** - Create signed integer variable (-32768 to 32767)
- **`WORD name [= value]`** - Create unsigned integer variable (0 to 65535)
- **`BIT name [= value]`** - Create boolean variable (0 or 1)

### Definition Commands
- **`FUNC name(params)`** - Start function definition (ends with `ENDFUNC`)
- **`BEGIN`** - Start main program definition (ends with `END`)

### Management Commands
- **`CLEAR`** - Reset all variables to default values, keep definitions
- **`VARS`** - Show all variables and current values
- **`FUNCS`** - Show all function signatures (overview)
- **`FORGET name`** - Remove variable or function

### System Commands
- **`MEM`** - Show available memory
- **`BYE`** - Exit interpreter

---

## Phase 1: Programming Language Features (Ultra-Minimal)

### Basic I/O
- **`PRINT expr`** - Output single value followed by newline

### Assignment
- **`var = expr`** - Assignment to existing variables

### Control Flow
- **`IF expr THEN statement [ENDIF]`** - Conditional execution (expr must be BIT type)
- **`RETURN [expr]`** - Return from function
- **`END`** - End main program

### Expressions & Operators (Minimal Set)
- **Arithmetic**: `+ -` (addition, subtraction)
- **Unary**: `-` (negation)
- **Comparison**: `= <>` (equal, not equal - returns BIT type)
- **Parentheses**: `(` `)` for precedence

### Output Formatting Rules

**Phase 1:**
- `PRINT expression` outputs the value followed by a newline

**Phase 3:**
- `PRINT expr1, expr2` outputs values separated by spaces, newline at end
- `PRINT expr1; expr2` outputs values with no separation, newline at end  
- `PRINT expr1;` outputs value with no newline (cursor stays on line)
- `PRINT expr1,` outputs value followed by space, no newline

### Type System Benefits
- **Type safety**: `IF count` is an error; must use `IF count <> 0`
- **Clear intent**: BIT variables clearly indicate boolean usage
- **Simple arithmetic**: Just addition and subtraction to start

---

## Phase 2: Storage and File Management

### Storage Commands
- **`SAVE "name"`** - Save complete session to EEPROM (tokenized form)
- **`LOAD "name"`** - Load complete session from EEPROM
- **`DIR`** - List saved programs
- **`DEL "name"`** - Delete saved program

### Technical Implementation
- **Tokenized storage**: Programs saved in tokenized form to save space
- **Complete sessions**: Variables, functions, and main program all saved together
- **EEPROM integration**: Use existing I2C buffer and routines for storage
- **Efficient format**: Minimal overhead for maximum program storage

---

## Phase 3: Extended Features

### Additional Types
- **`BYTE name [= value]`** - 8-bit unsigned (0 to 255) for hardware I/O
- **`STRING name [= "value"]`** - Text handling with string operations
- **Arrays**: `INT numbers[10]` - single-dimensional arrays

### Constants
- **`CONST name = value`** - Define immutable constants

### Enhanced I/O
- **`INPUT var`** - Read value from keyboard into variable
- **`INPUT "prompt",var`** - Read with prompt
- **`PRINT expr[,expr...]`** - Multiple values separated by spaces, newline at end
- **`PRINT expr[;expr...]`** - Multiple values with no separation, newline at end
- **`PRINT expr;`** - Output value with no newline (cursor stays on line)
- **`PRINT expr,`** - Output value followed by space, no newline

### Extended Control Flow
- **`FOR var = start TO end [STEP increment]`** - Counted loops
- **`NEXT var`** - End of FOR loop
- **`WHILE expr`...`WEND`** - Conditional loops
- **`BREAK`** - Exit from loops early
- **`CONTINUE`** - Skip to next loop iteration
- **`CONT`** - Continue execution after break (console command)

### Additional Operators
- **More comparison**: `< > <= >=` (less than, greater than, etc.)
- **Arithmetic**: `* /` (multiplication, division)
- **Logical**: `AND OR NOT` (BIT operands only)

### Built-in Functions
- **Math functions**: `ABS(x)`, `RND(x)`
- **String functions**: `LEN()`, `MID()`, `LEFT()`, `RIGHT()` (if strings added)
- **Conversion functions**: Type conversion between INT/WORD/BYTE

### Hardware I/O (If space permits)
- **`READ(pin)`** - Digital pin input
- **`WRITE(pin, value)`** - Digital pin output
- **`PWM(pin, value)`** - Analog output
- **`DELAY(milliseconds)`** - Pause execution
- **`PINMODE(pin, mode)`** - Configure pin as input/output

---

## Grammar

### Console Commands
```
console_command := NEW | LIST | RUN | CLEAR | VARS | FUNCS | MEM | BYE
                 | FORGET identifier
                 | SAVE string_literal
                 | LOAD string_literal  
                 | DIR
                 | DEL string_literal
```

### Variable Declarations
```
variable_decl := type_keyword identifier [ "=" expression ]
type_keyword := INT | WORD | BIT | BYTE | STRING
```

### Program Structure
```
program := { statement }*

statement := variable_decl
           | assignment
           | print_statement
           | if_statement
           | function_definition
           | main_program
           | return_statement
           | expression_statement

assignment := identifier "=" expression

print_statement := PRINT expression [ print_separator ]
                  | PRINT expression_list

expression_list := expression print_separator expression_list
                 | expression [ print_separator ]

print_separator := "," | ";"

if_statement := IF expression THEN statement [ ENDIF ]

function_definition := FUNC identifier "(" [ parameter_list ] ")"
                      { statement }*
                      ENDFUNC

main_program := BEGIN
               { statement }*
               END

return_statement := RETURN [ expression ]

parameter_list := identifier [ "," identifier ]*
```

### Expressions (Phase 1)
```
expression := comparison_expr

comparison_expr := additive_expr [ comparison_op additive_expr ]
comparison_op := "=" | "<>"

additive_expr := unary_expr [ additive_op unary_expr ]
additive_op := "+" | "-"

unary_expr := [ "-" ] primary_expr

primary_expr := number
              | identifier
              | "(" expression ")"
              | function_call

function_call := identifier "(" [ argument_list ] ")"
argument_list := expression [ "," expression ]*

number := decimal_digits
identifier := letter [ letter | digit ]*
string_literal := '"' { character }* '"'
```

### Extended Grammar (Phase 3)
```
statement := ... (Phase 1 statements)
           | for_statement
           | while_statement
           | break_statement
           | continue_statement
           | const_declaration
           | array_declaration

for_statement := FOR identifier "=" expression TO expression [ STEP expression ]
                { statement }*
                NEXT identifier

while_statement := WHILE expression
                  { statement }*
                  WEND

const_declaration := CONST identifier "=" expression

array_declaration := type_keyword identifier "[" number "]" [ "=" array_initializer ]
array_initializer := "{" expression [ "," expression ]* "}"

comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="
additive_op := "+" | "-" | "*" | "/"
logical_op := AND | OR | NOT

primary_expr := ... (Phase 1 expressions)
              | array_access
              | built_in_function

array_access := identifier "[" expression "]"
built_in_function := ABS "(" expression ")"
                   | RND "(" expression ")"
                   | LEN "(" expression ")"
                   | hardware_function

hardware_function := READ "(" expression ")"
                   | WRITE "(" expression "," expression ")"
                   | PWM "(" expression "," expression ")"
                   | DELAY "(" expression ")"
                   | PINMODE "(" expression "," expression ")"
```

### Lexical Elements
```
letter := 'A'..'Z' | 'a'..'z'
digit := '0'..'9'
decimal_digits := digit { digit }*
character := any printable ASCII character except '"'
whitespace := ' ' | '\t'
comment := "//" { character }* end_of_line
```

### Type System
- **INT**: 16-bit signed integer (-32768 to 32767)
- **WORD**: 16-bit unsigned integer (0 to 65535)  
- **BIT**: Boolean value (0 or 1)
- **BYTE**: 8-bit unsigned integer (0 to 255) [Phase 3]
- **STRING**: Null-terminated character array [Phase 3]

### Operator Precedence (Highest to Lowest)
1. Function calls, array access, parentheses
2. Unary minus (-)
3. Multiplication (*), Division (/) [Phase 3]
4. Addition (+), Subtraction (-)
5. Comparison (=, <>, <, >, <=, >=)
6. Logical NOT [Phase 3]
7. Logical AND [Phase 3]
8. Logical OR [Phase 3]

---

## Technical Architecture

### Built on Hopper VM Foundation
- **Reuse existing runtime**: Serial I/O, memory management, stack operations
- **Leverage proven code**: Use existing helper functions and utilities
- **Maintain compatibility**: Work within established memory layout

### Memory Layout (Preserved from Hopper VM)
- **$0200-$02FF**: Serial input buffer (256 bytes)
- **$0300-$03FF**: Call stack LSB (256 bytes)
- **$0400-$04FF**: Call stack MSB (256 bytes)  
- **$0500-$05FF**: Type stack (256 bytes)
- **$0600-$06FF**: Value stack LSB (256 bytes)
- **$0700-$07FF**: Value stack MSB (256 bytes)
- **$0800-$08FF**: I2C buffer (256 bytes, used for EEPROM)
- **$0900+**: Dynamic heap for BASIC programs/variables

### Zero Page Usage (Hopper VM Standard)
- **SP, BP**: Stack pointers
- **PC**: Program counter for execution
- **IDX, IDY**: General purpose address registers
- **TOP, NEXT, ACC**: Working registers for operations
- **Serial I/O pointers**: Buffer management
- **Other standard Hopper VM zero page allocations**

### Memory Management
- **Variable storage**: Use Allocate/Free for dynamic program storage
- **Simple error handling**: Halt on error, no recovery
- **EEPROM integration**: Use existing I2C buffer and routines

### ROM Size Target
- **8K maximum** ($E000-$FFFF)
- **Leverage existing code**: Hopper VM runtime provides foundation
- **Focus on interpreter logic**: Don't reimplement low-level functions

---

## Implementation Strategy

1. **Phase 1a**: Console commands (NEW, LIST, VARS, variable declarations)
2. **Phase 1b**: Basic expressions (numbers, variables, +, -, unary -, =, <>)
3. **Phase 1c**: PRINT statement and assignment
4. **Phase 1d**: IF/THEN control flow
5. **Phase 1e**: Functions (FUNC/ENDFUNC/RETURN) and main program (BEGIN/END)
6. **Phase 2**: Add tokenized SAVE/LOAD functionality with EEPROM storage
7. **Phase 3**: Add constants, loops, input, additional operators, built-in functions

This approach maximizes code reuse while delivering a clean, simple BASIC interpreter that feels familiar to users but leverages the robust Hopper VM foundation.