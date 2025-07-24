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

## Phase 1: Core Functionality (Implementation Status)

### Console Commands
- ✅ **`NEW`** - Clear everything (program, variables, functions)
- ❌ **`LIST`** - Display complete program (main + functions)
- ❌ **`RUN`** - Execute the main program
- ❌ **`CLEAR`** - Reset all variables to default values, keep definitions
- ❌ **`VARS`** - Show all variables and current values
- ❌ **`FUNCS`** - Show all function signatures (overview)
- ❌ **`FORGET name`** - Remove variable or function
- ✅ **`MEM`** - Show available memory
- ✅ **`BYE`** - Exit interpreter

### Variable Declaration Commands
- ❌ **`INT name [= value]`** - Create signed integer variable (-32768 to 32767)
- ❌ **`WORD name [= value]`** - Create unsigned integer variable (0 to 65535)
- ❌ **`BIT name [= value]`** - Create boolean variable (0 or 1)

### Definition Commands
- ❌ **`FUNC name(params)`** - Start function definition (ends with `ENDFUNC`)
- ❌ **`BEGIN`** - Start main program definition (ends with `END`)

### Core Language Features

#### Basic I/O
- ✅ **`PRINT expr`** - Output single value followed by newline
- ✅ **`PRINT`** - Output empty line (no arguments)

#### Expressions & Operators (Complete Implementation)
- ✅ **Arithmetic**: `+ -` (addition, subtraction)
- ✅ **Multiplicative**: `* / MOD` (multiplication, division, modulo)
- ✅ **Unary**: `-` (negation)
- ✅ **Comparison**: `= <>` (equal, not equal - returns BIT type)
- ✅ **Ordering**: `< > <= >=` (less than, greater than, less/greater equal)
- ✅ **Logical**: `AND OR NOT` (BIT operands)
- ✅ **Parentheses**: `(` `)` for precedence

#### Type System
- ✅ **INT**: 16-bit signed integer (-32768 to 32767)
- ✅ **WORD**: 16-bit unsigned integer (0 to 65535)
- ✅ **BIT**: Boolean value (0 or 1)
- ✅ **Type promotion**: Automatic promotion between compatible types
- ✅ **Type safety**: Proper type checking with meaningful error messages

#### Control Flow
- ✅ **`IF expr THEN statement`** - Conditional execution (expr must be BIT type)
- ❌ **`RETURN [expr]`** - Return from function (stub implemented)
- ❌ **`END`** - End main program (stub implemented)

#### Core Infrastructure
- ✅ **Tokenizer**: Complete lexical analysis with inline literals
- ✅ **Expression evaluator**: Full recursive descent parser with proper precedence
- ✅ **Type system**: Runtime type checking and promotion
- ✅ **Error handling**: Comprehensive error messages and reporting
- ✅ **Memory management**: Integration with Hopper VM memory system
- ✅ **Interactive REPL**: Command processing and execution loop

### Assignment
- ❌ **`var = expr`** - Assignment to existing variables

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
- **EEPROM integration**: Use existing I2C buffer and routines for storage
- **Efficient format**: Minimal overhead for maximum program storage

---

## Phase 3: Extended Features

### Additional Types
- ❌ **`BYTE name [= value]`** - 8-bit unsigned (0 to 255) for hardware I/O
- ❌ **`STRING name [= "value"]`** - Text handling with string operations
- ❌ **Arrays**: `INT numbers[10]` - single-dimensional arrays

### Constants
- ❌ **`CONST name = value`** - Define immutable constants

### Enhanced I/O
- ❌ **`INPUT var`** - Read value from keyboard into variable
- ❌ **`INPUT "prompt",var`** - Read with prompt
- ❌ **`PRINT expr[,expr...]`** - Multiple values separated by spaces, newline at end
- ❌ **`PRINT expr[;expr...]`** - Multiple values with no separation, newline at end
- ❌ **`PRINT expr;`** - Output value with no newline (cursor stays on line)
- ❌ **`PRINT expr,`** - Output value followed by space, no newline

### Extended Control Flow
- ❌ **`FOR var = start TO end [STEP increment]`** - Counted loops
- ❌ **`NEXT var`** - End of FOR loop
- ❌ **`WHILE expr`...`WEND`** - Conditional loops
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

---

## Technical Implementation Status

### Completed Infrastructure
- ✅ **Tokenizer System**: 512-byte token buffer with inline literal storage
- ✅ **Expression Parser**: Recursive descent with proper operator precedence
- ✅ **Type System**: Runtime type checking, promotion, and error reporting
- ✅ **Memory Integration**: Working with Hopper VM memory management
- ✅ **Error Handling**: Comprehensive error messages and reporting
- ✅ **Interactive Console**: REPL with command processing
- ✅ **Arithmetic Operations**: Full set including multiplication, division, modulo
- ✅ **Comparison Operations**: All six comparison operators with proper type handling
- ✅ **Logical Operations**: AND, OR, NOT with BIT type requirements

### Memory Layout (Preserved from Hopper VM)
- **$0200-$02FF**: Serial input buffer (256 bytes)
- **$0300-$03FF**: Call stack LSB (256 bytes)
- **$0400-$04FF**: Call stack MSB (256 bytes)  
- **$0500-$05FF**: Type stack (256 bytes)
- **$0600-$06FF**: Value stack LSB (256 bytes)
- **$0700-$07FF**: Value stack MSB (256 bytes)
- **$0800-$08FF**: I2C buffer (256 bytes, used for EEPROM)
- **$0900-$097F**: Basic input buffer (128 bytes)
- **$0980-$09BF**: Process buffer 1 (64 bytes)
- **$09C0-$09DF**: Process buffer 2 (32 bytes)
- **$09E0-$09FF**: Process buffer 3 (32 bytes)
- **$0A00-$0BFF**: Tokenizer buffer (512 bytes)
- **$0C00+**: Dynamic heap for BASIC programs/variables

### Zero Page Usage
- **Standard Hopper VM allocations**: SP, BP, PC, IDX, IDY, TOP, NEXT, ACC
- **BASIC-specific (0x30-0x39)**: Input length, tokenizer state, error handling
- **Available (0x3A-0x3F)**: 6 bytes reserved for future BASIC features

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

comparison_expr := logical_or_expr [ comparison_op logical_or_expr ]
comparison_op := "=" | "<>" | "<" | ">" | "<=" | ">="

logical_or_expr := logical_and_expr [ OR logical_and_expr ]

logical_and_expr := additive_expr [ AND additive_expr ]

additive_expr := multiplicative_expr [ additive_op multiplicative_expr ]
additive_op := "+" | "-"

multiplicative_expr := unary_expr [ multiplicative_op unary_expr ]
multiplicative_op := "*" | "/" | MOD

unary_expr := [ "-" | NOT ] primary_expr

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
2. Unary minus (-), Logical NOT
3. Multiplication (*), Division (/), Modulo (MOD)
4. Addition (+), Subtraction (-)
5. Comparison (=, <>, <, >, <=, >=)
6. Logical AND
7. Logical OR

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
- **Automatic promotion**: Compatible types promote safely (BYTE→INT→WORD)

---

# HopperBASIC Symbol Table Design

## Overview

HopperBASIC uses a unified symbol table to store all program identifiers: variables, constants, and functions (including the main program).

## Design Decisions

### Unified Table Structure
- **Single table** stores all identifier types (variables, constants, functions)
- **Shared implementation** reduces code size and complexity
- **Generic operations** work across all identifier types

### Simple Data Structure
- **Linked list** implementation
- Linear search through entries
- No optimization for lookup speed

### Runtime Resolution
- **Identifiers stored as strings** during tokenization  
- **Lookup performed at runtime** when tokens execute
- **No caching** of resolved addresses

## Rationale

### Why Simple Linear Search?
- **Small scale**: Typical programs have 5-20 total identifiers
- **Rare operation**: Lookups only occur during statement execution
- **6502 performance**: Linear search of 20 items is microseconds
- **User perception**: No noticeable delay

### Why Runtime Resolution?
- **Forward references**: Functions can call functions defined later
- **Dynamic modification**: FORGET and redefinition change symbol meanings
- **Implementation simplicity**: No cache invalidation or dependency tracking needed

### Why Unified Table?
- **Code reuse**: Single implementation for all identifier types
- **FORGET command**: Single lookup covers variables, constants, and functions
- **Memory efficiency**: Shared allocation and management strategy
- **Debugging**: One table structure to understand and debug

## Table Operations

### Core Functions
- `TableLookup(name)` - Find entry by name across all types
- `TableAdd(name, type, dataType, value)` - Add new entry  
- `TableRemove(name)` - Remove entry (for FORGET)
- `TableClear()` - Clear all entries (for NEW)
- `TableIterate(entryType)` - Iterate by type (for VARS, FUNCS, CONSTS commands)

### Entry Types
- **VARIABLE** - Mutable values (INT, WORD, BIT, BYTE, STRING, ARRAY)
- **CONSTANT** - Immutable values (defined with CONST)
- **FUNCTION** - Executable code blocks (including main program from BEGIN/END)

## Implementation Benefits

1. **Minimal code footprint** - Single table implementation
2. **Predictable behavior** - Consistent operations across identifier types  
3. **Easy debugging** - One data structure to inspect and validate
4. **Flexible execution** - Supports forward references and dynamic redefinition
5. **Memory efficient** - No duplicate table management code

## Performance Characteristics

- **Lookup time**: O(n) where n ≈ 20 maximum
- **Memory overhead**: Minimal - simple linked list nodes
- **Runtime cost**: Negligible on target 6502 systems
- **Development cost**: Low complexity, easy to implement and maintain

---

## Technical Architecture

### Built on Hopper VM Foundation
- **Reuse existing runtime**: Serial I/O, memory management, stack operations
- **Leverage proven code**: Use existing helper functions and utilities
- **Maintain compatibility**: Work within established memory layout

### ROM Size Target
- **8K maximum** ($E000-$FFFF)
- **Leverage existing code**: Hopper VM runtime provides foundation
- **Focus on interpreter logic**: Don't reimplement low-level functions

---

## Next Implementation Priorities

### Immediate (Complete Phase 1)
1. **Symbol Table System**: Implement unified table for variables, constants, functions
2. **Variable Declarations**: INT, WORD, BIT types with optional initialization
3. **Assignment Statements**: `var = expr` with type compatibility checking
4. **Function System**: FUNC/ENDFUNC definitions and RETURN statements
5. **Program Structure**: BEGIN/END main program blocks
6. **Management Commands**: VARS, FUNCS, LIST, CLEAR, FORGET

### Next Phase (Storage)
1. **SAVE/LOAD Commands**: Tokenized program storage to EEPROM
2. **File Management**: DIR, DEL commands for program management

### Future Phases
1. **Enhanced Types**: BYTE, STRING, Arrays
2. **Extended Control Flow**: FOR/NEXT, WHILE/WEND loops
3. **Built-in Functions**: Math, string, and hardware I/O functions
4. **Advanced I/O**: INPUT statements, formatted PRINT output

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

---

## Current Status Summary

**Phase 1 Progress**: ~40% complete
- ✅ Core expression evaluation system (complete)
- ✅ Basic console commands (partial)
- ✅ PRINT statement (basic version)
- ✅ IF/THEN control flow (basic version)
- ❌ Variable system (not started)
- ❌ Function system (stubs only)
- ❌ Assignment statements (not started)

**Key Achievement**: We have a working expression evaluator that handles all arithmetic, comparison, and logical operations with proper type checking and promotion. This is the foundation for all other language features.

**Next Milestone**: Implement the symbol table system to enable variable declarations and assignments, completing the core language functionality.