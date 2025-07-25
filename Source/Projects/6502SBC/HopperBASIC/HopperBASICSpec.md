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
- ✅ **Unary**: `-` (negation), `NOT` (logical negation)
- ✅ **Comparison**: `= <>` (equal, not equal - returns BIT type)
- ✅ **Ordering**: `< > <= >=` (less than, greater than, less/greater equal)
- ✅ **Logical**: `AND OR NOT` (BIT operands, also bitwise for numeric types)
- ✅ **Parentheses**: `(` `)` for precedence

#### Type System
- ✅ **INT**: 16-bit signed integer (-32768 to 32767)
- ✅ **WORD**: 16-bit unsigned integer (0 to 65535)
- ✅ **BIT**: Boolean value (0 or 1)
- ✅ **Type promotion**: Automatic promotion between compatible types
- ✅ **Type safety**: Proper type checking with meaningful error messages
- ✅ **Type compatibility checking**: Comprehensive validation for all operations
- ✅ **Mixed-type operations**: BYTE↔INT, BYTE↔WORD, INT↔WORD (when non-negative)

#### Control Flow
- ✅ **`IF expr THEN statement`** - Conditional execution (expr must evaluate to BIT type)
- ❌ **`RETURN [expr]`** - Return from function (stub implemented)
- ❌ **`END`** - End main program (stub implemented)

#### Core Infrastructure
- ✅ **Tokenizer**: Complete lexical analysis with inline literals
- ✅ **Expression evaluator**: Full recursive descent parser with proper precedence
- ✅ **Type system**: Runtime type checking and promotion
- ✅ **Error handling**: Comprehensive error messages and reporting
- ✅ **Memory management**: Integration with Hopper VM memory system
- ✅ **Interactive REPL**: Command processing and execution loop
- ✅ **Number parsing**: 16-bit integers with overflow detection and type determination
- ✅ **Keyword recognition**: Complete keyword table with uppercase conversion
- ✅ **Statement framework**: Extensible statement execution with proper error propagation
- ✅ **Symbol table foundation**: Complete low-level table operations
- ✅ **Objects layer**: Symbol management with type checking and memory management
- ✅ **Variables layer**: Variable and constant declaration/manipulation
- ✅ **Functions layer**: Function declaration and management infrastructure
- ✅ **Arguments layer**: Function parameter handling
- ✅ **Comprehensive testing**: Complete test suites for all symbol table layers

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
- ❌ **`INPUT var1, var2, ...`** - Read multiple values in one statement
- ❌ **`PRINT expr[,expr...]`** - Multiple values separated by spaces, newline at end
- ❌ **`PRINT expr[;expr...]`** - Multiple values with no separation, newline at end
- ❌ **`PRINT expr;`** - Output value with no newline (cursor stays on line)
- ❌ **`PRINT expr,`** - Output value followed by space, no newline

### Extended Control Flow
- ❌ **`FOR var = start TO end [STEP increment]`** - Counted loops
- ❌ **`NEXT var`** - End of FOR loop
- ❌ **`WHILE expr`...`WEND`** - Conditional loops
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

---

## Technical Implementation Status

### Completed Infrastructure
- ✅ **Tokenizer System**: 512-byte token buffer with inline literal storage
- ✅ **Expression Parser**: Recursive descent with proper operator precedence
- ✅ **Type System**: Runtime type checking, promotion, and error reporting
- ✅ **Memory Integration**: Working with Hopper VM memory management
- ✅ **Error Handling**: Comprehensive error messages and reporting
- ✅ **Interactive Console**: REPL with command processing
- ✅ **Arithmetic Operations**: Complete set including multiplication, division, modulo with signed/unsigned handling
- ✅ **Comparison Operations**: All six comparison operators with proper type handling
- ✅ **Logical Operations**: AND, OR, NOT with BIT type requirements and bitwise operations for numeric types
- ✅ **Symbol Table Foundation**: Complete 4-layer symbol table system
  - ✅ **Table Layer**: Generic linked list operations with memory management
  - ✅ **Objects Layer**: Symbol node management with type/value/name storage
  - ✅ **Variables Layer**: Variable and constant declaration, find, and manipulation
  - ✅ **Functions Layer**: Function declaration, signature management, and body storage
  - ✅ **Arguments Layer**: Function parameter management with ordered lists
- ✅ **Number Tokenization**: Inline storage with overflow detection and automatic type assignment
- ✅ **Keyword Recognition**: Complete keyword table with uppercase conversion
- ✅ **Statement Framework**: Extensible statement execution with proper error propagation
- ✅ **Interactive Loop**: Full REPL with startup banner and graceful error handling
- ✅ **Comprehensive Testing**: Complete test suites validating all symbol table layers
- ✅ **Type Compatibility System**: Full type checking with promotion rules for all operations
- ✅ **Instruction Set**: Complete arithmetic and logical operations with type safety

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
- **Standard Hopper VM allocations**: SP, BP, PC, IDX, IDY, TOP, NEXT, ACC (0x00-0x1F)
- **Workspace Variables**: W0-W7, TICK0-TICK3, TARGET0-TARGET3 (0x20-0x2F)
- **BASIC-specific (0x30-0x39)**: 10 bytes allocated
  - **Console Input**: BasicInputLength (0x30)
  - **Tokenizer State**: TokenBufferLength (16-bit: 0x31-0x32), TokenizerPos (16-bit: 0x33-0x34)
  - **Error Handling**: LastError (16-bit: 0x35-0x36)
  - **Token Cache**: CurrentToken (0x37), TokenLiteralPos (16-bit: 0x38-0x39)
- **Available Primary (0x3A-0x4F)**: 22 bytes reserved for future BASIC features
- **Memory Manager Workspace (0x50-0x5F)**: M0-M15 used by Memory.Allocate/Free
- **Function Workspace (0x60-0x6F)**: F0-F15 used by various system functions
- **Symbol Table Range (0x70-0x7F)**: 16 bytes allocated for Objects system
  - **Table Head Pointers**: VariablesList (16-bit: 0x70-0x71), FunctionsList (16-bit: 0x72-0x73)
  - **Symbol Node Working Storage**: SymbolType (0x74), SymbolValue (16-bit: 0x75-0x76), SymbolName (16-bit: 0x77-0x78), SymbolTokens (16-bit: 0x79-0x7A), SymbolLength (0x7B), SymbolTemp0-1 (0x7C-0x7D)
- **UInt Operations (0x80-0x87)**: U0-U7 workspace for arithmetic operations
- **Hardware I/O**: Platform-dependent mappings (0xEC+, 0xF0+)

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

### Expressions (Phase 1 - Complete Implementation)
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
           | do_until_statement
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

do_until_statement := DO
                     { statement }*
                     UNTIL expression

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

### Type Promotion and Compatibility Rules
- **BYTE → INT**: Always compatible (unsigned 8-bit fits in signed 16-bit)
- **BYTE → WORD**: Always compatible (unsigned promotion)
- **INT → WORD**: Compatible only when INT ≥ 0 (runtime check)
- **WORD → INT**: Compatible only when WORD ≤ 32767 (runtime check)
- **BIT operations**: Logical AND/OR/NOT for BIT types, bitwise for numeric types
- **Comparison results**: All comparisons return BIT type
- **Operation modes**: Arithmetic (rejects BIT), Equality (allows all), Bitwise (allows all), Ordering (rejects BIT)

### Operator Precedence (Highest to Lowest)
1. Function calls, array access, parentheses
2. Unary minus (-), Logical NOT
3. Multiplication (*), Division (/), Modulo (MOD)
4. Addition (+), Subtraction (-)
5. Logical AND
6. Logical OR
7. Comparison (=, <>, <, >, <=, >=)

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
- **Automatic promotion**: Compatible types promote safely (BYTE→INT→WORD when safe)
- **Runtime validation**: INT/WORD mixing checked at runtime for safe operations

---

# HopperBASIC Symbol Table Design

## Overview

HopperBASIC uses a unified symbol table architecture with four distinct layers to store all program identifiers: variables, constants, and functions (including the main program).

## Four-Layer Architecture

### Layer 1: Table (table.asm)
**Generic linked list operations**
- Memory allocation and node management
- Insertion at end of list (preserves declaration order)
- Traversal and deletion operations
- Works with any node size and structure

### Layer 2: Objects (objects.asm)
**Symbol-specific node management**
- Defines symbol node structure (next, type|dataType, tokens, value, name)
- Handles symbol type filtering (VARIABLE, CONSTANT, FUNCTION, ARGUMENT)
- Manages symbol-to-symbol comparisons and lookups
- Provides iterator support with type filtering

### Layer 3: Variables (variables.asm)
**Variable and constant operations**
- Variable declaration with type checking
- Value get/set with immutability enforcement (constants cannot be modified)
- Name-based lookup with type filtering
- Integration with tokenizer for initialization expressions

### Layer 4: Functions (functions.asm)
**Function-specific operations**
- Function signature management (name, return type, parameter list)
- Function body token storage and retrieval  
- Integration with Arguments layer for parameter handling
- Function call resolution and type checking

### Supporting Layer: Arguments (arguments.asm)
**Function parameter management**
- Ordered parameter lists stored as linked lists
- Parameter type information and name storage
- Index-based parameter lookup (for stack frame construction)
- Automatic cleanup when functions are removed

## Design Decisions

### Unified Table Structure
- **Single implementation** for all linked list operations
- **Shared memory management** reduces code duplication
- **Generic operations** work across all identifier types

### Layered Architecture Benefits
- **Separation of concerns**: Each layer has a specific responsibility
- **Code reuse**: Higher layers build on lower layer functionality
- **Testability**: Each layer can be tested independently
- **Maintainability**: Clear interfaces between layers

### Runtime Resolution
- **Identifiers stored as strings** during tokenization  
- **Lookup performed at runtime** when tokens execute
- **No caching** of resolved addresses (simple linear search)

## Rationale

### Why Four Layers?
- **Table Layer**: Provides generic linked list operations for any use case
- **Objects Layer**: Adds symbol-specific knowledge while remaining general
- **Variables/Functions Layers**: Implement language-specific semantics
- **Clear separation**: Each layer has distinct responsibilities and can be tested separately

### Why Simple Linear Search?
- **Small scale**: Typical programs have 5-20 total identifiers
- **Rare operation**: Lookups only occur during statement execution
- **6502 performance**: Linear search of 20 items is microseconds
- **User perception**: No noticeable delay

### Why Runtime Resolution?
- **Forward references**: Functions can call functions defined later
- **Dynamic modification**: FORGET and redefinition change symbol meanings
- **Implementation simplicity**: No cache invalidation or dependency tracking needed

## Table Operations

### Table Layer Functions
- `Table.Add(size)` - Allocate and link new node
- `Table.GetFirst()` - Start iteration
- `Table.GetNext()` - Continue iteration
- `Table.Delete(node)` - Remove specific node
- `Table.Clear()` - Remove all nodes

### Objects Layer Functions
- `Objects.Add(name, type, tokens, value)` - Add symbol with all metadata
- `Objects.Find(name)` - Locate symbol by name
- `Objects.GetData()` - Extract type/tokens/value from node
- `Objects.SetValue()` - Update value (variables only)
- `Objects.IterateStart(filter)` - Begin filtered iteration

### Variables Layer Functions
- `Variables.Declare(name, type, value, tokens)` - Create variable/constant
- `Variables.Find(name, expectedType)` - Locate with type checking
- `Variables.GetValue()` - Retrieve current value and type
- `Variables.SetValue()` - Update value (variables only, enforces immutability)
- `Variables.Remove(name)` - Delete and free tokens

### Functions Layer Functions
- `Functions.Declare(name, returnType, argsListHead, bodyTokens)` - Create function
- `Functions.Find(name)` - Locate function with type verification
- `Functions.GetSignature()` - Extract return type, body tokens, and arguments
- `Functions.GetArguments()` - Get parameter list head pointer
- `Functions.Remove(name)` - Delete function and all parameters

### Arguments Layer Functions
- `Arguments.Add(functionNode, argName, argType)` - Add parameter to function
- `Arguments.Find(functionNode, argName)` - Locate parameter by name
- `Arguments.FindByIndex(functionNode, index)` - Get parameter by position
- `Arguments.GetCount(functionNode)` - Count parameters
- `Arguments.Clear(functionNode)` - Remove all parameters

## Implementation Benefits

1. **Minimal code footprint** - Shared table implementation across all layers
2. **Predictable behavior** - Consistent operations across identifier types  
3. **Easy debugging** - Clear layer boundaries with defined interfaces
4. **Flexible execution** - Supports forward references and dynamic redefinition
5. **Memory efficient** - No duplicate table management code
6. **Comprehensive testing** - Each layer tested independently and in integration

## Performance Characteristics

- **Lookup time**: O(n) where n ≈ 20 maximum
- **Memory overhead**: Minimal - simple linked list nodes with symbol metadata
- **Runtime cost**: Negligible on target 6502 systems
- **Development cost**: Low complexity per layer, easy to implement and maintain

## Memory Management

### Node Structure
**Variable/Constant Node (Objects layer):**
```
Offset 0-1: next pointer (Table layer)
Offset 2:   symbolType|dataType (VARIABLE|INT, CONSTANT|WORD, etc.)
Offset 3-4: tokens pointer (initialization expression)
Offset 5-6: value (16-bit variable/constant value)
Offset 7+:  null-terminated name string
```

**Function Node (Objects layer):**
```
Offset 0-1: next pointer (Table layer)  
Offset 2:   symbolType|returnType (FUNCTION|INT, FUNCTION|WORD, etc.)
Offset 3-4: function body tokens pointer
Offset 5-6: arguments list head pointer
Offset 7+:  null-terminated function name string
```

**Argument Node (Arguments layer):**
```
Offset 0-1: next pointer (Arguments layer)
Offset 2:   argument type (INT, WORD, BIT, etc.)
Offset 3+:  null-terminated argument name string
```

### Memory Allocation Strategy
- **Exact-size allocation**: Nodes allocated to exact size needed (overhead + name length)
- **Automatic cleanup**: Memory.Free() called when symbols are removed
- **Token stream management**: Initialization and function body tokens stored separately and freed with symbols
- **Argument integration**: Function arguments stored as separate linked list, automatically cleaned up with function

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
1. ✅ **Symbol Table Foundation**: Complete 4-layer symbol table system with comprehensive testing
2. **Variable System Integration**: Connect symbol table to parser and statement execution
3. **Variable Declarations**: INT, WORD, BIT types with optional initialization in parser
4. **Assignment Statements**: `var = expr` with type compatibility checking
5. **Function System Integration**: FUNC/ENDFUNC definitions and RETURN statements in parser
6. **Program Structure**: BEGIN/END main program blocks
7. **Management Commands**: VARS, FUNCS, LIST, CLEAR, FORGET integration with symbol tables

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

1. **Phase 1a**: ✅ Console commands (NEW, MEM, BYE working)
2. **Phase 1b**: ✅ Complete expression system (numbers, operators, precedence, type checking)
3. **Phase 1c**: ✅ PRINT statement and IF/THEN control flow  
4. **Phase 1d**: ✅ Complete symbol table foundation (4 layers + comprehensive testing)
5. **Phase 1e**: **NEXT** - Variable declarations and assignment (connect symbol tables to parser)
6. **Phase 1f**: Functions (FUNC/ENDFUNC/RETURN) and main program (BEGIN/END)
7. **Phase 2**: Add tokenized SAVE/LOAD functionality with EEPROM storage
8. **Phase 3**: Add constants, loops, input, additional operators, built-in functions

This approach maximizes code reuse while delivering a clean, simple BASIC interpreter that feels familiar to users but leverages the robust Hopper VM foundation.

---

## Current Status Summary

**Phase 1 Progress**: ~85% complete
- ✅ Core expression evaluation system (complete with all operators and type checking)
- ✅ Basic console commands (NEW, MEM, BYE working; LIST, VARS, FUNCS, CLEAR, FORGET stubs)
- ✅ PRINT statement (working for all expression types)
- ✅ IF/THEN control flow (working with proper BIT type checking)
- ✅ Complete tokenizer with number parsing and keyword recognition
- ✅ Statement execution framework with proper error handling
- ✅ **Complete symbol table system** (4-layer architecture with comprehensive testing)
  - ✅ **Table layer**: Generic linked list operations with memory management
  - ✅ **Objects layer**: Symbol-specific operations with type filtering
  - ✅ **Variables layer**: Variable/constant declaration and manipulation
  - ✅ **Functions layer**: Function signature and body management
  - ✅ **Arguments layer**: Function parameter handling
  - ✅ **Comprehensive testing**: All layers tested with memory leak detection
- ✅ **Type system**: Complete type compatibility checking with promotion rules
- ✅ **Instruction set**: All arithmetic, logical, and comparison operations implemented
- ❌ **Variable system integration**: Connect symbol tables to parser (next priority)
- ❌ **Function system integration**: Connect function tables to parser
- ❌ **Assignment statements**: Variable assignment with type checking

**Major Achievement**: We now have a complete, tested symbol table system that can handle variables, constants, and functions with proper memory management, type checking, and comprehensive test coverage. The foundation is solid and ready for integration with the parser.

**Next Milestone**: Implement variable system integration to connect the completed symbol table foundation with the BASIC parser, enabling variable declarations (`INT name = value`) and assignments (`name = value`) to complete the core language functionality.

**Testing Status**: All symbol table layers have comprehensive test suites with memory leak detection. The system has been validated to properly handle:
- Variable and constant declaration with type checking
- Function declaration with parameter management  
- Symbol lookup with type filtering
- Memory management with automatic cleanup
- Type compatibility checking across all operations
- Error handling with proper error messages

The symbol table foundation is production-ready and provides a robust base for the remaining parser integration work.