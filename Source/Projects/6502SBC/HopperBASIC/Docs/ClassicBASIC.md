# Integer BASIC vs Altair BASIC: A Technical Comparison

## Overview

Two influential BASIC implementations emerged in the mid-1970s, each designed for different platforms and reflecting distinct design philosophies:

- **Steve Wozniak's Integer BASIC** (1976-1977): Apple I/II
- **Bill Gates' Microsoft Altair BASIC** (1975-1976): Altair 8800

## Design Philosophy and Constraints

### Integer BASIC
- **Compact design**: Fit entirely in 2KB of ROM
- **Simplicity focus**: Minimal feature set for maximum efficiency
- **Integer-only arithmetic**: No floating point operations
- **Resource constrained**: Designed for very limited memory systems

### Altair BASIC
- **Feature completeness**: More comprehensive BASIC implementation
- **Multiple versions**: 4K BASIC, 8K BASIC, Extended BASIC
- **Professional development**: Commercial product with broader capabilities
- **Expandable design**: Could grow with system capabilities

## Data Types and Variables

### Integer BASIC
- **Integers only**: 16-bit signed integers (-32768 to 32767)
- **No strings**: No string variables or manipulation
- **Single-letter names**: Variables A-Z only
- **Simple arrays**: Single-letter array names (A(n), B(n))
- **Auto-initialization**: All variables defaulted to 0

### Altair BASIC
- **Multiple types**: Floating point (default), integer (%), string ($)
- **Longer names**: First letter + optional digit
- **Type suffixes**: NAME$, COUNT%, VALUE
- **Multi-dimensional arrays**: More sophisticated array support
- **String operations**: Full string variable support

## Language Features

### Integer BASIC Keywords
**Core Operations:**
- Program control: RUN, LIST, NEW, DEL, END
- Variables: LET, PRINT, INPUT
- Arithmetic: +, -, *, /, MOD
- Control flow: FOR/NEXT, IF/THEN, GOTO, GOSUB/RETURN
- Arrays: DIM
- Utility: REM, CLEAR

### Altair BASIC Keywords
**All Integer BASIC features plus:**
- String functions: LEFT$, RIGHT$, MID$, LEN, ASC, CHR$
- Math functions: SIN, COS, TAN, LOG, EXP, SQR, ABS, SGN
- System access: PEEK, POKE, USR
- Data handling: READ/DATA, RESTORE
- User functions: DEF FN

## Function and Subroutine Support

### Integer BASIC
- **No user-defined functions**
- **GOSUB/RETURN only**: Simple subroutine mechanism
- **Global variables**: All data sharing through global scope
- **Limited built-ins**: Only basic arithmetic operations

### Altair BASIC
- **Rich built-in library**: Mathematical and string functions
- **DEF FN capability**: Single-line user-defined functions
- **GOSUB/RETURN**: Enhanced subroutine support
- **Global scope**: All variables and functions global
- **Function parameters**: DEF FN supported parameters

## Variable Scoping

Both implementations shared a common limitation:
- **Global scope only**: No concept of local variables
- **Shared state**: All subroutines accessed global variables
- **No encapsulation**: Data could be modified anywhere in the program

This global-only approach made larger programs difficult to manage and debug, reflecting the early stage of programming language development and the constraints of the target hardware.

## Historical Impact

**Integer BASIC** enabled the Apple II to have a built-in programming environment in ROM, making programming immediately accessible to users without requiring additional software purchases.

**Altair BASIC** established Microsoft as a major software company and became the foundation for many subsequent BASIC implementations, setting standards for microcomputer BASIC dialects.

Both implementations influenced the direction of personal computing, with Integer BASIC prioritizing immediacy and simplicity, while Altair BASIC emphasized capability and expandability.

## Three-Way Comparison: Classic BASIC vs Modern BASIC

| Feature Category | Wozniak's Integer BASIC (1976) | Gates' Altair BASIC (1975-76) | Hopper BASIC (2025) |
|------------------|--------------------------------|-------------------------------|----------------------|
| **Target Platform** | Apple I/II (6502, 2KB ROM) | Altair 8800 (8080, 4K-8K RAM) | 6502 Systems (16KB ROM) |
| **Design Philosophy** | Minimal, ROM-resident | Feature-complete, expandable | Structured, self-hosted development |
| **Memory Footprint** | 2KB ROM, minimal RAM | 4KB-8KB RAM requirement | 16KB ROM target |
| **Data Types** | Integer only (-32768 to 32767) | Float, integer, string | INT, WORD, BIT, STRING |
| **String Support** | None | Full string variables & functions | Immutable strings only |
| **Variable Names** | Single letter (A-Z) | Letter + digit | Full identifiers |
| **Variable Scope** | Global only | Global only | Global and local |
| **Arrays** | Simple single-letter | Multi-dimensional | Single-dimensional, bit-packed BIT arrays |
| **User Functions** | None | DEF FN (single-line) | Full FUNC/ENDFUNC blocks with arguments |
| **Built-in Functions** | None | SIN, COS, LOG, string functions | MILLIS(), ABS(), RND() |
| **Control Structures** | FOR/NEXT, IF/THEN, GOSUB | FOR/NEXT, IF/THEN, GOSUB | IF/THEN, FOR/NEXT, WHILE/WEND |
| **Program Structure** | Line numbers + GOTO/GOSUB | Line numbers + GOTO/GOSUB | No line numbers, structured functions |
| **Recursion Support** | None | None | Full recursion with local scope |
| **I/O Capabilities** | PRINT, INPUT | PRINT, INPUT, file operations | PRINT, hardware I/O |
| **Program Storage** | RAM only | RAM + optional storage | EEPROM storage |
| **Development Model** | Self-hosted immediate mode REPL | Self-hosted immediate mode REPL | Self-hosted immediate mode REPL |
| **Error Handling** | Basic error messages | Enhanced error reporting | Comprehensive error messages |
| **Type System** | None (integer only) | Weak typing with suffixes | Strong typing with promotion |
| **Boolean Type** | No dedicated boolean | No dedicated boolean | BIT type for conditionals & comparisons |
| **Expression Parsing** | Full expression evaluation | Full expression evaluation | Full expression evaluation |
| **Hardware Integration** | None | PEEK/POKE, USR | PEEK/POKE, native pin control |
| **Modern Features** | N/A | N/A | Hex literals, constants, enhanced comments |
| **Execution Model** | Interpreted tokens | Interpreted tokens | JIT compilation of functions to opcodes |

## Key Distinctions

**Integer BASIC Strengths:**
- Instant availability (ROM-resident)
- Extremely efficient for simple programs
- Perfect for learning programming basics
- Zero memory overhead for language runtime

**Altair BASIC Strengths:**
- Professional programming capabilities
- Rich mathematical function library
- String processing abilities
- Expandable architecture for complex programs

**Hopper BASIC Advantages:**
- Modern type safety with structured programming
- Local variable scope and function arguments
- Recursion support for advanced algorithms
- Hardware-oriented design for embedded systems
- Immutable strings avoid memory management complexity
- Function blocks eliminate GOTO/GOSUB programming
- JIT compilation for 6502 efficiency
- Dedicated BIT type for clean boolean logic
- No line numbers - structured code organization
- Memory-efficient bit-packed BIT arrays (8 elements per byte)
- Global arrays with function parameter passing

**Fundamental Programming Model Differences:**
- **Classic BASIC**: Line numbers + GOTO/GOSUB for program flow
- **Hopper BASIC**: Structured functions with local scope and recursion
- **Boolean Logic**: Hopper's BIT type provides clean conditional expressions
- **Function Design**: Hopper supports full function arguments and local variables vs single-line DEF FN
- **Array Efficiency**: Hopper's BIT arrays use bit-packing (8 boolean values per byte) for memory efficiency in embedded systems
- **Array Scope**: Hopper restricts arrays to global scope to maintain simplicity while allowing function parameter passing

**Common Strengths:**
- All three are self-hosted development environments
- Immediate mode REPL for interactive programming
- Full expression evaluation capabilities
- Designed for their respective hardware constraints

**Modern Relevance:**
- **Integer BASIC**: Historical reference for minimal language design
- **Altair BASIC**: Foundation for microcomputer software industry
- **Hopper BASIC**: Contemporary embedded development with structured programming principles