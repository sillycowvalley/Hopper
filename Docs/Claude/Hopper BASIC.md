# 6502 Structured BASIC Specification

## Language Features

**Data Types (5):**
- **INT** (16-bit signed: -32768 to 32767)
- **WORD** (16-bit unsigned: 0 to 65535)  
- **BYTE** (8-bit unsigned: 0 to 255)
- **STRING** (fixed-length only, no dynamic allocation)
- **BIT** (bit-packed arrays: 8x memory efficient)

**Control Structures (4):**
- **IF/ELSE/ENDIF** - structured conditionals
- **WHILE/ENDWHILE** - loops with conditions
- **FOR/NEXT** - counted loops
- **BREAK** - early loop exit

**Program Structure (4):**
- **BEGIN/END** - main program block
- **FUNC/ENDFUNC** - function definitions (always return INT)
- **RETURN** - function return
- **CALL** - function invocation

**I/O & Hardware (4):**
- **READ** - digital/analog pin input
- **WRITE** - digital pin output
- **PWM** - analog output
- **DELAY** - millisecond delays

**Variables & Constants (4):**
- **LET** - optional assignment
- **CONST** - constant definitions
- **AND/OR/NOT** - bitwise operations
- **PRINT** - debug output

## Language Behavior

### Case Sensitivity
- **Case-insensitive input**: Keywords and variable names can be entered in any case
- **Tokenization**: All keywords/identifiers converted to uppercase before tokenization
- **String preservation**: Only string literals (content between quotes) preserve mixed case
- **LIST output**: Always displays keywords and variables in uppercase for consistency

**Examples:**
```basic
Input:    print "Hello World"; count
Stored:   [TOK_PRINT] [STRING:"Hello World"] [TOK_SEMICOLON] [VAR:COUNT]  
LIST:     PRINT "Hello World"; COUNT
```

### Variable Handling
- **Auto-creation**: Undefined variables automatically created with default values (classic BASIC behavior)
- **Type inference**: Variable type determined by name suffix or first assignment
  - No suffix = INT
  - `$` suffix = STRING  
  - Explicit declarations override inference
- **Default values**:
  - INT/WORD/BYTE: 0
  - STRING: "" (empty string)
  - BIT arrays: all bits clear
- **Strict type checking**: Runtime error if variable used with incompatible type

**Examples:**
```basic
PRINT X          // X auto-created as INT = 0, prints: 0
LET Y$ = "test"  // Y$ auto-created as STRING
PRINT X + Y$     // Runtime error: TYPE MISMATCH
```

## System Commands (10 total)

### Program Management
- **`RUN`** - Execute the current program starting from BEGIN/END block
- **`LIST`** - Display the current program in memory (tokenized back to source)
- **`NEW`** - Clear current program from memory, reset all variables

### Variable Management
- **`CLEAR`** - Clear all global variables (but keep program and functions)

### EEPROM File System  
- **`SAVE "name"`** - Save current program to EEPROM with given name
- **`LOAD "name"`** - Load named program from EEPROM into memory
- **`DEL "name"`** - Delete named program from EEPROM
- **`DIR`** - List all saved programs in EEPROM (shows names + sizes)

### Development/Debug
- **`VARS`** - Show all global variables and their current values
- **`FUNCS`** - Show all defined functions and their signatures

## Behavioral Distinctions

- **`NEW`** - Nuclear option: clears program, functions, AND variables
- **`CLEAR`** - Surgical option: only clears global variables, keeps program/functions intact
- **`RUN`** - Should automatically CLEAR variables before execution (fresh start)

## Example Usage

```basic
> LET count = 42
> LET name$ = "test"
> VARS
COUNT         42
NAME$         "test"
> CLEAR
> VARS
(no variables defined)
> RUN
(program runs with fresh variables)
```

## Syntax Examples

**Variable Declarations:**
```basic
CONST LED_PIN = 13
CONST SIZE = 8190
BYTE PINS[8]
INT VALUES[10] 
BIT FLAGS[SIZE]    // Only 1024 bytes storage!
```

**Program Structure:**
```basic
FUNC BLINK(PIN, COUNT)
  FOR I = 1 TO COUNT
    WRITE PIN, 1
    DELAY 500
    WRITE PIN, 0
    DELAY 500
  NEXT
ENDFUNC

BEGIN
  BLINK(LED_PIN, 5)
END
```

**File Operations:**
```basic
SAVE "blink"       // Save to EEPROM
LOAD "sensor"      // Load from EEPROM  
DIR               // List all programs
DEL "old_prog"    // Delete program
```

## Implementation Architecture

**Memory Layout:**
```
$0000-$00FF: Zero Page (variables, pointers)
$0100-$01FF: Stack
$0200-$02FF: 256 byte serial buffer
...
$C000-$FFFF: 16K ROM (interpreter)
```

**16K ROM Contents:**
- **Core Interpreter** (2K) - bytecode execution engine
- **Tokenizer/Optimizer** (2K) - pattern recognition & optimization  
- **Expression Evaluator** (2K) - stack-based arithmetic
- **Runtime Library** (2K) - memory management, math functions
- **I/O Routines** (2K) - serial, EEPROM, GPIO, timers
- **Bytecode Handlers** (2K) - optimized operation implementations
- **Built-in Functions** (2K) - string operations, utilities  
- **Constants/Tables** (2K) - lookup tables, vectors

## Performance Optimizations

**Compilation Strategy:**
- **Immediate tokenization** during program entry
- **Pattern recognition** for common constructs (i++, i^2, FOR loops)
- **Aggressive optimization** at block completion (BEGIN/END, FUNC/ENDFUNC)
- **Specialized opcodes** for frequent operations

**Optimized Bytecode:**
- **OP_INC_VAR** - var++ (17 cycles vs 180+)
- **OP_SQUARE** - var^2 (44 cycles vs 100+)  
- **OP_CLEAR_BIT** - bit arrays (15 cycles vs 45)
- **OP_FOR_RANGE** - optimized loops (30 cycles vs 200+)

**Performance Targets:**
- **Sieve Benchmark (10 iterations, 8190 size)**
- **Target: 15 seconds @ 1MHz** 
- **vs Applesoft BASIC: 500 seconds** (**33x faster**)
- **vs BBC BASIC: 30 seconds** (**2x faster**)

## Key Design Principles

1. **Fastest possible execution** - optimized for runtime speed over compile time
2. **Minimal memory usage** - bit-packed arrays, efficient storage
3. **Hardware-focused** - designed for microcontroller/embedded use  
4. **Structured programming** - no line numbers, block-structured syntax
5. **Single-token keywords** - easier parsing (ENDIF not END IF)
6. **Classic BASIC behavior** - case-insensitive, auto-creating variables
7. **Forward compatibility** - designed to port back to 6502 assembly

**Total Keywords: ~30** (incredibly minimal for full structured language)

This specification delivers **1980s assembly-level performance** using a **high-level structured BASIC** that fits in **16K ROM** and beats every interpreted BASIC from the era.

# Hopper BASIC Architecture Summary

## Key Design Decisions

### Memory Layout (ROM-based interpreter)
- **$E000-$FFFF**: ROM-based BASIC interpreter (~8K)
- **$0800-$xxxx**: Dynamic heap for function bodies and data structures
- **Function-based program storage**: Each function (including "MAIN") stored as separate allocated blocks
- **Runtime stacks**: 6502 hardware stack + Hopper type/value stacks for expression evaluation
- **Global variables**: Persistent table that survives program runs

### Execution Strategy: **Threaded Code Interpreter**
- **Bytecode as jump table indices**: Each bytecode points to optimized 6502 handler routines
- **Performance**: Near-native speed with bytecode flexibility
- **Simplicity**: Much easier than full compilation, more efficient than traditional interpretation
- **Handler-based**: Each BASIC operation becomes a specialized 6502 routine ending in RTS

### Program Organization
- **Function index table**: Maps function names to tokenized code blocks
- **Easy redefinition**: Replace function by allocating new block, updating index
- **Incremental development**: Add handlers one at a time
- **REPL support**: Immediate statements create temporary functions

### Memory Management
- **Leverage existing runtime**: Reuse `Allocate.asm`/`Free.asm` from Hopper
- **Reference types on heap**: Strings/arrays allocated dynamically
- **Stack frames**: Y register as frame pointer for locals/parameters
- **No fragmentation concerns**: Existing allocator handles this well

### Variable System
- **Hash-based lookup**: Fast variable resolution using name hashing
- **Auto-creation with defaults**: Classic BASIC behavior - undefined variables auto-created
- **Type inference**: From name suffix (`$` = string) or first assignment
- **Strict runtime typing**: Type mismatch errors prevent silent bugs

### EEPROM Storage (64K)
- **Simple file system**: Directory + program data
- **Function-level storage**: Save/load individual functions or complete programs
- **Standard commands**: SAVE, LOAD, DIR, DEL

## Why Threaded Code Won

**Rejected full compilation** because:
- Interactive development complexity
- Forward reference handling
- Development time vs. benefit

**Rejected pure bytecode interpretation** because:
- Performance overhead for compute-heavy operations
- 6502 deserves closer-to-metal execution

**Threaded code provides**:
- ✅ 4x+ performance improvement over interpretation
- ✅ Bytecode simplicity and flexibility  
- ✅ Easy REPL implementation
- ✅ Incremental development path
- ✅ Reuse of existing Hopper runtime components

## Implementation Priority
1. Basic threaded interpreter loop + simple handlers (LET, PRINT)
2. Expression evaluation handlers
3. Control flow (FOR, IF, WHILE)
4. Function calls and local variables
5. EEPROM save/load system

This architecture balances performance, simplicity, and development feasibility while staying true to BASIC's interactive nature and the 6502's capabilities.