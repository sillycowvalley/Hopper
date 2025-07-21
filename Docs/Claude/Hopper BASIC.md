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

**REPL Commands (9):**
- **RUN, LIST, NEW** - program execution/management
- **SAVE/LOAD/DEL** - named program storage to 64K EEPROM
- **DIR** - list saved programs
- **VARS, FUNCS** - show variables/functions

## Syntax Examples

**Variable Declarations:**
```basic
CONST LED_PIN = 13
CONST SIZE = 8190
BYTE pins[8]
INT values[10] 
BIT flags[SIZE]    // Only 1024 bytes storage!
```

**Program Structure:**
```basic
FUNC blink(pin, count)
  FOR i = 1 TO count
    WRITE pin, 1
    DELAY 500
    WRITE pin, 0
    DELAY 500
  NEXT
ENDFUNC

BEGIN
  blink(LED_PIN, 5)
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
6. **Forward compatibility** - designed to port back to 6502 assembly

**Total Keywords: ~30** (incredibly minimal for full structured language)

This specification delivers **1980s assembly-level performance** using a **high-level structured BASIC** that fits in **16K ROM** and beats every interpreted BASIC from the era.