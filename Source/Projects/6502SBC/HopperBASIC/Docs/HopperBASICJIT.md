# HopperBASIC JIT Compilation Project

## Overview

This project transforms HopperBASIC from a direct token interpreter to a Just-In-Time (JIT) compiling interpreter that generates and executes optimized opcodes. This architectural change will dramatically improve execution performance while maintaining full compatibility with existing BASIC programs.

## Current Architecture

HopperBASIC currently uses a recursive descent parser that directly executes operations as it parses:
- Tokens are parsed from the token buffer
- Each expression node immediately evaluates and pushes results to the stack
- No intermediate representation exists between tokens and execution

## Proposed Architecture

The new architecture introduces a compilation phase:
1. **Tokenization** (unchanged) - Source text → Token stream
2. **Compilation** (new) - Token stream → Opcode stream  
3. **Execution** (new) - Opcode stream → Results

## Memory Layout

### Buffer Allocation
```asm
// HopperBASIC buffers
const uint BasicInputBuffer       = 0x0900;  // 128 bytes - raw user input
const uint BasicProcessBuffer1    = 0x0980;  // 64 bytes - variable/general workspace
const uint BasicProcessBuffer2    = 0x09C0;  // 32 bytes - secondary workspace
const uint BasicProcessBuffer3    = 0x09E0;  // 32 bytes - tertiary workspace  
const uint BasicTokenizerBuffer   = 0x0A00;  // 512 bytes - tokenized line storage
const uint BasicOpcodeBuffer      = 0x0C00;  // 512 bytes - JIT compiled opcodes
const uint HopperData             = 0x0E00;  // start of Hopper RAM (program, then heap)
```

### Zero Page Allocation
```asm
// === JIT COMPILER STATE (0x3A-0x3F) ===
const byte OpcodeBufferLength   = 0x3A;  // Length of opcodes in BasicOpcodeBuffer (16-bit)
const byte OpcodeBufferLengthL  = 0x3A;  // Low byte
const byte OpcodeBufferLengthH  = 0x3B;  // High byte
// Note: Opcode execution uses ZP.PCL/ZP.PCH from Hopper VM
const byte CompilerTokenPos     = 0x3C;  // Token position during compilation (16-bit)
const byte CompilerTokenPosL    = 0x3C;  // Low byte
const byte CompilerTokenPosH    = 0x3D;  // High byte
const byte CompilerFlags        = 0x3E;  // Compilation flags (bit 0: in function, etc.)
const byte OpcodeTemp           = 0x3F;  // Temporary byte for opcode construction
```

## Key Design Decisions

### 1. Fixed-Size Buffers
- 512-byte opcode buffer matches tokenizer buffer size
- Opcodes are more compact than tokens, ensuring sufficient space
- Simple bounds checking and overflow detection
- No dynamic memory allocation required

### 2. Opcode Format
Opcodes will be byte-aligned with variable-length encoding:
- Single-byte opcodes for common operations (ADD, SUB, etc.)
- Two-byte opcodes for 8-bit offsets (PUSHVARB, PUSHSTRINGB)
- Three-byte opcodes for 16-bit offsets when token buffer exceeds 256 bytes

### 3. Literal Reference Strategy
Instead of duplicating literal data, opcodes reference the original token stream:
```
Token stream:   [TOKEN_STRING]["HELLO"][TOKEN_NUMBER][0x002A][0]
Opcode stream:  [PUSHSTRINGB][0x01][PUSHNUMBERB][0x08][ADD]
```

This approach:
- Minimizes memory usage (critical on 6502)
- Enables single-byte addressing for most programs
- Avoids data duplication

### 4. Storage Strategy
For immediate mode (console commands):
- Opcodes generated in temporary buffer
- Executed immediately
- Buffer reused for next command

For functions (future phase):
- Opcodes cached after first compilation
- Stored as extension to function node structure
- Reused on subsequent calls

## Implementation Plan

### Phase 1: Core Infrastructure (2 days)
1. Create `Opcodes.asm` defining opcode constants
2. Create `Compiler.asm` with:
   - Opcode emission functions
   - Opcode buffer management
   - Token offset tracking
3. Create `Executor.asm` with:
   - Opcode dispatch loop using ZP.PCL/ZP.PCH
   - Handlers for each opcode

### Phase 2: Expression Compilation (3 days)
1. Clone `Expression.asm` to create compilation variants:
   - `compileExpression()` - Main entry point
   - `compilePrimary()` - Numbers, variables, parentheses
   - `compileAdditive()` - Addition, subtraction
   - `compileMultiplicative()` - Multiplication, division, modulo
   - etc.
2. Implement opcode emission instead of direct execution
3. Track token buffer offsets for literals

### Phase 3: Statement Integration (2 days)
1. Modify statement execution flow:
   ```
   Current:  Parse → Execute
   New:      Parse → Compile → Execute
   ```
2. Update `Statement.asm` to use compilation
3. Handle all statement types (PRINT, assignments, etc.)

### Phase 4: Testing & Optimization (2 days)
1. Comprehensive testing comparing old vs. new execution
2. Add specialized opcodes for common patterns
3. Implement peephole optimizations

## Opcode Set (Initial)

### Stack Operations
- `PUSHNUMBERB <offset>` - Push number from token buffer
- `PUSHVARB <offset>` - Push variable value by name offset
- `PUSHSTRINGB <offset>` - Push string address from token buffer

### Arithmetic
- `ADD` - Pop two values, push sum
- `SUB` - Pop two values, push difference  
- `MUL` - Pop two values, push product
- `DIV` - Pop two values, push quotient
- `MOD` - Pop two values, push remainder
- `NEG` - Pop value, push negation

### Comparison
- `EQ` - Pop two values, push equality result
- `NE` - Pop two values, push inequality result
- `LT`, `GT`, `LE`, `GE` - Comparison operators

### Control Flow (Future)
- `JUMP <offset>` - Unconditional jump
- `JUMPZ <offset>` - Jump if top of stack is zero
- `CALL <offset>` - Function call
- `RETURN` - Return from function

## Memory Impact

- Opcode buffer: 512 bytes (fixed allocation)
- Zero page usage: 6 bytes (0x3A-0x3F)
- Opcode definitions: ~200 bytes ROM
- Compiler code: ~2KB ROM
- Executor code: ~1KB ROM

Total: ~3.5KB additional ROM, 518 bytes additional RAM

## Performance Expectations

Based on similar interpreters:
- **3-5x faster** arithmetic operations
- **2-3x faster** overall program execution
- Near-instant execution of compiled functions (future)

## Success Criteria

1. All existing BASIC programs run unchanged
2. Execution produces identical results
3. Measurable performance improvement
4. Clean separation between compilation and execution
5. Foundation ready for function compilation (future)

## Future Extensions

This architecture enables:
- Function compilation and caching
- Advanced optimizations (constant folding, CSE)
- Potential native code generation for hot paths
- Debugging/profiling capabilities

## Technical Notes

- The 512-byte opcode buffer is guaranteed sufficient since opcodes reference tokens rather than embedding literals
- Using ZP.PCL/ZP.PCH from Hopper VM for opcode execution maintains consistency with VM design
- Fixed buffer sizes align with HopperBASIC's philosophy of predictable memory usage