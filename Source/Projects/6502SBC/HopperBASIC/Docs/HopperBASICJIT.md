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

## Key Design Decisions

### 1. Opcode Format
Opcodes will be byte-aligned with variable-length encoding:
- Single-byte opcodes for common operations (ADD, SUB, etc.)
- Two-byte opcodes for 8-bit offsets (PUSHVARB, PUSHSTRINGB)
- Three-byte opcodes for 16-bit offsets when token buffer exceeds 256 bytes

### 2. Literal Reference Strategy
Instead of duplicating literal data, opcodes reference the original token stream:
```
Token stream:   [TOKEN_STRING][5]["HELLO"][TOKEN_NUMBER][42][0]
Opcode stream:  [PUSHSTRINGB][0x00][PUSHNUMBERB][0x08][ADD]
```

This approach:
- Minimizes memory usage (critical on 6502)
- Enables single-byte addressing for most programs
- Avoids data duplication

### 3. Storage Strategy
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
   - Opcode dispatch loop
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

Estimated overhead:
- Opcode definitions: ~200 bytes
- Compiler code: ~2KB 
- Executor code: ~1KB
- Temporary opcode buffer: 256 bytes

Total: ~3.5KB additional ROM

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