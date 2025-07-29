# HopperBASIC JIT Compilation Project

## Overview

This project transforms HopperBASIC from a direct token interpreter to a Just-In-Time (JIT) compiling interpreter that generates and executes optimized opcodes. This architectural change will dramatically improve execution performance while maintaining full compatibility with existing BASIC programs.

## Current Architecture

HopperBASIC currently uses a recursive descent parser that directly executes operations as it parses:
- Tokens are parsed from the token buffer in infix notation
- Each expression node immediately evaluates and pushes results to the Hopper VM value stack
- No intermediate representation exists between tokens and execution

## New Architecture

The new architecture introduces a compilation phase that converts infix expressions to postfix opcodes:
1. **Tokenization** (unchanged) - Source text → Infix token stream
2. **Compilation** (new) - Infix token stream → Postfix opcode stream  
3. **Execution** (new) - Postfix opcode stream → Results via VM stack

### Infix to Postfix Conversion
The key transformation is converting infix expressions to postfix (Reverse Polish Notation):
```
Infix tokens:    A + B * C
Postfix opcodes: [PUSHVARB A][PUSHVARB B][PUSHVARB C][MUL][ADD]
```

This conversion:
- Eliminates the need for recursive parsing during execution
- Naturally handles operator precedence through compilation
- Enables efficient stack-based execution
- Maintains identical mathematical semantics

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
Opcodes use a 6+2 bit encoding scheme with variable-length instructions:
- **No operands (0x00-0x3F)**: Single-byte opcodes for common operations (ADD, SUB, etc.)
- **One byte operand (0x40-0x7F)**: Two-byte opcodes for 8-bit offsets (PUSHVARB, PUSHSTRINGB)
- **Two byte operands (0x80-0xBF)**: Three-byte opcodes for 16-bit values (PUSHINT, JUMPW)
- **Reserved (0xC0-0xFF)**: Future extensions

### 3. Literal Reference Strategy
Instead of duplicating literal data, opcodes reference the original token stream:
```
Token stream:   [TOKEN_STRING]["HELLO"][TOKEN_NUMBER][0x002A][0]
Opcode stream:  [PUSHGLOBAL 0x01][PUSHBYTE 0x08][ADD]
```

This approach:
- Minimizes memory usage (critical on 6502)
- Enables single-byte addressing for most programs
- Avoids data duplication

### 4. Stack Integration
The JIT executor integrates directly with the existing Hopper VM stack system:
- **Value Stack**: Uses existing `Address.ValueStackLSB/MSB` (0x0600-0x0700)
- **Type Stack**: Uses existing `Address.TypeStackLSB` (0x0500-0x05FF)
- **Stack Operations**: Leverages proven `Stacks.PushTop()`, `Stacks.PopTop()`, etc.
- **Stack Management**: VM automatically handles overflow/underflow detection

### 5. Storage Strategy
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
1. **Complete Compiler.asm**:
   - Opcode emission functions (`EmitOpcode()`, `EmitOpcodeWithByte()`, `EmitOpcodeWithWord()`)
   - Buffer management (`InitOpcodeBuffer()`, `CheckBufferSpace()`)
   - Token offset tracking for literal references
   - Opcode buffer bounds checking

2. **Complete Executor.asm**:
   - Opcode dispatch loop using `ZP.PCL/ZP.PCH`
   - Handlers for all opcodes defined in existing `Opcodes.asm`
   - Integration with Hopper VM stack operations
   - Literal data fetching from token buffer

### Phase 2: Expression Compilation (3 days)
1. **Replace Expression.asm Functions**:
   - Transform `parseLogical()` → `compileLogical()` - emit opcodes instead of executing
   - Transform `parseComparison()` → `compileComparison()` - handle all comparison operators
   - Transform `parseBitwiseOr()` → `compileBitwiseOr()` - maintain operator precedence
   - Transform `parseAdditive()` → `compileAdditive()` - addition, subtraction
   - Transform `parseMultiplicative()` → `compileMultiplicative()` - multiplication, division, modulo
   - Transform `parsePrimary()` → `compilePrimary()` - numbers, variables, parentheses

2. **Implement Compilation Logic**:
   - **Numbers**: Emit `PUSHBIT`, `PUSHBYTE`, `PUSHINT`, `PUSHWORD` with token buffer offsets
   - **Variables**: Emit `PUSHGLOBAL` with identifier name offset in token buffer
   - **Operators**: Emit appropriate arithmetic/logical opcodes directly
   - **Type Compatibility**: Maintain existing type checking during compilation
   - **Error Handling**: Preserve Messages integration and error propagation

3. **Maintain Parser Structure**:
   - Keep identical recursive descent structure and precedence rules
   - Support all current expression types and operators without changes
   - Ensure token advancement and position tracking works identically

### Phase 3: Statement Integration (2 days)
1. **Transform Statement.asm Execution Flow**:
   ```
   Current:  Parse tokens → Execute directly
   New:      Parse tokens → Compile to opcodes → Execute opcodes
   ```

2. **Update Core Statements**:
   - **PRINT**: Compile expression, emit `SYSCALL 0x01` (PRINT)
   - **Assignment**: Compile RHS expression, emit `POPGLOBAL` opcode
   - **Variable Declaration**: Compile optional initialization expression
   - **Management Commands**: Keep direct execution (NEW, VARS, MEM, etc.)

3. **Integration Points**:
   - Add compilation phase to `Statement.Execute()` main switch
   - Route compiled opcodes through executor
   - Maintain identical error handling and Messages integration

### Phase 4: Direct Replacement & Testing (2 days)
1. **Replace Implementation**:
   - Remove old direct-execution parsing functions entirely
   - Connect new compilation functions to existing call sites
   - Update `Expression.Evaluate()` to use compilation path

2. **Validation Testing**:
   - Test all expression types: arithmetic, logical, comparison
   - Verify variable access and assignment works correctly
   - Test error handling produces identical error messages
   - Validate stack state after expression evaluation

3. **Performance Measurement**:
   - Add timing hooks to measure execution speed improvements
   - Test with complex nested expressions
   - Validate expected 3-5x performance improvement on arithmetic operations

### Phase 5: Optimization & Polish (1 day)
1. **Performance Tuning**:
   - Add specialized opcodes for common patterns if beneficial
   - Implement simple peephole optimizations (optional)
   - Verify opcode buffer usage stays well within 512-byte limit

2. **Final Integration**:
   - Clean up any temporary testing infrastructure
   - Update documentation and code comments
   - Verify all existing BASIC programs work identically

## Opcode Set (Complete)

### Stack Operations (One Byte Operand)
- `PUSHBIT <value>` - Push BIT literal (0 or 1)
- `PUSHBYTE <value>` - Push BYTE immediate value
- `PUSHGLOBAL <index>` - Push global variable by index
- `PUSHLOCAL <offset>` - Push local variable by signed offset
- `POPGLOBAL <index>` - Pop to global variable by index
- `POPLOCAL <offset>` - Pop to local variable by signed offset

### Stack Operations (Two Byte Operands)
- `PUSHINT <lsb> <msb>` - Push INT immediate value
- `PUSHWORD <lsb> <msb>` - Push WORD immediate value

### Arithmetic (No Operands)
- `ADD` - Pop two values, push sum
- `SUB` - Pop two values, push difference  
- `MUL` - Pop two values, push product
- `DIV` - Pop two values, push quotient
- `MOD` - Pop two values, push remainder
- `NEG` - Pop value, push negation

### Bitwise Operations (No Operands)
- `BITWISE_AND` - Pop two values, push bitwise AND
- `BITWISE_OR` - Pop two values, push bitwise OR

### Logical Operations (No Operands, BIT type only)
- `LOGICAL_AND` - Pop two BIT values, push logical AND
- `LOGICAL_OR` - Pop two BIT values, push logical OR
- `LOGICAL_NOT` - Pop BIT value, push logical NOT

### Comparison (No Operands)
- `EQ` - Pop two values, push equality result (BIT)
- `NE` - Pop two values, push inequality result (BIT)
- `LT`, `GT`, `LE`, `GE` - Comparison operators returning BIT results

### Control Flow
- `JUMPB <offset>` - Unconditional jump (signed byte offset)
- `JUMPZB <offset>` - Jump if zero (signed byte offset)  
- `JUMPNZB <offset>` - Jump if non-zero (signed byte offset)
- `JUMPW <lsb> <msb>` - Unconditional jump (signed word offset)
- `JUMPZW <lsb> <msb>` - Jump if zero (signed word offset)
- `JUMPNZW <lsb> <msb>` - Jump if non-zero (signed word offset)

### Function Operations (No Operands)
- `RETURN` - Return from function (no return value)
- `RETURNVAL` - Return from function (pop return value from stack)

### System Calls (One Byte Operand)
- `SYSCALL <id>` - System call (0x01=PRINT, 0x02=PRINTLN, etc.)
- `CALL <index>` - Function call by index

### Stack Manipulation (No Operands)
- `DECSP` - Decrement stack pointer (discard top value)
- `DUP` - Duplicate top stack value
- `NOP` - No operation (useful for optimization)

## Memory Impact

- Opcode buffer: 512 bytes (fixed allocation at 0x0C00-0x0DFF)
- Zero page usage: 6 bytes (0x3A-0x3F)
- Opcode definitions: ~300 bytes ROM (existing `Opcodes.asm`)
- Compiler code: ~2KB ROM
- Executor code: ~1KB ROM

**Total: ~3.5KB additional ROM, 518 bytes additional RAM**

## Performance Expectations

Based on similar interpreters and the elimination of recursive parsing overhead:
- **3-5x faster** arithmetic operations (no recursive descent during execution)
- **2-3x faster** overall program execution (compilation overhead amortized)
- **Near-instant** repeated execution of compiled functions (future phase)
- **Identical behavior** with significantly improved speed

## Success Criteria

1. **Functional Compatibility**: All existing BASIC programs run unchanged
2.iden **Behavioral Identical**: Execution produces identical results and error messages
3. **Performance Improvement**: Measurable speed increase, targeting 3-5x on arithmetic
4. **Memory Efficiency**: Stays within defined 512-byte opcode buffer limits
5. **Clean Architecture**: Foundation ready for function compilation caching (future)
6. **Error Handling**: Maintains existing error reporting and debugging capabilities

## Future Extensions

This architecture enables:
- **Function Compilation Caching**: Store compiled opcodes with function definitions
- **Advanced Optimizations**: Constant folding, common subexpression elimination
- **Hot Path Optimization**: Native code generation for frequently executed code
- **Debugging Capabilities**: Opcode-level stepping and inspection
- **Performance Profiling**: Execution time analysis and bottleneck identification

## Technical Notes

### Buffer Design
- The 512-byte opcode buffer is guaranteed sufficient since opcodes reference tokens rather than embedding literals
- Token buffer references use 1-2 bytes vs. potentially much larger embedded data
- Most programs will use <256 bytes of opcodes, enabling single-byte addressing
- Postfix opcodes are typically more compact than infix tokens due to elimination of precedence parsing overhead

### VM Integration
- Using `ZP.PCL/ZP.PCH` from Hopper VM for opcode execution maintains consistency
- Existing stack operations (`Stacks.PushTop()`, `Stacks.PopTop()`, etc.) handle all type management
- Fixed buffer sizes align with HopperBASIC's philosophy of predictable memory usage

### Error Compatibility
- Compilation errors map directly to existing Messages system
- Runtime errors during opcode execution produce identical error messages
- Stack overflow/underflow detection maintained through existing VM mechanisms

### Implementation Safety
- Opcode emission includes bounds checking to prevent buffer overflow
- Invalid opcodes cause immediate `BRK` with error message (no silent failures)
- Type checking during compilation prevents runtime type errors
- Infix to postfix conversion maintains mathematical precedence and associativity rules