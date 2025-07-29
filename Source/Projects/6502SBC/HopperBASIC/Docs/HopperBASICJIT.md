# HopperBASIC JIT Compilation Project - Updated Status

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
Postfix opcodes: [PUSHGLOBAL A][PUSHGLOBAL B][PUSHGLOBAL C][MUL][ADD]
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
const uint BasicProcessBuffer2    = 0x09C0;  // 32 bytes - secondary workspace (Statement.asm)
const uint BasicProcessBuffer3    = 0x09E0;  // 32 bytes - tertiary workspace (Compiler/Executor)
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

### BasicProcessBuffer3 Layout (Compiler/Executor shared space)
```asm
// Private Compiler layer storage - BasicProcessBuffer3 (32 bytes at 0x09E0-0x09FF)
const uint compilerSavedTokenPosL = Address.BasicProcessBuffer3;      // 0x09E0: 1 byte - saved tokenizer pos low
const uint compilerSavedTokenPosH = Address.BasicProcessBuffer3 + 1;  // 0x09E1: 1 byte - saved tokenizer pos high
const uint compilerLiteralOffsetL = Address.BasicProcessBuffer3 + 2;  // 0x09E2: 1 byte - literal offset low
const uint compilerLiteralOffsetH = Address.BasicProcessBuffer3 + 3;  // 0x09E3: 1 byte - literal offset high
const uint compilerScratch1      = Address.BasicProcessBuffer3 + 4;   // 0x09E4: 1 byte - general scratch
const uint compilerScratch2      = Address.BasicProcessBuffer3 + 5;   // 0x09E5: 1 byte - general scratch
const uint compilerOperatorToken = Address.BasicProcessBuffer3 + 6;   // 0x09E6: 1 byte - saved operator token
const uint compilerBufferAddr    = Address.BasicProcessBuffer3 + 7;   // 0x09E7: 2 bytes - calculated buffer address
// Executor can use remaining 23 bytes (0x09E9-0x09FF) for executor-specific state
```

## Implementation Status

### ✅ Phase 1: Core Infrastructure (COMPLETED)
1. **✅ Complete Compiler.asm**:
   - ✅ Opcode emission functions (`EmitOpcode()`, `EmitOpcodeWithByte()`, `EmitOpcodeWithWord()`)
   - ✅ Buffer management (`InitOpcodeBuffer()`, `CheckBufferSpace()`)
   - ✅ Token offset tracking for literal references (`CalculateTokenOffset()`)
   - ✅ Opcode buffer bounds checking with proper error handling
   - ✅ Complete expression compilation chain:
     - `CompileExpression()` - Main entry point
     - `compileLogical()` - OR operators (lowest precedence)
     - `compileLogicalAnd()` - AND operators  
     - `compileComparison()` - =, <>, <, >, <=, >=
     - `compileBitwiseOr()` - Bitwise OR
     - `compileAdditive()` - +, - operators
     - `compileMultiplicative()` - *, /, % operators  
     - `compileUnary()` - -, NOT operators
     - `compilePrimary()` - Numbers, variables, parentheses (highest precedence)
   - ✅ Type-aware opcode emission (PUSHBIT, PUSHBYTE, PUSHINT, PUSHWORD)
   - ✅ Operator-specific emission functions for all operation types
   - ✅ Proper zero page variable usage and dedicated buffer space allocation

2. **❌ Complete Executor.asm** (NEXT PRIORITY):
   - ❌ Opcode dispatch loop using `ZP.PCL/ZP.PCH`
   - ❌ Handlers for all opcodes defined in existing `Opcodes.asm`
   - ❌ Integration with Hopper VM stack operations
   - ❌ Literal data fetching from token buffer using offsets
   - ❌ Use remaining BasicProcessBuffer3 space for executor state

### ❌ Phase 2: Expression Compilation Integration (PENDING)
1. **Replace Expression.asm Functions**:
   - Update `Expression.Evaluate()` to use `Compiler.CompileExpression()` + `Executor.ExecuteOpcodes()`
   - Maintain identical API and error handling
   - Preserve type checking and stack integration

2. **Validation Testing**:
   - Test all expression types: arithmetic, logical, comparison
   - Verify variable access works correctly
   - Test error handling produces identical error messages
   - Validate stack state after expression evaluation

### ❌ Phase 3: Statement Integration (PENDING)
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

### ❌ Phase 4: Performance Testing & Optimization (PENDING)
1. **Performance Measurement**:
   - Add timing hooks to measure execution speed improvements
   - Test with complex nested expressions
   - Validate expected 3-5x performance improvement on arithmetic operations

2. **Final Integration**:
   - Clean up any temporary testing infrastructure
   - Update documentation and code comments
   - Verify all existing BASIC programs work identically

## Key Design Decisions (Implemented)

### 1. Fixed-Size Buffers ✅
- 512-byte opcode buffer matches tokenizer buffer size
- Opcodes are more compact than tokens, ensuring sufficient space
- Simple bounds checking and overflow detection implemented
- No dynamic memory allocation required

### 2. Opcode Format ✅
Opcodes use a 6+2 bit encoding scheme with variable-length instructions:
- **No operands (0x00-0x3F)**: Single-byte opcodes for common operations (ADD, SUB, etc.)
- **One byte operand (0x40-0x7F)**: Two-byte opcodes for 8-bit offsets (PUSHGLOBAL, PUSHBYTE)
- **Two byte operands (0x80-0xBF)**: Three-byte opcodes for 16-bit values (PUSHINT, PUSHWORD)
- **Reserved (0xC0-0xFF)**: Future extensions

### 3. Literal Reference Strategy ✅
Instead of duplicating literal data, opcodes reference the original token stream:
```
Token stream:   [TOKEN_STRING]["HELLO"][TOKEN_NUMBER][0x002A][0]
Opcode stream:  [PUSHGLOBAL 0x01][PUSHBYTE 0x08][ADD]
```

This approach:
- Minimizes memory usage (critical on 6502)
- Enables single-byte addressing for most programs
- Avoids data duplication

### 4. Stack Integration ✅
The JIT executor will integrate directly with the existing Hopper VM stack system:
- **Value Stack**: Uses existing `Address.ValueStackLSB/MSB` (0x0600-0x0700)
- **Type Stack**: Uses existing `Address.TypeStackLSB` (0x0500-0x05FF)
- **Stack Operations**: Leverages proven `Stacks.PushTop()`, `Stacks.PopTop()`, etc.
- **Stack Management**: VM automatically handles overflow/underflow detection

### 5. Storage Strategy ✅
For immediate mode (console commands):
- Opcodes generated in temporary buffer
- Executed immediately
- Buffer reused for next command

For functions (future phase):
- Opcodes cached after first compilation
- Stored as extension to function node structure
- Reused on subsequent calls

## Opcode Set (Complete) ✅

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

## IMMEDIATE NEXT STEPS

### 1. Implement Executor.asm (TOP PRIORITY)
The executor needs these key components:

#### Memory Layout for Executor
Use remaining BasicProcessBuffer3 space (0x09E9-0x09FF, 23 bytes):
```asm
// Executor-specific storage in BasicProcessBuffer3
const uint executorPCL           = Address.BasicProcessBuffer3 + 9;   // 0x09E9: 1 byte - execution PC low
const uint executorPCH           = Address.BasicProcessBuffer3 + 10;  // 0x09EA: 1 byte - execution PC high
const uint executorStartAddrL    = Address.BasicProcessBuffer3 + 11;  // 0x09EB: 1 byte - opcode buffer start low
const uint executorStartAddrH    = Address.BasicProcessBuffer3 + 12;  // 0x09EC: 1 byte - opcode buffer start high
const uint executorEndAddrL      = Address.BasicProcessBuffer3 + 13;  // 0x09ED: 1 byte - opcode buffer end low
const uint executorEndAddrH      = Address.BasicProcessBuffer3 + 14;  // 0x09EE: 1 byte - opcode buffer end high
const uint executorOperandL      = Address.BasicProcessBuffer3 + 15;  // 0x09EF: 1 byte - current operand low
const uint executorOperandH      = Address.BasicProcessBuffer3 + 16;  // 0x09F0: 1 byte - current operand high
const uint executorTokenAddrL    = Address.BasicProcessBuffer3 + 17;  // 0x09F1: 1 byte - token fetch addr low
const uint executorTokenAddrH    = Address.BasicProcessBuffer3 + 18;  // 0x09F2: 1 byte - token fetch addr high
// 13 bytes remaining for future executor needs (0x09F3-0x09FF)
```

#### Core Functions Needed
1. **`ExecuteOpcodes()`** - Main entry point
2. **`InitExecutor()`** - Set up execution state from opcode buffer
3. **`FetchOpcode()`** - Get next opcode using ZP.PCL/PCH
4. **`FetchOperandByte()`** - Get single byte operand
5. **`FetchOperandWord()`** - Get word operand  
6. **`FetchTokenData()`** - Get literal data from token buffer using offset
7. **Opcode Handlers** - Individual functions for each opcode type:
   - `executeAdd()`, `executeSub()`, `executeMul()`, etc.
   - `executePushBit()`, `executePushByte()`, etc.
   - `executePushGlobal()`, `executePopGlobal()`
   - All other opcodes from the complete opcode set

#### Integration Points
- Use existing `Stacks.PushTop()`, `Stacks.PopTop()` for all stack operations
- Use existing `Variables.Find()`, `Variables.GetValue()`, `Variables.SetValue()` for variable access
- Use existing `Messages` system for error handling
- Use `ZP.PCL/ZP.PCH` from Hopper VM for opcode execution pointer

### 2. After Executor.asm is Complete
1. **Test Basic Compilation + Execution**:
   - Simple expressions like `5 + 3`
   - Variable access expressions like `A + B`
   - Mixed type expressions with proper type handling

2. **Replace Expression.Evaluate()**:
   - Update `Expression.Evaluate()` to call `Compiler.CompileExpression()` then `Executor.ExecuteOpcodes()`
   - Ensure identical behavior and error handling

3. **Integration Testing**:
   - Test through existing PRINT statements
   - Test through variable assignments
   - Verify all existing functionality still works

## Success Criteria

1. **Functional Compatibility**: All existing BASIC programs run unchanged
2. **Behavioral Identical**: Execution produces identical results and error messages
3. **Performance Improvement**: Measurable speed increase, targeting 3-5x on arithmetic
4. **Memory Efficiency**: Stays within defined 512-byte opcode buffer limits
5. **Clean Architecture**: Foundation ready for function compilation caching (future)
6. **Error Handling**: Maintains existing error reporting and debugging capabilities

## Memory Impact

- Opcode buffer: 512 bytes (fixed allocation at 0x0C00-0x0DFF)
- Zero page usage: 6 bytes (0x3A-0x3F)
- Opcode definitions: ~300 bytes ROM (existing `Opcodes.asm`)
- Compiler code: ~2KB ROM ✅
- Executor code: ~1KB ROM (needs implementation)

**Total: ~3.5KB additional ROM, 518 bytes additional RAM**

## Files Status
- ✅ **OpCodes.asm** - Complete opcode definitions
- ✅ **Compiler.asm** - Complete compilation infrastructure  
- ❌ **Executor.asm** - Empty shell, needs full implementation
- ❌ **Expression.asm** - Needs integration with compiler/executor
- ❌ **Statement.asm** - Needs integration for assignments and PRINT

## Technical Notes

### Rule Compliance ✅
All implemented code follows project rules:
- **Rule #1**: No silent failures - all errors use proper Messages + BRK
- **Rule #2**: Uses dedicated ZP variables and buffer space, no unauthorized ZP usage
- **Rule #4**: Complete methods, no "rest of function" shortcuts
- **Rule #7**: C/NC flags used consistently for success/failure
- **Rule #8**: CamelCase identifiers throughout
- **Rule #9**: Direct enum syntax (OpcodeType.ADD vs Opcodes.OpcodeType.ADD)

### Implementation Quality ✅
- Clean API with proper documentation
- Comprehensive error handling
- Memory-efficient literal referencing
- Type-aware opcode emission
- Proper bounds checking
- Integration with existing Hopper VM systems