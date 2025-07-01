# Hopper VM Runtime Implementation Specification

## Overview

This document describes the C# implementation of the Hopper Virtual Machine - a stack-based VM designed to execute Hopper bytecode programs. The VM features 32-bit stack slots, .NET garbage collection integration, and a modular system call architecture.

## VM Architecture

### Stack-Based Virtual Machine
- **32-bit stack slots** for all values (optimized for modern 64-bit architectures)
- **Value types**: `byte`, `char`, `uint`, `int`, `long`, `float`, `bool` stored directly on stack
- **Reference types**: `string`, lists, dictionaries, arrays stored on managed heap (.NET GC)
- **Single-threaded execution** model

### Stack Value Structure
```csharp
public struct StackValue
{
    public uint UIntValue;     // Also handles byte, char, bool
    public int IntValue; 
    public long LongValue;     // Full 64-bit value type
    public float FloatValue;   // Full 32-bit value type
    public object? RefValue;   // Reference types (GC managed)
    public ValueType Type;     // Type discriminator
}
```

### VM State Management
```csharp
private readonly Stack<StackValue> stack;
private readonly Dictionary<uint, StackValue> globals;
private uint pc;  // Program counter
private uint bp;  // Base pointer for locals
private readonly Stack<uint> callStack;
private readonly Stack<uint> basePointers;
```

### Memory Management
- **Primitives**: Stack-allocated value types in 32-bit slots
- **References**: .NET garbage collector handles all reference types
- **No manual memory management** required
- **Efficient long/float**: Full value types instead of heap allocations

## Binary File Format & Loading

### Header Structure (6 bytes)
```
0x0000: Binary version number (2 bytes)
0x0002: Offset to constant data (2 bytes)  
0x0004: Entry point offset (2 bytes)
```

### File Layout
```
[Header: 6 bytes]
[Method Table: variable size, 4 bytes per entry]
[Constants: variable size]
[Code: starts at entry point offset]
```

### BinaryLoader Architecture
- **Core/BinaryLoader.cs**: Handles all file parsing and validation
- **LoadedProgram struct**: Contains parsed program, constants, and method table
- **Validation**: File format validation with comprehensive error checking
- **Load-time optimization**: CALL → CALLI instruction optimization

### Method Table
- Located between header and constants
- Each entry: 4 bytes (2-byte index + 2-byte address)
- Addresses are relative to entry point
- Used for `CALL` instruction indirection

## Complete Instruction Set Reference

The Hopper VM implements 175+ opcodes divided into minimal runtime instructions and packed instructions for code density optimization.

### Core Stack Operations

#### 0x00 - NOP
**Function**: No operation  
**Description**: Creates a delay or aligns instructions in memory  
**Stack**: No change  
**Usage**: `NOP`

#### 0x01 - DUP0
**Function**: Duplicate top stack value  
**Stack**: `[top, ...] → [top, top, ...]`  
**Usage**: `DUP0`

#### 0x02 - PUSHR0
**Function**: Push register R0 to stack  
**Stack**: `[...] → [R0, ...]`  
**Usage**: `PUSHR0`

#### 0x03 - POPR0
**Function**: Pop stack top to register R0  
**Stack**: `[top, ...] → [...]`  
**Register**: `R0 = top`  
**Usage**: `POPR0`

#### 0x43 - SWAP
**Function**: Swap top two stack values  
**Stack**: `[value1, value2, ...] → [value2, value1, ...]`  
**Usage**: `SWAP`

### Immediate Value Operations

#### 0x37 - PUSHI
**Function**: Push 16-bit immediate value  
**Stack**: `[...] → [value, ...]`  
**Usage**: `PUSHI value`

#### 0x44 - PUSHI0
**Function**: Push constant 0  
**Stack**: `[...] → [0, ...]`  
**Usage**: `PUSHI0`

#### 0x45 - PUSHI1
**Function**: Push constant 1  
**Stack**: `[...] → [1, ...]`  
**Usage**: `PUSHI1`

#### 0x46 - PUSHIM1
**Function**: Push constant -1  
**Stack**: `[...] → [-1, ...]`  
**Usage**: `PUSHIM1`

#### 0x1A - PUSHIB
**Function**: Push 8-bit immediate value (packed)  
**Stack**: `[...] → [value, ...]`  
**Usage**: `PUSHIB value`

#### 0xAA - PUSHIBB
**Function**: Push two immediate bytes  
**Stack**: `[...] → [byte2, byte1, ...]`  
**Usage**: `PUSHIBB byte1 byte2`

### Local Variable Access

#### 0x39 - PUSHLOCAL
**Function**: Push local variable (16-bit offset)  
**Stack**: `[...] → [local_value, ...]`  
**Usage**: `PUSHLOCAL offset`

#### 0x38 - POPLOCAL
**Function**: Pop to local variable (16-bit offset)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPLOCAL offset`

#### 0x1C - PUSHLOCALB
**Function**: Push local variable (8-bit signed offset)  
**Stack**: `[...] → [local_value, ...]`  
**Usage**: `PUSHLOCALB offset`  
**Note**: Offset is signed byte (-128 to +127 from BP)

#### 0x1B - POPLOCALB
**Function**: Pop to local variable (8-bit signed offset)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPLOCALB offset`

#### 0x4E - PUSHLOCALB00
**Function**: Push local variable at offset 0 (optimized)  
**Stack**: `[...] → [local[0], ...]`  
**Usage**: `PUSHLOCALB00`

#### 0x4F - PUSHLOCALB01
**Function**: Push local variable at offset 1 (optimized)  
**Stack**: `[...] → [local[1], ...]`  
**Usage**: `PUSHLOCALB01`

#### 0x4C - POPLOCALB00
**Function**: Pop to local variable at offset 0 (optimized)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPLOCALB00`

#### 0x4D - POPLOCALB01
**Function**: Pop to local variable at offset 1 (optimized)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPLOCALB01`

### Global Variable Access

#### 0x3D - PUSHGLOBAL
**Function**: Push global variable (16-bit address)  
**Stack**: `[...] → [global_value, ...]`  
**Usage**: `PUSHGLOBAL address`

#### 0x3C - POPGLOBAL
**Function**: Pop to global variable (16-bit address)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPGLOBAL address`

#### 0x20 - PUSHGLOBALB
**Function**: Push global variable (8-bit offset)  
**Stack**: `[...] → [global_value, ...]`  
**Usage**: `PUSHGLOBALB offset`

#### 0x1F - POPGLOBALB
**Function**: Pop to global variable (8-bit offset)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPGLOBALB offset`

#### 0x52 - PUSHGLOBALBB
**Function**: Push two global variables (8-bit offsets)  
**Stack**: `[...] → [global[offset2], global[offset1], ...]`  
**Usage**: `PUSHGLOBALBB offset1 offset2`

### Relative Addressing

#### 0x3B - PUSHREL
**Function**: Push value from relative address (16-bit offset)  
**Stack**: `[...] → [value, ...]`  
**Usage**: `PUSHREL offset`

#### 0x3A - POPREL
**Function**: Pop to relative address (16-bit offset)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPREL offset`

#### 0x1E - PUSHRELB
**Function**: Push value from relative address (8-bit offset)  
**Stack**: `[...] → [value, ...]`  
**Usage**: `PUSHRELB offset`

#### 0x1D - POPRELB
**Function**: Pop to relative address (8-bit offset)  
**Stack**: `[value, ...] → [...]`  
**Usage**: `POPRELB offset`

### Arithmetic Operations

#### 0x80 - ADD
**Function**: Add top two stack values  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = value2 + value1`  
**Usage**: `ADD`

#### 0x81 - ADDI
**Function**: Add immediate to top stack value  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value + immediate`  
**Usage**: `ADDI immediate`

#### 0x6D - ADDB
**Function**: Add immediate byte to top stack value (packed)  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value + immediate_byte`  
**Usage**: `ADDB immediate_byte`

#### 0x82 - SUB
**Function**: Subtract top from next stack value  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = value2 - value1`  
**Usage**: `SUB`

#### 0x83 - SUBI
**Function**: Subtract immediate from top stack value  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value - immediate`  
**Usage**: `SUBI immediate`

#### 0x6E - SUBB
**Function**: Subtract immediate byte from top stack value (packed)  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value - immediate_byte`  
**Usage**: `SUBB immediate_byte`

#### 0x86 - MUL
**Function**: Multiply top two stack values  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = value1 * value2`  
**Usage**: `MUL`

#### 0x87 - MULI
**Function**: Multiply top stack value by immediate  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value * immediate`  
**Usage**: `MULI immediate`

#### 0x84 - DIV
**Function**: Divide next by top stack value  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = value2 / value1`  
**Usage**: `DIV`

#### 0x85 - DIVI
**Function**: Divide top stack value by immediate  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value / immediate`  
**Usage**: `DIVI immediate`

#### 0x88 - MOD
**Function**: Modulus of next by top stack value  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = value2 % value1`  
**Usage**: `MOD`

#### 0x89 - MODI
**Function**: Modulus of top stack value by immediate  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = value % immediate`  
**Usage**: `MODI immediate`

### Comparison Operations

#### 0x8A - GT
**Function**: Greater than (unsigned)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 > value1) ? 1 : 0`  
**Usage**: `GT`

#### 0x8B - GTI
**Function**: Greater than (signed)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 > value1) ? 1 : 0`  
**Usage**: `GTI`

#### 0x8C - LT
**Function**: Less than (unsigned)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 < value1) ? 1 : 0`  
**Usage**: `LT`

#### 0x8D - LTI
**Function**: Less than (signed)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 < value1) ? 1 : 0`  
**Usage**: `LTI`

#### 0x8E - GE
**Function**: Greater than or equal (unsigned)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 >= value1) ? 1 : 0`  
**Usage**: `GE`

#### 0x8F - GEI
**Function**: Greater than or equal (signed)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 >= value1) ? 1 : 0`  
**Usage**: `GEI`

#### 0x90 - LE
**Function**: Less than or equal (unsigned)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 <= value1) ? 1 : 0`  
**Usage**: `LE`

#### 0x91 - LEI
**Function**: Less than or equal (signed)  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 <= value1) ? 1 : 0`  
**Usage**: `LEI`

#### 0x92 - EQ
**Function**: Equality comparison  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 == value1) ? 1 : 0`  
**Usage**: `EQ`

#### 0x94 - NE
**Function**: Inequality comparison  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 != value1) ? 1 : 0`  
**Usage**: `NE`

### Logical Operations

#### 0x96 - BOOLOR
**Function**: Boolean OR  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 || value1)`  
**Usage**: `BOOLOR`

#### 0x98 - BOOLAND
**Function**: Boolean AND  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 && value1)`  
**Usage**: `BOOLAND`

#### 0x41 - BOOLNOT
**Function**: Boolean NOT  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = !value`  
**Usage**: `BOOLNOT`

### Bitwise Operations

#### 0x9A - BITAND
**Function**: Bitwise AND  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 & value1)`  
**Usage**: `BITAND`

#### 0x9C - BITOR
**Function**: Bitwise OR  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 | value1)`  
**Usage**: `BITOR`

#### 0x9E - BITXOR
**Function**: Bitwise XOR  
**Stack**: `[value1, value2, ...] → [result, ...]`  
**Operation**: `result = (value2 ^ value1)`  
**Usage**: `BITXOR`

#### 0x42 - BITNOT
**Function**: Bitwise NOT  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = ~value`  
**Usage**: `BITNOT`

#### 0xA2 - BITSHL
**Function**: Bitwise shift left  
**Stack**: `[shift_amount, value, ...] → [result, ...]`  
**Operation**: `result = (value << shift_amount)`  
**Usage**: `BITSHL`

#### 0xA0 - BITSHR
**Function**: Bitwise shift right  
**Stack**: `[shift_amount, value, ...] → [result, ...]`  
**Operation**: `result = (value >> shift_amount)`  
**Usage**: `BITSHR`

#### 0x04 - BITSHL8
**Function**: Shift left by 8 bits (optimized)  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = (value << 8)`  
**Usage**: `BITSHL8`

#### 0x05 - BITSHR8
**Function**: Shift right by 8 bits (optimized)  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = (value >> 8)`  
**Usage**: `BITSHR8`

#### 0x06 - BITANDFF
**Function**: Bitwise AND with 0xFF (optimized)  
**Stack**: `[value, ...] → [result, ...]`  
**Operation**: `result = (value & 0xFF)`  
**Usage**: `BITANDFF`

### Control Flow Operations

#### 0x33 - JW
**Function**: Unconditional jump (wide)  
**Stack**: No change  
**Usage**: `JW address`

#### 0x30 - JB
**Function**: Unconditional jump (byte offset)  
**Stack**: No change  
**Usage**: `JB offset`

#### 0x31 - JZ
**Function**: Jump if zero (16-bit address)  
**Stack**: `[condition, ...] → [...]`  
**Usage**: `JZ address`

#### 0x32 - JNZ
**Function**: Jump if not zero (16-bit address)  
**Stack**: `[condition, ...] → [...]`  
**Usage**: `JNZ address`

#### 0x2E - JZB
**Function**: Jump if zero (byte offset)  
**Stack**: `[condition, ...] → [...]`  
**Usage**: `JZB offset`

#### 0x2F - JNZB
**Function**: Jump if not zero (byte offset)  
**Stack**: `[condition, ...] → [...]`  
**Usage**: `JNZB offset`

#### 0x69 - JIX
**Function**: Jump indexed  
**Stack**: `[index, ...] → [...]`  
**Usage**: `JIX`  
**Note**: Uses index for jump table lookup

### Function Call Operations

#### 0x34 - CALL
**Function**: Call function (direct address)  
**Stack**: Arguments passed via stack  
**Usage**: `CALL address`  
**Note**: Pushes return address to call stack

#### 0x6A - CALLI
**Function**: Call function (indirect)  
**Stack**: `[address, ...] → [...]`  
**Usage**: `CALLI`  
**Note**: Address popped from stack

#### 0x4B - CALLREL
**Function**: Call function (relative address)  
**Stack**: Arguments passed via stack  
**Usage**: `CALLREL offset`  
**Note**: Address relative to current PC

### Function Entry/Exit Operations

#### 0x49 - ENTER
**Function**: Enter function (16-bit frame size)  
**Stack**: Sets up new stack frame  
**Usage**: `ENTER frame_size`  
**Note**: Saves BP, allocates locals

#### 0x5F - ENTERB
**Function**: Enter function (8-bit frame size)  
**Stack**: Sets up new stack frame  
**Usage**: `ENTERB frame_size`

#### 0x35 - RET
**Function**: Return from function  
**Stack**: Restores previous frame  
**Usage**: `RET`

#### 0x36 - RETRES
**Function**: Return with result  
**Stack**: `[result, ...] → [result, ...]`  
**Usage**: `RETRES`

#### 0x2A - RETB
**Function**: Return with stack adjustment (byte)  
**Stack**: Adjusts SP by operand  
**Usage**: `RETB operand`

#### 0x2B - RETRESB
**Function**: Return with result and stack adjustment (byte)  
**Stack**: `[result, ...] → [result, ...]`  
**Usage**: `RETRESB operand`

#### 0x4A - RET0
**Function**: Return (optimized for no locals)  
**Stack**: Restores frame efficiently  
**Usage**: `RET0`

#### 0x61 - RETFAST
**Function**: Fast return (optimized)  
**Stack**: Minimal frame restoration  
**Usage**: `RETFAST`

### Stack Management Operations

#### 0x28 - DECSP
**Function**: Decrement stack pointer  
**Stack**: `[top, ...] → [...]`  
**Usage**: `DECSP`

#### 0x3E - PUSHSTACKADDR
**Function**: Push address of stack top  
**Stack**: `[...] → [address, ...]`  
**Usage**: `PUSHSTACKADDR`

#### 0x21 - PUSHSTACKADDRB
**Function**: Push stack address with byte offset  
**Stack**: `[...] → [address, ...]`  
**Usage**: `PUSHSTACKADDRB offset`

#### 0x47 - PUSHGP
**Function**: Push global pointer  
**Stack**: `[...] → [GP, ...]`  
**Usage**: `PUSHGP`

#### 0x48 - COPYNEXTPOP
**Function**: Copy second value and pop top  
**Stack**: `[value1, value2, ...] → [value1, ...]`  
**Usage**: `COPYNEXTPOP`

### Variable Increment/Decrement Operations

#### 0x22 - INCLOCALB
**Function**: Increment local variable (byte offset)  
**Operation**: `local[offset]++`  
**Usage**: `INCLOCALB offset`

#### 0x23 - DECLOCALB
**Function**: Decrement local variable (byte offset)  
**Operation**: `local[offset]--`  
**Usage**: `DECLOCALB offset`

#### 0xA4 - INCLOCALIB
**Function**: Increment local variable by immediate (byte)  
**Operation**: `local[offset] += immediate`  
**Usage**: `INCLOCALIB offset immediate`

#### 0xA6 - DECLOCALIB
**Function**: Decrement local variable by immediate (byte)  
**Operation**: `local[offset] -= immediate`  
**Usage**: `DECLOCALIB offset immediate`

#### 0x53 - INCGLOBALB
**Function**: Increment global variable (byte offset)  
**Operation**: `global[offset]++`  
**Usage**: `INCGLOBALB offset`

#### 0x54 - DECGLOBALB
**Function**: Decrement global variable (byte offset)  
**Operation**: `global[offset]--`  
**Usage**: `DECGLOBALB offset`

#### 0xA5 - INCGLOBALIB
**Function**: Increment global variable by immediate (byte)  
**Operation**: `global[offset] += immediate`  
**Usage**: `INCGLOBALIB offset immediate`

#### 0xA7 - DECGLOBALIB
**Function**: Decrement global variable by immediate (byte)  
**Operation**: `global[offset] -= immediate`  
**Usage**: `DECGLOBALIB offset immediate`

### System Call Operations

#### 0x26 - SYSCALL
**Function**: Call system function (16-bit ID)  
**Stack**: Depends on system function  
**Usage**: `SYSCALL function_id`

#### 0x24 - SYSCALL0
**Function**: System call with 0 parameters  
**Stack**: `[...] → [result, ...]`  
**Usage**: `SYSCALL0 function_id`

#### 0x25 - SYSCALL1
**Function**: System call with 1 parameter  
**Stack**: `[param, ...] → [result, ...]`  
**Usage**: `SYSCALL1 function_id`

#### 0x0B - SYSCALL2
**Function**: System call with 2 parameters  
**Stack**: `[param2, param1, ...] → [result, ...]`  
**Usage**: `SYSCALL2 function_id`

### Library Call Operations

#### 0x08 - LIBCALL
**Function**: Call library function  
**Stack**: Depends on library function  
**Usage**: `LIBCALL function_id`

#### 0x09 - LIBCALL0
**Function**: Library call with 0 parameters  
**Stack**: `[...] → [result, ...]`  
**Usage**: `LIBCALL0 function_id`

#### 0x0A - LIBCALL1
**Function**: Library call with 1 parameter  
**Stack**: `[param, ...] → [result, ...]`  
**Usage**: `LIBCALL1 function_id`

### Type Conversion Operations

#### 0x51 - CAST
**Function**: Cast top stack value to different type  
**Stack**: `[value, ...] → [casted_value, ...]`  
**Usage**: `CAST type_id`

#### 0x60 - PUSHD
**Function**: Push double-precision float  
**Stack**: `[...] → [double_value, ...]`  
**Usage**: `PUSHD double_value`

### OpCode Metadata System
```csharp
public class InstructionInfo
{
    public OpCode OpCode { get; }
    public string Name { get; }
    public string Description { get; }
    public int OperandBytes { get; }
    public int StackPop { get; }      // -1 = variable
    public int StackPush { get; }     // -1 = variable
}
```

### Call Optimization
- **Load-time optimization**: `CALL` instructions converted to `CALLI`
- Method table lookup performed once during loading
- Runtime executes direct address calls for efficiency
- **Future-proof**: Can disable optimization for 4-byte addresses

## System Call Architecture

### SystemCallDispatcher Design
- **Centralized routing**: All syscalls go through dispatcher
- **ID-based organization**: Syscalls grouped by ranges (String: 0x00-0x0F, Screen: 0x20-0x3F)
- **Scalable**: Easy to add 100+ syscalls without VM complexity
- **Type-safe**: Uses StackValue operations for parameter handling

### Current System Calls
| ID | Name | Description |
|----|------|-------------|
| 0x05 | String.NewFromConstant | Create string from constant data |
| 0x29 | Screen.Print | Print text with colors |
| 0x2A | Screen.PrintLn | Print newline |

### System Call Ranges
```csharp
// String operations: 0x00-0x0F
// Screen operations: 0x20-0x3F  
// Keyboard operations: 0x40-0x4F
// File operations: 0x50-0x5F
// Time operations: 0x60-0x6F
// Math operations: 0x70-0x7F
// List operations: 0x80-0x8F
// Dictionary operations: 0x90-0x9F
```

## Local Variable & Parameter Access

### Stack Frame Layout
```
[... previous stack ...]
[parameter N]          <- BP-N
[parameter 1]          <- BP-2  
[parameter 0]          <- BP-1
[saved BP]             <- BP
[local 0]              <- BP+1
[local 1]              <- BP+2
[current stack top]    <- growing upward
```

### PUSHLOCALB Implementation
```csharp
byte offset = program[pc++];
sbyte signedOffset = (sbyte)offset; // -128 to +127
int stackPosition = (int)bp + signedOffset;
// Convert to array index and push value
```

### Function Call Sequence
1. **Caller**: Push parameters, execute `CALL`/`CALLI`
2. **CALL**: Push return address, jump to function
3. **ENTER/ENTERB**: Save BP, allocate locals
4. **Execution**: Access params (negative offsets), locals (positive offsets)
5. **RETB**: Clean up locals, restore BP, return

## Debugging & Tracing

### Trace System
- **Global flag**: `Program.TraceEnabled` accessible throughout runtime
- **Command line**: `-t` flag enables tracing
- **Instruction tracing**: Shows PC, instruction name, and operands
- **Load tracing**: Binary parsing and optimization information

### Debug Features
```csharp
// Instruction formatting with operands
OpCodeInfo.FormatInstruction(opcode, operands)

// VM state inspection
vm.GetState() // PC, BP, stack count, globals count

// Program summary
BinaryLoader.GetProgramSummary(loadedProgram)
```

## Project Structure

### Modular Architecture
```
HopperRuntime/
├── Core/
│   ├── StackValue.cs       // 32-bit stack value system
│   ├── OpCode.cs          // Complete instruction set + metadata
│   ├── BinaryLoader.cs    // File parsing & validation  
│   └── HopperVM.cs        // Main execution engine
├── SystemLibrary/
│   ├── SystemCallDispatcher.cs  // Syscall routing
│   ├── ScreenUnit.cs      // Console I/O
│   └── StringUnit.cs      // String operations
└── Program.cs             // Entry point with tracing
```

### Key Design Principles
- **Single responsibility**: Each file has one clear purpose
- **Separation of concerns**: VM execution separate from file loading
- **Scalability**: Easy to add opcodes, syscalls, and system units
- **Testability**: Components can be tested independently

## Error Handling

### Runtime Exceptions
- **Unknown opcodes**: `InvalidOperationException` with PC location
- **Invalid file format**: Comprehensive validation in BinaryLoader
- **Stack underflow**: Detected during instruction execution
- **Method table misses**: Clear error messages for debugging

### Validation Features
- **File format validation**: Header, method table, and section bounds
- **Instruction validation**: Operand bounds checking
- **Stack safety**: Proper cleanup and bounds checking

## Performance Considerations

### Optimizations Implemented
- **32-bit stack slots**: Efficient on modern architectures
- **Load-time call optimization**: Eliminates runtime method table lookups
- **Direct instruction dispatch**: Switch-based execution
- **.NET GC integration**: No custom memory management overhead
- **Packed instructions**: Higher code density for common operations

### Future Optimizations
- **Instruction fusion**: Combine common instruction patterns
- **JIT compilation**: Hot path optimization
- **Stack slot reuse**: Memory usage optimization

## Implementation Status

### ✅ Completed
- Complete instruction set (175+ opcodes with detailed specifications)
- 32-bit stack value system with type safety
- Binary file loading with validation
- Method table optimization
- Modular system call architecture
- Local variable and parameter access
- Function call/return mechanism
- Debugging and tracing system
- Comprehensive error handling
- Packed instruction support for code density optimization

### 🚧 In Progress
- Full system library implementation (String, Keyboard, File, Time units)
- Advanced debugging features
- Performance monitoring

### 📋 Future Work
- JIT compilation for performance
- Cross-platform system unit implementations
- Debugging protocol for IDE integration
- Profiling and performance analysis tools

## Instruction Set Categories

### Minimal Runtime Instructions (Core Set)
These instructions form the minimal viable implementation:
- **Stack Operations**: `NOP`, `DUP0`, `PUSHR0`, `POPR0`, `SWAP`
- **Arithmetic**: `ADD`, `SUB`, `MUL`, `DIV`, `MOD` (and immediate variants)
- **Comparisons**: `GT`, `LT`, `GE`, `LE`, `EQ`, `NE` (signed/unsigned variants)
- **Logic**: `BOOLOR`, `BOOLAND`, `BOOLNOT`
- **Bitwise**: `BITAND`, `BITOR`, `BITXOR`, `BITNOT`, `BITSHL`, `BITSHR`
- **Control Flow**: `JZ`, `JNZ`, `JW`, `CALL`, `CALLI`, `RET`, `RETRES`
- **Memory Access**: `PUSHI`, `PUSHLOCAL`, `POPLOCAL`, `PUSHGLOBAL`, `POPGLOBAL`
- **Function Management**: `ENTER`, `SYSCALL`, `LIBCALL`
- **Optimized Operations**: `BITSHL8`, `BITSHR8`, `BITANDFF`

### Packed Instructions (Code Density Optimization)
These instructions combine common operations for smaller code size:
- **Immediate Constants**: `PUSHI0`, `PUSHI1`, `PUSHIM1`, `PUSHIB`, `PUSHIBB`
- **Byte Addressing**: `PUSHGLOBALB`, `POPGLOBALB`, `PUSHLOCALB`, `POPLOCALB`
- **Optimized Access**: `PUSHLOCALB00`, `PUSHLOCALB01`, `POPLOCALB00`, `POPLOCALB01`
- **Increment/Decrement**: `INCLOCALB`, `DECLOCALB`, `INCGLOBALB`, `DECGLOBALB`
- **System Calls**: `SYSCALL0`, `SYSCALL1`, `SYSCALL2`
- **Library Calls**: `LIBCALL0`, `LIBCALL1`
- **Control Flow**: `JB`, `JZB`, `JNZB`, `RETB`, `RETRESB`, `ENTERB`
- **Arithmetic**: `ADDB`, `SUBB`
- **Bitwise**: `BITSHLB`, `BITSHRB`, `BITANDB`, `BITORB`

### Special Purpose Instructions
- **Type Conversion**: `CAST`, `PUSHD`
- **Advanced Control**: `JIX`, `CALLREL`
- **Stack Management**: `PUSHSTACKADDR`, `PUSHSTACKADDRB`, `COPYNEXTPOP`
- **Register Operations**: `PUSHGP`
- **Fast Returns**: `RET0`, `RETFAST`

## Implementation Guidelines

### Opcode Dispatch Pattern
```csharp
public void ExecuteInstruction()
{
    byte opcode = program[pc++];
    
    switch ((OpCode)opcode)
    {
        case OpCode.NOP:
            // No operation
            break;
            
        case OpCode.PUSHI:
            {
                ushort value = ReadUInt16();
                Push(new StackValue { UIntValue = value, Type = ValueType.UInt });
            }
            break;
            
        case OpCode.ADD:
            {
                var b = Pop();
                var a = Pop();
                var result = new StackValue 
                { 
                    UIntValue = a.UIntValue + b.UIntValue, 
                    Type = ValueType.UInt 
                };
                Push(result);
            }
            break;
            
        // ... other opcodes
        
        default:
            throw new InvalidOperationException($"Unknown opcode: 0x{opcode:X2} at PC: 0x{pc-1:X4}");
    }
}
```

### Stack Safety Implementation
```csharp
private StackValue Pop()
{
    if (stack.Count == 0)
        throw new InvalidOperationException($"Stack underflow at PC: 0x{pc:X4}");
    return stack.Pop();
}

private void Push(StackValue value)
{
    stack.Push(value);
    if (stack.Count > MaxStackSize)
        throw new InvalidOperationException($"Stack overflow at PC: 0x{pc:X4}");
}
```

### Local Variable Access Implementation
```csharp
private void ExecutePushLocalB()
{
    byte offset = program[pc++];
    sbyte signedOffset = (sbyte)offset;
    
    // Calculate stack position relative to base pointer
    int stackPosition = (int)bp + signedOffset;
    
    // Validate bounds
    if (stackPosition < 0 || stackPosition >= stack.Count)
        throw new InvalidOperationException($"Invalid local access at offset {signedOffset}");
        
    // Convert to array index and push value
    var locals = stack.ToArray();
    int arrayIndex = locals.Length - 1 - stackPosition;
    Push(locals[arrayIndex]);
}
```

### Global Variable Access Implementation
```csharp
private void ExecutePushGlobalB()
{
    byte offset = program[pc++];
    uint globalAddress = (uint)offset;
    
    if (!globals.TryGetValue(globalAddress, out StackValue value))
    {
        // Initialize to default value if not exists
        value = new StackValue { UIntValue = 0, Type = ValueType.UInt };
        globals[globalAddress] = value;
    }
    
    Push(value);
}
```

### System Call Integration
```csharp
private void ExecuteSysCall()
{
    ushort syscallId = ReadUInt16();
    
    try
    {
        systemCallDispatcher.Execute(syscallId, this);
    }
    catch (Exception ex)
    {
        throw new InvalidOperationException(
            $"System call 0x{syscallId:X2} failed at PC: 0x{pc-3:X4}", ex);
    }
}
```

## Architecture Decisions

### Why 32-bit Stack Slots?
- **Modern architecture optimization**: Efficient on 64-bit systems
- **Simplified implementation**: No variable-width slot management
- **Value type efficiency**: `long` and `float` as direct values
- **Alignment benefits**: Natural memory alignment

### Why .NET GC Integration?
- **Mature memory management**: Leverages decades of optimization
- **Reference type safety**: Automatic lifetime management
- **Development speed**: No custom GC implementation needed
- **Debugging support**: GC-aware debugging tools

### Why Load-time Call Optimization?
- **Runtime performance**: Eliminates method table lookups
- **Future compatibility**: Easy to disable for larger address spaces
- **Clean separation**: Optimization logic isolated in loader

### Why Packed Instructions?
- **Code density**: Smaller program size for embedded systems
- **Performance**: Fewer instruction fetches for common operations
- **Backward compatibility**: Minimal runtime can ignore packed instructions
- **Incremental adoption**: Can add packed variants without breaking existing code

## Testing Strategy

### Unit Test Coverage
- **Individual opcodes**: Test each instruction in isolation
- **Stack operations**: Verify proper push/pop behavior
- **Control flow**: Test all jump and call scenarios
- **Error conditions**: Stack underflow, invalid opcodes, bounds checking
- **Type safety**: Ensure proper type handling across operations

### Integration Testing
- **Function calls**: Complete call/return sequences
- **Local variables**: Parameter passing and local access
- **System calls**: Integration with system library
- **Binary loading**: File format validation and loading

### Performance Testing
- **Instruction throughput**: Measure instructions per second
- **Memory usage**: Stack and heap utilization
- **Call overhead**: Function call performance
- **Packed vs standard**: Code density and performance comparison

This specification serves as both implementation guide and maintenance reference for the Hopper VM runtime system, providing complete coverage of the instruction set with detailed implementation guidance.