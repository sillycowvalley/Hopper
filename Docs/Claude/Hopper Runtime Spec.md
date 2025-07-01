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

## Instruction Set Implementation

### Complete Opcode Coverage
The VM implements the full Hopper instruction set with 175+ opcodes including:

#### Stack Operations
- `PUSHI0`, `PUSHI1`, `PUSHIM1`: Push immediate constants
- `PUSHI`, `PUSHIB`: Push immediate values
- `DUP0`, `DUP`: Duplicate stack items
- `SWAP`: Swap top two stack items

#### Memory Operations
- `PUSHGLOBALB`, `POPGLOBALB`: Global variable access
- `PUSHLOCALB`, `POPLOCALB`: Local variable access (signed byte offset)
- `PUSHRELB`, `POPRELB`: Relative addressing
- `PUSHSTACKADDRB`: Stack address calculation

#### Control Flow
- `CALL`, `CALLI`: Function calls (with method table optimization)
- `RETB`, `RETRESB`: Returns with stack cleanup
- `JB`, `JZB`, `JNZB`: Conditional jumps (byte offset)
- `J`, `JZ`, `JNZ`: Conditional jumps (word offset)

#### Arithmetic & Logic
- `ADD`/`ADDI`, `SUB`/`SUBI`: Unsigned/signed arithmetic
- `MUL`/`MULI`, `DIV`/`DIVI`, `MOD`/`MODI`: Advanced arithmetic
- `GT`/`GTI`, `LT`/`LTI`, `GE`/`GEI`, `LE`/`LEI`: Comparisons
- `EQ`, `NE`: Equality testing
- `BITAND`, `BITOR`, `BITXOR`, `BITSHL`, `BITSHR`: Bitwise operations

#### Function Entry/Exit
- `ENTER`, `ENTERB`: Function entry with local allocation
- `RET0`, `RETFAST`: Optimized returns

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

### Future Optimizations
- **Instruction fusion**: Combine common instruction patterns
- **JIT compilation**: Hot path optimization
- **Stack slot reuse**: Memory usage optimization

## Implementation Status

### ✅ Completed
- Complete instruction set (175+ opcodes)
- 32-bit stack value system with type safety
- Binary file loading with validation
- Method table optimization
- Modular system call architecture
- Local variable and parameter access
- Function call/return mechanism
- Debugging and tracing system
- Comprehensive error handling

### 🚧 In Progress
- Full system library implementation (String, Keyboard, File, Time units)
- Advanced debugging features
- Performance monitoring

### 📋 Future Work
- JIT compilation for performance
- Cross-platform system unit implementations
- Debugging protocol for IDE integration
- Profiling and performance analysis tools

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

This specification serves as both implementation guide and maintenance reference for the Hopper VM runtime system.