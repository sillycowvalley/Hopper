# Hopper Runtime Implementation Specification

## Overview

This document describes the implementation of a Hopper runtime in C#. Hopper is a programming language designed for microcontrollers and 8-bit microprocessors, featuring a stack-based virtual machine with garbage collection.

## Language Characteristics

### Core Design Principles
- **Single-threaded execution** on a stack-based virtual machine
- **Garbage collection** for objects not reachable from the stack
- **Zero initialization** of all variables
- **Type safety** with no implicit casting between different numeric types
- **No `void` or `null` keywords**

### Key Language Features
1. **Entry Point**: Programs use `Hopper()` instead of `main()`
2. **Method Declaration**: Methods with no return type omit the return type specification
3. **List Operations**: Use methods (`Append`, `GetItem`, `SetItem`) instead of bracket indexing
4. **Switch Statements**: Never fall through, no `break` required
5. **Loop Keyword**: `loop` for infinite loops (shorthand for `while(true)`)
6. **Array Definitions**: Format is `type[size] name`, not `type name[size]`
7. **System/Library Methods**: 
   - `system` keyword: implemented by runtime
   - `library` keyword: implemented only in MCU/6502 runtimes

### Type System
- `char` and `byte` are distinct types
- `uint` and `byte` are distinct types
- Explicit casting required between different types
- Zero initialization for all variables:
  - `string myString;` → `""`
  - `uint a;` → `0`
  - `float bob;` → `0.0`
  - `bool ok;` → `false`

## Runtime Architecture

### Stack-Based Virtual Machine
- **32-bit stack slots** for all values (optimized for 32/64-bit architectures)
- **Value types**: `byte`, `char`, `uint`, `int`, `long`, `float`, `bool` stored directly on stack
- **Reference types**: `string`, lists, dictionaries, arrays stored on managed heap (.NET GC)

### Stack Value Structure
```csharp
public struct StackValue
{
    public uint UIntValue;     // Also handles byte, char, bool
    public int IntValue; 
    public long LongValue;     // Full 64-bit value type
    public float FloatValue;   // Full 32-bit value type
    public object RefValue;    // Reference types (GC managed)
    public ValueType Type;     // Type discriminator
}
```

### Memory Management
- **Primitives**: Stack-allocated value types
- **References**: .NET garbage collector handles all reference types
- **No manual memory management** required

## Binary File Format

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

### Method Table
- Located between header and constants
- Each entry: 4 bytes (2-byte index + 2-byte address)
- Addresses are relative to entry point
- Used for `CALL` instruction indirection

### Constant Data
- Starts at constant offset
- Null-terminated strings
- Accessible via syscalls (e.g., `String.NewFromConstant`)

## Instruction Set

### Core Instructions Implemented
| Opcode | Name | Description |
|--------|------|-------------|
| 0x44   | PUSHI0 | Push 0 onto stack |
| 0x37   | PUSHI | Push 16-bit immediate value |
| 0x1F   | POPGLOBALB | Pop to global variable (byte index) |
| 0x20   | PUSHGLOBALB | Push global variable (byte index) |
| 0x5F   | ENTERB | Function entry with local count |
| 0x49   | ENTER | Function entry without locals |
| 0x1C   | PUSHLOCALB | Push local variable (byte offset) |
| 0x34   | CALL | Call method via method table |
| 0x6A   | CALLI | Call immediate address (optimized) |
| 0x2A   | RETB | Return with stack cleanup |
| 0xA8   | SYSCALLB0 | System call with 1 parameter |
| 0xAD   | SYSCALL10 | Two consecutive system calls |

### Local Variable Addressing
- `PUSHLOCALB 0xFF`: Special case for BP-1 (function parameter)
- Other offsets: Regular local variables relative to base pointer

### Call Optimization
- **Load-time optimization**: `CALL` instructions converted to `CALLI`
- Method table lookup performed once during loading
- Runtime executes direct address calls for efficiency
- **Future consideration**: Disable optimization when 4-byte addresses needed

## System Call Interface

### Implemented System Calls
| ID | Name | Description |
|----|------|-------------|
| 0x05 | String.NewFromConstant | Create string from constant data |
| 0x29 | Screen.Print | Print text with colors |
| 0x2A | Screen.PrintLn | Print newline |

### System Call Pattern
```csharp
private void HandleSyscall(byte syscallId, byte param)
{
    switch (syscallId)
    {
        case 0x05: // String.NewFromConstant
            string str = ReadConstantString(param);
            stack.Push(StackValue.FromRef(str));
            break;
        // ... other syscalls
    }
}
```

## Function Call Mechanism

### Call Stack Management
```csharp
private readonly Stack<uint> callStack = new Stack<uint>();
private readonly Stack<uint> basePointers = new Stack<uint>();
private uint pc;  // Program counter
private uint bp;  // Base pointer
```

### Call Sequence
1. `CALL/CALLI`: Push current PC, jump to target
2. `ENTER/ENTERB`: Save BP, set new BP, allocate locals
3. Function execution with local/parameter access
4. `RETB`: Clean up locals, restore BP, return to caller

### Parameter Passing
- Parameters passed on stack
- `PUSHLOCALB 0xFF` accesses first parameter (BP-1)
- Multiple parameters accessible via stack manipulation

## Error Handling

### Runtime Exceptions
- Unknown opcodes throw `InvalidOperationException`
- Method table misses throw `InvalidOperationException`
- Stack underflow/overflow should be detected (TODO)

### Debugging Support
- PC tracking for error reporting
- Opcode logging capability
- Stack state inspection

## System Library Units

### Screen Unit
```csharp
public class ScreenUnit
{
    public uint ForeColour { get; set; }
    public uint BackColour { get; set; }
    public void Print(string text, uint foreColor, uint backColor)
    public void PrintLn()
}
```

### String Unit (Placeholder)
- String operations to be implemented
- Integration with .NET string type

## Usage

### Command Line
```
HopperRuntime <program.hex>
```

### Visual Studio Debugging
- Set command line arguments in Project Properties → Debug
- Or modify `launchSettings.json`

## Implementation Status

### Completed
- ✅ Basic VM structure with 32-bit stack slots
- ✅ Binary file loading with header parsing
- ✅ Method table loading and call optimization
- ✅ Core instruction set implementation
- ✅ Function call/return mechanism
- ✅ Basic system call interface
- ✅ Screen unit for console output
- ✅ String constant loading

### TODO
- ⏳ Complete instruction set implementation
- ⏳ Full system library units (String, List, Dictionary, etc.)
- ⏳ Keyboard input handling
- ⏳ File I/O operations
- ⏳ Error handling and bounds checking
- ⏳ Debugging and introspection tools
- ⏳ Performance optimization
- ⏳ Memory usage monitoring

## Architecture Decisions

### Why 32-bit Stack Slots?
- Simplifies implementation on modern architectures
- Makes `long` and `float` efficient value types
- Reduces complexity compared to variable-width slots

### Why Load-time Call Optimization?
- Better runtime performance (no table lookups)
- Future-proof when 4-byte addresses needed
- Clean separation of concerns

### Why .NET GC Integration?
- Eliminates need for custom garbage collector
- Leverages mature, optimized memory management
- Simplifies reference type handling

## Future Considerations

### 4-Byte Address Support
- Method table entries would become 6 bytes (2-byte index + 4-byte address)
- Call optimization would be disabled
- `CALLI` would need 4-byte immediate addressing mode

### Performance Optimizations
- JIT compilation for hot code paths
- Instruction fusion for common patterns
- Stack slot reuse analysis

### Platform Integration
- Native library bindings for hardware access
- Platform-specific system call implementations
- Cross-compilation support for embedded targets