# HopperBASIC Zero Page Allocation (Current)
**Document Type: Zero Page Map**

## System Zero Page Usage (0x00-0x0F)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x00 | PCL | Hopper VM | Program counter low byte |
| 0x01 | PCH | Hopper VM | Program counter high byte |
| 0x02 | FLAGS | Hopper VM | System flags (platform, debugger, etc.) |
| 0x03 | SP | Hopper VM | Stack pointer |
| 0x04 | BP | Hopper VM | Base pointer |
| 0x05 | CSP | Hopper VM | Call stack pointer |
| 0x06 | FREELISTL | Memory | Heap free list pointer low byte |
| 0x07 | FREELISTH | Memory | Heap free list pointer high byte |
| 0x08 | HEAPSTART | Memory | Heap start page number |
| 0x09 | HEAPSIZE | Memory | Heap size in pages |
| 0x0A | SerialInWritePointer | Serial | Serial input buffer write pointer |
| 0x0B | SerialInReadPointer | Serial | Serial input buffer read pointer |
| 0x0C | SerialBreakFlag | Serial | Serial break/interrupt flag |
| 0x0D | CODESTARTL | Hopper VM | Code start address low byte |
| 0x0E | CODESTARTH | Hopper VM | Code start address high byte |
| 0x0F | TraceIndent | BASIC | Trace indentation level (HOPPER_BASIC only) |

## Working Registers (0x10-0x1F)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x10 | ACCL | Hopper VM | Accumulator low byte |
| 0x11 | ACCH | Hopper VM | Accumulator high byte |
| 0x12 | TOPL | Hopper VM | Top stack value low byte |
| 0x13 | TOPH | Hopper VM | Top stack value high byte |
| 0x14 | NEXTL | Hopper VM | Next stack value low byte |
| 0x15 | NEXTH | Hopper VM | Next stack value high byte |
| 0x16 | IDXL | Hopper VM | Index register low byte |
| 0x17 | IDXH | Hopper VM | Index register high byte |
| 0x18 | IDYL | Hopper VM | Index Y register low byte |
| 0x19 | IDYH | Hopper VM | Index Y register high byte |
| 0x1A | ACCT | Hopper VM | Accumulator type |
| 0x1B | TOPT | Hopper VM | Top stack value type |
| 0x1C | NEXTT | Hopper VM | Next stack value type |
| 0x1D | PROGSIZE | Hopper VM | Program size (Hopper specific) |
| 0x1E | I2CInWritePtr | I2C | I2C input buffer write pointer |
| 0x1F | I2CInReadPtr | I2C | I2C input buffer read pointer |

## Workspace Registers (0x20-0x2F)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x20 | W0 | Workspace | General workspace (Serial, I2C) |
| 0x21 | W1 | Workspace | General workspace |
| 0x22 | W2 | Workspace | Packed syscalls |
| 0x23 | W3/I2CADDR | Workspace/I2C | I2C device address |
| 0x24 | W4/OutB | Workspace/I2C | I2C output byte |
| 0x25 | W5/InB | Workspace/I2C | I2C input byte |
| 0x26 | W6/LastAck | Workspace/I2C | I2C last ACK status |
| 0x27 | W7/PLUGNPLAY | Workspace | Device detection flags |
| 0x28 | TICK0 | Time | Timer tick counter byte 0 |
| 0x29 | TICK1 | Time | Timer tick counter byte 1 |
| 0x2A | TICK2 | Time | Timer tick counter byte 2 |
| 0x2B | TICK3 | Time | Timer tick counter byte 3 |
| 0x2C | TARGET0 | Time | Timer target byte 0 |
| 0x2D | TARGET1 | Time | Timer target byte 1 |
| 0x2E | TARGET2 | Time | Timer target byte 2 |
| 0x2F | TARGET3 | Time | Timer target byte 3 |

## HopperBASIC Allocation (0x30-0x4F)

| Address | Name | Usage | Purpose | Status |
|---------|------|-------|---------|---------|
| 0x30 | BasicInputLength | Console | Length of current input in buffer | **ALLOCATED** |
| 0x31 | TokenBufferLengthL | Tokenizer | Token buffer length low byte | **ALLOCATED** |
| 0x32 | TokenBufferLengthH | Tokenizer | Token buffer length high byte | **ALLOCATED** |
| 0x33 | TokenizerPosL | Tokenizer | Current position in token buffer low | **ALLOCATED** |
| 0x34 | TokenizerPosH | Tokenizer | Current position in token buffer high | **ALLOCATED** |
| 0x35 | LastErrorL | Messages | Error message pointer low byte | **ALLOCATED** |
| 0x36 | LastErrorH | Messages | Error message pointer high byte | **ALLOCATED** |
| 0x37 | CurrentToken | Tokenizer | Current token type/value | **ALLOCATED** |
| 0x38 | TokenLiteralPosL | Tokenizer | Literal data position low byte | **ALLOCATED** |
| 0x39 | TokenLiteralPosH | Tokenizer | Literal data position high byte | **ALLOCATED** |
| 0x3A | OpCodeBufferLengthL | Compiler | JIT opcode buffer length low byte | **ALLOCATED** |
| 0x3B | OpCodeBufferLengthH | Compiler | JIT opcode buffer length high byte | **ALLOCATED** |
| 0x3C | CompilerTokenPosL | Compiler | Token position during compilation low | **ALLOCATED** |
| 0x3D | CompilerTokenPosH | Compiler | Token position during compilation high | **ALLOCATED** |
| 0x3E | CompilerFlags | Compiler | Compilation flags (bit 0: in function, etc.) | **ALLOCATED** |
| 0x3F | OpCodeTemp | Compiler | Temporary byte for opcode construction | **ALLOCATED** |
| 0x40 | - | Available | - | Unallocated |
| 0x41 | - | Available | - | Unallocated |
| 0x42 | - | Available | - | Unallocated |
| 0x43 | - | Available | - | Unallocated |
| 0x44 | - | Available | - | Unallocated |
| 0x45 | - | Available | - | Unallocated |
| 0x46 | - | Available | - | Unallocated |
| 0x47 | - | Available | - | Unallocated |
| 0x48 | - | Available | - | Unallocated |
| 0x49 | - | Available | - | Unallocated |
| 0x4A | SystemState | State | Universal system state (Success/Failure/Exiting) | **ALLOCATED** |
| 0x4B | - | Available | - | Unallocated |
| 0x4C | - | Available | - | Unallocated |
| 0x4D | - | Available | - | Unallocated |
| 0x4E | - | Available | - | Unallocated |
| 0x4F | - | Available | - | Unallocated |

## Memory Manager Workspace (0x50-0x5F)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x50 | M0 | Memory | Memory management workspace |
| 0x51 | M1 | Memory | Memory management workspace |
| 0x52 | M2 | Memory | Memory management workspace |
| 0x53 | M3 | Memory | Memory management workspace |
| 0x54 | M4 | Memory | Memory management workspace |
| 0x55 | M5 | Memory | Memory management workspace |
| 0x56 | M6 | Memory | Memory management workspace |
| 0x57 | M7 | Memory | Memory management workspace |
| 0x58 | M8 | Memory | Memory management workspace |
| 0x59 | M9 | Memory | Memory management workspace |
| 0x5A | M10 | Memory | Memory management workspace |
| 0x5B | M11 | Memory | Memory management workspace |
| 0x5C | M12 | Memory | Memory management workspace |
| 0x5D | M13 | Memory | Memory management workspace |
| 0x5E | M14 | Memory | Memory management workspace |
| 0x5F | M15 | Memory | Memory management workspace |

## Function Workspace (0x60-0x6F)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x60 | F0 | Functions | General syscall functions |
| 0x61 | F1 | Functions | General syscall functions |
| 0x62 | F2 | Functions | General syscall functions |
| 0x63 | F3 | Functions | General syscall functions |
| 0x64 | F4 | Functions | General syscall functions |
| 0x65 | F5 | Functions | General syscall functions |
| 0x66 | F6 | Functions | General syscall functions |
| 0x67 | F7 | Functions | General syscall functions |
| 0x68 | F8 | Functions | General syscall functions |
| 0x69 | F9 | Functions | General syscall functions |
| 0x6A | F10 | Functions | General syscall functions |
| 0x6B | F11 | Functions | General syscall functions |
| 0x6C | F12 | Functions | General syscall functions |
| 0x6D | F13 | Functions | General syscall functions |
| 0x6E | F14 | Functions | General syscall functions |
| 0x6F | F15 | Functions | General syscall functions |

## HopperBASIC Symbol Table (0x70-0x7F)

| Address | Name | Usage | Purpose | Status |
|---------|------|-------|---------|---------|
| 0x70 | VariablesListL | Symbol Table | Variables/constants table head pointer low | **ALLOCATED** |
| 0x71 | VariablesListH | Symbol Table | Variables/constants table head pointer high | **ALLOCATED** |
| 0x72 | FunctionsListL | Symbol Table | Functions/arguments table head pointer low | **ALLOCATED** |
| 0x73 | FunctionsListH | Symbol Table | Functions/arguments table head pointer high | **ALLOCATED** |
| 0x74 | SymbolType | Symbol Table | Storage for symbolType\|dataType | **ALLOCATED** |
| 0x75 | SymbolValueL | Symbol Table | Storage for symbol value low byte | **ALLOCATED** |
| 0x76 | SymbolValueH | Symbol Table | Storage for symbol value high byte | **ALLOCATED** |
| 0x77 | SymbolNameL | Symbol Table | Storage for symbol name pointer low byte | **ALLOCATED** |
| 0x78 | SymbolNameH | Symbol Table | Storage for symbol name pointer high byte | **ALLOCATED** |
| 0x79 | SymbolTokensL | Symbol Table | Storage for symbol tokens pointer low byte | **ALLOCATED** |
| 0x7A | SymbolTokensH | Symbol Table | Storage for symbol tokens pointer high byte | **ALLOCATED** |
| 0x7B | SymbolIteratorFilter | Symbol Table | Type filter for symbol iteration | **ALLOCATED** |
| 0x7C | SymbolLength | Symbol Table | Storage for symbol name length | **ALLOCATED** |
| 0x7D | SymbolTemp0 | Symbol Table | General temporary storage | **ALLOCATED** |
| 0x7E | SymbolTemp1 | Symbol Table | General temporary storage | **ALLOCATED** |
| 0x7F | SymbolTemp2 | Symbol Table | General temporary storage | **ALLOCATED** |

## UInt Operations (0x80-0x87)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0x80 | U0 | UInt | UInt operations workspace |
| 0x81 | U1 | UInt | UInt operations workspace |
| 0x82 | U2 | UInt | UInt operations workspace |
| 0x83 | U3 | UInt | UInt operations workspace |
| 0x84 | U4 | UInt | UInt operations workspace |
| 0x85 | U5 | UInt | UInt operations workspace |
| 0x86 | U6 | UInt | UInt operations workspace |
| 0x87 | U7 | UInt | UInt operations workspace |

## Hardware I/O (Platform-Dependent)

### ZEROPAGE_IO Platform (Default)

| Address | Name | Usage | Purpose |
|---------|------|-------|---------|
| 0xEC | ACIACONTROL/STATUS | Serial | ACIA control/status register |
| 0xED | ACIADATA | Serial | ACIA data register |
| 0xF0 | EmulatorPCL | BASIC | Emulator PC capture low (BIT to capture) | **ALLOCATED** |
| 0xF1 | EmulatorPCH | BASIC | Emulator PC capture high | **ALLOCATED** |
| 0xF2 | TraceMessageL | BASIC | Trace message pointer low byte | **ALLOCATED** |
| 0xF3 | TraceMessageH | BASIC | Trace message pointer high byte | **ALLOCATED** |
| 0xF4 | SystemState | BASIC | Universal system state (Success/Failure/Exiting) | **ALLOCATED** |
| 0xF5 | - | VIA | Available (was T1CH) |
| 0xF6 | - | VIA | Available (was T1LL) |
| 0xF7 | - | VIA | Available (was T1LH) |
| 0xF8 | - | VIA | Available (was T2CL) |
| 0xF9 | - | VIA | Available (was T2CH) |
| 0xFA | - | VIA | Available (was SR) |
| 0xFB | - | VIA | Available (was ACR) |
| 0xFC | - | VIA | Available (was PCR) |
| 0xFD | - | VIA | Available (was IFR) |
| 0xFE | - | VIA | Available (was IER) |
| 0xFF | - | VIA | Available (was ORA_NO_HANDSHAKE) |

**Current Allocations Summary**

**HopperBASIC Core Allocation (0x30-0x3F):**
- **0x30**: BasicInputLength (Console input management)
- **0x31-0x32**: TokenBufferLength (16-bit tokenizer buffer length)
- **0x33-0x34**: TokenizerPos (16-bit current position in token buffer)
- **0x35-0x36**: LastError (16-bit error message pointer)
- **0x37**: CurrentToken (Current token cache)
- **0x38-0x39**: TokenLiteralPos (16-bit literal data position)
- **0x3A-0x3B**: OpCodeBufferLength (16-bit JIT opcode buffer length)
- **0x3C-0x3D**: CompilerTokenPos (16-bit token position during compilation)
- **0x3E**: CompilerFlags (Compilation state flags)
- **0x3F**: OpCodeTemp (Temporary opcode construction byte)

**HopperBASIC State Management:**
- **0x4A**: SystemState (Universal success/failure/exit state)
- **0x0F**: TraceIndent (Trace system indentation level)
- **0xF0-0xF1**: EmulatorPCL/H (PC capture for error reporting)
- **0xF2-0xF3**: TraceMessageL/H (Trace message pointer)
- **0xF4**: SystemState (Alternative location - verify with code)

**HopperBASIC Symbol Tables (0x70-0x7F):**
- **0x70-0x71**: VariablesList (16-bit variables/constants table head)
- **0x72-0x73**: FunctionsList (16-bit functions/arguments table head)
- **0x74**: SymbolType (Symbol type and data type packed byte)
- **0x75-0x76**: SymbolValue (16-bit symbol value working storage)
- **0x77-0x78**: SymbolName (16-bit symbol name pointer working storage)
- **0x79-0x7A**: SymbolTokens (16-bit symbol tokens pointer working storage)
- **0x7B**: SymbolIteratorFilter (Type filter for symbol iteration)
- **0x7C**: SymbolLength (Symbol name length including null terminator)
- **0x7D**: SymbolTemp0 (General temporary storage)
- **0x7E**: SymbolTemp1 (General temporary storage)
- **0x7F**: SymbolTemp2 (General temporary storage)

**Available for Future HopperBASIC Features:**
- **0x40-0x49**: 10 bytes in extended range
- **0x4B-0x4F**: 5 bytes after SystemState

**Total HopperBASIC allocation**: 32 bytes allocated, 15 bytes available

## SystemState Management

### SystemState Enum
```hopper
enum SystemState 
{
    Failure = 0,        // Zero for easy testing (CMP -> Z flag)
    Success = 1,        // Normal completion
    Exiting = 2         // User exit request (BYE, Ctrl+C)
}
```

### Symbol Node Structure
**Enhanced Objects Node Layout:**
- **Offset 0-1**: next pointer (managed by Table unit)
- **Offset 2**: function flags byte (or symbolType|dataType for variables)
- **Offset 3-4**: tokens pointer (initialization/body token stream)
- **Offset 5-6**: value/args (value for variables, arguments list head for functions)
- **Offset 7-8**: opcode stream pointer (JIT compiled opcodes for functions)
- **Offset 9+**: null-terminated name string

### JIT Compilation Enhancement
- **Function Flags**: Track compilation state (FunctionFlags.Compiled)
- **Dual-purpose tokens/opcodes**: Functions can store both tokens and compiled opcodes
- **Dynamic opcode storage**: Compiled opcodes allocated in heap for permanent storage
- **Compilation invalidation**: OpCodes cleared when environment changes

## Allocation Guidelines

### Primary Range (0x30-0x3F) - FULLY ALLOCATED
- **Console Operations**: Input length tracking
- **Tokenizer State**: Buffer management and position tracking
- **Error Handling**: Message pointer storage
- **JIT Compiler**: Opcode buffer management and compilation state
- **Token Processing**: Current token cache and literal position tracking

### Available Range (0x40-0x4F) - 15 bytes available
- **Future BASIC features**: Function local variables, complex expressions
- **Enhanced debugging**: Additional debug state tracking
- **Performance optimization**: Caching frequently-used values

### Symbol Table Range (0x70-0x7F) - FULLY ALLOCATED
- **Symbol table management**: Complete symbol table operations
- **Persistent storage**: Survives Memory.Allocate() calls
- **Working variables**: Symbol processing temporary storage
- **Iterator state**: Symbol table traversal management

### Trace System Integration
- **TraceIndent** (0x0F): Manages trace output indentation level
- **TraceMessageL/H** (0xF2-0xF3): Pointer to trace message strings
- **EmulatorPCL/H** (0xF0-0xF1): PC capture for error context

### Allocation Process
1. **Document first**: Update this table before implementing
2. **Group logically**: Related variables should be adjacent
3. **Consider persistence**: Variables that survive memory allocation
4. **Coordinate with VM**: Ensure no conflicts with Hopper VM usage
5. **Platform awareness**: Consider platform-specific I/O variations

## Notes
- Zero page variables shown as **ALLOCATED** are actively used in current code
- Symbol table area (0x70-0x7F) is now fully utilized for comprehensive symbol management
- SystemState provides unified success/failure/exit tracking across all subsystems
- JIT compilation system uses both temporary (0x3A-0x3F) and permanent storage
- Trace system integration enables comprehensive debugging capabilities
- VIA hardware locations (0xF5-0xFF) repurposed for BASIC-specific functionality
- Memory manager workspace (0x50-0x5F) remains dedicated to heap operations
- Function workspace (0x60-0x6F) used for general syscall parameter passing