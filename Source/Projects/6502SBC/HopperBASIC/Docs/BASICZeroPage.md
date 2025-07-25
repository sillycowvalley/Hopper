# HopperBASIC Zero Page Allocation

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
| 0x0F | CNP | Hopper VM | Copy Next Pop (Hopper specific) |

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

## HopperBASIC Allocation (0x30-0x3F)

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
| 0x3A | - | Available | - | Unallocated |
| 0x3B | - | Available | - | Unallocated |
| 0x3C | - | Available | - | Unallocated |
| 0x3D | - | Available | - | Unallocated |
| 0x3E | - | Available | - | Unallocated |
| 0x3F | - | Available | - | Unallocated |

## Available Range (0x40-0x4F)

| Address | Name | Usage | Purpose | Status |
|---------|------|-------|---------|---------|
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
| 0x4A | - | Available | - | Unallocated |
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

## HopperBASIC Symbol Table & Storage (0x70-0x7F)

| Address | Name | Usage | Purpose | Status |
|---------|------|-------|---------|---------|
| 0x70 | SymbolListL | Symbol Table | Symbol table head pointer low byte | **ALLOCATED** |
| 0x71 | SymbolListH | Symbol Table | Symbol table head pointer high byte | **ALLOCATED** |
| 0x72 | SymbolType | Symbol Table | Storage for symbolType\|dataType | **ALLOCATED** |
| 0x73 | SymbolValueL | Symbol Table | Storage for symbol value low byte | **ALLOCATED** |
| 0x74 | SymbolValueH | Symbol Table | Storage for symbol value high byte | **ALLOCATED** |
| 0x75 | SymbolNameL | Symbol Table | Storage for symbol name pointer low byte | **ALLOCATED** |
| 0x76 | SymbolNameH | Symbol Table | Storage for symbol name pointer high byte | **ALLOCATED** |
| 0x77 | - | Available | - | Unallocated |
| 0x78 | - | Available | - | Unallocated |
| 0x79 | - | Available | - | Unallocated |
| 0x7A | - | Available | - | Unallocated |
| 0x7B | - | Available | - | Unallocated |
| 0x7C | - | Available | - | Unallocated |
| 0x7D | - | Available | - | Unallocated |
| 0x7E | - | Available | - | Unallocated |
| 0x7F | - | Available | - | Unallocated |

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
| 0xF0 | PORTB | VIA | VIA Port B |
| 0xF1 | PORTA | VIA | VIA Port A |
| 0xF2 | DDRB | VIA | VIA Data Direction B |
| 0xF3 | DDRA | VIA | VIA Data Direction A |
| 0xF4 | T1CL | VIA | Timer 1 Counter Low |
| 0xF5 | T1CH | VIA | Timer 1 Counter High |
| 0xF6 | T1LL | VIA | Timer 1 Latch Low |
| 0xF7 | T1LH | VIA | Timer 1 Latch High |
| 0xF8 | T2CL | VIA | Timer 2 Counter Low |
| 0xF9 | T2CH | VIA | Timer 2 Counter High |
| 0xFA | SR | VIA | Shift Register |
| 0xFB | ACR | VIA | Auxiliary Control Register |
| 0xFC | PCR | VIA | Peripheral Control Register |
| 0xFD | IFR | VIA | Interrupt Flag Register |
| 0xFE | IER | VIA | Interrupt Enable Register |
| 0xFF | ORA_NO_HANDSHAKE | VIA | Output Register A (no handshake) |

**Current Allocations Summary**

**HopperBASIC Specific Usage:**
- **0x30**: BasicInputLength (Console module)
- **0x31**: TokenBufferLengthL (Tokenizer buffer length low)
- **0x32**: TokenBufferLengthH (Tokenizer buffer length high) 
- **0x33**: TokenizerPosL (Tokenizer position low)
- **0x34**: TokenizerPosH (Tokenizer position high)
- **0x35**: LastErrorL (Messages error handling)
- **0x36**: LastErrorH (Messages error handling)
- **0x37**: CurrentToken (Current token cache)
- **0x38**: TokenLiteralPosL (Literal data position low)
- **0x39**: TokenLiteralPosH (Literal data position high)
- **0x70**: SymbolListL (Symbol table head pointer low)
- **0x71**: SymbolListH (Symbol table head pointer high)
- **0x72**: SymbolType (Symbol type/data type storage)
- **0x73**: SymbolValueL (Symbol value storage low)
- **0x74**: SymbolValueH (Symbol value storage high)
- **0x75**: SymbolNameL (Symbol name pointer low)
- **0x76**: SymbolNameH (Symbol name pointer high)

**Available for Future HopperBASIC Features:**
- **0x3A-0x3F**: 6 bytes in primary range
- **0x40-0x4F**: 16 bytes in extended range
- **0x77-0x7F**: 9 bytes in symbol table range

**Total HopperBASIC allocation**: 17 bytes allocated, 31 bytes available

## Allocation Guidelines

### Primary Range (0x30-0x3F)
- **Core functionality first**: Tokenizer, parser, interpreter state
- **Group related functions**: Keep related variables together for better locality
- **16-bit values**: Use consecutive addresses (e.g., 0x38/0x39 for 16-bit value)

### Available Range (0x40-0x4F)
- **General expansion**: 16 contiguous bytes for future BASIC features
- **Large structures**: Good for multi-byte temporary storage or buffers
- **Function parameters**: Suitable for complex function call parameter passing

### Symbol Table Range (0x70-0x7F)
- **Symbol table management**: Primary symbol table operations and temporary storage
- **Persistent across Memory.Allocate()**: These locations survive memory allocation calls
- **Table unit workspace safe**: Separate from ZP.Lxx variables used by Table unit

### Allocation Process
1. **Document first**: Update this table before implementing
2. **Group logically**: Related variables should be adjacent
3. **Consider scope**: Temporary vs. persistent variables
4. **Review regularly**: Ensure efficient use of limited space

## Notes
- Zero page variables shown as **ALLOCATED** are actively used in the current code
- Variables listed in ZeroPage.asm but not yet used in implementation are noted
- Always check this document before allocating new variables
- Update the Status column when variables are allocated/freed
- HopperBASIC is constrained to avoid conflicts with Hopper VM's extensive zero page usage
- The 0x70-0x7F range provides dedicated space for symbol table operations that persist across memory allocation calls