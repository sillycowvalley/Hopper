# HopperBASIC Zero Page Reference
**Document Type: Zero Page Map**

## Overview
The 6502's zero page (0x00-0xFF) provides fast 8-bit addressing. HopperBASIC allocates this precious resource across VM core, BASIC runtime, and hardware I/O.

## Critical Immovable Addresses

### Core VM Registers (0x00-0x05) - Architecture Dependent
- **0x00-0x01**: PC/PCL/PCH - Program counter (core to instruction execution)
- **0x02**: FLAGS - System flags register (bit positions are API contracts)
- **0x03-0x05**: SP/BP/CSP - Stack pointers (assumed by all stack operations)

### Emulator Interface (0x1D-0x22) - Hardcoded in C# Emulator
- **0x1D-0x20**: TICK0-3 - Timer tick counters
  - Reading TICK0 triggers snapshot of all 4 bytes
  - Reading TICK3 triggers snapshot of all 4 bytes
- **0x21-0x22**: EmulatorPCL/H - PC capture for debugging
  - BIT $21 instruction triggers PC capture
  - Used by Debug.asm for crash dumps and breakpoints

### Hardware I/O (0xEC-0xFF) - Platform Hardware Addresses
- **0xEC-0xED**: ACIA registers - Serial communication
- **0xF0-0xFF**: VIA registers - Parallel I/O, timers, interrupts
- These are physical hardware addresses, not arbitrary choices

## Memory Map

### Core VM Variables (0x00-0x1C)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x00-0x01 | PC/PCL/PCH | Program counter (IMMOVABLE) |
| 0x02 | FLAGS | System flags byte (IMMOVABLE) |
| 0x03 | SP | Stack pointer (IMMOVABLE) |
| 0x04 | BP | Base pointer (IMMOVABLE) |
| 0x05 | CSP | Call stack pointer (IMMOVABLE) |
| 0x06-0x07 | FREELIST/FREELISTL/FREELISTH | Heap free list pointer |
| 0x08 | HEAPSTART | Heap start page |
| 0x09 | HEAPSIZE | Heap size in pages |
| 0x0A | SerialInWritePointer | Serial buffer write position |
| 0x0B | SerialInReadPointer | Serial buffer read position |
| 0x0C | SerialBreakFlag | Serial break detected |
| 0x0D | TraceIndent | Debug trace indentation level |
| 0x0E-0x0F | ACC/ACCL/ACCH | Accumulator register |
| 0x10-0x11 | TOP/TOPL/TOPH | Top of stack value |
| 0x12-0x13 | NEXT/NEXTL/NEXTH | Next stack value |
| 0x14-0x15 | IDX/IDXL/IDXH | Index X register |
| 0x16-0x17 | IDY/IDYL/IDYH | Index Y register |
| 0x18 | ACCT | Accumulator type |
| 0x19 | TOPT | Top type |
| 0x1A | NEXTT | Next type |
| 0x1B | I2CInWritePtr | I2C buffer write pointer |
| 0x1C | I2CInReadPtr | I2C buffer read pointer |

### Timer & Debug (0x1D-0x24)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x1D-0x20 | TICK0-3 | Timer tick counter (IMMOVABLE) |
| 0x21-0x22 | EmulatorPCL/H | PC capture (IMMOVABLE) |
| 0x23 | WorkSpaceHexIn | Serial.asm workspace |
| 0x24 | WorkSpaceWaitForChar | Serial.asm workspace |

### BASIC Core Variables (0x25-0x38)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x25 | BasicInputLength | Length of current input line |
| 0x26-0x27 | TokenBufferContentSize/L/H | Token buffer content size (16-bit) |
| 0x28-0x29 | TokenizerPos/L/H | Current tokenizer position |
| 0x2A-0x2B | LastErrorL/H | Error message pointer |
| 0x2C | CurrentToken | Cached current token type |
| 0x2D-0x2E | TokenLiteralPosL/H | Literal data position |
| 0x2F-0x30 | OpCodeBufferContentSize/L/H | JIT buffer content size |
| 0x31-0x32 | CompilerTokenPos/L/H | Compiler token position |
| 0x33 | CompilerFlags | Compilation flags |
| 0x34 | OpCodeTemp | Temporary opcode byte |
| 0x35-0x36 | TokenBuffer/L/H | Current tokenizer buffer pointer |
| 0x37-0x38 | OpCodeBuffer/L/H | Current opcode buffer pointer |

### Symbol Tables (0x39-0x48)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x39-0x3A | VariablesList/L/H | Variables table head pointer |
| 0x3B-0x3C | FunctionsList/L/H | Functions table head pointer |
| 0x3D | SymbolType | Current symbol type/data type |
| 0x3E-0x3F | SymbolValue/L/H | Symbol value storage |
| 0x40-0x41 | SymbolName/L/H | Symbol name pointer |
| 0x42-0x43 | SymbolTokens/L/H | Symbol tokens pointer |
| 0x44 | SymbolIteratorFilter | Symbol iteration filter |
| 0x45 | SymbolLength | Symbol name length |
| 0x46-0x48 | SymbolTemp0-2 | Temporary storage |

### Debug & State (0x49-0x4B)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x49-0x4A | TraceMessageL/H | Trace message pointer |
| 0x4B | SystemState | Success/Failure/Exiting state |

### Shared Leaf Function Workspace (0x4C-0x5B)

Complex leaf methods that never call each other can share this space:
- Memory.Allocate and Memory.Free (mutually exclusive)
- Debug.asm (never calls Memory functions)
- Time.Delay() and Time.Seconds() (mutually exclusive)
- IntMath operations (leaf functions)
- TokenIterator (LIST, FUNCS commands)

| Address | Symbol | Description | Aliases |
|---------|--------|-------------|---------|
| 0x4C-0x5B | M0-M15 | Multi-use workspace | See below |

**Workspace Aliases:**
- **Debug.asm**: DB0-DB15 (alias for M0-M15)
- **Time.Delay()**: TARGET0-3 (alias for M0-M3)
- **Time.Seconds()**: LRESULT0-7 (alias for M0-M7)
- **IntMath**: UWIDE4-7 (alias for M0-M3)
- **TokenIterator**: TOKCUR, TOKBASE/L/H, TOKINDENT, TOKPOS/L/H (alias for M0-M5)

### Function Parameters (0x5C-0x6B)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x5C-0x5D | FSOURCEADDRESS/L/H | Source address for operations |
| 0x5E-0x5F | FDESTINATIONADDRESS/L/H | Destination address |
| 0x60-0x61 | FLENGTH/L/H | Transfer/operation length |
| 0x62-0x63 | LCURRENT/L/H | List current pointer |
| 0x64-0x65 | LHEAD/L/H | List head pointer |
| 0x66 | FSIGN | Sign flag |
| 0x67 | LHEADX | List head extension |
| 0x68-0x69 | LPREVIOUS/L/H | List previous pointer |
| 0x6A-0x6B | LNEXT/L/H | List next pointer |

### Math Workspace (0x6C-0x7B)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x6C-0x6F | UWIDE0-3 | IntMath 32-bit multiply |
| 0x70-0x73 | LNEXT0-3 | Long math next operand |
| 0x74-0x77 | LTOP0-3 | Long math top operand |
| 0x78-0x79 | STR/STRL/STRH | String pointer |
| 0x7A-0x7B | STR2/STR2L/STR2H | String pointer 2 |

### Large Unused Region (0x7C-0xEB)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x7C-0xEB | - | Currently unused (112 bytes available!) |

### Hardware I/O (0xEC-0xFF) - IMMOVABLE

| Address | Symbol | Description |
|---------|--------|-------------|
| 0xEC | ACIACONTROL/ACIASTATUS | 6850 ACIA control/status |
| 0xED | ACIADATA | ACIA data register |
| 0xEE-0xEF | - | Reserved for hardware |
| 0xF0 | PORTB | VIA Port B data |
| 0xF1 | PORTA | VIA Port A data |
| 0xF2 | DDRB | Data Direction Register B |
| 0xF3 | DDRA | Data Direction Register A |
| 0xF4 | T1CL | Timer 1 Counter Low |
| 0xF5 | T1CH | Timer 1 Counter High |
| 0xF6 | T1LL | Timer 1 Latch Low |
| 0xF7 | T1LH | Timer 1 Latch High |
| 0xF8 | T2CL | Timer 2 Counter Low |
| 0xF9 | T2CH | Timer 2 Counter High |
| 0xFA | SR | Shift Register |
| 0xFB | ACR | Auxiliary Control Register |
| 0xFC | PCR | Peripheral Control Register |
| 0xFD | IFR | Interrupt Flag Register |
| 0xFE | IER | Interrupt Enable Register |
| 0xFF | ORA_NO_HANDSHAKE | Output Register A (no handshake) |

## FLAGS Register Bits (0x02)

| Bit | Description |
|-----|-------------|
| 7 | (unused in BASIC) |
| 6 | (unused in BASIC) |
| 5 | (unused in BASIC) |
| 4 | (unused in BASIC) |
| 3 | REPL mode flag |
| 2 | TRON/TROFF (trace on/off) |
| 1 | Temporary exit flag for Console.processTokens() loop control |
| 0 | Program has been loaded |

## CompilerFlags Bits (0x33)

| Bit | Description |
|-----|-------------|
| 0 | Currently compiling inside a function |
| 1-7 | Reserved for future use |

## Optimization Opportunities

Based on the 6502Optimization.md analysis:
- **Heavy contention on 0x0E-0x1A** (ACC, TOP, NEXT, IDX, IDY) - "fighting over 12 bytes"
- **Large unused region**: 0x7C-0xEB (112 bytes available)
- **Shared workspace model** (0x4C-0x5B) shows how leaf functions can efficiently share ZP
- **Total optimization potential**: 8-12KB savings by eliminating ZP juggling