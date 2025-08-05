# HopperBASIC Zero Page Reference
**Document Type: Zero Page Map**

## Overview
The 6502's zero page (0x00-0xFF) provides fast 8-bit addressing. HopperBASIC allocates this precious resource across VM core, BASIC runtime, and hardware I/O.

## Core VM Variables (0x00-0x1F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x00-0x01 | PC/PCL/PCH | Program counter |
| 0x02 | FLAGS | System flags byte (see below) |
| 0x03 | SP | Stack pointer |
| 0x04 | BP | Base pointer |
| 0x05 | CSP | Call stack pointer |
| 0x06-0x07 | FREELIST/FREELISTL/FREELISTH | Heap free list pointer |
| 0x08 | HEAPSTART | Heap start page |
| 0x09 | HEAPSIZE | Heap size in pages |
| 0x0A | SerialInWritePointer | Serial buffer write position |
| 0x0B | SerialInReadPointer | Serial buffer read position |
| 0x0C | SerialBreakFlag | Serial break detected |
| 0x0D-0x0E | CODESTART/CODESTARTL/CODESTARTH | Code start address |
| 0x0F | TraceIndent | Debug trace indentation level |
| 0x10-0x11 | ACC/ACCL/ACCH | Accumulator register |
| 0x12-0x13 | TOP/TOPL/TOPH | Top of stack value |
| 0x14-0x15 | NEXT/NEXTL/NEXTH | Next stack value |
| 0x16-0x17 | IDX/IDXL/IDXH | Index X register |
| 0x18-0x19 | IDY/IDYL/IDYH | Index Y register |
| 0x1A | ACCT | Accumulator type |
| 0x1B | TOPT | Top type |
| 0x1C | NEXTT | Next type |
| 0x1D | PROGSIZE | Program size |
| 0x1E | I2CInWritePtr | I2C buffer write pointer |
| 0x1F | I2CInReadPtr | I2C buffer read pointer |

## Time & Workspace (0x20-0x2F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x20-0x23 | TICK0-3 | Timer tick counter (32-bit) |
| 0x24-0x27 | TARGET0-3 | Timer target value (32-bit) |
| 0x28 | WorkSpaceHexIn | Serial.asm workspace |
| 0x29 | WorkSpaceWaitForChar | Serial.asm workspace |
| 0x2A-0x2F | - | Currently unused |

## BASIC Core Variables (0x30-0x3F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x30 | BasicInputLength | Length of current input line |
| 0x31-0x32 | TokenBufferLength/L/H | Token buffer size (16-bit) |
| 0x33-0x34 | TokenizerPos/L/H | Current tokenizer position |
| 0x35-0x36 | LastErrorL/H | Error message pointer |
| 0x37 | CurrentToken | Cached current token type |
| 0x38-0x39 | TokenLiteralPosL/H | Literal data position |
| 0x3A-0x3B | OpCodeBufferLength/L/H | JIT buffer length |
| 0x3C-0x3D | CompilerTokenPos/L/H | Compiler token position |
| 0x3E | CompilerFlags | Compilation flags |
| 0x3F | OpCodeTemp | Temporary opcode byte |

## Symbol Tables (0x40-0x4F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x40-0x41 | VariablesList/L/H | Variables table head pointer |
| 0x42-0x43 | FunctionsList/L/H | Functions table head pointer |
| 0x44 | SymbolType | Current symbol type/data type |
| 0x45-0x46 | SymbolValue/L/H | Symbol value storage |
| 0x47-0x48 | SymbolName/L/H | Symbol name pointer |
| 0x49-0x4A | SymbolTokens/L/H | Symbol tokens pointer |
| 0x4B | SymbolIteratorFilter | Symbol iteration filter |
| 0x4C | SymbolLength | Symbol name length |
| 0x4D-0x4F | SymbolTemp0-2 | Temporary storage |

## Debug & State (0x50-0x5F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x50-0x51 | EmulatorPCL/H | PC capture for debug (BIT 0x50 trigger) |
| 0x52-0x53 | TraceMessageL/H | Trace message pointer |
| 0x54 | SystemState | Success/Failure/Exiting state |
| 0x55-0x5F | - | Currently unused |

## Memory Manager Workspace (0x60-0x6F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x60-0x6F | M0-M15 | Memory.Allocate/Free workspace |
| 0x60-0x6F | DB0-DB15 | Debug.asm aliases (when not allocating) |

## Unused Region (0x70-0x7F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x70-0x7F | - | Currently unused (16 bytes available) |

## Function Workspace (0x80-0x8F)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x80-0x81 | FSOURCEADDRESS/L/H | Source address for operations |
| 0x82-0x83 | FDESTINATIONADDRESS/L/H | Destination address |
| 0x84-0x85 | FLENGTH/L/H | Transfer/operation length |
| 0x86-0x87 | LCURRENT/L/H | List current pointer |
| 0x88-0x89 | LHEAD/L/H | List head pointer |
| 0x8A | FSIGN | Sign flag |
| 0x8B | LHEADX | List head extension |
| 0x8C-0x8D | LPREVIOUS/L/H | List previous pointer |
| 0x8E-0x8F | LNEXT/L/H | List next pointer |

## Math Workspace (0x90-0xA5)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0x90-0x93 | UWIDE0-3 | IntMath.asm workspace |
| 0x94-0x97 | LNEXT0-3 | Long math next operand |
| 0x98-0x9B | LTOP0-3 | Long math top operand |
| 0x9C-0xA3 | LRESULT0-7 | Long math result (64-bit) |
| 0xA4-0xA5 | STR/STRL/STRH | String pointer |

## Large Unused Region (0xA6-0xEB)

| Address | Symbol | Description |
|---------|--------|-------------|
| 0xA6-0xEB | - | Currently unused (70 bytes available!) |

## Hardware I/O (0xEC-0xFF)

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
| 7 | MCU platform indicator |
| 6 | Program exited (ended well or via Crash/Die) |
| 5 | Breakpoints exist |
| 4 | In debugger |
| 3 | 8-bit SP and BP (always true?) |
| 2 | TRON/TROFF (trace on/off) |
| 1 | Warp mode |
| 0 | Program has been loaded |

## CompilerFlags Bits (0x3E)

| Bit | Description |
|-----|-------------|
| 0 | Currently compiling inside a function |
| 1-7 | Reserved for future use |

## Optimization Opportunities

Based on the 6502Optimization.md analysis:
- **Heavy contention on 0x10-0x1C** (ACC, TOP, NEXT, IDX, IDY)
- **Large unused regions**: 0x55-0x5F (11 bytes), 0x70-0x7F (16 bytes), 0xA6-0xEB (70 bytes)
- **Total unused**: 97 bytes available for dedicated subsystem workspaces