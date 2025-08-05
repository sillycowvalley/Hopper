# HopperBASIC Zero Page Reference
**Document Type: Zero Page Map**

## Core VM (0x00-0x1F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x00-0x01 | PC/PCL/PCH | Program counter |
| 0x02 | FLAGS | System flags byte |
| 0x03-0x05 | SP/BP/CSP | Stack pointers |
| 0x06-0x07 | FREELIST | Heap free list pointer |
| 0x08-0x09 | HEAPSTART/HEAPSIZE | Heap parameters |
| 0x0A-0x0C | Serial pointers | I/O buffer management |
| 0x0D-0x0E | CODESTART | Code start address |
| 0x0F | TraceIndent | Debug trace indentation |
| 0x10-0x11 | ACC/ACCL/ACCH | Accumulator register |
| 0x12-0x13 | TOP/TOPL/TOPH | Top of stack value |
| 0x14-0x15 | NEXT/NEXTL/NEXTH | Next stack value |
| 0x16-0x17 | IDX/IDXL/IDXH | Index X register |
| 0x18-0x19 | IDY/IDYL/IDYH | Index Y register |
| 0x1A-0x1C | ACCT/TOPT/NEXTT | Type bytes |
| 0x1D-0x1F | PROGSIZE/I2C | Program size, I2C pointers |

## Time & Workspace (0x20-0x2F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x20-0x23 | TICK0-3 | Timer tick counter |
| 0x24-0x27 | TARGET0-3 | Timer target value |
| 0x28-0x29 | WorkSpace* | Serial.asm workspace |

## BASIC Core (0x30-0x3F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x30 | BasicInputLength | Input buffer length |
| 0x31-0x32 | TokenBufferLength | Token buffer size |
| 0x33-0x34 | TokenizerPos | Current token position |
| 0x35-0x36 | LastError | Error message pointer |
| 0x37 | CurrentToken | Cached token type |
| 0x38-0x39 | TokenLiteralPos | Literal data position |
| 0x3A-0x3B | OpCodeBufferLength | JIT buffer size |
| 0x3C-0x3D | CompilerTokenPos | Compiler position |
| 0x3E | CompilerFlags | Compilation state |
| 0x3F | OpCodeTemp | Temporary opcode byte |

## Symbol Tables (0x40-0x4F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x40-0x41 | VariablesList | Variables table head |
| 0x42-0x43 | FunctionsList | Functions table head |
| 0x44-0x4A | Symbol workspace | Type/value/name/tokens |
| 0x4B | SymbolIteratorFilter | Iteration filter |
| 0x4C-0x4F | SymbolTemp0-2 | Temporary storage |

## Debug & State (0x50-0x54)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x50-0x51 | EmulatorPC | PC capture for debug |
| 0x52-0x53 | TraceMessage | Trace message pointer |
| 0x54 | SystemState | Success/failure/exit |

## Memory Manager (0x60-0x6F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x60-0x6F | M0-M15 | Memory.Allocate workspace |
| 0x60-0x71 | DB0-DB17 | Debug.asm alias (overlaps M0-M15) |

## Function Workspace (0x80-0x8F)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x80-0x81 | FSOURCEADDRESS | Source pointer |
| 0x82-0x83 | FDESTINATIONADDRESS | Destination pointer |
| 0x84-0x85 | FLENGTH | Transfer length |
| 0x86-0x89 | LCURRENT/LHEAD | List operations |
| 0x8A | FSIGN | Sign flag |
| 0x8B-0x8F | List workspace | Previous/next pointers |

## Math Workspace (0x90-0xA3)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0x90-0x93 | UWIDE0-3 | IntMath workspace |
| 0x94-0x9B | LNEXT/LTOP | Long math operands |
| 0x9C-0xA3 | LRESULT0-7 | Long math result |

## Hardware I/O (0xEC-0xFF)
| Address | Symbol | Description |
|---------|--------|-------------|
| 0xEC | ACIACONTROL/STATUS | 6850 ACIA control |
| 0xED | ACIADATA | ACIA data register |
| 0xF0-0xF3 | PORT*/DDR* | VIA GPIO ports |
| 0xF4-0xF9 | Timer registers | T1/T2 counters |
| 0xFA-0xFF | VIA control | SR/ACR/PCR/IFR/IER/ORA |

## FLAGS Register Bits
- Bit 7: MCU platform
- Bit 6: Program exited
- Bit 5: Breakpoints exist
- Bit 4: In debugger
- Bit 3: 8-bit SP/BP
- Bit 2: Checked build
- Bit 1: Warp mode
- Bit 0: Program loaded