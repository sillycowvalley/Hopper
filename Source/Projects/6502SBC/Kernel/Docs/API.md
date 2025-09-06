Good point about not doubling up. Here's the updated table with suggestions:

## SysCall API with Suggestions

| SysCall | Current Inputs | Current Outputs | **Suggestions** |
|---------|---------------|-----------------|-----------------|
| **Memory Management** |
| MemAllocate | ZP.ACC = size | ZP.IDX = address<br>C = success | ✓ Good as is |
| MemFree | ZP.IDX = address | C = success | ✓ Good as is |
| MemAvailable | - | ZP.ACC = free bytes | ✓ Good as is |
| MemMaximum | - | ZP.ACC = largest block | ✓ Good as is |
| **File Operations** |
| FileExists | ZP.STR = filename<br>A = DirWalkAction | C = exists | ⚠️ Move A param to ZP.ACCL for consistency |
| FileDelete | ZP.STR = filename | C = success | ✓ Good as is |
| FileDir | - | C = success | ✓ Good as is |
| FileStartSave | ZP.STR = filename | C = success | ✓ Good as is |
| FileAppendStream | SectorSource = ptr<br>TransferLength = count | C = success | ✓ Good as is |
| FileEndSave | A = file type | C = success | ⚠️ Move A param to ZP.ACCL |
| FileStartLoad | ZP.STR = filename<br>A = DirWalkAction | C = success | ⚠️ Move A param to ZP.ACCL |
| FileNextStream | - | C = data available<br>TransferLength = count | ✓ Good as is |
| FileFormat | - | C = success | ✓ Good as is |
| **Serial I/O** |
| SerialWriteChar | A = character | - | ⚠️ Move A param to ZP.ACCL |
| SerialWaitForChar | - | A = character | ✓ Good - return in A |
| SerialIsAvailable | - | C = available | ✓ Good as is |
| IsBreak | - | C = break detected | ✓ Good as is |
| **Print/Console** |
| PrintString | ZP.STR = string | - | ⚠️ Add C = success |
| PrintChar | ZP.ACCL = character | - | ✓ Good as is |
| PrintHex | A = byte | - | ⚠️ Move A param to ZP.ACCL |
| PrintNewLine | - | - | ✓ Good as is |
| PrintSpace | - | - | ✓ Good as is |
| PrintSpaces | X = count | - | ⚠️ Move X param to ZP.ACCL |
| **Timer Services** |
| TimeDelay | ZP.TOP = ms | - | ⚠️ Add C = success |
| TimeMillis | - | ZP.TOP = ms (32-bit) | ✓ Good as is |
| TimeSeconds | - | ZP.TOP = seconds | ✓ Good as is |
| **Long Math** |
| LongAdd | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongSub | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongMul | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| LongDiv | ZP.NEXT, ZP.TOP | ZP.NEXT = quotient | ⚠️ Add C = success (NC on div by 0) |
| LongMod | ZP.NEXT, ZP.TOP | ZP.NEXT = remainder | ⚠️ Add C = success (NC on div by 0) |
| LongPrint | ZP.TOP = value | - | ⚠️ Add C = success |
| LongLT | ZP.NEXT, ZP.TOP | C = (NEXT < TOP) | ✓ Good as is |
| LongGT | ZP.NEXT, ZP.TOP | C = (NEXT > TOP) | ✓ Good as is |
| LongEQ | ZP.NEXT, ZP.TOP | C = (NEXT == TOP) | ✓ Good as is |
| LongNE | ZP.NEXT, ZP.TOP | C = (NEXT != TOP) | ✓ Good as is |
| LongLE | ZP.NEXT, ZP.TOP | C = (NEXT <= TOP) | ✓ Good as is |
| LongGE | ZP.NEXT, ZP.TOP | C = (NEXT >= TOP) | ✓ Good as is |
| **Float Math** |
| FloatAdd | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatSub | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatMul | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ⚠️ Add C = success (NC on overflow) |
| FloatDiv | ZP.NEXT, ZP.TOP | ZP.NEXT = quotient | ⚠️ Add C = success (NC on div by 0) |
| FloatToLong | ZP.NEXT = float | ZP.NEXT = long | ⚠️ Add C = success (NC on overflow) |
| FloatLT | ZP.NEXT, ZP.TOP | C = (NEXT < TOP) | ✓ Good as is |
| FloatEQ | ZP.NEXT, ZP.TOP | C = (NEXT == TOP) | ✓ Good as is |
| **GPIO** |
| PinMode | ZP.ACCL = pin<br>ZP.ACCH = mode | - | ⚠️ Add C = success |
| PinRead | ZP.ACCL = pin | ZP.ACCH = value | ⚠️ Return value in A instead |
| PinWrite | ZP.ACCL = pin<br>ZP.ACCH = value | - | ⚠️ Add C = success |

## Summary of Proposed Conventions:

1. **Inputs**: Always via ZP slots (never A or X registers)
2. **8-bit returns**: Use A register when appropriate
3. **16/32-bit returns**: Keep in ZP slots
4. **Success/failure**: Always use C flag (set = success)
5. **Preserve**: Y register (caller can rely on it)
6. **May modify**: A, X, flags, ZP.LastError, BIOS working areas



## Missing Methods Analysis

| Module | Missing Method | **Should be SysCall?** | Reasoning |
|--------|---------------|------------------------|-----------|
| **Memory** |
| | `Copy()` | ✅ **Yes** | User programs need to copy memory blocks |
| | `Clear()` / `ClearPage()` | ✅ **Yes** | User programs need to clear memory |
| | `ClearPages()` | ❌ No | Can be done with loop of ClearPage |
| **String** |
| | `Length()` | ✅ **Yes** | Essential string operation |
| | `Compare()` | ✅ **Yes** | Essential string operation |
| | `ToUpperSTR()` | ⚠️ Maybe | Could be useful, but users can loop with Char.ToUpper |
| **Char** |
| | `IsDigit()` | ✅ **Yes** | Essential for parsing |
| | `IsAlpha()` | ✅ **Yes** | Essential for parsing |
| | `IsAlphaNumeric()` | ✅ **Yes** | Essential for parsing |
| | `IsHex()` | ✅ **Yes** | Essential for hex parsing |
| | `IsLower()` | ⚠️ Maybe | Less critical |
| | `IsPrintable()` | ✅ **Yes** | Useful for display routines |
| | `ToUpper()` | ✅ **Yes** | Essential for case-insensitive operations |
| | `ToLower()` | ⚠️ Maybe | Less common than ToUpper |
| **Serial** |
| | `EmptyTheBuffer()` | ✅ **Yes** | Important for clearing input buffer |
| | `HexOut()` | ❌ No | Already have PrintHex |
| | `HexIn()` | ✅ **Yes** | Useful for reading hex input |
| **I2C** |
| | `Scan()` | ✅ **Yes** | Hardware discovery |
| | `BeginTx()` | ✅ **Yes** | I2C communication |
| | `Write()` | ✅ **Yes** | I2C communication |
| | `EndTx()` | ✅ **Yes** | I2C communication |
| | `RequestFrom()` | ✅ **Yes** | I2C communication |
| | `Read()` | ✅ **Yes** | I2C communication |
| **EEPROM** |
| | `Detect()` | ✅ **Yes** | Check if EEPROM present |
| | `GetSize()` | ✅ **Yes** | Determine storage capacity |
| **File** |
| | `GetFileLength()` | ✅ **Yes** | Useful for file operations |
| | `GetAvailable()` | ✅ **Yes** | Check free space |
| | `ValidateFilename()` | ❌ No | Can be done in user code |
| **Long** |
| | `FromDecimal()` | ✅ **Yes** | Parse decimal strings |
| | `FromHex()` | ✅ **Yes** | Parse hex strings |
| | `NegateNext()` | ⚠️ Maybe | Can be done with 0 - value |
| **Float** |
| | `IsZeroNext()` | ⚠️ Maybe | Can use EQ with 0.0 |
| | `New()` | ❌ No | Just zeroing memory |
| **Print** |
| | `Decimal()` | ❌ No | Already have LongPrint |






## BIOS Zero Page Map

| Address | Name | Description | Category |
|---------|------|-------------|----------|
| **0x00** | FLAGS | System flags (bit 0: NMI break, bit 1: XON/XOFF) | System |
| **0x01-0x05** | *unused* | | |
| **0x06-0x07** | FREELIST | Heap free list pointer | Heap |
| **0x08** | HEAPSTART | Heap start page | Heap |
| **0x09** | HEAPSIZE | Heap size in pages | Heap |
| **0x0A** | SerialInWritePointer | Serial buffer write position | Serial |
| **0x0B** | SerialInReadPointer | Serial buffer read position | Serial |
| **0x0C-0x0D** | ACC | Accumulator for parameters/results | Parameters |
| **0x0E-0x0F** | *unused* | | |
| **0x10** | TEMP | Super volatile temporary | Scratch |
| **0x11** | LastError | Last error code | System |
| **0x12-0x15** | TOP0-3 | 32-bit operand/result | Math/Parameters |
| **0x16-0x19** | NEXT0-3 | 32-bit operand/result | Math/Parameters |
| **0x1A** | *unused* | | |
| **0x1B-0x1C** | IDX | General pointer | Pointers |
| **0x1D-0x1E** | IDY | General pointer | Pointers |
| **0x1F** | *unused* | | |
| **0x20-0x21** | JumpTable | Jump table for Hopper Assembler | System |
| **0x22-0x25** | TICK0-3 | 32-bit timer tick counter | Timer |
| **0x26-0x27** | STR | String pointer | Parameters |
| **0x28** | *unused* | | |
| **0x29-0x2A** | STR2 | Second string pointer (DEBUG only) | Debug |
| **0x2B-0x2C** | BIOSDISPATCH | BIOS syscall dispatch vector | System |
| **0x2D-0x2E** | FSOURCEADDRESS | Source address for memory ops | Parameters |
| **0x2F-0x30** | FDESTINATIONADDRESS | Destination for memory ops | Parameters |
| **0x31-0x32** | FLENGTH | Length for memory ops | Parameters |
| **0x33-0x4E** | *unused* | | |
| **0x4F-0x60** | M0-M17 | Multi-use workspace (18 bytes) | Workspace |
| | TARGET0-3 | Time.Delay() workspace (aliases M0-M3) | |
| | RESULT0-7 | Long/Float math results (aliases M0-M7) | |
| | DB0-DB15 | Debug workspace (aliases M0-M15) | |
| **0x61-0x74** | *unused* | | |
| **0x75-0x76** | EmulatorPC | Emulator PC capture | Emulator |
| **0x77** | I2CInWritePtr | I2C buffer write pointer | I2C |
| **0x78** | I2CInReadPtr | I2C buffer read pointer | I2C |
| **0x79-0x87** | *unused* | | |
| **0x88** | OutB | I2C output byte | I2C |
| **0x89** | InB | I2C input byte | I2C |
| **0x8A** | LastAck | I2C last ACK status | I2C |
| **0x8B** | PLUGNPLAY | Hardware detection flags | System |
| **0x8C-0x9B** | FS0-FS15 | File system workspace (16 bytes) | File System |
| | SectorSource | File ops source (aliases FS0-FS1) | |
| | TransferLength | File transfer size (aliases FS2-FS3) | |
| | CurrentFileSector | Current sector (alias FS4) | |
| | FileStartSector | First sector (alias FS5) | |
| | CurrentFileEntry | Directory entry (alias FS6) | |
| | FilePosition | File position (aliases FS7-FS8) | |
| | NextFileSector | Next sector (alias FS9) | |
| | BytesRemaining | Bytes left (aliases FS10-FS11) | |
| | SectorPosition | Position in sector (aliases FS12-FS13) | |
| | CurrentDirectorySector | Directory sector (alias FS14) | |
| **0x9C-0xEB** | *unused* | | |

### Hardware I/O (Platform-specific, when ZEROPAGE_IO defined):
| Address | Name | Description |
|---------|------|-------------|
| **0xEC** | ACIACONTROL/STATUS | 6850 ACIA control/status |
| **0xED** | ACIADATA | 6850 ACIA data |
| **0xEE-0xEF** | *reserved* | Hardware expansion |
| **0xF0-0xFF** | VIA registers | 65C22 VIA I/O |

## Summary of Usage:

### Protected BIOS Areas (preserved across syscalls):
- **0x00-0x0D**: System state and heap management
- **0x20-0x21**: Jump table (Hopper Assembler requirement)
- **0x22-0x25**: Timer ticks
- **0x2B-0x2C**: BIOS dispatch vector
- **0x75-0x76**: Emulator hooks

### Parameter Passing Areas (used for syscalls):
- **0x0C-0x0D**: ACC (general parameters)
- **0x12-0x19**: TOP/NEXT (math operands)
- **0x26-0x27**: STR (string pointer)
- **0x1B-0x1E**: IDX/IDY (pointers)
- **0x2D-0x32**: Memory operation parameters

### Working Areas (may be modified by syscalls):
- **0x10**: TEMP (super volatile)
- **0x4F-0x60**: M0-M17 (shared workspace)
- **0x77-0x8B**: I2C working area
- **0x8C-0x9B**: File system workspace

