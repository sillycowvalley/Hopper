# Hopper 6502 BIOS API Specification

## Zero Page Memory Map (Reorganized)

### System Critical (0x00-0x0C)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x00** | FLAGS | System flags register | Yes |
| | | Bit 2: Serial XON/XOFF (set if stopped) | |
| | | Bit 1: EEPROM exists | |
| | | Bit 0: NMI break flag | |
| **0x01** | LastError | Last error code | Yes |
| **0x02-0x03** | FREELIST | Heap free list pointer | Yes |
| **0x04** | HEAPSTART | Heap start page | Yes |
| **0x05** | HEAPSIZE | Heap size in pages | Yes |
| **0x06** | SerialInWritePointer | Serial buffer write position | Yes |
| **0x07** | SerialInReadPointer | Serial buffer read position | Yes |
| **0x08** | I2CInWritePtr | I2C buffer write pointer | Yes |
| **0x09** | I2CInReadPtr | I2C buffer read pointer | Yes |
| **0x0A** | OutB | I2C output byte | No |
| **0x0B** | InB | I2C input byte | No |
| **0x0C** | LastAck | I2C last ACK status | No |

### Volatile (0x0D-0x0F)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x0D** | TEMP | Super volatile temporary | No |
| **0x0E-0x0F** | STR2 | Debug string pointer | No |

### General Registers (0x10-0x1F)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x10-0x11** | ACC | 16-bit accumulator | No |
| **0x12-0x15** | TOP | 32-bit operand/result | No |
| **0x16-0x19** | NEXT | 32-bit operand/result | No |
| **0x1A-0x1B** | IDX | General pointer | No |
| **0x1C-0x1D** | IDY | General pointer | No |
| **0x1E-0x1F** | STR | String pointer | No |

### System Reserved (0x20-0x29)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x20-0x21** | JumpTable | Hopper Assembler requirement | Yes |
| **0x22-0x23** | BIOSDISPATCH | BIOS syscall vector | Yes |
| **0x24-0x27** | TICK | 32-bit timer counter | Yes |
| **0x28-0x29** | EmulatorPC | Emulator PC capture | Yes |

### Workspace (0x30-0x57)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x30-0x41** | M0-M17 | Shared workspace (18 bytes) | No |
| **0x42-0x51** | FS0-FS15 | File system workspace | No |
| **0x52-0x53** | FSOURCEADDRESS | Memory op source | No |
| **0x54-0x55** | FDESTINATIONADDRESS | Memory op destination | No |
| **0x56-0x57** | FLENGTH | Memory op length | No |

### Available for User Programs
| Address | Range |
|---------|-------|
| **0x58-0xEB** | User space (148 bytes) |

---

## Current SysCall API

### Conventions
- **Inputs**: Via ZP slots (never A or X registers currently)
- **Outputs**: A for 8-bit values, ZP for 16/32-bit, C flag for success/boolean
- **Preserved**: Y register, user memory, system critical ZP
- **Modified**: A, X, flags, LastError, working ZP areas

### 🔴 **OPEN ISSUE**: With A now preserved through switch statements, should migrate byte parameters from ZP.ACCL to A register

---

## Existing SysCalls

### Memory Management
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **MemAllocate** | ZP.ACC = size (16-bit) | ZP.IDX = address<br>C = success | ✅ Good |
| **MemFree** | ZP.IDX = address | C = success | ✅ Good |
| **MemAvailable** | - | ZP.ACC = free bytes | ✅ Good |
| **MemMaximum** | - | ZP.ACC = largest block | ✅ Good |

### File Operations
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **FileExists** | ZP.STR = filename<br>ZP.ACCL = DirWalkAction | C = exists | 🔴 Should use A |
| **FileDelete** | ZP.STR = filename | C = success | ✅ Good |
| **FileDir** | - | C = success | ✅ Good |
| **FileStartSave** | ZP.STR = filename | C = success | ✅ Good |
| **FileAppendStream** | FS0-1 = ptr<br>FS2-3 = count | C = success | ✅ Good |
| **FileEndSave** | ZP.ACCL = file type | C = success | 🔴 Should use A |
| **FileStartLoad** | ZP.STR = filename<br>ZP.ACCL = DirWalkAction | C = success | 🔴 Should use A |
| **FileNextStream** | - | C = data available<br>FS2-3 = count | ✅ Good |
| **FileFormat** | - | C = success | ✅ Good |

### Serial I/O
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **SerialWriteChar** | ZP.ACCL = character | - | 🔴 Should use A |
| **SerialWaitForChar** | - | A = character | ✅ Good |
| **SerialIsAvailable** | - | C = available | ✅ Good |
| **IsBreak** | - | C = break detected | ✅ Good |

### Print/Console
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **PrintString** | ZP.STR = string | - | ✅ Good |
| **PrintChar** | ZP.ACCL = character | - | 🔴 Should use A |
| **PrintHex** | ZP.ACCL = byte | - | 🔴 Should use A |
| **PrintNewLine** | - | - | ✅ Good |
| **PrintSpace** | - | - | ✅ Good |
| **PrintSpaces** | ZP.ACCL = count | - | 🔴 Should use A |

### Timer Services
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **TimeDelay** | ZP.TOP = ms (16-bit) | - | ✅ Good |
| **TimeMillis** | - | ZP.TOP = ms (32-bit) | ✅ Good |
| **TimeSeconds** | - | ZP.TOP = seconds (32-bit) | ✅ Good |

### Long Math (32-bit)
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **LongAdd/Sub/Mul** | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ✅ Good |
| **LongDiv** | ZP.NEXT, ZP.TOP | ZP.NEXT = quotient | 🟡 Add C flag for div/0 |
| **LongMod** | ZP.NEXT, ZP.TOP | ZP.NEXT = remainder | 🟡 Add C flag for div/0 |
| **LongPrint** | ZP.TOP = value | - | ✅ Good |
| **LongLT/GT/EQ/NE/LE/GE** | ZP.NEXT, ZP.TOP | C = comparison result | ✅ Good |

### Float Math (IEEE 754)
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **FloatAdd/Sub/Mul/Div** | ZP.NEXT, ZP.TOP | ZP.NEXT = result | ✅ Good |
| **FloatToLong** | ZP.NEXT = float | ZP.NEXT = long | 🟡 Add C flag for overflow |
| **FloatLT/EQ** | ZP.NEXT, ZP.TOP | C = comparison result | ✅ Good |

### GPIO
| SysCall | Inputs | Outputs | Status |
|---------|--------|---------|--------|
| **PinMode** | ZP.ACCL = pin<br>ZP.ACCH = mode | - | 🔴 Should use A, Y |
| **PinRead** | ZP.ACCL = pin | ZP.ACCH = value | 🔴 Should use A in/out |
| **PinWrite** | ZP.ACCL = pin<br>ZP.ACCH = value | - | 🔴 Should use A, Y |

---

## Proposed New SysCalls

### High Priority Additions

#### Memory Operations
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **MemCopy** | FSOURCE = source<br>FDEST = dest<br>FLENGTH = count | - | Essential operation |
| **MemClear** | ZP.IDX = address<br>ZP.ACC = count | - | Clear memory range |
| **MemClearPage** | A = page number | - | Clear 256-byte page |
| **MemClearPages** | A = page number<br>X = page count | - | Clear multiple pages |

#### String Operations
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **StringLength** | ZP.STR = string | A = length (0-255) | Essential for strings |
| **StringCompare** | ZP.STR = string1<br>ZP.IDX = string2 | C = equal | Essential for strings |

#### Character Tests (Currently Missing)
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **CharIsDigit** | A = character | C = is digit | Essential for parsing |
| **CharIsAlpha** | A = character | C = is alpha | Essential for parsing |
| **CharIsUpper** | A = character | C = is uppercase | Text processing |
| **CharIsLower** | A = character | C = is lowercase | Text processing |
| **CharIsHex** | A = character | C = is hex | Essential for hex parsing |
| **CharIsAlphaNumeric** | A = character | C = is alphanumeric | Parsing support |
| **CharIsPrintable** | A = character | C = printable | Display routines |

#### Character Conversions
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **CharToUpper** | A = character | A = uppercase | Common operation |
| **CharToLower** | A = character | A = lowercase | Text processing |

#### I2C Operations (Complete Suite)
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **I2CScan** | A = address (7-bit) | C = device found | Hardware discovery |
| **I2CBeginTx** | A = address (7-bit) | - | Start transmission |
| **I2CEndTx** | - | C = success (ACK) | Complete transmission |
| **I2CWrite** | A = byte to send | - | Send data byte |
| **I2CBeginRx** | A = address (7-bit) | - | Start reception |
| **I2CRead** | - | A = byte read | Read data byte |
| **I2CRequestFrom** | A = address<br>Y = byte count | A = bytes read | Read multiple bytes |
| **I2CAvailable** | - | C = data available | Check buffer status |

#### Serial Operations
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **SerialHexIn** | - | A = hex byte | Read hex input |


### Medium Priority Additions

#### Long Math Conversions
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **LongFromDecimal** | ZP.STR = string | ZP.TOP = value<br>C = success | String to number |
| **LongFromHex** | ZP.STR = string | ZP.TOP = value<br>C = success | Hex string to number |

#### File Operations
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **FileGetAvailable** | - | ZP.TOP = free bytes | Storage management |
| **FileGetLength** | - | ZP.ACC = file size | File information |


### Low Priority Additions

#### String Operations
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **StringToUpper** | ZP.STR = string | - | Can loop with CharToUpper |

#### Long Math
| SysCall | Proposed Inputs | Proposed Outputs | Rationale |
|---------|-----------------|------------------|-----------|
| **LongNegate** | ZP.NEXT = value | ZP.NEXT = -value | Can use 0 - value |


---

## Summary of New SysCalls by Category

### Essential Missing Functionality (25 syscalls)
- **Memory**: 4 (Copy, Clear, ClearPage, ClearPages)
- **Strings**: 2 (Length, Compare)
- **Characters**: 9 (IsDigit, IsAlpha, IsUpper, IsLower, IsHex, IsAlphaNumeric, IsPrintable, ToUpper, ToLower)
- **I2C**: 8 (complete I2C interface)
- **Serial**: 2 (HexIn)




---

## Open Issues Summary

### 🔴 Critical Changes Needed
1. **Migrate byte parameters to A register** (7 syscalls)
   - SerialWriteChar, PrintChar, PrintHex, PrintSpaces
   - FileExists, FileEndSave, FileStartLoad (secondary param)

2. **GPIO should use registers**
   - PinMode: A = pin, Y = mode
   - PinRead: A = pin → A = value
   - PinWrite: A = pin, Y = value

### 🟡 Minor Improvements
1. **Add error flags where meaningful**
   - LongDiv/Mod: C flag for divide by zero
   - FloatToLong: C flag for overflow

### 🟢 Design Decisions
1. **No error flags for operations that can't fail**
   - Math operations that wrap (Add, Sub, Mul)
   - Print operations
   - Time operations

---

## Implementation Priority

1. **Phase 1**: Add essential missing syscalls (MemCopy, MemClear, String ops, Char tests)
2. **Phase 2**: Migrate byte parameters to A register for efficiency
3. **Phase 3**: Add error detection where meaningful
4. **Phase 4**: Consider medium priority additions based on user needs