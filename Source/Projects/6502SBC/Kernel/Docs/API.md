# Hopper 6502 BIOS API Documentation

## System Architecture

### Memory Layout
| Region | Address Range | Description |
|--------|--------------|-------------|
| **Zero Page** | 0x0000-0x00FF | System variables, workspace, hardware I/O |
| **Stack Page 0** | 0x0100-0x01FF | Primary stack |
| **Stack Pages 1-3** | 0x0200-0x04FF | Extended stack space |
| **Serial Buffer** | 0x0500-0x05FF | 256-byte serial input buffer |
| **Work Space** | 0x0700-0x07FF | 256-byte general workspace |
| **Line Buffer** | 0x0700-0x073F | First 64 bytes for command line parser |
| **Entry Point** | 0x0B00 | Program entry point |

## Zero Page Memory Map

### System Critical (0x00-0x0D)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x00** | FLAGS | System flags register | Yes |
| | | Bit 7: Exiting | |
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
| **0x0A** | OutB | I2C/File system output byte | No |
| **0x0B** | InB | I2C/File system input byte | No |
| **0x0C** | LastAck | I2C last ACK status | No |
| **0x0D** | TEMP | Super volatile temporary | No |

### Debug Support (0x0E-0x0F)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x0E-0x0F** | STR2 | Debug string pointer (DEBUG builds only) | No |

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
| **0x20-0x21** | JumpTable | Hopper Assembler jump table | Yes |
| **0x22-0x23** | BIOSDISPATCH | BIOS syscall vector | Yes |
| **0x24-0x27** | TICK | 32-bit timer counter (LSB first) | Yes |
| **0x28-0x29** | EmulatorPC | Emulator PC capture | Yes |

### Shared Workspace (0x30-0x41)
| Address | Name | Description | Usage |
|---------|------|-------------|--------|
| **0x30-0x41** | M0-M17 | Multi-use workspace (18 bytes) | Shared by leaf functions |
| | TARGET0-3 | Time.Delay() workspace | Alias: M0-M3 |
| | RESULT0-7 | Time.Seconds()/Long math | Alias: M0-M7 |
| | DB0-DB15 | Debug workspace | Alias: M0-M15 |

### File System Workspace (0x42-0x58)
| Address | Name | Description | Preserved |
|---------|------|-------------|-----------|
| **0x42-0x52** | FS0-FS16 | File system workspace | No |
| **0x53-0x54** | FSOURCEADDRESS | Source address parameter | No |
| **0x55-0x56** | FDESTINATIONADDRESS | Destination address parameter | No |
| **0x57-0x58** | FLENGTH | Length parameter | No |

### Available for User Programs
| Address Range | Size | Notes |
|--------------|------|-------|
| **0x59-0xEB** | 147 bytes | Free for user allocation |

### Hardware I/O (ZEROPAGE_IO Configuration)
| Address | Name | Description |
|---------|------|-------------|
| **0xEC** | ACIACONTROL/STATUS | 6850 ACIA control/status |
| **0xED** | ACIADATA | 6850 ACIA data register |
| **0xF0** | PORTB | VIA Port B data |
| **0xF1** | PORTA | VIA Port A data |
| **0xF2** | DDRB | Data Direction Register B |
| **0xF3** | DDRA | Data Direction Register A |
| **0xF4-0xF5** | T1CL/H | Timer 1 Counter Low/High |
| **0xF6-0xF7** | T1LL/H | Timer 1 Latch Low/High |
| **0xF8-0xF9** | T2CL/H | Timer 2 Counter Low/High |
| **0xFA** | SR | Shift Register |
| **0xFB** | ACR | Auxiliary Control Register |
| **0xFC** | PCR | Peripheral Control Register |
| **0xFD** | IFR | Interrupt Flag Register |
| **0xFE** | IER | Interrupt Enable Register |
| **0xFF** | ORA_NO_HANDSHAKE | Output Register A (no handshake) |

## System Call Interface

### Calling Convention
- **Call Method**: Load X with SysCall ID, then JSR through BIOSDISPATCH vector
- **Register Usage**:
  - X: System call ID (preserved across call)
  - A: 8-bit parameters/results
  - Y: Sometimes used for parameters (preserved unless documented)
- **Flag Returns**: Carry flag typically indicates success (Set=success, Clear=error)
- **Error Handling**: LastError (0x01) contains error code on failure

### Memory Management

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **MemAllocate** | 0x00 | ZP.ACC = size (16-bit) | ZP.IDX = address<br>C = success | Allocates memory block |
| **MemFree** | 0x01 | ZP.IDX = address | C = success | Frees memory block |
| **MemAvailable** | 0x02 | None | ZP.ACC = free bytes | Returns available memory |
| **MemMaximum** | 0x03 | None | ZP.ACC = largest block | Returns largest contiguous block |

### File Operations

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **FileExists** | 0x04 | ZP.STR = filename<br>A = DirWalkAction | C = exists | Checks if file exists |
| **FileDelete** | 0x05 | ZP.STR = filename | C = success | Deletes file from EEPROM |
| **FileDir** | 0x06 | None | C = success | Lists directory to serial |
| **FileStartSave** | 0x07 | ZP.STR = filename | C = success | Opens file for writing |
| **FileAppendStream** | 0x08 | FSOURCEADDRESS = data<br>FLENGTH = bytes | C = success | Writes data chunk |
| **FileEndSave** | 0x09 | A = file type (0x80=exec, 0x00=data) | C = success | Closes and finalizes file |
| **FileStartLoad** | 0x0A | ZP.STR = filename<br>A = DirWalkAction | C = success | Opens file for reading |
| **FileNextStream** | 0x0B | None | C = data available<br>FLENGTH = bytes | Reads next chunk |
| **FileFormat** | 0x0C | None | C = success | Formats EEPROM filesystem |

### Serial I/O

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **SerialWriteChar** | 0x0D | A = character | None | Writes character to serial |
| **SerialWaitForChar** | 0x0E | None | A = character | Blocks until char available |
| **SerialIsAvailable** | 0x0F | None | C = available | Checks serial buffer status |
| **IsBreak** | 0x10 | None | C = break detected | Checks for Ctrl+C/NMI break |

### Console Output

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **PrintString** | 0x11 | ZP.STR = string pointer | None | Prints null-terminated string |
| **PrintChar** | 0x12 | A = character | None | Prints single character |
| **PrintHex** | 0x13 | A = byte value | None | Prints byte as 2 hex digits |
| **PrintNewLine** | 0x14 | None | None | Prints newline character |
| **PrintSpace** | 0x15 | None | None | Prints single space |
| **PrintSpaces** | 0x16 | Y = count | None | Prints Y spaces |

### Timer Services

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **TimeDelay** | 0x17 | ZP.TOP = milliseconds (32-bit) | None | Delays execution |
| **TimeMillis** | 0x18 | None | ZP.TOP = ms (32-bit) | Returns ms since boot |
| **TimeSeconds** | 0x19 | None | ZP.TOP = seconds (32-bit) | Returns seconds since boot |

### 32-bit Integer Math

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **LongAdd** | 0x1A | ZP.NEXT, ZP.TOP | ZP.NEXT = NEXT + TOP | 32-bit addition |
| **LongSub** | 0x1B | ZP.NEXT, ZP.TOP | ZP.NEXT = NEXT - TOP | 32-bit subtraction |
| **LongMul** | 0x1C | ZP.NEXT, ZP.TOP | ZP.NEXT = NEXT * TOP | 32-bit multiplication |
| **LongDiv** | 0x1D | ZP.NEXT, ZP.TOP | ZP.NEXT = NEXT / TOP | 32-bit division |
| **LongMod** | 0x1E | ZP.NEXT, ZP.TOP | ZP.NEXT = NEXT % TOP | 32-bit modulo |
| **LongPrint** | 0x1F | ZP.TOP = value | None | Prints 32-bit decimal |
| **LongLT** | 0x20 | ZP.NEXT, ZP.TOP | C = (NEXT < TOP) | Less than comparison |
| **LongGT** | 0x21 | ZP.NEXT, ZP.TOP | C = (NEXT > TOP) | Greater than comparison |
| **LongEQ** | 0x22 | ZP.NEXT, ZP.TOP | C = (NEXT == TOP) | Equality comparison |
| **LongNE** | 0x23 | ZP.NEXT, ZP.TOP | C = (NEXT != TOP) | Not equal comparison |
| **LongLE** | 0x24 | ZP.NEXT, ZP.TOP | C = (NEXT <= TOP) | Less or equal comparison |
| **LongGE** | 0x25 | ZP.NEXT, ZP.TOP | C = (NEXT >= TOP) | Greater or equal comparison |

### GPIO Operations

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **PinMode** | 0x26 | A = pin (0-15)<br>Y = mode (0=IN, 1=OUT) | None | Configure pin direction |
| **PinRead** | 0x27 | A = pin (0-15) | A = value (0/1)<br>Z = LOW | Read digital pin state |
| **PinWrite** | 0x28 | A = pin (0-15)<br>Y = value (0/1) | None | Write digital pin state |

### Float Math (Optional - requires HASFLOAT)

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **FloatAdd** | 0x29 | ZP.NEXT, ZP.TOP (IEEE 754) | ZP.NEXT = NEXT + TOP | Float addition |
| **FloatSub** | 0x2A | ZP.NEXT, ZP.TOP (IEEE 754) | ZP.NEXT = NEXT - TOP | Float subtraction |
| **FloatMul** | 0x2B | ZP.NEXT, ZP.TOP (IEEE 754) | ZP.NEXT = NEXT * TOP | Float multiplication |
| **FloatDiv** | 0x2C | ZP.NEXT, ZP.TOP (IEEE 754) | ZP.NEXT = NEXT / TOP | Float division |
| **FloatToLong** | 0x2D | ZP.NEXT (IEEE float) | ZP.NEXT = (long)NEXT | Convert float to long |
| **FloatLT** | 0x2E | ZP.NEXT, ZP.TOP (IEEE floats) | C = (NEXT < TOP) | Float less than |
| **FloatEQ** | 0x2F | ZP.NEXT, ZP.TOP (IEEE floats) | C = (NEXT == TOP) | Float equality |

### File I/O (C-style - Optional)

| SysCall | ID | Inputs | Outputs | Description |
|---------|-----|--------|---------|-------------|
| **FOpen** | 0x30 | STR = filename<br>NEXT = mode ("w"/"r") | TOP = FILE*/NULL | Open file |
| **FClose** | 0x31 | NEXT = FILE* | TOP = 0/-1 | Close file |
| **FGetC** | 0x32 | NEXT = FILE* | TOP = char(0-255)/-1 | Read character |
| **FRead** | 0x33 | IDX = buffer<br>IDY = elem size<br>ACC = count<br>NEXT = FILE* | TOP = bytes read/-1 | Read data |
| **FPutC** | 0x34 | ACC = char<br>NEXT = FILE* | TOP = char/-1 | Write character |
| **FWrite** | 0x35 | IDX = buffer<br>IDY = elem size<br>ACC = count<br>NEXT = FILE* | TOP = bytes written/-1 | Write data |

## Usage Examples

### Memory Allocation
```hopper
// Allocate 256 bytes
LDA #0x00
STA ZP.ACCL
LDA #0x01
STA ZP.ACCH
LDX #BIOSInterface.SysCall.MemAllocate
JSR [ZP.BIOSDISPATCH]
if (NC)
{
    // Handle allocation failure
    return;
}
// ZP.IDX now contains pointer to allocated memory
```

### Print String
```hopper
// Print a message
LDA #(message % 256)
STA ZP.STRL
LDA #(message / 256)
STA ZP.STRH
LDX #BIOSInterface.SysCall.PrintString
JSR [ZP.BIOSDISPATCH]
```

### GPIO Control
```hopper
// Set pin 5 as output
LDA #5          // Pin number
LDY #1          // OUTPUT mode
LDX #BIOSInterface.SysCall.PinMode
JSR [ZP.BIOSDISPATCH]

// Write HIGH to pin 5
LDA #5          // Pin number
LDY #1          // HIGH value
LDX #BIOSInterface.SysCall.PinWrite
JSR [ZP.BIOSDISPATCH]
```

### 32-bit Math
```hopper
// Add two 32-bit numbers
// Load first number into NEXT
LDA #(value1 & 0xFF)
STA ZP.NEXT0
LDA #((value1 >> 8) & 0xFF)
STA ZP.NEXT1
LDA #((value1 >> 16) & 0xFF)
STA ZP.NEXT2
LDA #((value1 >> 24) & 0xFF)
STA ZP.NEXT3

// Load second number into TOP
// ... similar for TOP0-3 ...

// Perform addition
LDX #BIOSInterface.SysCall.LongAdd
JSR [ZP.BIOSDISPATCH]
// Result is now in ZP.NEXT
```

## Error Handling

Most system calls return status via the Carry flag:
- **C set (SEC)**: Operation successful
- **C clear (CLC)**: Operation failed

The LastError byte (0x01) contains additional error information when operations fail.

## Platform Variations

The BIOS adapts to different hardware configurations:

### Hardware Platforms
- **BENEATER_IO**: VIA at 0x6000, ACIA at 0x5000
- **X16_IO**: VIA at 0x9F20, ACIA at 0x9F10
- **MECB6502_IO**: VIA at 0xF000, ACIA at 0xF008
- **ZEROPAGE_IO**: VIA at 0xF0, ACIA at 0xEC (default)

### ROM Configurations
- **ROM_32K**: Origin at 0x8000 (default)
- **ROM_16K**: Origin at 0xC000
- **ROM_8K**: Origin at 0xE000
- **ROM_4K**: Origin at 0xF000
- **ROM_1K**: Origin at 0xFC00

### CPU Targets
- **CPU_6502**: Original MOS 6502 instruction set
- **CPU_65C02S**: Enhanced 65C02S instruction set
- **CPU_65UINO**: Similar to ROM_4K and CPU_6502

## Future Enhancements

The TODO comment in BIOSInterface.asm indicates I2C support is planned for future versions.