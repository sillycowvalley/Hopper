# Hopper 6502 VM Specification - Complete Programming Guide

## Overview
A stack-based virtual machine for 6502 processors that provides exceptional code density (8-10× better than native 6502) while maintaining direct hardware access through BIOS syscalls. The VM uses the hardware stack directly and constrains functions to single pages for optimal performance.

## Core Programming Principles

### 1. Stack-Based Architecture
The VM is purely stack-based - all operations work through the hardware stack. There are no VM registers except for marshalling data to BIOS calls via zero page.

### 2. Direct BIOS Integration
System functionality is accessed through SYSCALL instructions that invoke BIOS functions directly. Data is marshalled through specific zero page locations before the syscall.

### 3. Program Structure
Every VM program consists of:
- `.CONST` section - Define constants and zero page mappings
- `.DATA` section - String constants
- `.FUNC` sections - User-defined functions
- `.MAIN` section - Program entry point

## Complete Zero Page Map for VM Programs

```asm
; BIOS Core Marshalling Registers (0x10-0x1F)
ZP.ACC       0x10-0x11  ; 16-bit accumulator (size, count parameters)
  ZP.ACCL    0x10       ; Low byte
  ZP.ACCH    0x11       ; High byte

ZP.TOP       0x12-0x15  ; 32-bit TOP value (return values, math operand)
  ZP.TOP0    0x12       ; Byte 0 (lowest)
  ZP.TOP1    0x13       ; Byte 1
  ZP.TOP2    0x14       ; Byte 2
  ZP.TOP3    0x15       ; Byte 3 (highest)

ZP.NEXT      0x16-0x19  ; 32-bit NEXT value (parameters, math operand)
  ZP.NEXT0   0x16       ; Byte 0 (lowest)
  ZP.NEXT1   0x17       ; Byte 1
  ZP.NEXT2   0x18       ; Byte 2
  ZP.NEXT3   0x19       ; Byte 3 (highest)

ZP.IDX       0x1A-0x1B  ; 16-bit index/pointer (memory addresses)
  ZP.IDXL    0x1A       ; Low byte
  ZP.IDXH    0x1B       ; High byte

ZP.IDY       0x1C-0x1D  ; 16-bit secondary index
  ZP.IDYL    0x1C       ; Low byte
  ZP.IDYH    0x1D       ; High byte

ZP.STR       0x1E-0x1F  ; 16-bit string pointer
  ZP.STRL    0x1E       ; Low byte
  ZP.STRH    0x1F       ; High byte

; Extended Workspace (for your use in programs)
ZP.BUF       0x80       ; Common buffer pointer location
```

## Stack Frame Layout

When a function is called, the stack frame is structured as follows:

```
High Memory (Higher addresses)
    [Argument N]            ; Last argument pushed
    ...
    [Argument 2]            ; Second argument
    [Argument 1]            ; First argument pushed
    [Return Y]              ; 1 byte - PC within calling function's page
    [Return Page Low]       ; 2 bytes - calling function's page address
    [Return Page High]      
    [Saved BP]              ; Previous base pointer
    [Local 1]               ; <-- BP points here (first local at BP+0)
    [Local 2]               ; BP-1 (second local)
    [Local 3]               ; BP-2 (third local)
    ...                     ; <-- SP points here
Low Memory (Lower addresses)
```

### Critical Stack Frame Facts:
- **BP points to the first local variable** (not to saved BP)
- **Saved BP is at BP+1** (one byte above BP)
- **First local variable is at BP+0**
- **Arguments start at BP+5** for single-byte values
- **Return address is 3 bytes total**: Y (1 byte) + codePage (2 bytes)
- **Saved BP is at BP+1** (between locals and return address)

### Accessing Variables:
```asm
; Arguments (positive offsets from BP):
PUSHLB 5            ; First 8-bit argument at BP+5
PUSHLB 6            ; Second 8-bit argument at BP+6
PUSHLW 5            ; First 16-bit argument (uses BP+5 and BP+6)
PUSHLW 7            ; Second 16-bit argument (uses BP+7 and BP+8)

; Local variables (BP+0 and negative offsets):
PUSHLB 0            ; First local (at BP+0)
PUSHLB -1           ; Second local at BP-1
PUSHLB -2           ; Third local at BP-2
POPLW 0             ; Store 16-bit at first two locals (BP+0, BP-1)
```

## Complete VM Instruction Set

### System Operations (0x00-0x02)
```asm
NOP      0x00       ; No operation (MUST be 0x00)
HALT     0x02       ; Stop execution, return to BIOS
```

### Stack Operations - Immediates (0x04-0x12)
```asm
PUSHB    0x04 byte  ; Push 8-bit immediate value
PUSHB0   0x06       ; Push 8-bit zero (optimized)
PUSHB1   0x08       ; Push 8-bit one (optimized)
PUSHW    0x0A word  ; Push 16-bit immediate value  
PUSHW0   0x0C       ; Push 16-bit zero (optimized)
PUSHW1   0x0E       ; Push 16-bit one (optimized)
PUSHA    0x10       ; Push A register to stack
PUSHC    0x12       ; Push carry flag (1 if set, 0 if clear)
```

### Stack Operations - Manipulation (0x14-0x20)
```asm
PUSHZ    0x14       ; Push zero flag (1 if set, 0 if clear)
DUPB     0x16       ; Duplicate byte on TOS
DUPW     0x18       ; Duplicate word on TOS
DROPB    0x1A       ; Remove byte from TOS
DROPW    0x1C       ; Remove word from TOS
SWAPB    0x1E       ; Swap top two bytes
SWAPW    0x20       ; Swap top two words
```

### Arithmetic Operations (0x24-0x32)
```asm
ADDB     0x24       ; Pop 2 bytes, push sum
SUBB     0x26       ; Pop 2 bytes, push difference (TOS-1 - TOS)
NEGB     0x28       ; Negate byte (2's complement)
ADDW     0x2A       ; Pop 2 words, push sum
SUBW     0x2C       ; Pop 2 words, push difference
NEGW     0x2E       ; Negate word (2's complement)
INCLB    0x30 offset; Increment local byte at BP+offset
INCLW    0x32 offset; Increment local word at BP+offset
```

### Comparison Operations (0x34-0x42)
```asm
EQB      0x34       ; Pop 2 bytes, set Z if equal
NEB      0x36       ; Pop 2 bytes, set Z if not equal
LTB      0x38       ; Pop 2 bytes, set C if TOS-1 < TOS (unsigned)
LEB      0x3A       ; Pop 2 bytes, set C if TOS-1 <= TOS (unsigned)
EQW      0x3C       ; Pop 2 words, set Z if equal
NEW      0x3E       ; Pop 2 words, set Z if not equal
LTW      0x40       ; Pop 2 words, set C if TOS-1 < TOS (unsigned)
LEW      0x42       ; Pop 2 words, set C if TOS-1 <= TOS (unsigned)
```

### Bitwise Operations (0x44-0x50)
```asm
ANDB     0x44       ; Pop 2 bytes, push bitwise AND
ORB      0x46       ; Pop 2 bytes, push bitwise OR
XORB     0x48       ; Pop 2 bytes, push bitwise XOR
NOTB     0x4A       ; Pop byte, push bitwise NOT
XORW     0x4C       ; Pop 2 words, push bitwise XOR
SHLW     0x4E byte  ; Shift word left by byte operand
SHRW     0x50 byte  ; Shift word right by byte operand
```

### Zero Page Operations (0x54-0x62)
```asm
PUSHZB   0x54 addr  ; Push byte from ZP[addr]
PUSHZW   0x56 addr  ; Push word from ZP[addr]
PUSHZQ   0x58 addr  ; Push 32-bit from ZP[addr]
POPZB    0x5A addr  ; Pop byte to ZP[addr]
POPZW    0x5C addr  ; Pop word to ZP[addr]
POPZQ    0x5E addr  ; Pop 32-bit to ZP[addr]
POPA     0x60       ; Pop byte from stack to A register
POPY     0x62       ; Pop byte from stack to Y register
```

### Local Variable Operations (0x64-0x6E)
```asm
PUSHLB   0x64 offset; Push byte from BP+offset
PUSHLW   0x66 offset; Push word from BP+offset
PUSHLQ   0x68 offset; Push 32-bit from BP+offset
POPLB    0x6A offset; Pop byte to BP+offset
POPLW    0x6C offset; Pop word to BP+offset
POPLQ    0x6E offset; Pop 32-bit to BP+offset
```

### Global Operations (0x70-0x76)
```asm
PUSHGB   0x70 offset; Push byte from globals+offset
PUSHGW   0x72 offset; Push word from globals+offset
POPGB    0x74 offset; Pop byte to globals+offset
POPGW    0x76 offset; Pop word to globals+offset
```

### Control Flow (0x7C-0x8A)
```asm
BRAF     0x7C byte  ; Branch forward by byte (0-255)
BRAR     0x7E byte  ; Branch backward by byte (0-255)
BZF      0x80 byte  ; Branch forward if Z flag set
BZR      0x82 byte  ; Branch backward if Z flag set
BNZF     0x84 byte  ; Branch forward if Z flag clear
BNZR     0x86 byte  ; Branch backward if Z flag clear
CALL     0x88 id    ; Call function by ID
RET      0x8A       ; Return from function
```

### System & Stack Frame (0x8C-0x94)
```asm
SYSCALL  0x8C id    ; Call BIOS function by ID
SYSCALLX 0x8E       ; Call BIOS function (fast, X preset)
ENTER    0x90 bytes ; Setup stack frame with local space
LEAVE    0x92       ; Restore stack frame
DUMP     0x94       ; Diagnostic stack dump
```

### String/Data Operations (0x98-0x9E)
```asm
PUSHD    0x98 byte  ; Push data address (byte offset)
PUSHD2   0x9A word  ; Push data address (word offset)
STRC     0x9C       ; Pop index, pop string, push char
STRCMP   0x9E       ; Pop 2 strings, push comparison result
```

### Memory Operations (0xA0-0xA2)
```asm
READB    0xA0       ; Pop address word, push byte from memory
WRITEB   0xA2       ; Pop address, pop byte, write to memory
```

## Complete BIOS System Call Reference

### Memory Management

#### MemAllocate (0x00)
```asm
; Allocate memory block from heap
; Input:  ZP.ACC = size in bytes (16-bit)
; Output: ZP.IDX = allocated address
;         C = 1 if successful, 0 if failed
PUSHW size
POPZW ZP.ACC
SYSCALL MemAllocate
BCC allocation_failed
; ZP.IDX now contains pointer
```

#### MemFree (0x01)
```asm
; Free previously allocated memory
; Input:  ZP.IDX = address to free
; Output: C = 1 if successful, 0 if failed
PUSHZW pointer
POPZW ZP.IDX
SYSCALL MemFree
```

#### MemAvailable (0x02)
```asm
; Get available free memory
; Input:  None
; Output: ZP.ACC = free bytes available
SYSCALL MemAvailable
PUSHZW ZP.ACC       ; Get result
```

#### MemMaximum (0x03)
```asm
; Get largest contiguous free block
; Input:  None
; Output: ZP.ACC = largest block size
SYSCALL MemMaximum
```

### File Operations

#### FileExists (0x04)
```asm
; Check if file exists
; Input:  ZP.STR = filename pointer
;         A = DirWalkAction
; Output: C = 1 if exists, 0 if not
PUSHD filename
POPZW ZP.STR
LDA #0              ; DirWalkAction
SYSCALL FileExists
```

#### FileDelete (0x05)
```asm
; Delete file from storage
; Input:  ZP.STR = filename pointer
; Output: C = 1 if successful, 0 if failed
PUSHD filename
POPZW ZP.STR
SYSCALL FileDelete
```

#### FileDir (0x06)
```asm
; List directory contents to serial
; Input:  None
; Output: C = 1 if successful
;         (output goes to serial port)
SYSCALL FileDir
```

#### FileStartSave (0x07)
```asm
; Open file for writing
; Input:  ZP.STR = filename pointer
; Output: C = 1 if successful
PUSHD filename
POPZW ZP.STR
SYSCALL FileStartSave
```

#### FileAppendStream (0x08)
```asm
; Write data chunk to open file
; Input:  SectorSource = data pointer
;         TransferLength = byte count
; Output: C = 1 if successful
```

#### FileEndSave (0x09)
```asm
; Close and finalize file
; Input:  A = 0x80 (executable) or 0x00 (data)
; Output: C = 1 if successful
LDA #0x00           ; Data file
SYSCALL FileEndSave
```

#### FileStartLoad (0x0A)
```asm
; Open file for reading
; Input:  ZP.STR = filename pointer
;         A = DirWalkAction
; Output: C = 1 if successful
PUSHD filename
POPZW ZP.STR
LDA #0
SYSCALL FileStartLoad
```

#### FileNextStream (0x0B)
```asm
; Read next chunk from open file
; Input:  None
; Output: C = 1 if data available
;         TransferLength = bytes read
;         Data in FileDataBuffer
SYSCALL FileNextStream
BCC end_of_file
```

#### FileFormat (0x0C)
```asm
; Format storage system
; Input:  None
; Output: C = 1 if successful
SYSCALL FileFormat
```

### Serial I/O

#### SerialWriteChar (0x0D)
```asm
; Write character to serial port
; Input:  A = character
; Output: None
PUSHB 'A'
POPA
SYSCALL SerialWriteChar
```

#### SerialWaitForChar (0x0E)
```asm
; Block until character available
; Input:  None
; Output: A = character received
SYSCALL SerialWaitForChar
; A now contains the character
```

#### SerialIsAvailable (0x0F)
```asm
; Check if serial input available
; Input:  None
; Output: C = 1 if available
SYSCALL SerialIsAvailable
BCC no_input
```

### Break Detection

#### IsBreak (0x10)
```asm
; Check for Ctrl+C or NMI break
; Input:  None
; Output: C = 1 if break detected
SYSCALL IsBreak
BCS user_break
```

### Console Output

#### PrintString (0x11)
```asm
; Print null-terminated string
; Input:  ZP.STR = string pointer
; Output: None
PUSHD "Hello World\n"
POPZW ZP.STR
SYSCALL PrintString
```

#### PrintChar (0x12)
```asm
; Print single character
; Input:  A = character
; Output: None
PUSHB 'X'
POPA
SYSCALL PrintChar
```

#### PrintHex (0x13)
```asm
; Print byte as 2 hex digits
; Input:  A = byte value
; Output: None (prints to serial)
PUSHB 0xA5
POPA
SYSCALL PrintHex    ; Prints "A5"
```

#### PrintNewLine (0x14)
```asm
; Print newline character
; Input:  None
; Output: None
SYSCALL PrintNewLine
```

#### PrintSpace (0x15)
```asm
; Print single space
; Input:  None
; Output: None
SYSCALL PrintSpace
```

#### PrintSpaces (0x16)
```asm
; Print multiple spaces
; Input:  Y = space count
; Output: None
LDY #8
SYSCALL PrintSpaces  ; Print 8 spaces
```

### Timer Services

#### TimeDelay (0x17)
```asm
; Delay execution
; Input:  ZP.TOP = milliseconds (32-bit)
; Output: None
PUSHW 1000          ; 1 second
PUSHW0              ; High word
POPZQ ZP.TOP
SYSCALL TimeDelay
```

#### TimeMillis (0x18)
```asm
; Get millisecond counter
; Input:  None
; Output: ZP.TOP = ms since boot (32-bit)
SYSCALL TimeMillis
; Result in ZP.TOP (4 bytes)
```

#### TimeSeconds (0x19)
```asm
; Get seconds since boot
; Input:  None
; Output: ZP.TOP = seconds (32-bit)
SYSCALL TimeSeconds
```

### 32-bit Integer Math

All long math operations use ZP.NEXT and ZP.TOP as 32-bit operands.

#### LongAdd (0x1A)
```asm
; 32-bit addition
; Input:  ZP.NEXT, ZP.TOP
; Output: ZP.NEXT = NEXT + TOP
```

#### LongSub (0x1B)
```asm
; 32-bit subtraction
; Input:  ZP.NEXT, ZP.TOP
; Output: ZP.NEXT = NEXT - TOP
```

#### LongMul (0x1C)
```asm
; 32-bit multiplication
; Input:  ZP.NEXT, ZP.TOP
; Output: ZP.NEXT = NEXT * TOP
```

#### LongDiv (0x1D)
```asm
; 32-bit division
; Input:  ZP.NEXT, ZP.TOP
; Output: ZP.NEXT = NEXT / TOP
```

#### LongMod (0x1E)
```asm
; 32-bit modulo
; Input:  ZP.NEXT, ZP.TOP
; Output: ZP.NEXT = NEXT % TOP
```

#### LongPrint (0x1F)
```asm
; Print 32-bit decimal number
; Input:  ZP.TOP = value to print
; Output: None (prints to serial)
```

#### Long Comparisons (0x20-0x25)
```asm
; All comparisons:
; Input:  ZP.NEXT, ZP.TOP
; Output: C = comparison result

LongLT  ; C = 1 if NEXT < TOP
LongGT  ; C = 1 if NEXT > TOP  
LongEQ  ; C = 1 if NEXT == TOP
LongNE  ; C = 1 if NEXT != TOP
LongLE  ; C = 1 if NEXT <= TOP
LongGE  ; C = 1 if NEXT >= TOP
```

### GPIO Operations

#### PinMode (0x26)
```asm
; Configure pin direction
; Input:  A = pin number (0-15)
;         Y = mode (0=INPUT, 1=OUTPUT)
; Output: None
LDA #5
LDY #1              ; OUTPUT
SYSCALL PinMode
```

#### PinRead (0x27)
```asm
; Read digital pin state
; Input:  A = pin number (0-15)
; Output: A = value (0/1)
;         Z = 1 if LOW
LDA #5
SYSCALL PinRead
BEQ pin_is_low
```

#### PinWrite (0x28)
```asm
; Write digital pin state
; Input:  A = pin number (0-15)
;         Y = value (0/1)
; Output: None
LDA #5
LDY #1              ; HIGH
SYSCALL PinWrite
```

### Float Math (Optional, requires HASFLOAT)

All float operations use IEEE 754 single precision format in ZP.NEXT and ZP.TOP.

#### FloatAdd (0x29)
```asm
; Float addition
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: ZP.NEXT = NEXT + TOP
```

#### FloatSub (0x2A)
```asm
; Float subtraction
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: ZP.NEXT = NEXT - TOP
```

#### FloatMul (0x2B)
```asm
; Float multiplication
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: ZP.NEXT = NEXT * TOP
```

#### FloatDiv (0x2C)
```asm
; Float division
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: ZP.NEXT = NEXT / TOP
```

#### FloatToLong (0x2D)
```asm
; Convert float to long
; Input:  ZP.NEXT (IEEE float)
; Output: ZP.NEXT = (long)NEXT
```

#### FloatLT (0x2E)
```asm
; Float less than
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: C = 1 if NEXT < TOP
```

#### FloatEQ (0x2F)
```asm
; Float equality
; Input:  ZP.NEXT, ZP.TOP (IEEE floats)
; Output: C = 1 if NEXT == TOP
```

### File Handle Operations

#### FOpen (0x30)
```asm
; Open file with mode
; Input:  ZP.STR = filename pointer
;         ZP.NEXT = mode string pointer ("r" or "w")
; Output: ZP.TOP = file handle or NULL
PUSHD filename
POPZW ZP.STR
PUSHD "r"
POPZW ZP.NEXT
SYSCALL FOpen
; Check ZP.TOP for NULL
```

#### FClose (0x31)
```asm
; Close file handle
; Input:  ZP.NEXT = file handle
; Output: ZP.TOP = 0 on success, -1 on error
PUSHZW handle
POPZW ZP.NEXT
SYSCALL FClose
```

#### FGetC (0x32)
```asm
; Read character from file
; Input:  ZP.NEXT = file handle
; Output: ZP.TOP = character (0-255) or -1 for EOF
PUSHZW handle
POPZW ZP.NEXT
SYSCALL FGetC
; Check for 0xFFFF (EOF)
```

#### FRead (0x33)
```asm
; Read data from file
; Input:  ZP.IDX = buffer pointer
;         ZP.IDY = element size
;         ZP.ACC = element count
;         ZP.NEXT = file handle
; Output: ZP.TOP = bytes read or -1 on error
```

#### FPutC (0x34)
```asm
; Write character to file
; Input:  ZP.ACC = character (0-255)
;         ZP.NEXT = file handle
; Output: ZP.TOP = character written or -1
PUSHB 'A'
POPZB ZP.ACC
PUSHZW handle
POPZW ZP.NEXT
SYSCALL FPutC
```

#### FWrite (0x35)
```asm
; Write data to file
; Input:  ZP.IDX = buffer pointer
;         ZP.IDY = element size
;         ZP.ACC = element count
;         ZP.NEXT = file handle
; Output: ZP.TOP = bytes written or -1
```

### Command Line Arguments

#### ArgCount (0x36)
```asm
; Get argument count
; Input:  None
; Output: A = number of arguments (including command name)
SYSCALL ArgCount
; A contains count
```

#### ArgGet (0x37)
```asm
; Get argument by index
; Input:  A = argument index (0=command, 1=first arg, etc.)
; Output: ZP.STR = pointer to argument string
PUSHB 1             ; First argument
POPA
SYSCALL ArgGet
; ZP.STR now points to argument
```

## Debugging with DUMP Opcode

The DUMP opcode is a powerful debugging tool that displays the current stack state without affecting any registers, flags, or stack contents.

### What DUMP Shows
- Stack contents from BP+10 down to BP-10
- Current BP location marked with `<-BP`
- Current SP location marked with `<-SP` (or `<-SP/BP` if they coincide)
- Memory addresses and their hex values

### Example DUMP Output
```
Stack dump:
011A: A5
011B: 00
011C: 20
011D: FF         <-BP
011E: 03
011F:            <-SP
```

### Using DUMP in Your Code
```asm
.FUNC ProcessData
    ENTER 2         ; Allocate locals
    
    DUMP            ; See initial stack state
    
    PUSHB 0x42
    POPLB 0         ; Store in first local
    
    DUMP            ; See stack after storing local
    
    CALL Helper
    
    DUMP            ; See stack after return
    
    LEAVE
    RET
```

### Key DUMP Features
- **Non-invasive**: Preserves all registers (A, X, Y) and flags
- **Safe to use anywhere**: Can be inserted between any instructions
- **Shows context**: Displays offsets relative to BP for easy debugging
- **Marks key locations**: SP and BP are clearly indicated

## Programming Patterns from Working Examples

### Pattern 1: File Reading (from Type.VMA)
```asm
.CONST
    ZP.STR       0x1E
    ZP.TOP       0x12
    ZP.NEXT      0x16
    
    Print.Char   0x12
    FOpen        0x30
    FClose       0x31
    FGetC        0x32
    ArgGet       0x37

.DATA
    STR0 "r"

.MAIN
    ; Get filename from command line
    PUSHB 2
    POPA
    SYSCALL ArgGet      ; Sets ZP.STR
    
    ; Open file
    PUSHD STR0
    POPZW ZP.NEXT
    SYSCALL FOpen       ; Returns handle in TOP
    
    ; Check success
    PUSHZW ZP.TOP
    PUSHW0
    NEW
    BZF file_error
    
read_loop:
    ; Read character
    PUSHZW ZP.TOP       ; File handle
    POPZW ZP.NEXT
    SYSCALL FGetC
    
    ; Check EOF
    PUSHZW ZP.TOP
    PUSHW 0xFFFF
    EQW
    BNZF close_file
    
    ; Print character
    PUSHZB ZP.TOP0
    POPA
    SYSCALL Print.Char
    
    BRAR read_loop
    
close_file:
    PUSHZW ZP.TOP
    POPZW ZP.NEXT
    SYSCALL FClose
    
file_error:
    HALT
```

### Pattern 2: Functions with Local Variables (from ZP.VMA)
```asm
.FUNC PrintAddress
    ENTER 0             ; No locals
    
    ; Arguments at BP+5, BP+6
    PUSHLB 5            ; High byte
    POPA
    SYSCALL Print.Hex
    
    PUSHLB 6            ; Low byte
    POPA
    SYSCALL Print.Hex
    
    LEAVE
    RET

.MAIN
    ENTER 2             ; Two byte locals
    
    ; Initialize local at BP+0
    PUSHW0
    POPLW 0             ; addr = 0
    
    ; Call function
    PUSHLW 0            ; Push argument
    CALL PrintAddress
    DROPW               ; Clean up
    
    LEAVE
    HALT
```

### Pattern 3: Memory Allocation (from HexDump.VMA)
```asm
; Allocate buffer
PUSHW 16
POPZW ZP.ACC
SYSCALL Mem.Allocate
BCC alloc_failed
PUSHZW ZP.IDX       ; Get pointer
POPZW ZP.BUF        ; Save it

; Use buffer...

; Free buffer
PUSHZW ZP.BUF
POPZW ZP.IDX
SYSCALL Mem.Free
```

### Pattern 4: Working with Globals
```asm
; Define global variables in .CONST
.CONST
    counter  0      ; Global at offset 0
    total    1      ; Global at offset 1 (word)

.MAIN
    ; Initialize global counter
    PUSHB 0
    POPGB counter
    
    ; Increment global total
    PUSHGW total
    PUSHW 10
    ADDW
    POPGW total
```

### Pattern 5: String Operations
```asm
; Using PUSHD for string addresses
PUSHD "Hello"       ; Push string address
POPZW ZP.STR
SYSCALL Print.String

; String character access with STRC
PUSHD message       ; Push string address
PUSHB 5             ; Index
STRC                ; Get 6th character
POPA
SYSCALL Print.Char

; String comparison
PUSHD str1
PUSHD str2
STRCMP              ; Returns -1/0/1
```

### Pattern 6: Bitwise Operations
```asm
; Masking bits
PUSHB 0xF0
PUSHB 0x37
ANDB                ; Result: 0x30

; Setting bits
PUSHB 0x40
PUSHB 0x05
ORB                 ; Result: 0x45

; Toggle bits
PUSHB 0xFF
PUSHB 0xA5
XORB                ; Result: 0x5A

; Shift operations
PUSHW 0x0001
SHLW 8              ; Result: 0x0100
```

## Critical Programming Rules

### 1. Stack Frame Offsets Are Critical
```asm
ENTER 2             ; Allocates 2 bytes
; First local:  BP+0
; Second local: BP-1
; First 8-bit arg:  BP+5
; Second 8-bit arg: BP+6
; First 16-bit arg: BP+5, BP+6
```

### 2. Always Initialize Local Variables
```asm
ENTER 2
PUSHB0
POPLB 0             ; Initialize first local
PUSHB0
POPLB -1            ; Initialize second local
```

### 3. Zero Page Marshalling is Mandatory
Before any SYSCALL, data MUST be in the correct zero page location:
```asm
; WRONG
PUSHD "Hello"
SYSCALL Print.String    ; FAILS!

; CORRECT
PUSHD "Hello"
POPZW ZP.STR
SYSCALL Print.String
```

### 4. Check SYSCALL Results
Most syscalls return status in carry flag or ZP.TOP:
```asm
SYSCALL Operation
BCC failed          ; Carry clear = failure
```

### 5. Clean Up Stack After Calls
```asm
PUSHW argument
CALL Function
DROPW               ; Caller cleans up!
```

### 6. The C Flag Trap with CMP
CMP sets carry OPPOSITE to intuition:
- C=1 if A >= value (no borrow)
- C=0 if A < value (borrow occurred)

Use EQB/EQW for equality tests to avoid confusion.

### 7. Branch Instructions and Flags
- BZF/BZR branch when Z flag is FALSE (zero flag clear)
- BNZF/BNZR branch when Z flag is TRUE (zero flag set)
- This is opposite to what the names might suggest!

## Common Mistakes to Avoid

### 1. Wrong Argument Offset
```asm
; WRONG - Using BP+3 for first argument
ENTER 2
PUSHLB 3           ; NO! First argument is at BP+5

; CORRECT
ENTER 2
PUSHLB 5            ; First argument at BP+5
```

### 2. Wrong Local Variable Offset
```asm
; WRONG - Negative offset for first local
ENTER 2
PUSHLB -1           ; NO! First local is at BP+0

; CORRECT
ENTER 2
PUSHLB 0            ; First local at BP+0
```

### 3. Forgetting Stack Cleanup
```asm
; WRONG
PUSHW value
CALL Function
; Stack now corrupted!

; CORRECT
PUSHW value
CALL Function
DROPW               ; Clean up argument
```

### 4. Not Marshalling to Zero Page
```asm
; WRONG
SYSCALL FGetC       ; No handle in NEXT!

; CORRECT
PUSHZW handle
POPZW ZP.NEXT
SYSCALL FGetC
```

### 5. Assuming Zero Page Preserved
Zero page marshalling registers are volatile:
```asm
; WRONG
POPZW ZP.STR
SYSCALL Something   ; May trash ZP.STR!
SYSCALL Print.String ; STR corrupted!

; CORRECT
POPZW ZP.STR
SYSCALL Print.String ; Use immediately
```

### 6. Confusing BZF/BNZF Behavior
```asm
; After EQW instruction:
; Z=1 if values were equal
; Z=0 if values were different

EQW
BZF not_equal      ; Branches if Z=0 (values different)
BNZF equal         ; Branches if Z=1 (values same)
```

## Summary

The Hopper VM provides exceptional code density (8-10× better than native 6502) through:
1. Stack-based computation model
2. Direct hardware stack usage
3. Efficient BIOS integration via zero page marshalling
4. Page-constrained functions
5. Compact bytecode representation

Key concepts to master:
- Stack frame layout (**BP+5 for first argument**, BP+0 for first local, BP+1 for saved BP)
- Complete instruction set including bitwise, global, and 32-bit operations
- Zero page marshalling for BIOS calls
- SYSCALL parameter conventions
- Stack cleanup responsibilities
- DUMP opcode for debugging
- Branch flag behavior (BZF branches when Z is false)

Remember: Arguments always start at BP+5, not BP+3! This is critical for correct function parameter access.