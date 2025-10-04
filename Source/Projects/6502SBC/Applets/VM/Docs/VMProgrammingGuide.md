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
    [Return PC (Y)]         ; 1 byte - PC within calling function's page
    [Return Page Low]       ; 2 bytes - calling function's page address
    [Return Page High]      
    [Saved BP]              ; Previous base pointer (saved at BP-1)
    [Local 1]               ; <-- BP points here (first local at BP+0)
    [Local 2]               ; BP-1 (second local)
    [Local 3]               ; BP-2 (third local)
    ...                     ; <-- SP points here
Low Memory (Lower addresses)
```

### Critical Stack Frame Facts:
- **BP points to the first local variable** (not to saved BP)
- **Saved BP is at BP-1** (one byte below BP)
- **First local variable is at BP+0**
- **Arguments start at BP+3** for single-byte values
- **Return address is 3 bytes total**: Y (1 byte) + codePage (2 bytes)

### Accessing Variables:
```asm
; Arguments (positive offsets from BP):
PUSHLB 3            ; First 8-bit argument
PUSHLB 4            ; Second 8-bit argument
PUSHLW 3            ; First 16-bit argument (uses BP+3 and BP+4)

; Local variables (BP+0 and negative offsets):
PUSHLB 0            ; First local (at BP+0)
PUSHLB -1           ; Second local
PUSHLB -2           ; Third local
POPLW 0             ; Store 16-bit at first two locals (BP+0, BP-1)
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
POPZW ZP.TOP+2
POPZW ZP.TOP
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

## Complete Instruction Set

### Stack Operations - Constants
```asm
PUSHB  value        ; Push 8-bit immediate value
PUSHB0              ; Push 8-bit zero (optimized)
PUSHB1              ; Push 8-bit one (optimized)
PUSHW  value        ; Push 16-bit immediate value
PUSHW0              ; Push 16-bit zero (optimized)
PUSHD  label        ; Push address of label (for strings/data)
```

### Stack Operations - Zero Page
```asm
PUSHZB address      ; Push 8-bit value from zero page
PUSHZW address      ; Push 16-bit value from zero page
POPZB  address      ; Pop 8-bit value to zero page
POPZW  address      ; Pop 16-bit value to zero page
```

### Stack Operations - Locals
```asm
PUSHLB offset       ; Push 8-bit local (BP+offset)
PUSHLW offset       ; Push 16-bit local (BP+offset)
POPLB  offset       ; Pop 8-bit to local (BP+offset)
POPLW  offset       ; Pop 16-bit to local (BP+offset)
INCLB  offset       ; Increment 8-bit local
INCLW  offset       ; Increment 16-bit local
```

### Stack Manipulation
```asm
DUPB                ; Duplicate 8-bit TOS
DUPW                ; Duplicate 16-bit TOS
DROPB               ; Remove 8-bit from TOS
DROPW               ; Remove 16-bit from TOS
POPA                ; Pop 8-bit to A register
```

### Memory Operations
```asm
READB               ; Read byte from [TOS] address
WRITEB              ; Write byte TOS-1 to [TOS] address
```

### Arithmetic
```asm
ADDB/ADDW           ; Add top two stack values
SUBB/SUBW           ; Subtract TOS from TOS-1
```

### Comparison
```asm
EQB/EQW             ; Compare equal (sets Z flag)
NEW                 ; Compare not equal (sets Z flag)
LTB/LTW             ; Less than (unsigned)
```

### Control Flow
```asm
BZF  label          ; Branch if Z flag false (NZ)
BNZF label          ; Branch if Z flag true (Z)
BZR  label          ; Branch if Z flag false backward
BNZR label          ; Branch if Z flag true backward
BCF  label          ; Branch if C flag false (NC)
BNCF label          ; Branch if C flag true (C)
BRAR label          ; Branch always backward (for loops)
```

### Function Operations
```asm
ENTER bytes         ; Setup stack frame, allocate locals
LEAVE               ; Restore stack frame
CALL function       ; Call function by name
RET                 ; Return from function
```

### System Operations
```asm
SYSCALL name        ; Call BIOS function
HALT                ; End program
DUMP                ; Debug stack dump (see below)
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
    
    ; Arguments at BP+3, BP+4
    PUSHLB 3            ; High byte
    POPA
    SYSCALL Print.Hex
    
    PUSHLB 4            ; Low byte
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

## Critical Programming Rules

### 1. Stack Frame Offsets Are Critical
```asm
ENTER 2             ; Allocates 2 bytes
; First local:  BP+0
; Second local: BP-1
; First arg:    BP+3 (8-bit) or BP+3,BP+4 (16-bit)
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

## Common Mistakes to Avoid

### 1. Wrong Local Variable Offset
```asm
; WRONG - Negative offset for first local
ENTER 2
PUSHLB -1           ; NO! First local is at BP+0

; CORRECT
ENTER 2
PUSHLB 0            ; First local at BP+0
```

### 2. Forgetting Stack Cleanup
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

### 3. Not Marshalling to Zero Page
```asm
; WRONG
SYSCALL FGetC       ; No handle in NEXT!

; CORRECT
PUSHZW handle
POPZW ZP.NEXT
SYSCALL FGetC
```

### 4. Assuming Zero Page Preserved
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

## Summary

The Hopper VM provides exceptional code density (8-10× better than native 6502) through:
1. Stack-based computation model
2. Direct hardware stack usage
3. Efficient BIOS integration via zero page marshalling
4. Page-constrained functions
5. Compact bytecode representation

Key concepts to master:
- Stack frame layout (BP+0 for first local, BP-1 for saved BP)
- Zero page marshalling for BIOS calls
- SYSCALL parameter conventions
- Stack cleanup responsibilities
- DUMP opcode for debugging

The three example programs (Type.VMA, ZP.VMA, HexDump.VMA) demonstrate all essential patterns for successful VM programming.