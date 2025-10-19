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
- `.CONST` section - Define constants, zero page mappings, and global variable offsets
- `.DATA` section - String constants, byte arrays, and word arrays
- `.FUNC` sections - User-defined functions
- `.MAIN` section - Program entry point

### 4. Label Syntax
Labels are defined with a colon suffix and used with branch instructions:
```asm
start:
    ; code here
    BRAF end        ; Branch forward to 'end'
loop:
    ; loop body
    BRAR loop       ; Branch backward to 'loop'
end:
    HALT
```

### 5. Comment Style Guidelines
Use clear inline comments to explain what each section does:
```asm
; Get filename argument (arg 2, since "VM" is arg 0)
PUSHB 2
POPA
SYSCALL ArgGet
; ZP.STR now points to filename
```

## .DATA Section Format

The .DATA section supports multiple formats for defining constants and arrays with explicit type control.

### Data Type Modifiers

**Three sizing modes are supported:**

1. **`byte` prefix** - Forces all values to single bytes (truncates values >255)
2. **`word` prefix** - Forces all values to 16-bit words (2 bytes each, LSB/MSB)
3. **No prefix** - Auto-sizing: bytes if ≤255, words if >255

### String Constants

Strings use label syntax with automatic null-termination:

```asm
.DATA
str_hello "Hello World\n"

filename "test.txt"

str_prompt "> "
```

### Byte Arrays

Define arrays of byte values using the `byte` prefix:

```asm
.DATA
byte lookup 10, 20, 30, 40, 50

byte flags 0x01, 0x02, 0x04, 0x08, 0x10

byte chars 'A', 'B', 'C', 'D'

byte pixels 128, 64, 32, 16, 8, 4, 2, 1
```

**Without `byte` prefix (auto-sizing):**
```asm
.DATA
lookup 10, 20, 30, 40, 50        ; All ≤255, stored as bytes

mixed 10, 300, 20, 400           ; 10,20 as bytes; 300,400 as words
```

### Word Arrays

Define arrays of 16-bit word values (stored as LSB, MSB):

```asm
.DATA
word addresses 0x2000, 0x4000, 0x6000, 0x8000

word values 1000, 2000, 3000

word pointers 0xC000, 0xD000, 0xE000
```

### Mixed Data Declarations

You can mix different types in the .DATA section:

```asm
.DATA
; String constants
str_start " ms (start)\n"

str_elapsed " ms (elapsed)\n"

; Byte arrays
byte lookup_table 10, 20, 30, 40, 50

byte status_codes 0x00, 0x01, 0xFF

; Word arrays
word memory_pages 0x2000, 0x4000, 0x6000

word timings 1000, 2000, 5000, 10000

; Auto-sized (will use bytes for small values, words for large)
thresholds 10, 100, 1000, 10000
```

### Accessing Data Arrays

Use `PUSHD`/`PUSHD2` to get the data address, then use `PUSHDAX`/`PUSHDAX2` with an index:

#### Byte Array Access Pattern
```asm
.CONST
    ZP.TOP    0x12

.DATA
byte lookup 10, 20, 30, 40, 50

.MAIN
    ; Access lookup[2] (value 30)
    PUSHB 2              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX 0            ; PUSHDAX offset, index from stack
    READB                ; Read the byte
    ; Result on stack: 30
```

#### Word Array Access Pattern
For word arrays, use `PUSHDAX2` which multiplies the index by 2:
```asm
.DATA
word addresses 0x2000, 0x4000, 0x6000

.MAIN
    ; Access addresses[1] (value 0x4000)
    PUSHB 1              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX2 0           ; Multiply index by 2, add offset
    DUPW                 ; Duplicate address
    READB                ; Read LSB
    POPZB ZP.TOP0
    PUSHW1
    ADDW                 ; Address + 1
    READB                ; Read MSB
    POPZB ZP.TOP1
    PUSHZW ZP.TOP        ; Full word on stack: 0x4000
```

### Data Section Best Practices

1. **Use explicit prefixes for clarity:**
```asm
byte flags 0x01, 0x02, 0x04      ; Clear: these are bytes
word pointers 0xC000, 0xD000     ; Clear: these are words
```

2. **Group related data:**
```asm
; Status codes
byte error_codes 0x01, 0x02, 0x03, 0xFF

; Error messages
str_error_1 "File not found\n"
str_error_2 "Access denied\n"
```

3. **Document array sizes:**
```asm
byte gamma_table 0, 1, 2, 3, 4, 5, 6, 7, 8  ; 9 entries
```

4. **Use meaningful names:**
```asm
byte sine_lookup_64 0, 6, 12, 18, 25, 31, 37, 43  ; 64-entry sine table (showing first 8)
```

## Global Variables - The 256-Byte Global Page

The VM provides a dedicated 256-byte page (Page 1) for global variables accessible from any function. This is separate from the stack and provides persistent storage throughout program execution.

### Defining Global Variables
Global variables are defined in the `.CONST` section as offsets from 0x00:

```asm
.CONST
    ; Global variable offsets (256 bytes available)
    GP.Counter   0x00   ; Single byte at offset 0
    GP.Total     0x01   ; Word at offset 1-2 (define both for clarity)
    GP.TotalH    0x02   ; High byte of Total
    
    ; For 32-bit values, use descriptive naming:
    GP.START.LOW  0x03   ; Low word of 32-bit timestamp
    GP.START.HIGH 0x05   ; High word of 32-bit timestamp
    
    ; Arrays and buffers
    GP.Buffer    0x07   ; Start of 16-byte buffer
    GP.BufferEnd 0x16   ; End marker
    
    ; Complex structures
    GP.Player.X  0x17   ; Player X coordinate
    GP.Player.Y  0x18   ; Player Y coordinate
    GP.Player.HP 0x19   ; Player hit points (word)
```

### Accessing Global Variables

#### Byte Operations
```asm
; Write byte to global
PUSHB 42
POPGB GP.Counter    ; Store 42 in Counter

; Read byte from global
PUSHGB GP.Counter   ; Push Counter value to stack
POPA               ; Move to A register
```

#### Word Operations
```asm
; Write word to global
PUSHW 1000
POPGW GP.Total      ; Store 1000 in Total (2 bytes)

; Read word from global
PUSHGW GP.Total     ; Push Total value to stack
```

#### 32-bit Value Pattern
For 32-bit values, split them across two word slots:

```asm
; Store 32-bit value in globals (from Test.VMA example)
SYSCALL Time.Millis     ; Get 32-bit milliseconds in ZP.TOP
PUSHZQ ZP.TOP          ; Push all 32 bits
POPGW GP.START.HIGH    ; Pop high word first
POPGW GP.START.LOW     ; Pop low word second

; Retrieve 32-bit value from globals
PUSHGW GP.START.LOW    ; Push low word first
PUSHGW GP.START.HIGH   ; Push high word second
POPZQ ZP.TOP          ; Pop as 32-bit value to ZP.TOP
```

### Global Variable Best Practices

1. **Organize by Function**: Group related globals together
```asm
; Game state (0x00-0x0F)
GP.Level     0x00
GP.Score     0x01   ; Word
GP.Lives     0x03

; Player data (0x10-0x1F)
GP.Player.X  0x10
GP.Player.Y  0x11

; Dynamic memory pointers (0x20-0x2F)
GP.MainBuffer   0x20   ; Pointer to allocated main buffer
GP.TempBuffer   0x22   ; Pointer to allocated temp buffer
```

2. **Document Usage**: Always comment what each global stores
```asm
GP.Flags     0x20   ; Bit 0: game active, Bit 1: paused, Bit 2: muted
GP.HeapPtr   0x30   ; Pointer to dynamically allocated memory (2 bytes)
```

3. **Use Meaningful Names**: Prefix with `GP.` for clarity
```asm
GP.TempWord  0x30   ; Temporary storage for calculations
GP.BufferPtr 0x32   ; Pointer to allocated buffer
```

4. **Store Memory Pointers**: Globals are perfect for storing 16-bit addresses from MemAllocate
```asm
; Allocate and store pointer
PUSHW 100
POPZW ZP.ACC
SYSCALL Mem.Allocate
PUSHZW ZP.IDX       ; Get allocated address
POPGW GP.BufferPtr  ; Store in global for program-wide access
```

5. **Consider Alignment**: Word values don't need alignment but grouping them can improve readability

6. **Reserve Space for Growth**: Leave gaps for future additions
```asm
; Network state (0x40-0x5F) - 32 bytes reserved
GP.NET.Status 0x40
; ... room for expansion
```

## Simple Hello World Example
```asm
; Simple Hello World program

.CONST
    ZP.STR       0x1E
    Print.String 0x11

.DATA
str_hello "Hello, World!\n"

.MAIN
    PUSHD str_hello     ; Push string address
    POPZW ZP.STR        ; Marshal to BIOS
    SYSCALL Print.String ; Print it
    HALT                ; Return to BIOS
```

## Complete Example with Globals (Test.VMA Pattern)
This example shows proper global variable usage for timing measurements:

```asm
.CONST
    ZP.TOP       0x12
    ZP.TOP2      0x14
    ZP.NEXT      0x16
    ZP.NEXT2     0x18
    ZP.STR       0x1E
    
    Time.Delay   0x17
    Time.Millis  0x18
    Time.Seconds 0x19
    Long.Sub     0x1B
    Long.Print   0x1F
    Print.String 0x11
    
    ; Global variable offsets
    GP.START.LOW  0x00   ; start millis low word
    GP.START.HIGH 0x02   ; start millis high word
    
.DATA
str_start " ms (start)\n"

str_current " ms (current)\n"

str_elapsed " ms (elapsed)\n"
    
.MAIN
    ENTER 0
    ; Get start millis
    SYSCALL Time.Millis     ; Result in ZP.TOP
    
    ; Save to globals
    PUSHZQ ZP.TOP
    POPGW GP.START.HIGH
    POPGW GP.START.LOW
    
    ; Print start time
    SYSCALL Long.Print
    PUSHD str_start
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Delay 100ms
    PUSHW 100
    PUSHW0
    POPZQ ZP.TOP
    SYSCALL Time.Delay
    
    ; Get current millis
    SYSCALL Time.Millis     ; Result in ZP.TOP
    
    ; Print current time
    PUSHZQ ZP.TOP
    
    SYSCALL Long.Print
    PUSHD str_current
    POPZW ZP.STR
    SYSCALL Print.String
    POPZQ ZP.NEXT          ; Pop current time to NEXT
    
    ; Load start time from globals to TOP
    PUSHGW GP.START.LOW
    PUSHGW GP.START.HIGH
    POPZQ ZP.TOP
    
    SYSCALL Long.Sub       ; NEXT = NEXT - TOP
    
    ; Move result from NEXT to TOP for printing
    PUSHZQ ZP.NEXT
    POPZQ ZP.TOP
    SYSCALL Long.Print
    
    PUSHD str_elapsed
    POPZW ZP.STR
    SYSCALL Print.String
    
    LEAVE
    HALT
```

## Complete Minimal File Reader Example (Type.VMA)
This complete working example demonstrates file operations, argument handling, and error management:

```asm
.CONST
    ZP.STR       0x1E
    ZP.NEXT      0x16
    ZP.TOP       0x12
    ZP.TOP0      0x12
    
    Print.NewLine 0x14
    Print.Char    0x12
    FOpen         0x30
    FClose        0x31
    FGetC         0x32
    ArgGet        0x37

.DATA
str_mode "r"

.MAIN
    ; Print newline
    SYSCALL Print.NewLine
    
    ; Get filename argument (arg 2, since "VM" is arg 0, program name is arg 1)
    PUSHB 2
    POPA
    SYSCALL ArgGet
    ; ZP.STR now points to filename
    
    ; Set up file mode ("r")
    PUSHD str_mode       ; Push address of "r" string
    POPZW ZP.NEXT        ; Store in ZP.NEXT for FOpen
    
    ; Open file: FOpen(filename, "r")
    SYSCALL FOpen
    
    ; Check if file opened successfully (TOP != 0)
    PUSHZW ZP.TOP
    PUSHW0
    NEW                  ; TOP != 0?
    BZF file_error       ; If TOP == 0, file failed to open
    
read_loop:
    ; Read character: FGetC(file_handle)
    ; File handle is already in ZP.TOP from FOpen
    PUSHZW ZP.TOP
    POPZW ZP.NEXT        ; Move file handle to ZP.NEXT for FGetC
    SYSCALL FGetC
    
    ; Check for EOF (TOP == 0xFFFF which is -1 in unsigned)
    PUSHZW ZP.TOP
    PUSHW 0xFFFF         ; Push -1 (0xFFFF in unsigned)
    EQW
    BNZF close_file      ; If equal to -1, EOF reached
    
    ; Print character (extract byte from word result)
    PUSHZB ZP.TOP0       ; Get low byte of character
    POPA                 ; Move to A register
    SYSCALL Print.Char
    
    BRAR read_loop       ; Continue reading
    
close_file:
    ; Close file: FClose(file_handle)
    PUSHZW ZP.TOP        ; File handle still in ZP.TOP
    POPZW ZP.NEXT        ; Move to ZP.NEXT for FClose
    SYSCALL FClose
    
file_error:
    HALT                 ; Common exit point for both error and normal termination
```

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

### Direct Zero Page Addressing
While named constants are preferred for clarity, you can use direct zero page addresses:
```asm
POPZW 0x14          ; Direct address (same as POPZW ZP.TOP+2)
PUSHZB 0x10         ; Direct read from address 0x10
POPZB 0x58          ; Store to application zero page
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
PUSHLW 6            ; First 16-bit argument (uses BP+5 and BP+6)
PUSHLW 8            ; Second 16-bit argument (uses BP+7 and BP+8)

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
SYSCALLX 0x8E       ; Call BIOS function (faster: does not load or store simulated A and Y registers or C an Z flags)
ENTER    0x90 bytes ; Setup stack frame with local space
LEAVE    0x92       ; Restore stack frame
DUMP     0x94       ; Diagnostic stack dump
```

### String/Data Operations (0x98-0xA2)
```asm
PUSHD    0x98 byte  ; Push data address (byte offset)
PUSHD2   0x9A word  ; Push data address (word offset)
PUSHDAX  0x9B byte  ; Push data address with index (offset + stack index)
PUSHDAX2 0x9C byte  ; Push data address with index*2 (offset + stack index*2)
STRC     0x9D       ; Pop index, pop string, push char
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
PUSHC               ; Push carry flag (1=success, 0=fail)
BZF allocation_failed ; Branch if 0 (failed)
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

.DATA
filename "test.txt"

; In code:
PUSHD filename
POPZW ZP.STR
PUSHB 0             ; DirWalkAction
POPA
SYSCALL FileExists
```

#### FileDelete (0x05)
```asm
; Delete file from storage
; Input:  ZP.STR = filename pointer
; Output: C = 1 if successful, 0 if failed

.DATA
filename "old.txt"

; In code:
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

.DATA
filename "output.txt"

; In code:
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
PUSHB 0x00          ; Data file
POPA
SYSCALL FileEndSave
```

#### FileStartLoad (0x0A)
```asm
; Open file for reading
; Input:  ZP.STR = filename pointer
;         A = DirWalkAction
; Output: C = 1 if successful

.DATA
filename "input.txt"

; In code:
PUSHD filename
POPZW ZP.STR
PUSHB 0
POPA
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
PUSHC               ; Push carry flag
BZF end_of_file     ; Branch if 0 (no more data)
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
PUSHC               ; Push carry flag
BZF no_input        ; Branch if 0 (no input)
```

### Break Detection

#### IsBreak (0x10)
```asm
; Check for Ctrl+C or NMI break
; Input:  None
; Output: C = 1 if break detected
SYSCALL IsBreak
PUSHC               ; Push carry flag (1 if break, 0 if not)
BNZF user_break     ; Branch if 1 (break detected)
```

### Console Output

#### PrintString (0x11)
```asm
; Print null-terminated string
; Input:  ZP.STR = string pointer
; Output: None

.DATA
msg_hello "Hello World\n"

; In code:
PUSHD msg_hello
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
PUSHB 8
POPY
SYSCALL PrintSpaces  ; Print 8 spaces
```

### Timer Services

#### TimeDelay (0x17)
```asm
; Delay execution
; Input:  ZP.TOP = milliseconds (32-bit)
; Output: None

; Complete pattern for 1000ms delay (0x000003E8)
PUSHW 0x03E8        ; Push low word (1000)
POPZW ZP.TOP        ; Store to ZP.TOP0-1
PUSHW0              ; Push high word (0)
POPZW 0x14          ; Store to ZP.TOP2-3 (or POPZW ZP.TOP+2)
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

; Complete pattern:
PUSHB 0             ; Pin 0
POPA                ; A = pin number
PUSHB 1             ; OUTPUT mode (1)
POPY                ; Y = mode
SYSCALL PinMode
```

#### PinRead (0x27)
```asm
; Read digital pin state
; Input:  A = pin number (0-15)
; Output: A = value (0/1)
;         Z = 1 if LOW
PUSHB 5
POPA
SYSCALL PinRead
PUSHA               ; Push A register result
PUSHB0
EQB                 ; Compare with 0
BNZF pin_is_low     ; Branch if equal to 0
```

#### PinWrite (0x28)
```asm
; Write digital pin state
; Input:  A = pin number (0-15)
;         Y = value (0/1)
; Output: None

; Complete pattern:
PUSHB 0             ; Pin 0
POPA                ; A = pin number
PUSHB 1             ; HIGH (1)
POPY                ; Y = value
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

.DATA
filename "data.txt"

readmode "r"

; In code:
PUSHD filename
POPZW ZP.STR
PUSHD readmode
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

; Pattern for EOF checking:
SYSCALL FGetC
PUSHZW ZP.TOP
PUSHW 0xFFFF         ; 0xFFFF is unsigned representation of -1
EQW
BNZF end_of_file     ; Branch if EOF reached
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
; IMPORTANT: Argument indexing:
;   - Arg 0 = "VM" (the VM itself)
;   - Arg 1 = program name (e.g. "TYPE.VMA")
;   - Arg 2 = first user argument
;   - Arg 3 = second user argument, etc.
;
; Input:  A = argument index (0=VM, 1=program, 2=first user arg, etc.)
; Output: ZP.STR = pointer to argument string

; Example: Get first user argument
PUSHB 2             ; First user argument (not 1!)
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


# SYSCALL vs SYSCALLX: Performance Optimization Guide

## Overview

The VM provides two opcodes for invoking BIOS functions: `SYSCALL` and `SYSCALLX`.
While both perform the same basic function, `SYSCALLX` is an optimized variant that saves cycles when certain BIOS call
features are not needed.

## Implementation Differences

### SYSCALL (Full-Featured)
```asm
SYSCALL:
    LDA [codePage], Y // byte BIOS call index
    INY
    TAX
    PHY
    LDY yStore        // Restore Y register from VM storage
    LDA aStore        // Restore A register from VM storage
    dispatchBIOS();
    STA aStore        // Save A register back to VM storage
    ; Save Z and C flags to vmFlags
    if (Z) { SMB6 vmFlags } else { RMB6 vmFlags }
    if (C) { SMB7 vmFlags } else { RMB7 vmFlags }
    PLY
    
    LDA [codePage], Y
    INY
    TAX
    JMP [opCodeJumps, X]
```

### SYSCALLX (Optimized)
```asm
SYSCALLX:
    LDA [codePage], Y // byte BIOS call index
    INY
    TAX
    PHY
    dispatchBIOS();   // A and Y registers NOT restored/saved
    PLY
    
    LDA [codePage], Y
    INY
    TAX
    JMP [opCodeJumps, X]
```

## Key Differences

### SYSCALL provides:
1. **Register marshalling** - Restores A and Y registers from VM storage (`aStore`, `yStore`) before the call
2. **Return value preservation** - Saves A register back to VM storage after the call
3. **Flag preservation** - Captures Z and C flags in `vmFlags` for `PUSHZ` and `PUSHC` instructions
4. **Full compatibility** - Works with all BIOS functions

### SYSCALLX omits:
1. **No register marshalling** - A and Y are NOT restored before the call
2. **No return value capture** - A register is NOT saved after the call
3. **No flag preservation** - Z and C flags are NOT captured in `vmFlags`
4. **Lighter weight** - Saves approximately 10-15 cycles per call

## When to Use SYSCALLX

Use `SYSCALLX` when **ALL** of the following conditions are true:

### 1. The BIOS Call Does Not Use A or Y Registers as Input
Most BIOS calls receive their parameters via zero page marshalling (e.g., `ZP.STR`, `ZP.TOP`, `ZP.NEXT`), not through registers.

**Good candidates:**
```asm
; PrintString only needs ZP.STR
PUSHD msg_hello
POPZW ZP.STR
SYSCALLX PrintString    ✓ Safe - no A/Y input needed

; FOpen needs ZP.STR and ZP.NEXT
PUSHD filename
POPZW ZP.STR
PUSHD mode
POPZW ZP.NEXT
SYSCALLX FOpen          ✓ Safe - no A/Y input needed

; PrintChar needs character in A register
PUSHB 'X'
POPA                    ; Character now in A
SYSCALL PrintChar       ✗ Must use SYSCALL - needs A register
```

### 2. You Don't Need the Return Value in A
Some BIOS calls return values in the A register. If you need that value, use `SYSCALL`.

```asm
; SerialWaitForChar returns character in A
SYSCALL SerialWaitForChar   ; Returns char in A, saved to aStore
PUSHA                       ; Can retrieve it with PUSHA

SYSCALLX SerialWaitForChar  ✗ Wrong - return value in A is lost
```

### 3. You Don't Need to Check Z or C Flags After the Call
If you need to branch based on success/failure or test results, use `SYSCALL`.

```asm
; FOpen sets C flag on success
SYSCALL FOpen
PUSHC                   ; Get the carry flag
BZF failed             ✓ Can check result

SYSCALLX FOpen
PUSHC                   ✗ Wrong - C flag not preserved in vmFlags
BZF failed             ; This won't work correctly
```

### 4. Zero Page Marshalling is Sufficient
The BIOS call must work entirely through zero page locations.

```asm
; PrintHex needs value in A
PUSHB 0xFF
POPA
SYSCALL PrintHex        ✓ Correct - needs A register

; PrintNewLine needs nothing
SYSCALLX PrintNewLine   ✓ Safe - no parameters needed
```

## Performance Impact

Using `SYSCALLX` instead of `SYSCALL` saves:
- **~8 bytes of code** (register restore/save operations)
- **~10-15 CPU cycles** per call
- **Zero page operations** (aStore access)
- **Flag manipulation** operations

For frequently called functions in tight loops, this can yield measurable performance improvements.

## Common BIOS Functions Safe for SYSCALLX

### Always Safe:
- `PrintString` - Uses ZP.STR
- `PrintNewLine` - No parameters
- `FClose` - Uses ZP.NEXT
- `FPutC` - Uses ZP.ACC and ZP.NEXT
- Most file operations (except those returning values)

### Never Safe (Use SYSCALL):
- `PrintChar` - Needs A register input
- `PrintHex` - Needs A register input
- `SerialWaitForChar` - Returns in A register
- `FGetC` - Returns in ZP.TOP and needs success checking
- `ArgGet` - Returns result that may need checking
- Any call where you check C or Z flags afterward

### Context Dependent:
- `FOpen` - Safe if you don't check the success flag, but you usually should check it
- `ArgCount` - Returns count in A, but if you just need it on stack use `PUSHA` after `SYSCALL`

## Example Optimization

### Before (using SYSCALL everywhere):
```asm
echo_loop:
    SYSCALL SerialWaitForChar   ; Need return value - must use SYSCALL
    PUSHA
    PUSHB CtrlZ
    EQB
    BNZF close_file
    
    POPA
    POPZB ZP.ACC
    PUSHGW GP.FileHandle
    POPZW ZP.NEXT
    SYSCALL FPutC               ; Could optimize this
    
    ; ... counter code ...
    
    SYSCALLX PrintChar          ; Wrong! PrintChar needs A register
```

### After (optimized):
```asm
echo_loop:
    SYSCALL SerialWaitForChar   ; Must use SYSCALL - returns in A
    PUSHA
    PUSHB CtrlZ
    EQB
    BNZF close_file
    
    POPA
    POPZB ZP.ACC
    PUSHGW GP.FileHandle
    POPZW ZP.NEXT
    SYSCALLX FPutC              ✓ Safe - uses only zero page
    
    ; ... counter code ...
    
    SYSCALL PrintChar           ✓ Must use SYSCALL - needs A
```

## Rule of Thumb

**Default to SYSCALL** unless you've verified the specific BIOS call:
1. Uses only zero page marshalling for input
2. Doesn't return values you need
3. Doesn't set flags you need to check

When in doubt, use `SYSCALL` - the safety is worth the small performance cost.

## Debugging Tip

If your code works with `SYSCALL` but fails with `SYSCALLX`, you're likely:
- Missing a return value that was in A
- Missing a flag check (C or Z)
- Calling a function that needs A or Y register input

Simply switch back to `SYSCALL` for that call.

## Programming Patterns from Working Examples

### Pattern 1: File Reading with Handle Preservation (from Type.VMA)
This pattern shows efficient file handle management by keeping the handle in ZP.TOP across operations:

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
str_mode "r"

.MAIN
    ; Get filename from command line
    PUSHB 2             ; First user argument
    POPA
    SYSCALL ArgGet      ; Sets ZP.STR
    
    ; Open file
    PUSHD str_mode
    POPZW ZP.NEXT
    SYSCALL FOpen       ; Returns handle in TOP
    
    ; Check success (handle != NULL)
    PUSHZW ZP.TOP
    PUSHW0
    NEW
    BZF file_error      ; Common error exit point
    
read_loop:
    ; File handle stays in ZP.TOP - just move to NEXT when needed
    PUSHZW ZP.TOP       
    POPZW ZP.NEXT
    SYSCALL FGetC       ; Returns char in TOP
    
    ; Check EOF (TOP == 0xFFFF)
    PUSHZW ZP.TOP
    PUSHW 0xFFFF        ; Unsigned representation of -1
    EQW
    BNZF close_file
    
    ; Extract byte from word result
    PUSHZB ZP.TOP0      ; Use PUSHZB to get low byte
    POPA
    SYSCALL Print.Char
    
    BRAR read_loop
    
close_file:
    PUSHZW ZP.TOP
    POPZW ZP.NEXT
    SYSCALL FClose
    
file_error:             ; Common exit for both error and normal flow
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

### Critical Pattern: Understanding Comparisons and Branching
The C flag behavior with CMP is tricky! For numeric comparisons:
```asm
; IMPORTANT: After comparison instructions (LEB, LTB, LEW, LTW):
; - C flag is SET (1) when condition is TRUE
; - C flag is CLEAR (0) when condition is FALSE

; Example: Check if j <= 1000
PUSHLW -11          ; Push j
PUSHW 1000          ; Push 1000
LEW                 ; Compare: sets C=1 if j <= 1000
; Now use PUSHC and branch on the value:
PUSHC               ; Push 1 if C set (condition true), 0 if clear (false)
BNZF continue_loop  ; Branch if value is 1 (condition was true)
BZF exit_loop       ; Branch if value is 0 (condition was false)

; The NOEL pattern uses this shorthand:
LEW                 ; Sets C flag
BNZR inner_loop     ; Branch backward if C was set (condition true)

; For equality comparisons (EQB, NEB, EQW, NEW):
; - Z flag is SET (1) when EQUAL
; - Z flag is CLEAR (0) when NOT EQUAL

; Example:
PUSHW value1
PUSHW value2
EQW                 ; Sets Z=1 if equal, Z=0 if not equal
BZF not_equal       ; Branch if Z=0 (values different)
BNZF equal          ; Branch if Z=1 (values same)
```

### Pattern 3: Dynamic Memory with Global Pointer Storage
This pattern shows how to allocate memory and store the pointer in a global variable for access throughout your program:

```asm
.CONST
    ZP.ACC       0x10
    ZP.IDX       0x1A
    
    Mem.Allocate 0x00
    Mem.Free     0x01
    
    ; Global variables
    GP.BufferPtr 0x00   ; 2-byte pointer to allocated memory
    GP.BufferSize 0x02  ; Size of allocated buffer

.MAIN
    ENTER 0
    
    ; Allocate 256-byte buffer
    PUSHW 256
    POPZW ZP.ACC
    SYSCALL Mem.Allocate
    PUSHC               ; Check if successful
    BZF alloc_failed
    
    ; Store pointer in global variable
    PUSHZW ZP.IDX       ; Get allocated address
    POPGW GP.BufferPtr  ; Save to global
    
    ; Store size for later reference
    PUSHW 256
    POPGW GP.BufferSize
    
    ; Now use the buffer - write byte to offset 10
    PUSHGW GP.BufferPtr ; Get buffer address
    PUSHW 10            ; Add offset
    ADDW
    PUSHB 0x42          ; Value to write
    WRITEB              ; Write to memory
    
    ; Read back from offset 10
    PUSHGW GP.BufferPtr ; Get buffer address
    PUSHW 10            ; Add offset
    ADDW
    READB               ; Read from memory
    ; Value 0x42 now on stack
    
    ; Clean up - free the buffer
    PUSHGW GP.BufferPtr ; Get pointer from global
    POPZW ZP.IDX
    SYSCALL Mem.Free
    
    LEAVE
    HALT
    
alloc_failed:
    ; Handle allocation failure
    LEAVE
    HALT
```

**Key Points:**
- Memory allocation returns a 16-bit address in ZP.IDX
- Store this address in a global variable (2 bytes) for program-wide access
- Use READB/WRITEB with the pointer to access allocated memory
- Add offsets to the pointer to access different locations in the buffer
- Always free allocated memory when done

### Pattern 4: Working with Globals
```asm
; Define global variables in .CONST
.CONST
    GP.Counter   0x00   ; Global counter byte
    GP.Total     0x01   ; Global total word
    GP.Buffer    0x03   ; Start of buffer
    GP.State     0x13   ; Game state flags

.MAIN
    ; Initialize global counter
    PUSHB 0
    POPGB GP.Counter
    
    ; Increment global total
    PUSHGW GP.Total
    PUSHW 10
    ADDW
    POPGW GP.Total
    
    ; Use global as loop counter
loop:
    PUSHGB GP.Counter
    PUSHB1
    ADDB
    DUPB                ; Keep copy for comparison
    POPGB GP.Counter
    PUSHB 100
    LEB                 ; Counter <= 100?
    PUSHC
    BNZF loop          ; Continue if true
```

### Pattern 5: String Operations
```asm
.DATA
str_hello "Hello"

str_message "A test message"

str1 "First string"

str2 "Second string"

.MAIN
    ; Using PUSHD for string addresses
    PUSHD str_hello     ; Push string address
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; String character access with STRC
    PUSHD str_message   ; Push string address
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

### Pattern 7: Hardware Control Loop with Break Detection
For continuous hardware operation with user break capability:
```asm
.CONST
    GPIO.PinMode    0x26
    GPIO.PinWrite   0x28
    Time.Delay      0x17
    IsBreak         0x10
    ZP.TOP          0x12

.MAIN
    ; Configure pin 0 as output
    PUSHB 0             ; Pin 0
    POPA                ; A = pin number
    PUSHB 1             ; OUTPUT mode
    POPY                ; Y = mode
    SYSCALL GPIO.PinMode
    
main_loop:
    ; Turn LED ON
    PUSHB 0             ; Pin 0
    POPA                ; A = pin
    PUSHB 1             ; HIGH
    POPY                ; Y = value
    SYSCALL GPIO.PinWrite
    
    ; Delay 1000ms
    PUSHW 0x03E8        ; 1000 (low word)
    POPZW ZP.TOP        ; Store to ZP.TOP0-1
    PUSHW0              ; 0 (high word)
    POPZW 0x14          ; Store to ZP.TOP2-3
    SYSCALL Time.Delay
    
    ; Check for break
    SYSCALL IsBreak
    PUSHC               ; Push carry flag
    BNZF exit           ; Exit if break detected
    
    ; Turn LED OFF
    PUSHB 0             ; Pin 0
    POPA                ; A = pin
    PUSHB 0             ; LOW
    POPY                ; Y = value
    SYSCALL GPIO.PinWrite
    
    ; Delay 1000ms again
    PUSHW 0x03E8
    POPZW ZP.TOP
    PUSHW0
    POPZW 0x14
    SYSCALL Time.Delay
    
    ; Check for break again
    SYSCALL IsBreak
    PUSHC
    BNZF exit
    
    BRAR main_loop      ; Loop forever
    
exit:
    HALT
```

### Pattern 8: Accessing Byte Arrays
Working with byte arrays defined in .DATA section:
```asm
.CONST
    ZP.TOP    0x12

.DATA
byte lookup 10, 20, 30, 40, 50

.MAIN
    ; Access lookup[2] (value 30)
    PUSHB 2              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX 0            ; PUSHDAX offset, index from stack
    READB                ; Read the byte
    ; Result on stack: 30
    
    ; Store in local or ZP
    POPZB ZP.TOP0
```

### Pattern 9: Accessing Word Arrays
Working with word arrays requires reading both LSB and MSB:
```asm
.DATA
word addresses 0x2000, 0x4000, 0x6000

.MAIN
    ; Access addresses[1] (value 0x4000)
    PUSHB 1              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX2 0           ; Multiply index by 2, add offset
    DUPW                 ; Duplicate address
    READB                ; Read LSB
    POPZB ZP.TOP0
    PUSHW1
    ADDW                 ; Address + 1
    READB                ; Read MSB
    POPZB ZP.TOP1
    PUSHZW ZP.TOP        ; Full word on stack: 0x4000
```

## Advanced Patterns (from NOEL Benchmark)

### Pattern 10: Complex Local Variable Organization
When working with many local variables, organize them clearly:
```asm
.MAIN
    ENTER 14            ; Allocate 14 bytes for locals
    
    ; Document your local variable layout:
    ; s low at [BP+0]
    ; s high at [BP-2]
    ; start low at [BP-4]
    ; start high at [BP-6]
    ; ss (seconds start) at [BP-8]
    ; i at [BP-10]
    ; j at [BP-11]
```

### Pattern 11: Working with 32-bit Values and Partial Storage
Sometimes you only need to store part of a 32-bit value:
```asm
; Save only low word of 32-bit seconds value
SYSCALL Time.Seconds    ; Result in ZP.TOP (32-bit)
PUSHZW ZP.TOP0         ; Push only low 16 bits
POPLW -8               ; Store in local

; Later, reconstruct with assumed high word
PUSHLW -8              ; Get saved low word
PUSHW0                 ; Assume high word is 0
POPZQ ZP.TOP0         ; Now have full 32-bit value
```

### Pattern 12: SYSCALLX for Optimized System Calls
Use SYSCALLX when the syscall ID is constant for slightly better performance:
```asm
; Setup operands in ZP.NEXT and ZP.TOP
PUSHLW -11
PUSHW0
POPZQ ZP.TOP0

SYSCALLX Long.Add      ; Faster than SYSCALL Long.Add
```

### Pattern 13: Efficient Stack Cleanup
Clean up multiple stack values in one line:
```asm
; After complex operations, clean up stack
DROPW DROPB DROPW DROPW DROPW DROPW DROPW
```

### Pattern 14: Direct Local Variable Increment
Use INCLB/INCLW to increment locals without push/pop:
```asm
; Increment loop counters efficiently
INCLB -10              ; i++
INCLW -11              ; j++ (for 16-bit counter)
```

### Pattern 15: Nested Loop Pattern
Efficient nested loop structure:
```asm
; Outer loop initialization
PUSHB1
POPLB -10              ; i = 1

outer_loop:
    ; Inner loop initialization
    PUSHW1
    POPLW -11          ; j = 1
    
inner_loop:
    ; Inner loop body
    ; ...
    
    INCLW -11          ; j++
    PUSHLW -11
    PUSHW 1000
    LEW                ; j <= 1000?
    PUSHC
    BNZF inner_loop    ; Continue if true
    
    ; After inner loop
    INCLB -10          ; i++
    PUSHLB -10
    PUSHB 10
    LEB                ; i <= 10?
    PUSHC
    BNZF outer_loop    ; Continue if true
```

### Pattern 16: Moving Values Between ZP Registers
When working with 32-bit math operations, you often need to swap operands:
```asm
; Move result from NEXT to TOP for printing
PUSHZQ ZP.NEXT0
POPZQ ZP.TOP0
SYSCALL Long.Print

; Move current value to NEXT for subtraction
PUSHZQ ZP.TOP0
POPZQ ZP.NEXT0
```

### Pattern 17: Common Error Exit Point
Use a single label for both error handling and normal termination:
```asm
.MAIN
    ; Try to open file
    SYSCALL FOpen
    PUSHZW ZP.TOP
    PUSHW0
    NEW
    BZF exit            ; Jump to common exit on error
    
    ; Process file...
    
    SYSCALL FClose
    
exit:                   ; Common exit for all paths
    HALT
```

### Pattern 18: Byte Extraction from Word Values
Use PUSHZB to extract specific bytes from multi-byte values:
```asm
; After FGetC returns word in ZP.TOP
PUSHZB ZP.TOP0          ; Extract just the low byte
POPA                    ; Move to A for PrintChar
SYSCALL Print.Char

; Or extract high byte
PUSHZB ZP.TOP1          ; Extract high byte
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
SYSCALL Print.String    ; FAILS! No string pointer set

; CORRECT
.DATA
msg_hello "Hello"

; In code:
PUSHD msg_hello
POPZW ZP.STR
SYSCALL Print.String
```

### 4. Check SYSCALL Results
Most syscalls return status in carry flag or ZP.TOP:
```asm
SYSCALL Operation
PUSHC               ; Push carry flag
BZF failed          ; Branch if 0 (failure)
```

### 5. Clean Up Stack After Calls
```asm
PUSHW argument
CALL Function
DROPW               ; Caller cleans up!
```

### 6. The C Flag and Branching
Use PUSHC to access carry flag, then branch on the value:
```asm
SYSCALL Something   ; Sets C flag
PUSHC               ; Push 1 if C set, 0 if clear
BZF carry_was_clear ; Branch if value is 0
BNZF carry_was_set  ; Branch if value is 1
```

### 7. Branch Instructions and Flags
- BZF/BZR branch when Z flag is FALSE (zero flag clear)
- BNZF/BNZR branch when Z flag is TRUE (zero flag set)
- This is opposite to what the names might suggest!

### 8. String Literals Must Be Defined in .DATA Section
```asm
; WRONG - Cannot use string literals directly
PUSHD "Hello"       ; INVALID!

; CORRECT - Define strings in .DATA section
.DATA
msg_hello "Hello"

.MAIN
    PUSHD msg_hello       ; Reference by label
    POPZW ZP.STR
    SYSCALL Print.String
```

### 9. Understand Argument Indexing
```asm
; Command line: VM TYPE.VMA myfile.txt
; Arg 0 = "VM"
; Arg 1 = "TYPE.VMA"  
; Arg 2 = "myfile.txt"  <-- First user argument!

PUSHB 2             ; Get first user argument
POPA
SYSCALL ArgGet
```

### 10. File Handle Preservation
Keep file handles in ZP.TOP across operations for efficiency:
```asm
SYSCALL FOpen       ; Handle returned in ZP.TOP
; Handle stays in TOP...
PUSHZW ZP.TOP       ; Only move when needed
POPZW ZP.NEXT       
SYSCALL FGetC       ; Now handle back in TOP
```

### 11. Global Variable Organization
Globals provide persistent storage across function calls:
```asm
; CORRECT - Well-organized globals
.CONST
    ; System state (0x00-0x0F)
    GP.Flags     0x00
    GP.Mode      0x01
    
    ; Counters (0x10-0x1F)
    GP.Count     0x10
    GP.Total     0x11   ; Word
    
    ; Buffers (0x20-0x3F)
    GP.Buffer    0x20   ; 32-byte buffer
```

### 12. Use PUSHDAX for Array Access
When accessing array elements, use PUSHDAX (for bytes) or PUSHDAX2 (for words):
```asm
; For byte arrays
PUSHB index
PUSHB0              ; Extend to word
PUSHDAX 0          ; offset + index
READB              ; Read the byte

; For word arrays  
PUSHB index
PUSHB0              ; Extend to word
PUSHDAX2 0         ; offset + (index * 2)
; Then read LSB and MSB
```

### 13. Use Explicit Type Prefixes in .DATA
For clarity and to prevent auto-sizing surprises, use explicit type prefixes:
```asm
; GOOD - Explicit intent
.DATA
byte flags 0x01, 0x02, 0x04, 0x08
word addresses 0x2000, 0x4000, 0x6000

; RISKY - Auto-sizing may surprise you
.DATA
values 10, 20, 300, 40  ; 300 becomes a word!
```

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

### 7. Using Native 6502 Instead of VM Instructions
```asm
; WRONG - Using native 6502
LDA #5
BCC failed

; CORRECT - Using VM opcodes
PUSHB 5
POPA
PUSHC               ; Get carry flag
BZF failed          ; Branch if carry was clear
```

### 8. Mixing .DATA Format Styles
```asm
; WRONG - Inconsistent style
.DATA
byte lookup 10, 20, 30
    STR0 "old style"   ; No label!

; CORRECT - Consistent style
.DATA
byte lookup 10, 20, 30

str_message "new style", 0
```

### 9. Wrong Command Line Argument Index
```asm
; WRONG - Assuming first user arg is index 1
PUSHB 1
POPA
SYSCALL ArgGet      ; Gets program name, not user arg!

; CORRECT - First user arg is index 2
PUSHB 2
POPA
SYSCALL ArgGet
```

### 10. Not Using 0xFFFF for EOF Check
```asm
; WRONG - Using signed value
PUSHW -1
EQW

; CORRECT - Using unsigned representation
PUSHW 0xFFFF        ; Unsigned representation of -1
EQW
```

### 11. Mixing Up Global and Local Access
```asm
; WRONG - Using local syntax for globals
PUSHLB GP.Counter   ; NO! PUSHLB is for locals

; CORRECT - Use PUSHGB for globals
PUSHGB GP.Counter   ; Access global variable
```

### 12. Not Documenting Global Usage
```asm
; WRONG - Unclear global usage
.CONST
    GP.X  0x00
    GP.Y  0x01

; CORRECT - Document what globals store
.CONST
    GP.CursorX  0x00   ; Screen cursor X position (0-79)
    GP.CursorY  0x01   ; Screen cursor Y position (0-24)
```

### 13. Forgetting to Extend Index to Word for PUSHDAX
```asm
; WRONG - Pushing byte index directly
PUSHB 5
PUSHDAX 0          ; Index must be word!

; CORRECT - Extend to word first
PUSHB 5
PUSHB0             ; Extend to word
PUSHDAX 0
```

### 14. Forgetting Type Prefix for Large Values
```asm
; WRONG - Large values without word prefix will auto-size
.DATA
pointers 0xC000, 0xD000  ; Works but unclear intent

; CORRECT - Explicit word prefix
.DATA
word pointers 0xC000, 0xD000  ; Clear: these are words
```

## Summary

The Hopper VM provides exceptional code density (8-10× better than native 6502) through:
1. Stack-based computation model
2. Direct hardware stack usage
3. Efficient BIOS integration via zero page marshalling
4. Page-constrained functions
5. Compact bytecode representation
6. Optimized instruction variants (SYSCALLX, INCLB/INCLW)
7. **256-byte global page for persistent storage**
8. **Flexible .DATA section with explicit byte and word type control**

Key concepts to master:
- Stack frame layout (**BP+5 for first argument**, BP+0 for first local, BP+1 for saved BP)
- Complete instruction set including bitwise, **global**, and 32-bit operations
- **.DATA section with `byte` and `word` prefixes for explicit type control**
- **Three sizing modes: `byte` (force bytes), `word` (force words), no prefix (auto-size)**
- **Array access using PUSHDAX and PUSHDAX2 instructions**
- **Global variable access through PUSHGB/POPGB and PUSHGW/POPGW**
- **Organizing globals with GP. prefix and documenting usage**
- **Storing dynamic memory pointers in globals** - allocated memory addresses are 2-byte values perfect for global storage
- Zero page marshalling for BIOS calls
- SYSCALL vs SYSCALLX optimization
- Stack cleanup responsibilities (including multi-DROP patterns)
- DUMP opcode for debugging
- Branch flag behavior (BZF branches when Z is false/clear, BNZF branches when Z is true/set)
- Using PUSHC to access carry flag for branching
- **String literals must be defined in .DATA section and referenced by label**
- **Labels use colon suffix** (loop:, exit:, etc.)
- **Direct zero page addresses can be used** (POPZW 0x14 for ZP.TOP+2)
- Efficient local variable organization and documentation
- **Global variable patterns for persistent state management**
- Working with partial 32-bit values
- Direct local increment operations (INCLB/INCLW)
- Moving values between ZP.TOP and ZP.NEXT for 32-bit operations
- **Complete hardware control patterns with break detection**
- **Proper GPIO and timer patterns from real examples**
- **Command line argument indexing** (arg 0=VM, arg 1=program, arg 2=first user arg)
- **File handle preservation pattern** - keep handles in ZP.TOP
- **EOF checking with 0xFFFF** - unsigned representation of -1
- **Common error exit pattern** - single label for all exit paths
- **PUSHZB for byte extraction** from word values
- **Clear inline commenting** for maintainable code

Remember: Arguments always start at BP+5, not BP+3! This is critical for correct function parameter access. All code must use VM opcodes - never use native 6502 instructions! String literals cannot be used directly with PUSHD - they must be defined in the .DATA section first! First user command line argument is at index 2, not 1! **The 256-byte global page provides persistent storage accessible from any function using PUSHGB/POPGB/PUSHGW/POPGW instructions!** **Dynamically allocated memory pointers (16-bit addresses from MemAllocate) can and should be stored in global variables for program-wide access - use READB/WRITEB to access the allocated memory!** **Use explicit `byte` or `word` prefixes in .DATA for clear intent and to avoid auto-sizing surprises!** **Access array elements using PUSHDAX for byte arrays and PUSHDAX2 for word arrays!**