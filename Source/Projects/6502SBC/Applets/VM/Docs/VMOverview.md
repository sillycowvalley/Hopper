# Simple 6502 VM Specification (v3.1) - CORRECTED

## Overview

A minimal, efficient virtual machine designed specifically for the 6502 processor. The VM emphasizes code density, simplicity, and performance through careful constraints that match the 6502 architecture.

## Core Design Principles

1. **Hardware stack only** - No simulated stack, use 6502's page 1 directly
2. **Page-aligned functions** - Each function occupies exactly one page (256 bytes)
3. **Even-numbered opcodes** - Enables fast dispatch without shifting
4. **Simple types only** - 8-bit and 16-bit values, no heap management for primitive types
5. **Immutable strings** - String constants only, stored as null-terminated C strings
6. **BIOS integration** - System calls provide file I/O, memory management, and hardware access

## Type System

```
Primitive Types:
- char: Signed 8-bit (-128 to +127)
- byte: Unsigned 8-bit (0 to 255)
- int:  Signed 16-bit (-32768 to +32767)
- word: Unsigned 16-bit (0 to 65535)

Reference Types (16-bit pointers):
- String constants (immutable, null-terminated)
- Global arrays (fixed size, allocated at startup)
- Dynamic memory pointers (from heap allocation)
```

## Memory Model

### Program Layout
```
0x0000-0x00FF: Zero page (VM registers, BIOS)
0x0100-0x01FF: Hardware stack
0x0200-0x1FFF: System use, VM interpreter
0x2000-0x7FFF: Function pages (up to 96 functions)
0x8000-0xFFFF: ROM (on systems with ROM)

At runtime (heap allocated):
- Globals block (allocated at startup, 256 bytes)
- Constants block (allocated at startup)
- Function table (256 bytes at fixed location)
```

### VM Runtime Registers (Zero Page 0x60-0x6F)
```
functionTable:  0x60-0x61  // Function table pointer (read-only)
BP:             0x63       // Base pointer for stack frame
globals:        0x64-0x65  // Globals base address (2 bytes, points to 256-byte page)
constants:      0x66-0x67  // Constants base address (2 bytes)
codePage:       0x68-0x69  // Current function page address
operand:        0x6A-0x6B  // Temporary operand storage
vmFlags:        0x6C       // VM status flags (bit 6=Z, bit 7=C from BIOS calls)
```

**Note:** The VM uses the 6502 Y register as the program counter (PC) within the current function page. This eliminates the need for zero page PC storage and enables efficient page-constrained execution.

### Global Variables Page
The VM provides a dedicated 256-byte page for global variables accessible from any function:
- Allocated at startup from heap
- Address stored in `globals` register (0x64-0x65)
- Accessed via PUSHGB/POPGB (bytes) and PUSHGW/POPGW (words) with byte offsets
- Perfect for storing dynamic memory pointers (16-bit addresses from MemAllocate)
- Persistent across all function calls

### Function Organization
- Functions start at page boundaries (0x2000, 0x2100, 0x2200...)
- Function IDs: 2, 4, 6, 8... 254 (even numbers only)
- .MAIN is always function ID 2
- Function table uses function ID directly as index (even numbers)
- Each function limited to 256 bytes
- No page-crossing within functions

## Instruction Set

### Opcode Format
- All opcodes are even numbers (0x00-0xFE)
- Odd numbers reserved for future expansion
- Maximum 128 distinct opcodes

### System Operations (0x00-0x02)
```
NOP          0x00                   // No operation (MUST be 0x00)
HALT         0x02                   // Stop execution (return to BIOS)
```

### Stack Operations - Immediates (0x04-0x12)
```
PUSHB        0x04  + byte           // Push 8-bit immediate
PUSHB0       0x06                   // Push 8-bit 0 (optimized)
PUSHB1       0x08                   // Push 8-bit 1 (optimized)
PUSHW        0x0A  + word           // Push 16-bit immediate
PUSHW0       0x0C                   // Push 16-bit 0 (optimized)
PUSHW1       0x0E                   // Push 16-bit 1 (optimized)
PUSHA        0x10                   // Push A register to stack
PUSHC        0x12                   // Push carry flag (1 if set, 0 if clear)
```

### Stack Operations - Manipulation (0x14-0x20)
```
PUSHZ        0x14                   // Push zero flag (1 if set, 0 if clear)
DUPB         0x16                   // Duplicate byte on TOS
DUPW         0x18                   // Duplicate word on TOS
DROPB        0x1A                   // Remove byte from TOS
DROPW        0x1C                   // Remove word from TOS
SWAPB        0x1E                   // Swap top two bytes
SWAPW        0x20                   // Swap top two words
```

### Arithmetic Operations (0x24-0x32)
```
ADDB         0x24                   // Pop 2 bytes, push sum
SUBB         0x26                   // Pop 2 bytes, push difference
NEGB         0x28                   // Negate byte (2's complement)
ADDW         0x2A                   // Pop 2 words, push sum
SUBW         0x2C                   // Pop 2 words, push difference
NEGW         0x2E                   // Negate word (2's complement)
INCLB        0x30  + char           // Local variable 8-bit increment
INCLW        0x32  + char           // Local variable 16-bit increment
```

### Comparison Operations (0x34-0x42)
```
EQB          0x34                   // Pop 2 bytes, push 1 if equal, 0 if not
NEB          0x36                   // Pop 2 bytes, push 1 if not equal
LTB          0x38                   // Pop 2 bytes, unsigned less than
LEB          0x3A                   // Pop 2 bytes, unsigned less or equal
EQW          0x3C                   // Pop 2 words, push 1 if equal, 0 if not
NEW          0x3E                   // Pop 2 words, push 1 if not equal
LTW          0x40                   // Pop 2 words, unsigned less than
LEW          0x42                   // Pop 2 words, unsigned less or equal
```

### Bitwise Operations (0x44-0x50)
```
ANDB         0x44                   // Pop 2 bytes, push bitwise AND
ORB          0x46                   // Pop 2 bytes, push bitwise OR
XORB         0x48                   // Pop 2 bytes, push bitwise XOR
NOTB         0x4A                   // Pop byte, push bitwise NOT
XORW         0x4C                   // Pop 2 words, push bitwise XOR
SHLW         0x4E  + byte           // Shift word left by byte operand
SHRW         0x50  + byte           // Shift word right by byte operand
```

### Zero Page Operations (0x54-0x62)
```
PUSHZB       0x54  + byte           // Push byte from ZP[offset]
PUSHZW       0x56  + byte           // Push word from ZP[offset]
PUSHZQ       0x58  + byte           // Push 32-bit from ZP[offset]
POPZB        0x5A  + byte           // Pop byte to ZP[offset]
POPZW        0x5C  + byte           // Pop word to ZP[offset]
POPZQ        0x5E  + byte           // Pop 32-bit to ZP[offset]
POPA         0x60                   // Pop byte from stack to A register
POPY         0x62                   // Pop byte from stack to Y register
```

### Local Operations (0x64-0x6E)
```
PUSHLB       0x64  + char           // Push byte from BP[offset] (signed offset)
PUSHLW       0x66  + char           // Push word from BP[offset] (signed offset)
PUSHLQ       0x68  + char           // Push 32-bit from BP[offset] (signed offset)
POPLB        0x6A  + char           // Pop byte to BP[offset] (signed offset)
POPLW        0x6C  + char           // Pop word to BP[offset] (signed offset)
POPLQ        0x6E  + char           // Pop 32-bit to BP[offset] (signed offset)
```

### Global Operations (0x70-0x76)
```
PUSHGB       0x70  + byte           // Push byte from global[offset]
PUSHGW       0x72  + byte           // Push word from global[offset]
POPGB        0x74  + byte           // Pop byte to global[offset]
POPGW        0x76  + byte           // Pop word to global[offset]
```

### Control Flow (0x7C-0x8A)
```
BRAF         0x7C  + byte           // Branch forward by byte (0-255)
BRAR         0x7E  + byte           // Branch reverse by byte (0-255)
BZF          0x80  + byte           // Branch forward by byte if TOS is zero
BZR          0x82  + byte           // Branch reverse by byte if TOS is zero
BNZF         0x84  + byte           // Branch forward by byte if TOS is not zero
BNZR         0x86  + byte           // Branch reverse by byte if TOS is not zero
CALL         0x88  + byte           // Call function (ID for table lookup)
RET          0x8A                   // Return from function
```

### System Calls & Stack Frame (0x8C-0x96)
```
SYSCALL      0x8C  + byte           // Call BIOS function via X register
SYSCALLX     0x8E  + byte           // Call BIOS function (fast version)
ENTER        0x90  + byte           // Push BP, set BP = SP, push zeros
LEAVE        0x92                   // Pop BP (stack frame teardown)
DUMP         0x94                   // Diagnostic stack dump
```

### String/Data Operations (0x98-0x9E)
```
PUSHD        0x98  + byte           // Push data address (byte offset)
PUSHD2       0x9A  + word           // Push data address (word offset)
STRC         0x9C                   // Pop index, pop string, push char
STRCMP       0x9E                   // Pop 2 strings, push -1/0/1
```

### Memory Access Operations (0xA0-0xA6)
```
READB        0xA0                   // Pop address word, push byte
WRITEB       0xA2                   // Pop address word, pop byte
READW        0xA4                   // Pop address word, push word
WRITEW       0xA6                   // Pop address word, pop word
```

### Data Section Array Access (0xA8-0xAA)
```
PUSHDAX      0xA8  + word           // Pop index, push (DATA_START + word offset + index)
PUSHDAX2     0xAA  + word           // Pop index, push (DATA_START + word offset + index*2)
```

**Notes:**
- PUSHDAX and PUSHDAX2 both take word offset operands and pop index from stack
- All data addresses are relative to DATA_START (0x0400)
- READB/WRITEB and READW/WRITEW work with any memory address, not just DATA section

### Word Bitwise and Arithmetic Operations (0xAC-0xB6)
```
ANDW         0xAC                   // Pop 2 words, push bitwise AND
ORW          0xAE                   // Pop 2 words, push bitwise OR
MULW         0xB0                   // Pop 2 words, push product (unsigned)
DIVW         0xB2                   // Pop 2 words, push quotient (unsigned)
MODW         0xB4                   // Pop 2 words, push remainder (unsigned)
NOTW         0xB6                   // Pop word, push bitwise NOT
```

### Extended Stack Operations (0xC0-0xC8)
```
OVERW        0xC0                   // Duplicate second word on stack
ROTW         0xC2                   // Rotate top 3 words (w3 w2 w1 → w1 w3 w2)
ROTB         0xC4                   // Rotate top 3 bytes (b3 b2 b1 → b1 b3 b2)
PICKW        0xC8  + byte           // Duplicate nth word from stack (n=0 is top)
```

### Branch Offset Handling

All branch instructions use positive byte offsets (0-255):
- **Forward branches** (BRAF, BZF, BNZF): Add offset to PC of next instruction
- **Reverse branches** (BRAR, BZR, BNZR): Subtract offset from PC of next instruction
- Branches are guaranteed to stay within the current 256-byte page
- No page crossing checks needed

## BIOS System Call IDs

These are the numeric values for BIOS system calls (from BIOSInterface.asm enum SysCall):

```
; Memory Management
MemAllocate          0x00  ; In: ZP.ACC=size(16b) | Out: ZP.IDX=addr, C=success
MemFree              0x01  ; In: ZP.IDX=address | Out: C=success
MemAvailable         0x02  ; In: None | Out: ZP.ACC=free bytes
MemMaximum           0x03  ; In: None | Out: ZP.ACC=largest block

; File Operations (requires EEPROM)
FileExists           0x04  ; In: ZP.STR=filename, A=type | Out: C=exists
FileDelete           0x05  ; In: ZP.STR=filename | Out: C=success
FileDir              0x06  ; In: None | Out: C=success (prints to serial)
FileStartSave        0x07  ; In: ZP.STR=filename | Out: C=success
FileAppendStream     0x08  ; In: SectorSource=data, TransferLength=bytes | Out: C=success
FileEndSave          0x09  ; In: A=type(0x80=exec,0x00=data) | Out: C=success
FileStartLoad        0x0A  ; In: ZP.STR=filename, A=type | Out: C=success
FileNextStream       0x0B  ; In: None | Out: C=data available, TransferLength=bytes
FileFormat           0x0C  ; In: None | Out: C=success

; Serial I/O
SerialWriteChar      0x0D  ; In: A=character | Out: None
SerialWaitForChar    0x0E  ; In: None | Out: A=character
SerialIsAvailable    0x0F  ; In: None | Out: C=available
IsBreak              0x10  ; In: None | Out: C=break detected

; Print/Console
PrintString          0x11  ; In: ZP.STR=string pointer | Out: None
PrintChar            0x12  ; In: A=character | Out: None
PrintHex             0x13  ; In: A=byte | Out: None (prints 2 hex digits)
PrintNewLine         0x14  ; In: None | Out: None
PrintSpace           0x15  ; In: None | Out: None
PrintSpaces          0x16  ; In: Y=count | Out: None

; Timer Services
TimeDelay            0x17  ; In: ZP.TOP=ms(32b) | Out: None
TimeMillis           0x18  ; In: None | Out: ZP.TOP=ms since boot(32b)
TimeSeconds          0x19  ; In: None | Out: ZP.TOP=seconds(32b)

; Long Math (32-bit)
LongAdd              0x1A  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT+TOP
LongSub              0x1B  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT-TOP
LongMul              0x1C  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT*TOP
LongDiv              0x1D  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT/TOP
LongMod              0x1E  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT%TOP
LongPrint            0x1F  ; In: ZP.TOP=value | Out: None (prints decimal)
LongLT               0x20  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT<TOP)
LongGT               0x21  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT>TOP)
LongEQ               0x22  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT==TOP)
LongNE               0x23  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT!=TOP)
LongLE               0x24  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT<=TOP)
LongGE               0x25  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT>=TOP)

; GPIO (uses VIA 65C22 ports A and B)
PinMode              0x26  ; In: A=pin(0-15), Y=mode(0=IN,1=OUT) | Out: None
PinRead              0x27  ; In: A=pin(0-15) | Out: A=value(0/1), Z=LOW
PinWrite             0x28  ; In: A=pin(0-15), Y=value(0/1) | Out: None

; Float Math (IEEE 754 single precision, requires HASFLOAT)
FloatAdd             0x29  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT+TOP
FloatSub             0x2A  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT-TOP
FloatMul             0x2B  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT*TOP
FloatDiv             0x2C  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT/TOP
FloatToLong          0x2D  ; In: ZP.NEXT(IEEE float) | Out: ZP.NEXT=(long)NEXT
FloatLT              0x2E  ; In: ZP.NEXT, ZP.TOP(IEEE floats) | Out: C=(NEXT<TOP)
FloatEQ              0x2F  ; In: ZP.NEXT, ZP.TOP(IEEE floats) | Out: C=(NEXT==TOP)

; File I/O (Optional - extended file operations)
FOpen                0x30  ; In: STR=filename, NEXT=mode("w"/"r") | Out: TOP=FILE*/NULL
FClose               0x31  ; In: NEXT=FILE* | Out: TOP=0/-1
FGetC                0x32  ; In: NEXT=FILE* | Out: TOP=char(0-255)/-1
FRead                0x33  ; In: IDX=buf, IDY=size, ACC=count, NEXT=FILE* | Out: TOP=bytes/-1
FPutC                0x34  ; In: ACC=char, NEXT=FILE* | Out: TOP=char/-1
FWrite               0x35  ; In: IDX=buf, IDY=size, ACC=count, NEXT=FILE* | Out: TOP=bytes/-1

; Command Line Arguments
ArgCount             0x36  ; Out: A = number of arguments (including command name)
ArgGet               0x37  ; In: A = argument index | Out: ZP.STR argument pointer

; Common Zero Page Locations (from ZeroPage.asm)
ZP.ACC               0x10
ZP.ACCL              0x10
ZP.ACCH              0x11
ZP.TOP               0x12
ZP.TOP0              0x12
ZP.TOP1              0x13
ZP.TOP2              0x14
ZP.TOP3              0x15
ZP.NEXT              0x16
ZP.NEXT0             0x16
ZP.NEXT1             0x17
ZP.NEXT2             0x18
ZP.NEXT3             0x19
ZP.IDX               0x1A
ZP.IDXL              0x1A
ZP.IDXH              0x1B
ZP.IDY               0x1C
ZP.IDYL              0x1C
ZP.IDYH              0x1D
ZP.STR               0x1E
ZP.STRL              0x1E
ZP.STRH              0x1F
```

## Implementation Details

### Dispatch Loop
The VM uses an efficient dispatch mechanism that leverages the 6502's indexed jump table:

```asm
mainLoop:
    LDA [codePage], Y      // Get opcode (Y is PC within function)
    INY                    // Advance PC
    TAX                    // Use opcode as index (already even)
    JMP [opCodeJumps, X]   // Jump to handler
    // Total: ~18 cycles overhead
```

### Page-Constrained Execution
- **Critical**: The function page address (codePage) only changes during CALL and RETURN
- All other operations only modify Y (the PC within the page)
- No page-crossing checks needed within functions
- Branches always stay within current page
- Enables massive optimization in dispatch

### Function Call Mechanism

From runtime.asm, the CALL operation:
1. Pushes current Y (PC within page) to stack
2. Pushes current codePage address to stack
3. Looks up new function page in function table
4. Sets new codePage address
5. Sets Y to 0 (functions start at page offset 0)

### Stack Frame Layout
```
High Memory
    [Argument N]
    ...
    [Argument 1]
    [Return PC (Y)]       <- Return PC within page
    [Return Page Low]     <- Return function page address
    [Return Page High]    
    [Saved BP]            <- Saved by ENTER
    [Local 1]             <- BP points here (first local at BP+0)
    [Local 2]             <- BP-1 (second local)
    [Local 3]             <- BP-2 (third local)
    ...                   <- SP points here
Low Memory

CRITICAL: Arguments start at BP+5 (not BP+3!)
- Saved BP is at BP+1
- Return address is 3 bytes (Y + codePage low + codePage high) at BP+2,BP+3,BP+4
- First argument is at BP+5
- Second argument is at BP+6 (for bytes) or BP+5,BP+6 (for words)
```

## Program Format

### File Header (from Buffer.asm)
```
Offset  Size  Description
0x0000  3     Magic number (0x564D42 "VMB")
0x0003  1     Function count
0x0004  2     Data size in bytes (little endian)
0x0006  ...   Function size table (2 bytes per function)
...     N     Constant data section
...     ...   Function code follows
```

### .DATA Section Format
The .DATA section supports multiple formats:

**New format (recommended):**
```asm
.DATA
str_hello:
    .byte "Hello, World!\n", 0

lookup_table:
    .byte 10, 20, 30, 40, 50

addresses:
    .word 0x2000, 0x4000, 0x6000
```

**Old format (still supported):**
```asm
.DATA
    STR0 "Hello, World!\n"
```

### Array Access
- **Byte arrays**: Use `PUSHDAX` which adds stack index to offset
- **Word arrays**: Use `PUSHDAX2` which adds (stack index × 2) to offset

```asm
; Access byte array element
PUSHB 2              ; Index
PUSHB0               ; Extend to word
PUSHDAX 0            ; Get address of element
READB                ; Read the byte

; Access word array element
PUSHB 1              ; Index
PUSHB0               ; Extend to word
PUSHDAX2 0           ; Get address of element (index*2)
; Then read LSB and MSB
```

### Memory Initialization
1. Load program header
2. Allocate globals block from heap (256 bytes, page-aligned)
3. Zero-initialize globals
4. Allocate constants block from heap (page-aligned)
5. Copy constant data to constants block
6. Build function table with page addresses
7. Call main function (ID 2, since .MAIN is always function ID 2)

## VM Assembly Examples

### Hello World
```asm
; Simple Hello World program
.CONST
    ZP.STR       0x1E
    PrintString  0x11
    
.DATA
str_hello:
    .byte "Hello, World!\n", 0

.MAIN
    PUSHD str_hello      ; Push string address
    POPZW ZP.STR         ; Marshal to BIOS
    SYSCALL PrintString  ; Print it
    
    HALT                 ; Return to BIOS
```

### Counter Example with Local Variables
```asm
; Count from 0 to 9 using local variables
.CONST
    ZP.STR       0x1E
    PrintString  0x11
    PrintChar    0x12
    PrintNewLine 0x14
    
.DATA
str_counting:
    .byte "Counting: ", 0

.MAIN
    ENTER 2              ; Allocate 2 bytes for locals
    
    ; Print header
    PUSHD str_counting             
    POPZW ZP.STR
    SYSCALL PrintString
    
    ; Initialize counter to 0 at first local (BP+0)
    PUSHB0
    POPLB 0              ; Store in first local variable at BP+0
    
loop:
    ; Print the digit
    PUSHLB 0             ; Get counter from BP+0
    PUSHB '0'            ; ASCII '0' 
    ADDB                 ; Convert to ASCII digit
    POPA                 
    SYSCALL PrintChar
    
    ; Print space
    PUSHB ' '
    POPA
    SYSCALL PrintChar
    
    ; Increment counter
    INCLB 0              ; Increment local variable directly at BP+0
    
    ; Check if we've done 10 digits
    PUSHLB 0             ; Get counter from BP+0
    PUSHB 10
    EQB                  ; Compare with 10
    BZF loop             ; Loop if not equal
    
    SYSCALL PrintNewLine
    
    LEAVE                ; Restore stack frame
    HALT
```

### Function Call Example (CORRECTED)
```asm
; Demonstrate function calls with parameters
; NOTE: .FUNC must be defined BEFORE .MAIN since it's called by .MAIN
.CONST
    PrintChar    0x12
    PrintNewLine 0x14

.FUNC PrintDigit
    ENTER 0              ; No local variables needed
    
    ; CRITICAL: First argument is at BP+5, not BP+3!
    PUSHLB 5             ; Get parameter (at BP+5)
    PUSHB '0'
    ADDB
    POPA
    SYSCALL PrintChar
    
    LEAVE
    RET

.MAIN
    ; Call PrintDigit with value 5
    PUSHB 5
    CALL PrintDigit
    DROPB                ; Clean up argument
    
    ; Call PrintDigit with value 7  
    PUSHB 7
    CALL PrintDigit
    DROPB                ; Clean up argument
    
    SYSCALL PrintNewLine
    HALT
```

### Dynamic Memory with Global Pointers
```asm
; Demonstrate allocating memory and storing pointer in globals
.CONST
    ZP.ACC       0x10
    ZP.IDX       0x1A
    MemAllocate  0x00
    MemFree      0x01
    
    ; Global variable offsets
    GP.BufferPtr 0x00    ; Pointer to allocated buffer (2 bytes)
    GP.BufferSize 0x02   ; Size of buffer (2 bytes)

.MAIN
    ENTER 0
    
    ; Allocate 256-byte buffer
    PUSHW 256
    POPZW ZP.ACC
    SYSCALL MemAllocate
    PUSHC                ; Check if successful
    BZF alloc_failed
    
    ; Store pointer in global variable
    PUSHZW ZP.IDX        ; Get allocated address
    POPGW GP.BufferPtr   ; Save to global
    
    ; Store size
    PUSHW 256
    POPGW GP.BufferSize
    
    ; Write byte to buffer[10]
    PUSHGW GP.BufferPtr  ; Get buffer address
    PUSHW 10             ; Add offset
    ADDW
    PUSHB 0x42           ; Value to write
    WRITEB               ; Write to memory
    
    ; Read back from buffer[10]
    PUSHGW GP.BufferPtr  ; Get buffer address
    PUSHW 10             ; Add offset
    ADDW
    READB                ; Read from memory
    ; Value 0x42 now on stack
    
    ; Write word to buffer[20]
    PUSHGW GP.BufferPtr  ; Get buffer address
    PUSHW 20             ; Add offset
    ADDW
    PUSHW 0x1234         ; Value to write
    WRITEW               ; Write word to memory
    
    ; Read back word from buffer[20]
    PUSHGW GP.BufferPtr  ; Get buffer address
    PUSHW 20             ; Add offset
    ADDW
    READW                ; Read word from memory
    ; Value 0x1234 now on stack (LSB, MSB)
    
    ; Free the buffer
    PUSHGW GP.BufferPtr  ; Get pointer from global
    POPZW ZP.IDX
    SYSCALL MemFree
    
    LEAVE
    HALT
    
alloc_failed:
    LEAVE
    HALT
```

### Array Access Example
```asm
; Demonstrate byte and word array access
.CONST
    ZP.TOP       0x12
    PrintHex     0x13

.DATA
byte_array:
    .byte 10, 20, 30, 40, 50

word_array:
    .word 0x1234, 0x5678, 0xABCD

.MAIN
    ENTER 0
    
    ; Access byte_array[2] (value 30)
    PUSHB 2              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX 0            ; Get address: byte_array + index
    READB                ; Read the byte
    POPA
    SYSCALL PrintHex     ; Print: 1E (hex for 30)
    
    ; Access word_array[1] (value 0x5678)
    PUSHB 1              ; Index
    PUSHB0               ; Extend to word
    PUSHDAX2 0           ; Get address: word_array + (index*2)
    READW                ; Read the word
    ; Stack now has: [78][56] (LSB, MSB)
    
    LEAVE
    HALT
```

## Performance Characteristics

### Speed
- Dispatch overhead: ~18 cycles
- Simple ops (PUSHB, DUPB): ~10-15 cycles
- Arithmetic ops: ~20-30 cycles
- Memory ops: ~25-35 cycles
- Function calls: ~40 cycles
- SYSCALL: ~50 cycles + BIOS time

### Code Density
- Most instructions: 1 byte (no operand)
- Common instructions: 2 bytes (byte operand)
- Rare instructions: 3 bytes (word operand)
- Average: ~1.5 bytes per operation
- Compression vs native: 3-4×

### Memory Usage
- VM interpreter: ~1KB
- Dispatch table: 256 bytes (even opcodes only)
- Function table: 256 bytes
- Global variables: 256 bytes (fixed allocation)
- Zero page usage: 16 bytes (0x60-0x6F)
- Stack usage: Hardware stack directly

## Benefits Summary

1. **Simplicity** - Complete VM under 1KB of 6502 code
2. **Speed** - Minimal dispatch overhead, no page checks
3. **Density** - Excellent compression, small programs
4. **Integration** - Direct BIOS access via SYSCALL
5. **Debugging** - Clear function boundaries, simple model
6. **Portability** - Runs on any 6502 with BIOS support
7. **Optimization** - Page-constrained execution eliminates checks
8. **Type Safety** - Separate byte/word operations prevent errors
9. **Memory Access** - Direct memory operations via READB/WRITEB/READW/WRITEW
10. **Register Usage** - Clever use of Y register as PC eliminates zero page PC storage
11. **Global Storage** - 256-byte dedicated page for persistent variables and memory pointers
12. **Array Support** - Built-in byte and word array access with PUSHDAX/PUSHDAX2 (both use word offsets)
13. **Word Operations** - Complete set of 16-bit bitwise and arithmetic operations
14. **Extended Stack** - Advanced stack manipulation with OVERW, ROTW, ROTB, PICKW

## Key Implementation Notes

1. **Stack Frame Offsets**: Arguments always start at BP+5 (saved BP at BP+1, return address at BP+2-4)
2. **Global Variables**: 256-byte dedicated page perfect for storing dynamic memory pointers
3. **Array Access**: Use PUSHDAX for byte arrays, PUSHDAX2 for word arrays (both take word offsets)
4. **Memory Pointers**: Store MemAllocate results in global variables for program-wide access
5. **Page Constraints**: Functions never cross page boundaries, enabling optimizations
6. **Y as PC**: Y register serves as program counter within current function page
7. **Word Operations**: READW/WRITEW complement READB/WRITEB for 16-bit memory access
8. **Operand Types**: PUSHDAX and PUSHDAX2 use word offset operands, not byte offsets
9. **Function IDs**: Even numbers only (2, 4, 6... 254), with .MAIN always at ID 2
10. **Function Order**: Functions must be defined before they are called

This VM occupies a sweet spot between interpreted BASIC and native assembly, providing good performance with excellent code density for memory-constrained 6502 systems. The page-constrained execution model and use of the Y register as PC are key innovations that make this VM particularly efficient on the 6502 architecture.