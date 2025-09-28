# Simple 6502 VM Specification (v3.0)

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
- Globals block (allocated at startup)
- Strings block (allocated at startup)
- Function table (256 bytes at fixed location)
```

### VM Registers (Zero Page)
```
PCL:      0x60  // Instruction pointer low (changes within function)
PCH:      0x61  // Instruction pointer high (page number, changes only on CALL/RETURN)
BP:       0x62  // Base pointer for stack frame
GLOBALS:  0x63  // Globals base address (2 bytes)
STRINGS:  0x65  // Strings base address (2 bytes)
```

### Function Organization
- Functions start at page boundaries (0x2000, 0x2100, 0x2200...)
- Function IDs: 0, 1, 2, 3... up to 127
- Function table uses even offsets: ID×2 gives table index
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
INCLB        0x30                   // Local variable 8-bit increment
INCLW        0x32                   // Local variable 16-bit increment
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
SHLW         0x4E                   // Shift word left by byte operand
SHRW         0x50                   // Shift word right by byte operand
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
PUSHD        0x98  + byte           // Push string address (byte offset)
PUSHD2       0x9A  + word           // Push string address (word offset)
STRC         0x9C                   // Pop index, pop string, push char
STRCMP       0x9E                   // Pop 2 strings, push -1/0/1
```

### Memory Access Operations (0xA0-0xA2)
```
READB        0xA0                   // Pop address word, push byte
WRITEB       0xA2                   // Pop address word, pop byte
```

### Branch Offset Handling

All branch instructions use positive byte offsets (0-255):
- **Forward branches** (BRAF, BZF, BNZF): Add offset to PC of next instruction
- **Reverse branches** (BRAR, BZR, BNZR): Subtract offset from PC of next instruction
- Branches are guaranteed to stay within the current 256-byte page
- No page crossing checks needed

## BIOS System Call IDs

These are the numeric values for BIOS system calls. Copy these directly into your .CONST section:

```
; Memory Management
Memory.Allocate      0x00  ; In: ZP.ACC=size(16b) | Out: ZP.IDX=addr, C=success
Memory.Free          0x01  ; In: ZP.IDX=address | Out: C=success
Memory.Available     0x02  ; In: None | Out: ZP.ACC=free bytes
Memory.Maximum       0x03  ; In: None | Out: ZP.ACC=largest block

; File Operations
File.Exists          0x04  ; In: ZP.STR=filename, A=type | Out: C=exists
File.Delete          0x05  ; In: ZP.STR=filename | Out: C=success
File.Dir             0x06  ; In: None | Out: C=success (prints to serial)
File.StartSave       0x07  ; In: ZP.STR=filename | Out: C=success
File.AppendStream    0x08  ; In: FSOURCEADDRESS=data, FLENGTH=bytes | Out: C=success
File.EndSave         0x09  ; In: A=type(0x80=exec,0x00=data) | Out: C=success
File.StartLoad       0x0A  ; In: ZP.STR=filename, A=type | Out: C=success
File.NextStream      0x0B  ; In: None | Out: C=data available, FLENGTH=bytes
File.Format          0x0C  ; In: None | Out: C=success

; Serial I/O
Serial.WriteChar     0x0D  ; In: A=character | Out: None
Serial.WaitForChar   0x0E  ; In: None | Out: A=character
Serial.IsAvailable   0x0F  ; In: None | Out: C=available
IsBreak              0x10  ; In: None | Out: C=break detected

; Print/Console
Print.String         0x11  ; In: ZP.STR=string pointer | Out: None
Print.Char           0x12  ; In: A=character | Out: None
Print.Hex            0x13  ; In: A=byte | Out: None (prints 2 hex digits)
Print.NewLine        0x14  ; In: None | Out: None
Print.Space          0x15  ; In: None | Out: None
Print.Spaces         0x16  ; In: Y=count | Out: None

; Timer Services
Time.Delay           0x17  ; In: ZP.TOP=ms(32b) | Out: None
Time.Millis          0x18  ; In: None | Out: ZP.TOP=ms since boot(32b)
Time.Seconds         0x19  ; In: None | Out: ZP.TOP=seconds(32b)

; Long Math (32-bit)
Long.Add             0x1A  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT+TOP
Long.Sub             0x1B  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT-TOP
Long.Mul             0x1C  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT*TOP
Long.Div             0x1D  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT/TOP, C=success
Long.Mod             0x1E  ; In: ZP.NEXT, ZP.TOP | Out: ZP.NEXT=NEXT%TOP, C=success
Long.Print           0x1F  ; In: ZP.TOP=value | Out: None (prints decimal)
Long.LT              0x20  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT<TOP)
Long.GT              0x21  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT>TOP)
Long.EQ              0x22  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT==TOP)
Long.NE              0x23  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT!=TOP)
Long.LE              0x24  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT<=TOP)
Long.GE              0x25  ; In: ZP.NEXT, ZP.TOP | Out: C=(NEXT>=TOP)

; GPIO
GPIO.PinMode         0x26  ; In: A=pin(0-15), Y=mode(0=IN,1=OUT) | Out: None
GPIO.PinRead         0x27  ; In: A=pin(0-15) | Out: A=value(0/1), Z=LOW
GPIO.PinWrite        0x28  ; In: A=pin(0-15), Y=value(0/1) | Out: None

; Float Math (Optional)
Float.Add            0x29  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT+TOP
Float.Sub            0x2A  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT-TOP
Float.Mul            0x2B  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT*TOP
Float.Div            0x2C  ; In: ZP.NEXT, ZP.TOP(IEEE754) | Out: ZP.NEXT=NEXT/TOP
Float.ToLong         0x2D  ; In: ZP.NEXT(IEEE float) | Out: ZP.NEXT=(long)NEXT
Float.LT             0x2E  ; In: ZP.NEXT, ZP.TOP(IEEE floats) | Out: C=(NEXT<TOP)
Float.EQ             0x2F  ; In: ZP.NEXT, ZP.TOP(IEEE floats) | Out: C=(NEXT==TOP)

; File I/O (Optional)
File.Open            0x30  ; In: STR=filename, NEXT=mode("w"/"r") | Out: TOP=FILE*/NULL
File.Close           0x31  ; In: NEXT=FILE* | Out: TOP=0/-1
File.GetC            0x32  ; In: NEXT=FILE* | Out: TOP=char(0-255)/-1
File.Read            0x33  ; In: IDX=buf, IDY=size, ACC=count, NEXT=FILE* | Out: TOP=bytes/-1
File.PutC            0x34  ; In: ACC=char, NEXT=FILE* | Out: TOP=char/-1
File.Write           0x35  ; In: IDX=buf, IDY=size, ACC=count, NEXT=FILE* | Out: TOP=bytes/-1

; Zero Page Locations (Common)
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
```asm
mainLoop:
    LDA [VM_IP_L]          // Get opcode
    TAX                    // Use as index (already even)
    INC VM_IP_L            // Next byte (no page check!)
    JMP [DISPATCH_TABLE,X] // Jump to handler
    // Total: ~18 cycles overhead
```

### Page-Constrained Execution
- **Critical**: VM_IP_H only changes during CALL and RETURN
- All other operations only modify VM_IP_L
- No page-crossing checks needed
- Branches always stay within current page
- Enables massive optimization in dispatch

### Branch Offset Handling

#### Forward Branches (BRAF, BZF, BNZF)
- Offsets from 0 to 255
- Add offset to PC of next instruction
- Always stays within current page

#### Reverse Branches (BRAR, BZR, BNZR)
- Offsets from 0 to 255
- Subtract offset from PC of next instruction
- Always stays within current page

### Function Call Mechanism

```asm
; CALL operation
LDA [VM_IP_L]         ; Get function ID
ASL                   ; × 2 for table offset
TAX
INC VM_IP_L           ; Skip operand
LDA VM_IP_L
PHA                   ; Push return address low
LDA VM_IP_H
PHA                   ; Push return address high
LDA #0
STA VM_IP_L           ; Functions start at page offset 0
LDA FUNCTION_TABLE,X
STA VM_IP_H           ; Load function page
; Continue execution
```

### Stack Frame Layout
```
High Memory
    [Argument N]
    ...
    [Argument 1]
    [Return IP Low]
    [Return IP High]
    [Saved BP]        <- BP+1 (saved by ENTER)
    [Local 1 Low]     <- BP-1,BP (first local, 16-bit)
    [Local 1 High]    
    [Local 2 Low]     <- BP-3,BP-2 (second local, 16-bit)  
    [Local 2 High]    
    ...               <- SP points here
Low Memory
```

## Program Format

### File Header
```
Offset  Size  Description
0x0000  3     Magic number (0x564D42 "VMB")
0x0003  1     Function count
0x0004  2     Data size in bytes (little endian)
0x0006  ...   Function table (4 bytes per function)
              Each entry: offset(2) + size(2)
...     N     Constant data section
...     ...   Function code follows
```

### Memory Initialization
1. Load program header
2. Allocate globals block from heap
3. Zero-initialize globals
4. Allocate strings block from heap
5. Copy string data to strings block
6. Build function table
7. Call main function (ID from header)

## Assembler Mnemonics

The VM assembler accepts these mnemonics:

```asm
; Example VM assembly program
.MAIN
    PUSHW 0x1234      ; Push constant
    POPGW      0      ; Store to global[0]
    
    PUSHD 0           ; Push first string
    POPZW ZP.STR
    SYSCALL Print.String ; Call BIOS print
    
    CALL Helper           ; Call function 2
    
loop:                     ; Labels for branches
    PUSHGB 0
    PUSHB1
    SUBB
    DUPW
    POPGB 0
    BNZR loop     ; Branch reverse
    
    RET

.FUNC Helper
    PUSHB 42
    POPA                  ; Store to A register
    SYSCALL Print.Char ; Call BIOS print
    RET                   ; Return
```

## VM Assembly Examples

### Hello World
```asm
; Simple Hello World program
.CONST
    ZP.STR       0x1E
    Print.String 0x11
    
.DATA
    STR0 "Hello, World!\n"

.MAIN
    PUSHD 0              ; Push string 0 address
    POPZW ZP.STR         ; Marshal to BIOS
    SYSCALL Print.String ; Print it
    
    HALT                 ; Return to BIOS
```

### Print Digits 0-9
```asm
; Print digits from 0 to 9
.CONST
    ZP.STR       0x1E
    Print.String 0x11
    Print.Char   0x12
    
.DATA
    STR0 "Counting: "
    STR1 "\nDone!\n"

.MAIN
    ; Print header
    PUSHD 0             
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Initialize counter to 0
    PUSHB0
    POPGB 0              ; global[0] = counter
    
loop:
    ; Print the digit
    PUSHGB 0             ; Get counter
    PUSHB '0'            ; ASCII '0' 
    ADDB                 ; Convert to ASCII digit
    POPA                 
    SYSCALL Print.Char
    
    ; Print space
    PUSHB ' '
    POPA
    SYSCALL Print.Char
    
    ; Increment counter
    PUSHGB 0
    PUSHB1
    ADDB
    DUPW                 ; Keep copy for comparison
    POPGB 0              ; Store back
    
    ; Check if we've done 10 digits
    PUSHB 10
    EQB                  ; Compare with 10
    BZF loop             ; Loop if not equal
    
    ; Print footer
    PUSHD 1
    POPZW ZP.STR
    SYSCALL Print.String
    
    HALT
```

### Memory Allocation Test
```asm
; Test memory allocation and deallocation
.CONST
    ZP.STR           0x1E
    ZP.ACC           0x10
    ZP.IDX           0x1A
    Print.String     0x11
    Print.Char       0x12
    Memory.Allocate  0x00
    Memory.Free      0x01
    
.DATA
    STR0 "Allocating 256 bytes..."
    STR1 "Success! Address: "
    STR2 "Failed!\n"
    STR3 "\nFreeing memory..."
    STR4 "Done.\n"

.MAIN
    ; Print allocation message
    PUSHD 0
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Allocate 256 bytes
    PUSHW 256
    POPZW ZP.ACC         ; Size to ZP.ACC
    SYSCALL Memory.Allocate
    PUSHC                ; Get success flag
    BZF failed
    
    ; Success - print message and address
    PUSHD 1
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Save and print address (in hex would be nice!)
    PUSHZW ZP.IDX        ; Get allocated address
    DUPW
    POPGW 0              ; Save for later
    
    ; Print high byte
    SWAPW                ; Get high byte
    CALL PrintHex
    
    ; Print low byte  
    CALL PrintHex
    
    ; Free the memory
    PUSHD 3
    POPZW ZP.STR
    SYSCALL Print.String
    
    PUSHGW 0             ; Get saved address
    POPZW ZP.IDX
    SYSCALL Memory.Free
    
    PUSHD 4
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
    
failed:
    PUSHD 2
    POPZW ZP.STR
    SYSCALL Print.String
    HALT

.FUNC PrintHex           ; Helper to print byte as 2 hex digits
    ; Input: byte on stack
    DUPW
    PUSHB 4
    SHRW                 ; High nibble
    CALL PrintNibble
    
    PUSHB 0x0F
    ANDB                 ; Low nibble
    CALL PrintNibble
    RET

.FUNC PrintNibble        ; Print single hex digit
    ; Input: nibble (0-15) on stack
    DUPW
    PUSHB 10
    LTB                  ; Check if < 10
    BZF letter
    
    PUSHB '0'
    ADDB
    POPA
    SYSCALL Print.Char
    RET
    
letter:
    PUSHB 10
    SUBB
    PUSHB 'A'
    ADDB
    POPA
    SYSCALL Print.Char
    RET
```

### String Comparison
```asm
; Compare two strings
.CONST
    ZP.STR       0x1E
    Print.String 0x11
    
.DATA
    STR0 "Hello"
    STR1 "Hello"
    STR2 "World"
    STR3 "Strings match!\n"
    STR4 "Strings differ!\n"

.MAIN
    ; Compare STR0 and STR1 (should match)
    PUSHD 0
    PUSHD 1
    STRCMP               ; Returns 0 if equal
    BNZF different1
    
    PUSHD 3
    POPZW ZP.STR
    SYSCALL Print.String
    BRAF test2
    
different1:
    PUSHD 4
    POPZW ZP.STR
    SYSCALL Print.String
    
test2:
    ; Compare STR0 and STR2 (should differ)
    PUSHD 0
    PUSHD 2
    STRCMP
    BNZF different2
    
    PUSHD 3
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
    
different2:
    PUSHD 4
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
```

### Memory Access Example
```asm
; Direct memory access using READB/WRITEB
.CONST
    ZP.STR       0x1E
    ZP.IDX       0x1A
    Print.String 0x11
    Print.Hex    0x13
    Memory.Allocate 0x00
    
.DATA
    STR0 "Allocated memory at: "
    STR1 "\nValue written: "
    STR2 "\nValue read back: "

.MAIN
    ; Allocate 16 bytes of memory
    PUSHW 16
    POPZW ZP.ACC
    SYSCALL Memory.Allocate
    PUSHC
    BZF failed
    
    ; Print allocation message
    PUSHD 0
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Print allocated address
    PUSHZW ZP.IDX
    DUPW
    POPGW 0              ; Save address for later
    
    ; Write a byte to allocated memory
    PUSHGW 0             ; Address
    PUSHB 0x42           ; Value to write
    WRITEB               ; Write byte to memory
    
    ; Print what we wrote
    PUSHD 1
    POPZW ZP.STR
    SYSCALL Print.String
    PUSHB 0x42
    POPA
    SYSCALL Print.Hex
    
    ; Read the byte back
    PUSHGW 0             ; Address  
    READB                ; Read byte from memory
    
    ; Print what we read
    PUSHD 2
    POPZW ZP.STR
    SYSCALL Print.String
    POPA
    SYSCALL Print.Hex
    
    HALT
    
failed:
    ; Handle allocation failure
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
- Dispatch table: 256 bytes
- Function table: 256 bytes
- Zero page usage: 16 bytes
- Stack usage: Hardware stack directly

## Language Support

### Minimum Requirements
- Fixed-size types only
- Static memory allocation
- Structured control flow
- Function calls with stack passing

### Ideal Language Features
- No dynamic allocation (or startup-only)
- No pointer arithmetic
- Value types only
- Compile-time sized arrays
- Minimal runtime overhead

### C Subset Example
```c
byte count;
int total;

void addValue(byte val) {
    total = total + val;
    count++;
}

void main() {
    count = 0;
    total = 0;
    
    for (byte i = 0; i < 10; i++) {
        addValue(i);
    }
    
    printf("Count: %d, Total: %d\n", count, total);
}
```

## New Features in v3.0

### Optimized Constant Instructions
- PUSHB0, PUSHB1, PUSHW0, PUSHW1 for common constants
- No operand bytes needed for these frequent values
- Improved code density for initialization and boolean operations

### Enhanced Stack Operations
- Separate byte and word operations (DUPB/DUPW, DROPB/DROPW, SWAPB/SWAPW)
- Better type safety and clearer semantics
- Optimized implementation for each data size

### 32-bit Support
- PUSHZQ, POPZQ, PUSHLQ, POPLQ for 32-bit operations
- Support for long integers and timer values
- Efficient 32-bit stack frame operations

### Local Variable Optimization
- INCLB, INCLW for direct local variable increment
- Reduced instruction count for common loop patterns
- Better compiler optimization opportunities

### Enhanced Control Flow
- BRAR (Branch Reverse) for backward branches
- Clearer semantics than signed offsets
- Symmetric forward/reverse branch instruction set

### Memory Access Operations
- READB/WRITEB for direct memory access via stack addresses
- Enables pointer-based operations and dynamic memory access
- Stack-based addressing for flexible memory manipulation

### System Call Improvements
- SYSCALLX for optimized system calls
- DUMP instruction for debugging support
- Better integration with BIOS debugging facilities

## Benefits Summary

1. **Simplicity** - Complete VM under 1KB of 6502 code
2. **Speed** - Minimal dispatch overhead, no page checks
3. **Density** - Excellent compression, small programs
4. **Integration** - Direct BIOS access via SYSCALL
5. **Debugging** - Clear function boundaries, simple model
6. **Portability** - Runs on any 6502 with BIOS support
7. **Optimization** - New constant and increment instructions
8. **Type Safety** - Separate byte/word operations prevent errors
9. **Memory Access** - Direct memory operations via stack addressing

This VM occupies a sweet spot between interpreted BASIC and native assembly, providing good performance with excellent code density for memory-constrained 6502 systems. Version 3.0 adds significant improvements in code density, type safety, and debugging support while maintaining the core simplicity that makes the VM practical for embedded systems.