# Simple 6502 VM Specification (v2.0)

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

### Stack Operations (0x00-0x12)
```
PUSHB        0x00  + byte           // Push 8-bit immediate
PUSHW        0x02  + word           // Push 16-bit immediate
PUSH0        0x04                   // Push 0 (optimized)
PUSH1        0x06                   // Push 1 (optimized)
DUP          0x08                   // Duplicate byte on TOS
DUPW         0x0A                   // Duplicate word on TOS
DROP         0x0C                   // Remove byte from TOS
DROPW        0x0E                   // Remove word from TOS
SWAP         0x10                   // Swap top two bytes
SWAPW        0x12                   // Swap top two words
```

### Arithmetic Operations (0x14-0x24)
```
ADD          0x14                   // Pop 2 words, push sum
SUB          0x16                   // Pop 2 words, push difference
MUL          0x18                   // Pop 2 words, push product
DIV          0x1A                   // Pop 2 words, push quotient (unsigned)
MOD          0x1C                   // Pop 2 words, push remainder
ADDB         0x1E                   // Pop 2 bytes, push sum
SUBB         0x20                   // Pop 2 bytes, push difference
NEG          0x22                   // Negate int (2's complement)
NEGB         0x24                   // Negate char (2's complement)
```

### Comparison Operations (0x26-0x38)
```
EQ           0x26                   // Pop 2 words or ints, push 1 if equal, 0 if not
NE           0x28                   // Pop 2 words or ints, push 1 if not equal
LT           0x2A                   // Pop 2 words, unsigned less than
GT           0x2C                   // Pop 2 words, unsigned greater than
LE           0x2E                   // Pop 2 words, unsigned less or equal
GE           0x30                   // Pop 2 words, unsigned greater or equal
LTC          0x32                   // Pop 2 chars, signed less than
GTC          0x34                   // Pop 2 chars, signed greater than
LTI          0x36                   // Pop 2 ints, signed less than
GTI          0x38                   // Pop 2 ints, signed greater than
```

### Bitwise Operations (0x3A-0x46)
```
AND          0x3A                   // Pop 2 words, push bitwise AND
OR           0x3C                   // Pop 2 words, push bitwise OR
XOR          0x3E                   // Pop 2 words, push bitwise XOR
NOT          0x40                   // Pop word, push bitwise NOT
SHL          0x42                   // Pop word and count, shift left
SHR          0x44                   // Pop word and count, logical shift right
SAR          0x46                   // Pop int and count, arithmetic shift right
```

### Memory Operations - Globals (0x48-0x56)
```
PUSHGB       0x48  + byte           // Push byte from global[offset]
PUSHGB2      0x4A  + word           // Push byte from global[offset] (>255)
PUSHGW       0x4C  + byte           // Push word from global[offset]
PUSHGW2      0x4E  + word           // Push word from global[offset] (>255)
POPGB        0x50  + byte           // Pop byte to global[offset]
POPGB2       0x52  + word           // Pop byte to global[offset] (>255)
POPGW        0x54  + byte           // Pop word to global[offset]
POPGW2       0x56  + word           // Pop word to global[offset] (>255)
```

### Memory Operations - Locals (0x58-0x5E)
```
PUSHLB       0x58  + byte           // Push byte from BP[offset]
PUSHLW       0x5A  + byte           // Push word from BP[offset]
POPLB        0x5C  + byte           // Pop byte to BP[offset]
POPLW        0x5E  + byte           // Pop word to BP[offset]
```

### String Operations (0x60-0x66)
```
PUSHSTR      0x60  + byte           // Push string address (byte offset)
PUSHSTR2     0x62  + word           // Push string address (word offset)
STRC         0x64                   // Pop index, pop string, push char
STRCMP       0x66                   // Pop 2 strings, push -1/0/1
```

### Control Flow (0x68-0x76)
```
CALL         0x68  + byte           // Call function (ID × 2 for table lookup)
RET          0x6A                   // Return from function
BRAB         0x6C  + byte           // Branch backward by byte (0-255)
BRAF         0x6E  + byte           // Branch forward by byte (0-255)
BZF          0x70  + byte           // Branch forward by byte if TOS is zero
BZB          0x72  + byte           // Branch backward by byte if TOS is zero
BNZF         0x74  + byte           // Branch forward by byte if TOS is not zero
BNZB         0x76  + byte           // Branch backward by byte if TOS is not zero
```

### Zero Page Operations (0x78-0x7E)
```
PUSHZB       0x78  + byte           // Push byte from ZP[offset]
PUSHZW       0x7A  + byte           // Push word from ZP[offset]
POPZB        0x7C  + byte           // Pop byte to ZP[offset]
POPZW        0x7E  + byte           // Pop word to ZP[offset]
```

### System Operations (0x80-0x8C)
```
SYSCALL      0x80  + byte           // Call BIOS function via X register
HALT         0x82                   // Stop execution (return to BIOS)
```

### Register Operations (0x84-0x8C)
```
POPA         0x84                   // Pop byte from stack to A register
POPY         0x86                   // Pop byte from stack to Y register
PUSHA        0x88                   // Push A register to stack
PUSHC        0x8A                   // Push carry flag (1 if set, 0 if clear)
PUSHZ        0x8C                   // Push zero flag (1 if set, 0 if clear)
```

### Branch Offset Handling

All branch instructions use positive byte offsets (0-255):
- **Forward branches** (BRAF, BZF, BNZF): Add offset to PC of next instruction
- **Backward branches** (BRAB, BZB, BNZB): Subtract offset from PC of next instruction
- Branches are guaranteed to stay within the current 256-byte page
- No page crossing checks needed
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

#### JUMP_CHAR (Most Common)
- Offsets from -128 to +127
- Covers 99% of branches
- Simply adds signed byte to VM_IP_L
- No page crossing possible

#### JUMP_INT (Rare)
- Offsets from -255 to +255
- For unusual cases (long function epilogue)
- Still only updates VM_IP_L
- Compiler uses sparingly

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
    [Saved BP]        <- New BP points here
    [Local 1]
    [Local 2]
    ...               <- SP points here
Low Memory
```

## Program Format

### File Header
```
Offset  Size  Description
0x0000  2     Magic number (0x564D "VM")
0x0002  2     Code size in bytes
0x0004  2     Globals size in bytes
0x0006  2     Strings size in bytes
0x0008  1     Function count
0x0009  1     Entry point (main function ID)
0x000A  ...   Code follows
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
    
    PUSHSTR 0         ; Push first string
    POPZW ZP.STR
    SYSCALL Print.String ; Call BIOS print
    
    CALL Helper           ; Call function 2
    
loop:                     ; Labels for branches
    PUSHGB 0
    PUSH1
    SUBB
    DUP
    POPGB 0
    JNZ loop     ; Branch backward
    
    RET

.FUNC Helper
    PUSH 42
    POPA                  ; Store to A BIOS
    SYSCALL Print.Char ; Call BIOS print
    RET                   ; Return
```

## VM Assembly Examples

### Hello World
```asm
; Simple Hello World program
.CONST
    ZP.STR  0x1E
    
    Print.String 0x11
    
.DATA
    STR0 "Hello, World!\n"

.MAIN
    PUSHSTR STR0          ; Push string address
    POPZW ZP.STR          ; Marshal to BIOS
    SYSCALL Print.String  ; Print it
    
    HALT                  ; Return to BIOS
```

### Print Digits 0-9
```asm
; Print digits from 0 to 9
.CONST
    ZP.STR  0x1E
    
    Print.String 0x11
    Print.Char   0x12
    
.DATA
    STR0 "Counting: "
    STR1 "\nDone!\n"

.MAIN
    ; Print header
    PUSHSTR STR0             
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Initialize counter to 0
    PUSH 0
    POPGB 0              ; global[0] = counter
    
loop:
    ; Print the digit
    PUSHGB 0             ; Get counter
    PUSH '0'             ; ASCII '0' 
    ADDB                 ; Convert to ASCII digit
    POPA                 
    SYSCALL Print.Char
    
    ; Print space
    PUSH ' '
    POPA
    SYSCALL Print.Char
    
    ; Increment counter
    PUSHGB 0
    PUSH1
    ADDB
    DUP                  ; Keep copy for comparison
    POPGB 0              ; Store back
    
    ; Check if we've done 10 digits
    PUSH 10
    EQ                   ; Compare with 10
    JZ loop              ; Loop if not equal
    
    ; Print footer
    PUSHSTR STR1
    POPZW ZP.STR
    SYSCALL Print.String
    
    HALT
```

### Memory Allocation Test
```asm
; Test memory allocation and deallocation
.DATA
    STR0 "Allocating 256 bytes..."
    STR1 "Success! Address: "
    STR2 "Failed!\n"
    STR3 "\nFreeing memory..."
    STR4 "Done.\n"

.MAIN
    ; Print allocation message
    PUSHSTR STR0
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Allocate 256 bytes
    PUSHW 256
    POPZW ZP.ACC         ; Size to ZP.ACC
    SYSCALL Memory.Allocate
    PUSHC                ; Get success flag
    JZ failed
    
    ; Success - print message and address
    PUSHSTR STR1
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Save and print address (in hex would be nice!)
    PUSHZW ZP.IDX        ; Get allocated address
    DUPW
    POPGW 0              ; Save for later
    
    ; Print high byte
    SWAP                 ; Get high byte
    CALL PrintHex
    
    ; Print low byte  
    CALL PrintHex
    
    ; Free the memory
    PUSHSTR STR3
    POPZW ZP.STR
    SYSCALL Print.String
    
    PUSHGW 0             ; Get saved address
    POPZW ZP.IDX
    SYSCALL Memory.Free
    
    PUSHSTR STR4
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
    
failed:
    PUSHSTR STR2
    POPZW ZP.STR
    SYSCALL Print.String
    HALT

.FUNC PrintHex           ; Helper to print byte as 2 hex digits
    ; Input: byte on stack
    DUP
    PUSH 4
    SHR                  ; High nibble
    CALL PrintNibble
    
    PUSH 0x0F
    AND                  ; Low nibble
    CALL PrintNibble
    RET

.FUNC PrintNibble        ; Print single hex digit
    ; Input: nibble (0-15) on stack
    DUP
    PUSH 10
    LT                   ; Check if < 10
    JZ letter
    
    PUSH '0'
    ADDB
    POPA
    SYSCALL Print.Char
    RET
    
letter:
    PUSH 10
    SUBB
    PUSH 'A'
    ADDB
    POPA
    SYSCALL Print.Char
    RET
```

### Fibonacci Sequence
```asm
; Print first 10 Fibonacci numbers
.DATA
    STR0: "Fibonacci: "
    STR1: " "

.MAIN
    ; Print header
    PUSHSTR STR0
    POPZW ZP.STR
    SYSCALL Print.String
    
    ; Initialize: fib[0]=0, fib[1]=1
    PUSH 0
    POPGB 0              ; n-2 term
    PUSH 1
    POPGB 1              ; n-1 term
    PUSH 0
    POPGB 2              ; counter
    
    ; Print first number (0)
    PUSH '0'
    POPA
    SYSCALL Print.Char
    CALL PrintSpace
    
    ; Print second number (1) 
    PUSH '1'
    POPA
    SYSCALL Print.Char
    CALL PrintSpace
    
loop:
    ; Calculate next: fib[n] = fib[n-1] + fib[n-2]
    PUSHGB 0             ; n-2
    PUSHGB 1             ; n-1
    ADDB
    DUP
    
    ; Update for next iteration
    PUSHGB 1
    POPGB 0              ; n-2 = old n-1
    POPGB 1              ; n-1 = new value
    
    ; Print the number (simplified, only works for single digits)
    PUSHGB 1
    PUSH '0'
    ADDB
    POPA
    SYSCALL Print.Char
    CALL PrintSpace
    
    ; Increment counter
    PUSHGB 2
    PUSH1
    ADDB
    DUP
    POPGB 2
    
    ; Check if we've printed 8 more (total 10)
    PUSH 8
    EQ
    JZ loop
    
    HALT

.FUNC PrintSpace
    PUSHSTR STR1         ; Space string
    POPZW ZP.STR
    SYSCALL Print.String
    RET
```

### String Comparison
```asm
; Compare two strings
.DATA
    STR0: "Hello"
    STR1: "Hello"
    STR2: "World"
    STR3: "Strings match!\n"
    STR4: "Strings differ!\n"

.MAIN
    ; Compare STR0 and STR1 (should match)
    PUSHSTR STR0
    PUSHSTR STR1
    STRCMP               ; Returns 0 if equal
    JNZ different1
    
    PUSHSTR STR3
    POPZW ZP.STR
    SYSCALL Print.String
    BRA test2
    
different1:
    PUSHSTR STR4
    POPZW ZP.STR
    SYSCALL Print.String
    
test2:
    ; Compare STR0 and STR2 (should differ)
    PUSHSTR STR0
    PUSHSTR STR2
    STRCMP
    JNZ different2
    
    PUSHSTR STR3
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
    
different2:
    PUSHSTR STR4
    POPZW ZP.STR
    SYSCALL Print.String
    HALT
```




## Performance Characteristics

### Speed
- Dispatch overhead: ~18 cycles
- Simple ops (PUSH, DUP): ~10-15 cycles
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

## Benefits Summary

1. **Simplicity** - Complete VM under 1KB of 6502 code
2. **Speed** - Minimal dispatch overhead, no page checks
3. **Density** - Excellent compression, small programs
4. **Integration** - Direct BIOS access via SYSCALL
5. **Debugging** - Clear function boundaries, simple model
6. **Portability** - Runs on any 6502 with BIOS support

This VM occupies a sweet spot between interpreted BASIC and native assembly, providing good performance with excellent code density for memory-constrained 6502 systems.