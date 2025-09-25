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
VM_IP_L:  0x60  // Instruction pointer low (changes within function)
VM_IP_H:  0x61  // Instruction pointer high (page number, changes only on CALL/RETURN)
VM_BP:    0x62  // Base pointer for stack frame
GLOBALS:  0x63  // Globals base address (2 bytes)
STRINGS:  0x65  // Strings base address (2 bytes)
VM_TEMP:  0x67  // Temporary workspace (2 bytes)
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
PUSH_BYTE    0x00  + byte           // Push 8-bit immediate
PUSH_WORD    0x02  + word           // Push 16-bit immediate
PUSH_ZERO    0x04                   // Push 0 (optimized)
PUSH_ONE     0x06                   // Push 1 (optimized)
DUP          0x08                   // Duplicate byte on TOS
DUP2         0x0A                   // Duplicate word on TOS
DROP         0x0C                   // Remove byte from TOS
DROP2        0x0E                   // Remove word from TOS
SWAP         0x10                   // Swap top two bytes
SWAP2        0x12                   // Swap top two words
```

### Arithmetic Operations (0x14-0x24)
```
ADD          0x14                   // Pop 2 words, push sum
SUB          0x16                   // Pop 2 words, push difference
MUL          0x18                   // Pop 2 words, push product
DIV          0x1A                   // Pop 2 words, push quotient (unsigned)
MOD          0x1C                   // Pop 2 words, push remainder
ADD_BYTE     0x1E                   // Pop 2 bytes, push sum
SUB_BYTE     0x20                   // Pop 2 bytes, push difference
NEG          0x22                   // Negate int (2's complement)
NEG_BYTE     0x24                   // Negate char (2's complement)
```

### Comparison Operations (0x26-0x38)
```
EQ           0x26                   // Pop 2 values, push 1 if equal, 0 if not
NE           0x28                   // Pop 2 values, push 1 if not equal
LT           0x2A                   // Pop 2 words, unsigned less than
GT           0x2C                   // Pop 2 words, unsigned greater than
LE           0x2E                   // Pop 2 words, unsigned less or equal
GE           0x30                   // Pop 2 words, unsigned greater or equal
LT_CHAR      0x32                   // Pop 2 chars, signed less than
GT_CHAR      0x34                   // Pop 2 chars, signed greater than
LT_INT       0x36                   // Pop 2 ints, signed less than
GT_INT       0x38                   // Pop 2 ints, signed greater than
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

### Memory Operations - Globals (0x48-0x5A)
```
LOAD_GLOBAL_B    0x48  + byte       // Load byte from global[offset]
LOAD_GLOBAL_W    0x4A  + word       // Load byte from global[offset] (>255)
LOAD_GLOBAL2_B   0x4C  + byte       // Load word from global[offset]
LOAD_GLOBAL2_W   0x4E  + word       // Load word from global[offset] (>255)
STORE_GLOBAL_B   0x50  + byte       // Store byte to global[offset]
STORE_GLOBAL_W   0x52  + word       // Store byte to global[offset] (>255)
STORE_GLOBAL2_B  0x54  + byte       // Store word to global[offset]
STORE_GLOBAL2_W  0x56  + word       // Store word to global[offset] (>255)
PUSH_GLOBAL_B    0x58  + byte       // Push address of global[offset]
PUSH_GLOBAL_W    0x5A  + word       // Push address of global[offset] (>255)
```

### Memory Operations - Locals (0x5C-0x62)
```
LOAD_LOCAL       0x5C  + byte       // Load byte from BP[offset]
LOAD_LOCAL2      0x5E  + byte       // Load word from BP[offset]
STORE_LOCAL      0x60  + byte       // Store byte to BP[offset]
STORE_LOCAL2     0x62  + byte       // Store word to BP[offset]
```

### Memory Operations - Indirect (0x64-0x6E)
```
LOAD_IND         0x64               // Pop address, push byte
LOAD_IND2        0x66               // Pop address, push word
STORE_IND        0x68               // Pop byte, pop address, store
STORE_IND2       0x6A               // Pop word, pop address, store
INDEX_BYTE       0x6C               // Pop byte index, pop base, push addr
INDEX_WORD       0x6E               // Pop word index, pop base, push addr
```

### String Operations (0x70-0x7E)
```
PUSH_STRING_B    0x70  + byte       // Push string address (byte offset)
PUSH_STRING_W    0x72  + word       // Push string address (word offset)
PUSH_STRING_0    0x74               // Push address of string 0
PUSH_STRING_1    0x76               // Push address of string 1
PUSH_STRING_2    0x78               // Push address of string 2
STRING_CHAR      0x7A               // Pop index, pop string, push char
STRING_CHAR_0    0x7C               // Pop string, push first char
STRING_COMPARE   0x7E               // Pop 2 strings, push -1/0/1
```

### Control Flow (0x80-0x8E)
```
CALL             0x80  + byte       // Call function (ID × 2 for table lookup)
RETURN           0x82               // Return from function
JUMP_CHAR        0x84  + sbyte      // Branch by signed byte offset
JUMP_INT         0x86  + word       // Branch by word offset (rare)
JUMP_Z_CHAR      0x88  + sbyte      // Branch if TOS is zero
JUMP_Z_INT       0x8A  + word       // Branch if TOS is zero (rare)
JUMP_NZ_CHAR     0x8C  + sbyte      // Branch if TOS is not zero
JUMP_NZ_INT      0x8E  + word       // Branch if TOS is not zero (rare)
```

### I/O Operations (0x90-0x9A)
```
PRINT_CHAR       0x90               // Pop char, print as ASCII
PRINT_BYTE       0x92               // Pop byte, print as 0-255
PRINT_INT        0x94               // Pop int, print as signed
PRINT_WORD       0x96               // Pop word, print as unsigned
PRINT_STRING     0x98               // Pop string pointer, print
READ_BYTE        0x9A               // Read byte from input, push
```

### System Operations (0x9C-0x9E)
```
SYSCALL          0x9C  + byte       // Call BIOS function via X register
HALT             0x9E               // Stop execution
```

## Implementation Details

### Dispatch Loop
```asm
mainLoop:
    LDA [VM_IP_L]         ; Get opcode
    TAX                   ; Use as index (already even)
    INC VM_IP_L           ; Next byte (no page check!)
    JMP [DISPATCH_TABLE,X]; Jump to handler
    ; Total: ~18 cycles overhead
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
.FUNC Main 0              ; Function 0 at 0x2000
    PUSH_WORD 0x1234      ; Push constant
    STORE_GLOBAL_B 0      ; Store to global[0]
    
    PUSH_STRING_0         ; Push first string
    PRINT_STRING          ; Print it
    
    CALL 2                ; Call function 2
    
loop:                     ; Labels for branches
    LOAD_GLOBAL_B 0
    PUSH_ONE
    SUB_BYTE
    DUP
    STORE_GLOBAL_B 0
    JUMP_NZ_CHAR loop     ; Branch backward
    
    RETURN

.FUNC Helper 2            ; Function 2 at 0x2200
    PUSH_BYTE 42
    PRINT_BYTE
    RETURN
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