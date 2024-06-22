## 6809 Instructions

### Online Emulator

http://6809.uk/

### 6809 Tools

https://www.chibiakumas.com/6809/6809DevTools.php

### Instructions

This table organizes the 6809 instructions into logical groups and provides a clear view of the opcodes, addressing modes, cycles, and bytes for each instruction.

### Data Transfer Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| LDA         | 0x86      | 0x96   | 0xA6    | 0xB6     | 2-5    | 2-4   |
| LDB         | 0xC6      | 0xD6   | 0xE6    | 0xF6     | 2-5    | 2-4   |
| LDD         | 0xCC      | 0xDC   | 0xEC    | 0xFC     | 3-6    | 3-5   |
| LDX         | 0x8E      | 0x9E   | 0xAE    | 0xBE     | 3-6    | 3-5   |
| LDY         | 0x10CE    | 0x10DE | 0x10EE  | 0x10FE   | 4-7    | 4-6   |
| LDU         | 0xCE      | 0xDE   | 0xEE    | 0xFE     | 3-6    | 3-5   |
| LDS         | 0x8E      | 0x9E   | 0xAE    | 0xBE     | 3-6    | 3-5   |
| STAA        | -         | 0x97   | 0xA7    | 0xB7     | 2-6    | 2-5   |
| STAB        | -         | 0xD7   | 0xE7    | 0xF7     | 2-6    | 2-5   |
| STD         | -         | 0xDD   | 0xED    | 0xFD     | 3-6    | 3-5   |
| STX         | -         | 0x9F   | 0xAF    | 0xBF     | 3-6    | 3-5   |
| STY         | -         | 0x10DF | 0x10EF  | 0x10FF   | 4-7    | 4-6   |
| STU         | -         | 0xDF   | 0xEF   | 0xFF     | 3-6    | 3-5   |
| STS         | -         | 0x9F   | 0xAF   | 0xBF     | 3-6    | 3-5   |
| CLR         | -         | 0x0F   | 0x6F   | 0x7F     | 6-7    | 2-4   |

### Arithmetic Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ADDA        | 0x8B      | 0x9B   | 0xAB    | 0xBB     | 2-5    | 2-4   |
| ADDB        | 0xCB      | 0xDB   | 0xEB    | 0xFB     | 2-5    | 2-4   |
| ADDD        | 0xC3      | 0xD3   | 0xE3    | 0xF3     | 4-7    | 3-5   |
| ADCA        | 0x89      | 0x99   | 0xA9    | 0xB9     | 2-5    | 2-4   |
| ADCB        | 0xC9      | 0xD9   | 0xE9    | 0xF9     | 2-5    | 2-4   |
| SBCA        | 0x82      | 0x92   | 0xA2    | 0xB2     | 2-5    | 2-4   |
| SBCB        | 0xC2      | 0xD2   | 0xE2    | 0xF2     | 2-5    | 2-4   |
| SUBA        | 0x80      | 0x90   | 0xA0    | 0xB0     | 2-5    | 2-4   |
| SUBB        | 0xC0      | 0xD0   | 0xE0    | 0xF0     | 2-5    | 2-4   |
| SUBD        | 0x83      | 0x93   | 0xA3    | 0xB3     | 4-7    | 3-5   |
| CMPA        | 0x81      | 0x91   | 0xA1    | 0xB1     | 2-5    | 2-4   |
| CMPB        | 0xC1      | 0xD1   | 0xE1    | 0xF1     | 2-5    | 2-4   |
| CMPD        | 0x1083    | 0x1093 | 0x10A3  | 0x10B3   | 5-8    | 4-6   |
| CMPX        | 0x8C      | 0x9C   | 0xAC    | 0xBC     | 3-6    | 3-5   |
| CMPY        | 0x108C    | 0x109C | 0x10AC  | 0x10BC   | 4-7    | 4-6   |
| CMPU        | 0x1183    | 0x1193 | 0x11A3  | 0x11B3   | 5-8    | 4-6   |
| CMPS        | 0x118C    | 0x119C | 0x11AC  | 0x11BC   | 5-8    | 4-6   |

### Logical Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ANDA        | 0x84      | 0x94   | 0xA4    | 0xB4     | 2-5    | 2-4   |
| ANDB        | 0xC4      | 0xD4   | 0xE4    | 0xF4     | 2-5    | 2-4   |
| ORAA        | 0x8A      | 0x9A   | 0xAA    | 0xBA     | 2-5    | 2-4   |
| ORAB        | 0xCA      | 0xDA   | 0xEA    | 0xFA     | 2-5    | 2-4   |
| EORA        | 0x88      | 0x98   | 0xA8    | 0xB8     | 2-5    | 2-4   |
| EORB        | 0xC8      | 0xD8   | 0xE8    | 0xF8     | 2-5    | 2-4   |
| BITA        | 0x85      | 0x95   | 0xA5    | 0xB5     | 2-5    | 2-4   |
| BITB        | 0xC5      | 0xD5   | 0xE5    | 0xF5     | 2-5    | 2-4   |
| ANDCC       | 0x1C      | -      | -       | -        | 3      | 2     |
| COM         | -         | 0x03   | 0x63    | 0x73     | 6-7    | 2-4   |
| CLR         | -         | 0x0F   | 0x6F    | 0x7F     | 6-7    | 2-4   |

### Shift and Rotate Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ASL         | -         | 0x08   | 0x68    | 0x78     | 6-7    | 2-4   |
| ASR         | -         | 0x07   | 0x67    | 0x77     | 6-7    | 2-4   |
| LSR         | -         | 0x04   | 0x64    | 0x74     | 6-7    | 2-4   |
| ROL         | -         | 0x09   | 0x69    | 0x79     | 6-7    | 2-4   |
| ROR         | -         | 0x06   | 0x66    | 0x76     | 6-7    | 2-4   |

### Branch Instructions

| Instruction | Opcode  | Cycles | Bytes |
|-------------|---------|--------|-------|
| BRA         | 0x20    | 3      | 2     |
| BRN         | 0x21    | 3      | 2     |
| BHI         | 0x22    | 3      | 2     |
| BLS         | 0x23    | 3      | 2     |
| BCC         | 0x24    | 3      | 2     |
| BCS         | 0x25    | 3      | 2     |
| BNE         | 0x26    | 3      | 2     |
| BEQ         | 0x27    | 3      | 2     |
| BVC         | 0x28    | 3      | 2     |
| BVS         | 0x29    | 3      | 2     |
| BPL         | 0x2A    | 3      | 2     |
| BMI         | 0x2B    | 3      | 2     |
| BGE         | 0x2C    | 3      | 2     |
| BLT         | 0x2D    | 3      | 2     |
| BGT         | 0x2E    | 3      | 2     |
| BLE         | 0x2F    | 3      | 2     |
| LBRA        | 0x16    | 5      | 3     |
| LBRN        | 0x17    | 5      | 3     |
| LBHI        | 0x1022  | 5      | 4     |
| LBLS        | 0x1023  | 5      | 4     |
| LBCC        | 0x1024  | 5      | 4     |
| LBCS        | 0x1025  | 5      | 4     |
| LBNE        | 0x1026  | 5      | 4     |
| LBEQ        | 0x1027  | 5      | 4     |
| LBVC        | 0x1028  | 5      | 4     |
| LBVS        | 0x1029  | 5      | 4     |
| LBPL        | 0x102A  | 5      | 4     |
| LBMI        | 0x102B  | 5      | 4     |
| LBGE        | 0x102C  | 5      | 4     |
| LBLT        | 0x102D  | 5      | 4     |
| LBGT        | 0x102E  | 5      | 4     |
| LBLE        | 0x102F  | 5      | 4     |

### Subroutine Instructions

| Instruction | Opcode | Cycles | Bytes |
|-------------|--------|--------|-------|
| JSR         | 0x9D   | 6-12   | 3-5   |
| BSR         | 0x8D   | 7      | 2     |
| LBSR        | 0x17   | 9      | 3     |
| RTS         | 0x39   | 5      | 1     |
| RTI         | 0x3B   | 6      | 1     |

### Miscellaneous Instructions

| Instruction | Opcode | Cycles | Bytes |
|-------------|--------|--------|-------|
| NOP         | 0x12   | 2      | 1     |
| SWI         | 0x3F   | 19     | 1     |
| CWAI        | 0x3C   | 20     | 1     |
| MUL         | 0x3D   | 11     | 2     |
| CLRA        | 0x4F   | 2      | 1     |
| CLRB        | 0x5F   | 2      | 1     |
| ABX         | 0x3A   | 3      | 1     |