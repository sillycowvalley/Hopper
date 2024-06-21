## 6809 Instructions

This table organizes the 6809 instructions into logical groups and provides a clear view of the opcodes, addressing modes, cycles, and bytes for each instruction.

### Data Transfer Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| LDA         | $86       | $96    | $A6     | $B6      | 2-5    | 2-4   |
| LDB         | $C6       | $D6    | $E6     | $F6      | 2-5    | 2-4   |
| LDD         | $CC       | $DC    | $EC     | $FC      | 3-6    | 3-5   |
| LDX         | $8E       | $9E    | $AE     | $BE      | 3-6    | 3-5   |
| LDY         | $10CE     | $10DE  | $10EE   | $10FE    | 4-7    | 4-6   |
| LDS         | $8E       | $9E    | $AE     | $BE      | 3-6    | 3-5   |
| STAA        | -         | $97    | $A7     | $B7      | 2-6    | 2-5   |
| STAB        | -         | $D7    | $E7     | $F7      | 2-6    | 2-5   |
| STD         | -         | $DD    | $ED     | $FD      | 3-6    | 3-5   |
| STX         | -         | $9F    | $AF     | $BF      | 3-6    | 3-5   |
| STY         | -         | $10DF  | $10EF   | $10FF    | 4-7    | 4-6   |
| STS         | -         | $9F    | $AF     | $BF      | 3-6    | 3-5   |
| CLR         | -         | $0F    | $6F     | $7F      | 6-7    | 2-4   |

### Arithmetic Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ADDA        | $8B       | $9B    | $AB     | $BB      | 2-5    | 2-4   |
| ADDB        | $CB       | $DB    | $EB     | $FB      | 2-5    | 2-4   |
| ADDD        | $C3       | $D3    | $E3     | $F3      | 4-7    | 3-5   |
| SUBA        | $80       | $90    | $A0     | $B0      | 2-5    | 2-4   |
| SUBB        | $C0       | $D0    | $E0     | $F0      | 2-5    | 2-4   |
| SUBD        | $83       | $93    | $A3     | $B3      | 4-7    | 3-5   |
| CMPA        | $81       | $91    | $A1     | $B1      | 2-5    | 2-4   |
| CMPB        | $C1       | $D1    | $E1     | $F1      | 2-5    | 2-4   |
| CMPD        | $1083     | $1093  | $10A3   | $10B3    | 5-8    | 4-6   |
| CMPX        | $8C       | $9C    | $AC     | $BC      | 3-6    | 3-5   |
| CMPY        | $108C     | $109C  | $10AC   | $10BC    | 4-7    | 4-6   |
| CMPU        | $1183     | $1193  | $11A3   | $11B3    | 5-8    | 4-6   |
| CMPS        | $118C     | $119C  | $11AC   | $11BC    | 5-8    | 4-6   |

### Logical Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ANDA        | $84       | $94    | $A4     | $B4      | 2-5    | 2-4   |
| ANDB        | $C4       | $D4    | $E4     | $F4      | 2-5    | 2-4   |
| ORAA        | $8A       | $9A    | $AA     | $BA      | 2-5    | 2-4   |
| ORAB        | $CA       | $DA    | $EA     | $FA      | 2-5    | 2-4   |
| EORA        | $88       | $98    | $A8     | $B8      | 2-5    | 2-4   |
| EORB        | $C8       | $D8    | $E8     | $F8      | 2-5    | 2-4   |
| BITA        | $85       | $95    | $A5     | $B5      | 2-5    | 2-4   |
| BITB        | $C5       | $D5    | $E5     | $F5      | 2-5    | 2-4   |
| COM         | -         | $03    | $63     | $73      | 6-7    | 2-4   |
| CLR         | -         | $0F    | $6F     | $7F      | 6-7    | 2-4   |

### Shift and Rotate Instructions

| Instruction | Immediate | Direct | Indexed | Extended | Cycles | Bytes |
|-------------|-----------|--------|---------|----------|--------|-------|
| ASL         | -         | $08    | $68     | $78      | 6-7    | 2-4   |
| ASR         | -         | $07    | $67     | $77      | 6-7    | 2-4   |
| LSR         | -         | $04    | $64     | $74      | 6-7    | 2-4   |
| ROL         | -         | $09    | $69     | $79      | 6-7    | 2-4   |
| ROR         | -         | $06    | $66     | $76      | 6-7    | 2-4   |

### Branch Instructions

| Instruction | Opcode  | Cycles | Bytes |
|-------------|---------|--------|-------|
| BRA         | $20     | 3      | 2     |
| BRN         | $21     | 3      | 2     |
| BHI         | $22     | 3      | 2     |
| BLS         | $23     | 3      | 2     |
| BCC         | $24     | 3      | 2     |
| BCS         | $25     | 3      | 2     |
| BNE         | $26     | 3      | 2     |
| BEQ         | $27     | 3      | 2     |
| BVC         | $28     | 3      | 2     |
| BVS         | $29     | 3      | 2     |
| BPL         | $2A     | 3      | 2     |
| BMI         | $2B     | 3      | 2     |
| BGE         | $2C     | 3      | 2     |
| BLT         | $2D     | 3      | 2     |
| BGT         | $2E     | 3      | 2     |
| BLE         | $2F     | 3      | 2     |
| LBRA        | $16     | 5      | 3     |
| LBRN        | $17     | 5      | 3     |
| LBHI        | $1022   | 5      | 4     |
| LBLS        | $1023   | 5      | 4     |
| LBCC        | $1024   | 5      | 4     |
| LBCS        | $1025   | 5      | 4     |
| LBNE        | $1026   | 5      | 4     |
| LBEQ        | $1027   | 5      | 4     |
| LBVC        | $1028   | 5      | 4     |
| LBVS        | $1029   | 5      | 4     |
| LBPL        | $102A   | 5      | 4     |
| LBMI        | $102B   | 5      | 4     |
| LBGE        | $102C   | 5      | 4     |
| LBLT        | $102D   | 5      | 4     |
| LBGT        | $102E   | 5      | 4     |
| LBLE        | $102F   | 5      | 4     |

### Subroutine Instructions

| Instruction | Opcode | Cycles | Bytes |
|-------------|--------|--------|-------|
| JSR         | $9D    | 6-12   | 3-5   |
| BSR         | $8D    | 7      | 2     |
| LBSR        | $17    | 9      | 3     |
| RTS         | $39    | 5      | 1     |
| RTI         | $3B    | 6      | 1     |

### Miscellaneous Instructions

| Instruction | Opcode | Cycles | Bytes |
|-------------|--------|--------|-------|
| NOP         | $12    | 2      | 1     |
| SWI         | $3F    | 19     | 1     |
| CWAI        | $3C    | 20     | 1     |
| MUL         | $3D    | 11     | 2     |

