// Tiny 6502 Disassembler for Hopper BIOS applets

// Opcode constants
const int ORA_ZP = 0x05;
const int RMB0_ZP = 0x07;
const int BBR0_ZP = 0x0F;
const int RMB1_ZP = 0x17;
const int CLC = 0x18;
const int INC_A = 0x1A;
const int BBR1_ZP = 0x1F;
const int JSR = 0x20;
const int AND_ZP = 0x25;
const int RMB2_ZP = 0x27;
const int BBR2_ZP = 0x2F;
const int SEC = 0x38;
const int PHA = 0x48;
const int JMP_ABS = 0x4C;
const int PHY = 0x5A;
const int RTS = 0x60;
const int STZ_ZP = 0x64;
const int ADC_ZP = 0x65;
const int PLA = 0x68;
const int ADC_IMM = 0x69;
const int JMP_IND = 0x6C;
const int PLY = 0x7A;
const int BRA = 0x80;
const int STA_ZP = 0x85;
const int STX_ZP = 0x86;
const int SMB0_ZP = 0x87;
const int TXA = 0x8A;
const int BBS0_ZP = 0x8F;
const int BCC = 0x90;
const int STA_IND_Y = 0x91;
const int STA_IND = 0x92;
const int SMB1_ZP = 0x97;
const int TYA = 0x98;
const int TXS = 0x9A;
const int BBS1_ZP = 0x9F;
const int LDY_IMM = 0xA0;
const int LDX_IMM = 0xA2;
const int LDA_ZP = 0xA5;
const int LDX_ZP = 0xA6;
const int SMB2_ZP = 0xA7;
const int TAY = 0xA8;
const int LDA_IMM = 0xA9;
const int BBS2_ZP = 0xAF;
const int BCS = 0xB0;
const int LDA_IND_Y = 0xB1;
const int LDA_IND = 0xB2;
const int LDA_ABS_Y = 0xB9;
const int TSX = 0xBA;
const int CPY_IMM = 0xC0;
const int CMP_IMM = 0xC9;
const int INY = 0xC8;
const int DEX = 0xCA;
const int BNE = 0xD0;
const int PHX = 0xDA;
const int INC_ZP = 0xE6;
const int INX = 0xE8;
const int SBC_IMM = 0xE9;
const int NOP = 0xEA;
const int BEQ = 0xF0;
const int PLX = 0xFA;

void decode_instruction(char* buffer, int pc, int* size) {
    char opcode = buffer[0];
    
    // Default to 1 byte
    *size = 1;
    
    printf("%04x: ", pc);
    
    // Decode based on opcode
    while (1)
    {
        if (opcode == ORA_ZP) {
            printf("%02x %02x     ORA $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == RMB0_ZP) {
            printf("%02x %02x     RMB0 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == BBR0_ZP) {
            printf("%02x %02x %02x  BBR0 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == RMB1_ZP) {
            printf("%02x %02x     RMB1 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == CLC) {
            printf("%02x        CLC", opcode);
            break;
        }
        if (opcode == INC_A) {
            printf("%02x        INC A", opcode);
            break;
        }
        if (opcode == BBR1_ZP) {
            printf("%02x %02x %02x  BBR1 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == JSR) {
            printf("%02x %02x %02x  JSR $%02x%02x", 
                   opcode, buffer[1], buffer[2], 
                   buffer[2], buffer[1]); 
            *size = 3;
            break;
        }
        if (opcode == AND_ZP) {
            printf("%02x %02x     AND $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == RMB2_ZP) {
            printf("%02x %02x     RMB2 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == BBR2_ZP) {
            printf("%02x %02x %02x  BBR2 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == SEC) {
            printf("%02x        SEC", opcode);
            break;
        }
        if (opcode == PHA) {
            printf("%02x        PHA", opcode);
            break;
        }
        if (opcode == JMP_ABS) {
            printf("%02x %02x %02x  JMP $%02x%02x", 
                   opcode, buffer[1], buffer[2], 
                   buffer[2], buffer[1]); 
            *size = 3;
            break;
        }
        if (opcode == PHY) {
            printf("%02x        PHY", opcode);
            break;
        }
        if (opcode == RTS) {
            printf("%02x        RTS", opcode);
            break;
        }
        if (opcode == STZ_ZP) {
            printf("%02x %02x     STZ $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == ADC_ZP) {
            printf("%02x %02x     ADC $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == PLA) {
            printf("%02x        PLA", opcode);
            break;
        }
        if (opcode == ADC_IMM) {
            printf("%02x %02x     ADC #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == JMP_IND) {
            printf("%02x %02x %02x  JMP ($%02x%02x)", 
                   opcode, buffer[1], buffer[2], 
                   buffer[2], buffer[1]); 
            *size = 3;
            break;
        }
        if (opcode == PLY) {
            printf("%02x        PLY", opcode);
            break;
        }
        if (opcode == BRA) {
            printf("%02x %02x     BRA $%04x", 
                   opcode, buffer[1], 
                   pc + 2 + (char)buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == STA_ZP) {
            printf("%02x %02x     STA $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == STX_ZP) {
            printf("%02x %02x     STX $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == SMB0_ZP) {
            printf("%02x %02x     SMB0 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == TXA) {
            printf("%02x        TXA", opcode);
            break;
        }
        if (opcode == BBS0_ZP) {
            printf("%02x %02x %02x  BBS0 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == BCC) {
            printf("%02x %02x     BCC $%04x", 
                   opcode, buffer[1], 
                   pc + 2 + (char)buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == STA_IND_Y) {
            printf("%02x %02x     STA ($%02x),Y", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == STA_IND) {
            printf("%02x %02x     STA ($%02x)", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == SMB1_ZP) {
            printf("%02x %02x     SMB1 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == TYA) {
            printf("%02x        TYA", opcode);
            break;
        }
        if (opcode == TXS) {
            printf("%02x        TXS", opcode);
            break;
        }
        if (opcode == BBS1_ZP) {
            printf("%02x %02x %02x  BBS1 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == LDY_IMM) {
            printf("%02x %02x     LDY #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDX_IMM) {
            printf("%02x %02x     LDX #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDA_ZP) {
            printf("%02x %02x     LDA $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDX_ZP) {
            printf("%02x %02x     LDX $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == SMB2_ZP) {
            printf("%02x %02x     SMB2 $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == TAY) {
            printf("%02x        TAY", opcode);
            break;
        }
        if (opcode == LDA_IMM) {
            printf("%02x %02x     LDA #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == BBS2_ZP) {
            printf("%02x %02x %02x  BBS2 $%02x,$%04x", 
                   opcode, buffer[1], buffer[2], buffer[1], 
                   pc + 3 + (char)buffer[2]); 
            *size = 3;
            break;
        }
        if (opcode == BCS) {
            printf("%02x %02x     BCS $%04x", 
                   opcode, buffer[1], 
                   pc + 2 + (char)buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDA_IND_Y) {
            printf("%02x %02x     LDA ($%02x),Y", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDA_IND) {
            printf("%02x %02x     LDA ($%02x)", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == LDA_ABS_Y) {
            printf("%02x %02x %02x  LDA $%02x%02x,Y", 
                   opcode, buffer[1], buffer[2], 
                   buffer[2], buffer[1]); 
            *size = 3;
            break;
        }
        if (opcode == TSX) {
            printf("%02x        TSX", opcode);
            break;
        }
        if (opcode == CPY_IMM) {
            printf("%02x %02x     CPY #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == CMP_IMM) {
            printf("%02x %02x     CMP #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == INY) {
            printf("%02x        INY", opcode);
            break;
        }
        if (opcode == DEX) {
            printf("%02x        DEX", opcode);
            break;
        }
        if (opcode == BNE) {
            printf("%02x %02x     BNE $%04x", 
                   opcode, buffer[1], 
                   pc + 2 + (char)buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == PHX) {
            printf("%02x        PHX", opcode);
            break;
        }
        if (opcode == INC_ZP) {
            printf("%02x %02x     INC $%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == INX) {
            printf("%02x        INX", opcode);
            break;
        }
        if (opcode == SBC_IMM) {
            printf("%02x %02x     SBC #$%02x", 
                   opcode, buffer[1], buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == NOP) {
            printf("%02x        NOP", opcode);
            break;
        }
        if (opcode == BEQ) {
            printf("%02x %02x     BEQ $%04x", 
                   opcode, buffer[1], 
                   pc + 2 + (char)buffer[1]); 
            *size = 2;
            break;
        }
        if (opcode == PLX) {
            printf("%02x        PLX", opcode);
            break;
        }
    
        printf("%02x        ???", opcode);
        break;
    } 
    
    printf("\n");
}

void main(char* exe, char* filename) {
    FILE* fp = fopen(filename, "r");
    if (!fp) {
        printf("Cannot open %s\n", filename);
        return;
    }
    
    // Get file size
    int size = 0;
    int c;
    while ((c = fgetc(fp)) >= 0) {
        size++;
    }
    fclose(fp);
    
    // Reopen and read entire file
    fp = fopen(filename, "r");
    char* buffer = malloc(size);
    fread(buffer, 1, size, fp);
    fclose(fp);
    
    // Parse header
    int entry_point = 0x0800;
    if (buffer[0] == JMP_ABS) {
        // JMP absolute - get entry point
        entry_point = 0x0800 + buffer[1] + (buffer[2] << 8) - 0x0800;
        printf("Entry: JMP $%04x\n", entry_point + 0x0800);
    }
    
    // Show BIOS dispatcher
    if (buffer[3] == JMP_IND) {
        printf("BIOS:  JMP [$%02x%02x]\n\n", 
               buffer[5], buffer[4]);
    }
    
    // Show data section if any
    if (entry_point > 6) {
        printf("=== DATA SECTION ===\n");
        int i;
        for (i = 6; i < entry_point; i++) {
            if (i % 16 == 6) {
                printf("%04x: ", i + 0x0800);
            }
            printf("%02x ", buffer[i]);
            if ((i - 6) % 16 == 15 || i == entry_point - 1) {
                printf("\n");
            }
        }
        printf("\n");
    }
    
    // Disassemble code section
    printf("=== CODE SECTION ===\n");
    int pc = entry_point + 0x0800;
    int i = entry_point;
    int inst_size;
    
    while (i < size) {
        decode_instruction(buffer + i, pc, &inst_size);
        i += inst_size;
        pc += inst_size;
    }
    
    free(buffer);
}