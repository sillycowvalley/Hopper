unit OpCodes
{
    enum Arguments
    {
        None,  // 0
        Byte,  // 1
        Char,  // 2
        Word,  // 3
        Int,   // 4
    }
    
    enum OpCode
    {
        // System Operations (0x00-0x02)
        NOP      = 0x00,  // No operation (MUST be 0x00)
        HALT     = 0x02,  // Stop execution (return to BIOS)
        
        // Stack Operations - Immediates (0x04-0x12)
        PUSHB    = 0x04,  // Push 8-bit immediate
        PUSHB0   = 0x06,  // Push 8-bit 0 (optimized)
        PUSHB1   = 0x08,  // Push 8-bit 1 (optimized)
        PUSHW    = 0x0A,  // Push 16-bit immediate
        PUSHW0   = 0x0C,  // Push 16-bit 0 (optimized)
        PUSHW1   = 0x0E,  // Push 16-bit 1 (optimized)
        PUSHA    = 0x10,  // Push A register to stack
        PUSHC    = 0x12,  // Push carry flag (1 if set, 0 if clear)
        
        // Stack Operations - Manipulation (0x14-0x20)
        PUSHZ    = 0x14,  // Push zero flag (1 if set, 0 if clear)
        DUPB     = 0x16,  // Duplicate byte on TOS
        DUPW     = 0x18,  // Duplicate word on TOS
        DROPB    = 0x1A,  // Remove byte from TOS
        DROPW    = 0x1C,  // Remove word from TOS
        SWAPB    = 0x1E,  // Swap top two bytes
        SWAPW    = 0x20,  // Swap top two words
        
        // Arithmetic Operations (0x22-0x32)
        ADDB     = 0x24,  // Pop 2 bytes, push sum
        SUBB     = 0x26,  // Pop 2 bytes, push difference
        NEGB     = 0x28,  // Negate byte (2's complement)
        ADDW     = 0x2A,  // Pop 2 words, push sum
        SUBW     = 0x2C,  // Pop 2 words, push difference
        NEGW     = 0x2E,  // Negate word (2's complement)
        INCLB    = 0x30,  // Local variable 8-bit increment
        INCLW    = 0x32,  // Local variable 16-bit increment
        
        // Comparison Operations (0x34-0x42)
        EQB      = 0x34,  // Pop 2 bytes, push 1 if equal, 0 if not
        NEB      = 0x36,  // Pop 2 bytes, push 1 if not equal
        LTB      = 0x38,  // Pop 2 bytes, unsigned less than
        LEB      = 0x3A,  // Pop 2 bytes, unsigned less or equal
        EQW      = 0x3C,  // Pop 2 words, push 1 if equal, 0 if not
        NEW      = 0x3E,  // Pop 2 words, push 1 if not equal
        LTW      = 0x40,  // Pop 2 words, unsigned less than
        LEW      = 0x42,  // Pop 2 words, unsigned less or equal
        
        // Bitwise Operations (0x44-0x50)
        ANDB     = 0x44,  // Pop 2 bytes, push bitwise AND
        ORB      = 0x46,  // Pop 2 bytes, push bitwise OR
        XORB     = 0x48,  // Pop 2 bytes, push bitwise XOR
        NOTB     = 0x4A,  // Pop byte, push bitwise NOT
        XORW     = 0x4C,  // Pop 2 words, push bitwise XOR
        SHLW     = 0x4E,  // Shift word left by byte operand
        SHRW     = 0x50,  // Shift word right by byte operand
        
        // Zero Page Operations (0x52-0x62)
        PUSHZB   = 0x54,  // Push byte from ZP[offset]
        PUSHZW   = 0x56,  // Push word from ZP[offset]
        PUSHZQ   = 0x58,  // Push 32-bit from ZP[offset]
        POPZB    = 0x5A,  // Pop byte to ZP[offset]
        POPZW    = 0x5C,  // Pop word to ZP[offset]
        POPZQ    = 0x5E,  // Pop 32-bit to ZP[offset]
        POPA     = 0x60,  // Pop byte from stack to A register
        POPY     = 0x62,  // Pop byte from stack to Y register
        
        // Local Operations (0x64-0x6E)
        PUSHLB   = 0x64,  // Push byte from BP[offset]
        PUSHLW   = 0x66,  // Push word from BP[offset]
        PUSHLQ   = 0x68,  // Push 32-bit from BP[offset]
        POPLB    = 0x6A,  // Pop byte to BP[offset]
        POPLW    = 0x6C,  // Pop word to BP[offset]
        POPLQ    = 0x6E,  // Pop 32-bit to BP[offset]
        
        // Global Operations (0x70-0x76)
        PUSHGB   = 0x70,  // Push byte from global[offset]
        PUSHGW   = 0x72,  // Push word from global[offset]
        POPGB    = 0x74,  // Pop byte to global[offset]
        POPGW    = 0x76,  // Pop word to global[offset]
        
        // Control Flow (0x78-0x8A)
        BRAF     = 0x7C,  // Branch forward by byte (0-255)
        BRAR     = 0x7E,  // Branch reverse by byte (0-255)
        BZF      = 0x80,  // Branch forward by byte if TOS is zero
        BZR      = 0x82,  // Branch reverse by byte if TOS is zero
        BNZF     = 0x84,  // Branch forward by byte if TOS is not zero
        BNZR     = 0x86,  // Branch reverse by byte if TOS is not zero
        CALL     = 0x88,  // Call function (ID for table lookup)
        RET      = 0x8A,  // Return from function
        
        // System Calls & Stack Frame (0x8C-0x96)
        SYSCALL  = 0x8C,  // Call BIOS function via X register
        SYSCALLX = 0x8E,  // Call BIOS function (fast version)
        ENTER    = 0x90,  // Push BP, set BP = SP, push zeros
        LEAVE    = 0x92,  // Pop BP (stack frame teardown)
        DUMP     = 0x94,  // Diagnostic stack dump
        
        // String/Data Operations (0x98-0x9E)
        PUSHD    = 0x98,  // Push string address (byte offset)
        PUSHD2   = 0x9A,  // Push string address (word offset)
        STRC     = 0x9C,  // Pop index, pop string, push char
        STRCMP   = 0x9E,  // Pop 2 strings, push -1/0/1
    }
    
    const byte[] opCodes = {
        // System Operations (0x00-0x02)
        OpCode.NOP,      Arguments.None, 3, 'N','O','P',                         // NOP
        OpCode.HALT,     Arguments.None, 4, 'H','A','L','T',                     // HALT
        
        // Stack Operations - Immediates (0x04-0x12)
        OpCode.PUSHB,    Arguments.Byte, 5, 'P','U','S','H','B',                 // PUSHB + byte
        OpCode.PUSHB0,   Arguments.None, 6, 'P','U','S','H','B','0',             // PUSHB0
        OpCode.PUSHB1,   Arguments.None, 6, 'P','U','S','H','B','1',             // PUSHB1
        OpCode.PUSHW,    Arguments.Word, 5, 'P','U','S','H','W',                 // PUSHW + word
        OpCode.PUSHW0,   Arguments.None, 6, 'P','U','S','H','W','0',             // PUSHW0
        OpCode.PUSHW1,   Arguments.None, 6, 'P','U','S','H','W','1',             // PUSHW1
        OpCode.PUSHA,    Arguments.None, 5, 'P','U','S','H','A',                 // PUSHA
        OpCode.PUSHC,    Arguments.None, 5, 'P','U','S','H','C',                 // PUSHC
        
        // Stack Operations - Manipulation (0x14-0x20)
        OpCode.PUSHZ,    Arguments.None, 5, 'P','U','S','H','Z',                 // PUSHZ
        OpCode.DUPB,     Arguments.None, 4, 'D','U','P','B',                     // DUPB
        OpCode.DUPW,     Arguments.None, 4, 'D','U','P','W',                     // DUPW
        OpCode.DROPB,    Arguments.None, 5, 'D','R','O','P','B',                 // DROPB
        OpCode.DROPW,    Arguments.None, 5, 'D','R','O','P','W',                 // DROPW
        OpCode.SWAPB,    Arguments.None, 5, 'S','W','A','P','B',                 // SWAPB
        OpCode.SWAPW,    Arguments.None, 5, 'S','W','A','P','W',                 // SWAPW
        
        // Arithmetic Operations (0x24-0x32)
        OpCode.ADDB,     Arguments.None, 4, 'A','D','D','B',                     // ADDB
        OpCode.SUBB,     Arguments.None, 4, 'S','U','B','B',                     // SUBB
        OpCode.NEGB,     Arguments.None, 4, 'N','E','G','B',                     // NEGB
        OpCode.ADDW,     Arguments.None, 4, 'A','D','D','W',                     // ADDW
        OpCode.SUBW,     Arguments.None, 4, 'S','U','B','W',                     // SUBW
        OpCode.NEGW,     Arguments.None, 4, 'N','E','G','W',                     // NEGW
        OpCode.INCLB,    Arguments.Char, 5, 'I','N','C','L','B',                 // INCLB + char
        OpCode.INCLW,    Arguments.Char, 5, 'I','N','C','L','W',                 // INCLW + char
        
        // Comparison Operations (0x34-0x42)
        OpCode.EQB,      Arguments.None, 3, 'E','Q','B',                         // EQB
        OpCode.NEB,      Arguments.None, 3, 'N','E','B',                         // NEB
        OpCode.LTB,      Arguments.None, 3, 'L','T','B',                         // LTB
        OpCode.LEB,      Arguments.None, 3, 'L','E','B',                         // LEB
        OpCode.EQW,      Arguments.None, 3, 'E','Q','W',                         // EQW
        OpCode.NEW,      Arguments.None, 3, 'N','E','W',                         // NEW
        OpCode.LTW,      Arguments.None, 3, 'L','T','W',                         // LTW
        OpCode.LEW,      Arguments.None, 3, 'L','E','W',                         // LEW
        
        // Bitwise Operations (0x44-0x50)
        OpCode.ANDB,     Arguments.None, 4, 'A','N','D','B',                     // ANDB
        OpCode.ORB,      Arguments.None, 3, 'O','R','B',                         // ORB
        OpCode.XORB,     Arguments.None, 4, 'X','O','R','B',                     // XORB
        OpCode.NOTB,     Arguments.None, 4, 'N','O','T','B',                     // NOTB
        OpCode.XORW,     Arguments.None, 4, 'X','O','R','W',                     // XORW
        OpCode.SHLW,     Arguments.Byte, 4, 'S','H','L','W',                     // SHLW + byte
        OpCode.SHRW,     Arguments.Byte, 4, 'S','H','R','W',                     // SHRW + byte
        
        // Zero Page Operations (0x54-0x62)
        OpCode.PUSHZB,   Arguments.Byte, 6, 'P','U','S','H','Z','B',             // PUSHZB + byte
        OpCode.PUSHZW,   Arguments.Byte, 6, 'P','U','S','H','Z','W',             // PUSHZW + byte
        OpCode.PUSHZQ,   Arguments.Byte, 6, 'P','U','S','H','Z','Q',             // PUSHZQ + byte
        OpCode.POPZB,    Arguments.Byte, 5, 'P','O','P','Z','B',                 // POPZB + byte
        OpCode.POPZW,    Arguments.Byte, 5, 'P','O','P','Z','W',                 // POPZW + byte
        OpCode.POPZQ,    Arguments.Byte, 5, 'P','O','P','Z','Q',                 // POPZQ + byte
        OpCode.POPA,     Arguments.None, 4, 'P','O','P','A',                     // POPA
        OpCode.POPY,     Arguments.None, 4, 'P','O','P','Y',                     // POPY
        
        // Local Operations (0x64-0x6E)
        OpCode.PUSHLB,   Arguments.Char, 6, 'P','U','S','H','L','B',             // PUSHLB + char
        OpCode.PUSHLW,   Arguments.Char, 6, 'P','U','S','H','L','W',             // PUSHLW + char
        OpCode.PUSHLQ,   Arguments.Char, 6, 'P','U','S','H','L','Q',             // PUSHLQ + char
        OpCode.POPLB,    Arguments.Char, 5, 'P','O','P','L','B',                 // POPLB + char
        OpCode.POPLW,    Arguments.Char, 5, 'P','O','P','L','W',                 // POPLW + char
        OpCode.POPLQ,    Arguments.Char, 5, 'P','O','P','L','Q',                 // POPLQ + char
        
        // Global Operations (0x70-0x76)
        OpCode.PUSHGB,   Arguments.Byte, 6, 'P','U','S','H','G','B',             // PUSHGB + byte
        OpCode.PUSHGW,   Arguments.Byte, 6, 'P','U','S','H','G','W',             // PUSHGW + byte
        OpCode.POPGB,    Arguments.Byte, 5, 'P','O','P','G','B',                 // POPGB + byte
        OpCode.POPGW,    Arguments.Byte, 5, 'P','O','P','G','W',                 // POPGW + byte
        
        // Control Flow (0x7C-0x8A)
        OpCode.BRAF,     Arguments.Byte, 4, 'B','R','A','F',                     // BRAF + byte
        OpCode.BRAR,     Arguments.Byte, 4, 'B','R','A','R',                     // BRAR + byte
        OpCode.BZF,      Arguments.Byte, 3, 'B','Z','F',                         // BZF + byte
        OpCode.BZR,      Arguments.Byte, 3, 'B','Z','R',                         // BZR + byte
        OpCode.BNZF,     Arguments.Byte, 4, 'B','N','Z','F',                     // BNZF + byte
        OpCode.BNZR,     Arguments.Byte, 4, 'B','N','Z','R',                     // BNZR + byte
        OpCode.CALL,     Arguments.Byte, 4, 'C','A','L','L',                     // CALL + byte
        OpCode.RET,      Arguments.None, 3, 'R','E','T',                         // RET
        
        // System Calls & Stack Frame (0x8C-0x94)
        OpCode.SYSCALL,  Arguments.Byte, 7, 'S','Y','S','C','A','L','L',         // SYSCALL + byte
        OpCode.SYSCALLX, Arguments.Byte, 8, 'S','Y','S','C','A','L','L','X',     // SYSCALLX + byte
        OpCode.ENTER,    Arguments.Byte, 5, 'E','N','T','E','R',                 // ENTER + byte
        OpCode.LEAVE,    Arguments.None, 5, 'L','E','A','V','E',                 // LEAVE
        OpCode.DUMP,     Arguments.None, 4, 'D','U','M','P',                     // DUMP
        
        // String/Data Operations (0x98-0x9E)
        OpCode.PUSHD,    Arguments.Byte, 5, 'P','U','S','H','D',                 // PUSHD + byte
        OpCode.PUSHD2,   Arguments.Word, 6, 'P','U','S','H','D','2',             // PUSHD2 + word
        OpCode.STRC,     Arguments.None, 4, 'S','T','R','C',                     // STRC
        OpCode.STRCMP,   Arguments.None, 6, 'S','T','R','C','M','P',             // STRCMP
        
        // End marker
        0xFF
    };
    
    // Input: STR points to opcode name to find
    // Output:
    //   C if found, NC if not
    //   OpCode in A
    //   Arguments in Y
    FindOpCode()
    {
        // Point IDX to start of opcode table
        LDA #(opCodes % 256)
        STA ZP.IDXL
        LDA #(opCodes / 256)
        STA ZP.IDXH
        
        String.Length();
        STA ZP.ACCL
        
        loop
        {
            // Check for end marker
            LDA [ZP.IDX]
            CMP #0xFF
            if (Z) 
            { 
                CLC  // Not found
                break;
            }
            
            // Get name length from table
            LDY #2
            LDA [ZP.IDX], Y  // Name length
            
            // Compare with tokenLength
            CMP ZP.ACCL
            if (Z)  // Lengths match, compare strings
            {
                TAX  // X = length

                // Start of name in table                
                CLC
                LDA ZP.IDXL
                ADC #3
                STA ZP.IDYL
                LDA ZP.IDXH
                ADC #0
                STA ZP.IDYH
                
                LDY #0 // offset in STR
                loop
                {
                    LDA [ZP.IDY]
                    CMP [ZP.STR], Y
                    if (NZ) { CLC break; }  // Mismatch
                    
                    DEX
                    if (Z)  // Matched all characters
                    {
                        // Found it!
                        LDA [ZP.IDX]      // Get opcode
                        PHA
                        INY
                        LDY #1
                        LDA [ZP.IDX], Y  // Get argument type
                        TAY
                        PLA
                        
                        SEC
                        break;
                    }
                    IncIDY();
                    INY  // Next char in table
                } // loop
                
                if (C) { break; }  // Found
            }
            
            // Skip to next entry: add 3 + name_length to IDX
            LDY #2
            LDA [ZP.IDX], Y  // Name length
            CLC
            ADC #3  // Header size
            
            // Add to IDX
            CLC
            ADC ZP.IDXL
            STA ZP.IDXL
            if (C)
            {
                INC ZP.IDXH
            }
        } // loop
    }
}

