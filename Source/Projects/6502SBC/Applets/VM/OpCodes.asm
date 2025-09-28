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
        // System Operations
        NOP      = 0x00,  // No operation
        
        // Stack Operations
        PUSHB    = 0x02,  // Push 8-bit immediate
        PUSHW    = 0x04,  // Push 16-bit immediate
        PUSH0    = 0x06,  // Push 16-bit 0 (optimized)
        PUSH1    = 0x08,  // Push 16-bit 1 (optimized)
        DUP      = 0x0A,  // Duplicate byte on TOS
        DUPW     = 0x0C,  // Duplicate word on TOS
        DROP     = 0x0E,  // Remove byte from TOS
        DROPW    = 0x10,  // Remove word from TOS
        SWAP     = 0x12,  // Swap top two bytes
        SWAPW    = 0x14,  // Swap top two words
        
        // Arithmetic Operations
        ADD      = 0x16,  // Pop 2 words, push sum
        SUB      = 0x18,  // Pop 2 words, push difference
        MUL      = 0x1A,  // Pop 2 words, push product
        DIV      = 0x1C,  // Pop 2 words, push quotient (unsigned)
        MOD      = 0x1E,  // Pop 2 words, push remainder
        ADDB     = 0x20,  // Pop 2 bytes, push sum
        SUBB     = 0x22,  // Pop 2 bytes, push difference
        NEG      = 0x24,  // Negate int (2's complement)
        NEGB     = 0x26,  // Negate char (2's complement)
        
        // Comparison Operations
        EQ       = 0x28,  // Pop 2 words/ints, push 1 if equal, 0 if not
        NE       = 0x2A,  // Pop 2 words/ints, push 1 if not equal
        LT       = 0x2C,  // Pop 2 words, unsigned less than
        GT       = 0x2E,  // Pop 2 words, unsigned greater than
        LE       = 0x30,  // Pop 2 words, unsigned less or equal
        GE       = 0x32,  // Pop 2 words, unsigned greater or equal
        LTC      = 0x34,  // Pop 2 chars, signed less than
        GTC      = 0x36,  // Pop 2 chars, signed greater than
        LTI      = 0x38,  // Pop 2 ints, signed less than
        GTI      = 0x3A,  // Pop 2 ints, signed greater than
        
        // Bitwise Operations
        AND      = 0x3C,  // Pop 2 words, push bitwise AND
        OR       = 0x3E,  // Pop 2 words, push bitwise OR
        XOR      = 0x40,  // Pop 2 words, push bitwise XOR
        NOT      = 0x42,  // Pop word, push bitwise NOT
        SHL      = 0x44,  // Pop word and count, shift left
        SHR      = 0x46,  // Pop word and count, logical shift right
        SAR      = 0x48,  // Pop int and count, arithmetic shift right
        
        // Memory Operations - Globals
        PUSHGB   = 0x4A,  // Push byte from global[offset]
        PUSHGB2  = 0x4C,  // Push byte from global[offset] (>255)
        PUSHGW   = 0x4E,  // Push word from global[offset]
        PUSHGW2  = 0x50,  // Push word from global[offset] (>255)
        POPGB    = 0x52,  // Pop byte to global[offset]
        POPGB2   = 0x54,  // Pop byte to global[offset] (>255)
        POPGW    = 0x56,  // Pop word to global[offset]
        POPGW2   = 0x58,  // Pop word to global[offset] (>255)
        
        // Memory Operations - Locals
        PUSHLB   = 0x5A,  // Push byte from BP[offset]
        PUSHLW   = 0x5C,  // Push word from BP[offset]
        PUSHLQ   = 0x5E,  // Push 32-bit from BP[offset]
        POPLB    = 0x60,  // Pop byte to BP[offset]
        POPLW    = 0x62,  // Pop word to BP[offset]
        POPLQ    = 0x64,  // Pop 32-bit to BP[offset]
        
        // Data/String Operations
        PUSHD    = 0x66,  // Push string address (byte offset)
        PUSHD2   = 0x68,  // Push string address (word offset)
        STRC     = 0x6A,  // Pop index, pop string, push char
        STRCMP   = 0x6C,  // Pop 2 strings, push -1/0/1
        
        // Control Flow
        CALL     = 0x6E,  // Call function (ID × 2 for table lookup)
        RET      = 0x70,  // Return from function
        BRAB     = 0x72,  // Branch backward by byte (0-255)
        BRAF     = 0x74,  // Branch forward by byte (0-255)
        BZF      = 0x76,  // Branch forward by byte if TOS is zero
        BZB      = 0x78,  // Branch backward by byte if TOS is zero
        BNZF     = 0x7A,  // Branch forward by byte if TOS is not zero
        BNZB     = 0x7C,  // Branch backward by byte if TOS is not zero
        
        // Zero Page Operations
        PUSHZB   = 0x7E,  // Push byte from ZP[offset]
        PUSHZW   = 0x80,  // Push word from ZP[offset]
        PUSHZQ   = 0x82,  // Push 32-bit from ZP[offset]
        POPZB    = 0x84,  // Pop byte to ZP[offset]
        POPZW    = 0x86,  // Pop word to ZP[offset]
        POPZQ    = 0x88,  // Pop 32-bit to ZP[offset]
        
        // System Operations
        SYSCALL  = 0x8A,  // Call BIOS function via X register
        SYSCALLX = 0xA0,  // Call BIOS function via X register
        HALT     = 0x8C,  // Stop execution (return to BIOS)
        
        // Register Operations
        POPA     = 0x8E,  // Pop byte from stack to A register
        POPY     = 0x90,  // Pop byte from stack to Y register
        PUSHA    = 0x92,  // Push A register to stack
        PUSHC    = 0x94,  // Push carry flag (1 if set, 0 if clear)
        PUSHZ    = 0x96,  // Push zero flag (1 if set, 0 if clear)
        
        // Stack Frame Operations
        ENTER    = 0x98,  // Push BP, set BP = SP (stack frame setup), byte number of zero bytes to push
        LEAVE    = 0x9A,  // Pop BP (stack frame teardown)
        
        INCLW    = 0x9C,  // local variable 16-bit increment
        
        // Debug Operations
        DUMP     = 0x9E,  // Diagnostic stack dump
    }
    
    const byte[] opCodes = {
        // System Operations
        OpCode.NOP, Arguments.None, 3, 'N','O','P',              // NOP
        
        // Stack Operations
        OpCode.PUSHB, Arguments.Byte, 5, 'P','U','S','H','B',      // PUSHB + byte
        OpCode.PUSHW, Arguments.Word, 5, 'P','U','S','H','W',      // PUSHW + word
        OpCode.PUSH0, Arguments.None, 5, 'P','U','S','H','0',      // PUSH0
        OpCode.PUSH1, Arguments.None, 5, 'P','U','S','H','1',      // PUSH1
        OpCode.DUP, Arguments.None, 3, 'D','U','P',              // DUP
        OpCode.DUPW, Arguments.None, 4, 'D','U','P','W',          // DUPW
        OpCode.DROP, Arguments.None, 4, 'D','R','O','P',          // DROP
        OpCode.DROPW, Arguments.None, 5, 'D','R','O','P','W',      // DROPW
        OpCode.SWAP, Arguments.None, 4, 'S','W','A','P',          // SWAP
        OpCode.SWAPW, Arguments.None, 5, 'S','W','A','P','W',      // SWAPW
        
        // Arithmetic Operations
        OpCode.ADD, Arguments.None, 3, 'A','D','D',              // ADD
        OpCode.SUB, Arguments.None, 3, 'S','U','B',              // SUB
        OpCode.MUL, Arguments.None, 3, 'M','U','L',              // MUL
        OpCode.DIV, Arguments.None, 3, 'D','I','V',              // DIV
        OpCode.MOD, Arguments.None, 3, 'M','O','D',              // MOD
        OpCode.ADDB, Arguments.None, 4, 'A','D','D','B',          // ADDB
        OpCode.SUBB, Arguments.None, 4, 'S','U','B','B',          // SUBB
        OpCode.NEG, Arguments.None, 3, 'N','E','G',              // NEG
        OpCode.NEGB, Arguments.None, 4, 'N','E','G','B',          // NEGB
        
        // Comparison Operations
        OpCode.EQ, Arguments.None, 2, 'E','Q',                  // EQ
        OpCode.NE, Arguments.None, 2, 'N','E',                  // NE
        OpCode.LT, Arguments.None, 2, 'L','T',                  // LT
        OpCode.GT, Arguments.None, 2, 'G','T',                  // GT
        OpCode.LE, Arguments.None, 2, 'L','E',                  // LE
        OpCode.GE, Arguments.None, 2, 'G','E',                  // GE
        OpCode.LTC, Arguments.None, 3, 'L','T','C',              // LTC
        OpCode.GTC, Arguments.None, 3, 'G','T','C',              // GTC
        OpCode.LTI, Arguments.None, 3, 'L','T','I',              // LTI
        OpCode.GTI, Arguments.None, 3, 'G','T','I',              // GTI
        
        // Bitwise Operations
        OpCode.AND, Arguments.None, 3, 'A','N','D',              // AND
        OpCode.OR, Arguments.None, 2, 'O','R',                  // OR
        OpCode.XOR, Arguments.None, 3, 'X','O','R',              // XOR
        OpCode.NOT, Arguments.None, 3, 'N','O','T',              // NOT
        OpCode.SHL, Arguments.None, 3, 'S','H','L',              // SHL
        OpCode.SHR, Arguments.None, 3, 'S','H','R',              // SHR
        OpCode.SAR, Arguments.None, 3, 'S','A','R',              // SAR
        
        // Memory Operations - Globals
        OpCode.PUSHGB, Arguments.Byte, 6, 'P','U','S','H','G','B',  // PUSHGB + byte
        OpCode.PUSHGB2, Arguments.Word, 7, 'P','U','S','H','G','B','2', // PUSHGB2 + word
        OpCode.PUSHGW, Arguments.Byte, 6, 'P','U','S','H','G','W',  // PUSHGW + byte
        OpCode.PUSHGW2, Arguments.Word, 7, 'P','U','S','H','G','W','2', // PUSHGW2 + word
        OpCode.POPGB, Arguments.Byte, 5, 'P','O','P','G','B',      // POPGB + byte
        OpCode.POPGB2, Arguments.Word, 6, 'P','O','P','G','B','2',  // POPGB2 + word
        OpCode.POPGW, Arguments.Byte, 5, 'P','O','P','G','W',      // POPGW + byte
        OpCode.POPGW2, Arguments.Word, 6, 'P','O','P','G','W','2',  // POPGW2 + word
        
        // Memory Operations - Locals
        OpCode.PUSHLB, Arguments.Char, 6, 'P','U','S','H','L','B',  // PUSHLB + char
        OpCode.PUSHLW, Arguments.Char, 6, 'P','U','S','H','L','W',  // PUSHLW + char
        OpCode.PUSHLQ, Arguments.Char, 6, 'P','U','S','H','L','Q',  // PUSHLQ + char
        OpCode.POPLB, Arguments.Char, 5, 'P','O','P','L','B',      // POPLB + char
        OpCode.POPLW, Arguments.Char, 5, 'P','O','P','L','W',      // POPLW + char
        OpCode.POPLQ, Arguments.Char, 5, 'P','O','P','L','Q',      // POPLQ + char
        
        // Data / String Operations
        OpCode.PUSHD, Arguments.Byte, 5, 'P','U','S','H','D',      // PUSHD + byte
        OpCode.PUSHD2, Arguments.Word, 6, 'P','U','S','H','D','2',  // PUSHD2 + word
        OpCode.STRC, Arguments.None, 4, 'S','T','R','C',          // STRC
        OpCode.STRCMP, Arguments.None, 6, 'S','T','R','C','M','P',  // STRCMP
        
        // Control Flow
        OpCode.CALL, Arguments.Byte, 4, 'C','A','L','L',          // CALL + byte
        OpCode.RET, Arguments.None, 3, 'R','E','T',              // RET
        OpCode.BRAB, Arguments.Byte, 4, 'B','R','A','B',          // BRAB + byte
        OpCode.BRAF, Arguments.Byte, 4, 'B','R','A','F',          // BRAF + byte
        OpCode.BZF, Arguments.Byte, 3, 'B','Z','F',              // BZF  + byte
        OpCode.BZB, Arguments.Byte, 3, 'B','Z','B',              // BZB  + byte
        OpCode.BNZF, Arguments.Byte, 4, 'B','N','Z','F',          // BNZF + byte
        OpCode.BNZB, Arguments.Byte, 4, 'B','N','Z','B',          // BNZB + byte
        
        // Zero Page Operations
        OpCode.PUSHZB, Arguments.Byte, 6, 'P','U','S','H','Z','B',  // PUSHZB + byte
        OpCode.PUSHZW, Arguments.Byte, 6, 'P','U','S','H','Z','W',  // PUSHZW + byte
        OpCode.PUSHZQ, Arguments.Byte, 6, 'P','U','S','H','Z','Q',  // PUSHZQ + byte
        OpCode.POPZB, Arguments.Byte, 5, 'P','O','P','Z','B',      // POPZB + byte
        OpCode.POPZW, Arguments.Byte, 5, 'P','O','P','Z','W',      // POPZW + byte
        OpCode.POPZQ, Arguments.Byte, 5, 'P','O','P','Z','Q',      // POPZQ + byte
        
        // System Operations
        OpCode.SYSCALL, Arguments.Byte, 7, 'S','Y','S','C','A','L','L', // SYSCALL + byte
        OpCode.SYSCALLX, Arguments.Byte, 8, 'S','Y','S','C','A','L','L','X', // SYSCALL + byte
        OpCode.HALT, Arguments.None, 4, 'H','A','L','T',          // HALT
        
        // Register Operations
        OpCode.POPA, Arguments.None, 4, 'P','O','P','A',          // POPA
        OpCode.POPY, Arguments.None, 4, 'P','O','P','Y',          // POPY
        OpCode.PUSHA, Arguments.None, 5, 'P','U','S','H','A',      // PUSHA
        OpCode.PUSHC, Arguments.None, 5, 'P','U','S','H','C',      // PUSHC
        OpCode.PUSHZ, Arguments.None, 5, 'P','U','S','H','Z',      // PUSHZ
        
        // Stack Frame Operations
        OpCode.ENTER, Arguments.Byte, 5, 'E','N','T','E','R',      // ENTER + byte
        OpCode.LEAVE, Arguments.None, 5, 'L','E','A','V','E',      // LEAVE
        
        OpCode.INCLW, Arguments.Char, 5, 'I','N','C','L','W',      // INCLW
        
        // Debug Operations
        OpCode.DUMP, Arguments.None, 4, 'D','U','M','P',          // DUMP
        
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

