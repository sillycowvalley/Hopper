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
    const byte[] opCodes = {
        // Stack Operations
        0x00, Arguments.Byte, 5, 'P','U','S','H','B',      // PUSHB + byte
        0x02, Arguments.Word, 5, 'P','U','S','H','W',      // PUSHW + word
        0x04, Arguments.None, 5, 'P','U','S','H','0',      // PUSH0
        0x06, Arguments.None, 5, 'P','U','S','H','1',      // PUSH1
        0x08, Arguments.None, 3, 'D','U','P',              // DUP
        0x0A, Arguments.None, 4, 'D','U','P','W',          // DUPW
        0x0C, Arguments.None, 4, 'D','R','O','P',          // DROP
        0x0E, Arguments.None, 5, 'D','R','O','P','W',      // DROPW
        0x10, Arguments.None, 4, 'S','W','A','P',          // SWAP
        0x12, Arguments.None, 5, 'S','W','A','P','W',      // SWAPW
        
        // Arithmetic Operations
        0x14, Arguments.None, 3, 'A','D','D',              // ADD
        0x16, Arguments.None, 3, 'S','U','B',              // SUB
        0x18, Arguments.None, 3, 'M','U','L',              // MUL
        0x1A, Arguments.None, 3, 'D','I','V',              // DIV
        0x1C, Arguments.None, 3, 'M','O','D',              // MOD
        0x1E, Arguments.None, 4, 'A','D','D','B',          // ADDB
        0x20, Arguments.None, 4, 'S','U','B','B',          // SUBB
        0x22, Arguments.None, 3, 'N','E','G',              // NEG
        0x24, Arguments.None, 4, 'N','E','G','B',          // NEGB
        
        // Comparison Operations
        0x26, Arguments.None, 2, 'E','Q',                  // EQ
        0x28, Arguments.None, 2, 'N','E',                  // NE
        0x2A, Arguments.None, 2, 'L','T',                  // LT
        0x2C, Arguments.None, 2, 'G','T',                  // GT
        0x2E, Arguments.None, 2, 'L','E',                  // LE
        0x30, Arguments.None, 2, 'G','E',                  // GE
        0x32, Arguments.None, 3, 'L','T','C',              // LTC
        0x34, Arguments.None, 3, 'G','T','C',              // GTC
        0x36, Arguments.None, 3, 'L','T','I',              // LTI
        0x38, Arguments.None, 3, 'G','T','I',              // GTI
        
        // Bitwise Operations
        0x3A, Arguments.None, 3, 'A','N','D',              // AND
        0x3C, Arguments.None, 2, 'O','R',                  // OR
        0x3E, Arguments.None, 3, 'X','O','R',              // XOR
        0x40, Arguments.None, 3, 'N','O','T',              // NOT
        0x42, Arguments.None, 3, 'S','H','L',              // SHL
        0x44, Arguments.None, 3, 'S','H','R',              // SHR
        0x46, Arguments.None, 3, 'S','A','R',              // SAR
        
        // Memory Operations - Globals
        0x48, Arguments.Byte, 6, 'P','U','S','H','G','B',  // PUSHGB + byte
        0x4A, Arguments.Word, 7, 'P','U','S','H','G','B','2', // PUSHGB2 + word
        0x4C, Arguments.Byte, 6, 'P','U','S','H','G','W',  // PUSHGW + byte
        0x4E, Arguments.Word, 7, 'P','U','S','H','G','W','2', // PUSHGW2 + word
        0x50, Arguments.Byte, 5, 'P','O','P','G','B',      // POPGB + byte
        0x52, Arguments.Word, 6, 'P','O','P','G','B','2',  // POPGB2 + word
        0x54, Arguments.Byte, 5, 'P','O','P','G','W',      // POPGW + byte
        0x56, Arguments.Word, 6, 'P','O','P','G','W','2',  // POPGW2 + word
        
        // Memory Operations - Locals
        0x58, Arguments.Char, 6, 'P','U','S','H','L','B',  // PUSHLB + char
        0x5A, Arguments.Char, 6, 'P','U','S','H','L','W',  // PUSHLW + char
        0x5C, Arguments.Char, 5, 'P','O','P','L','B',      // POPLB + char
        0x5E, Arguments.Char, 5, 'P','O','P','L','W',      // POPLW + char
        
        // String Operations
        0x60, Arguments.Byte, 5, 'P','U','S','H','S',      // PUSHS + byte
        0x62, Arguments.Word, 6, 'P','U','S','H','S','2',  // PUSHS2 + word
        0x64, Arguments.None, 4, 'S','T','R','C',          // STRC
        0x66, Arguments.None, 6, 'S','T','R','C','M','P',  // STRCMP
        
        // Control Flow
        0x68, Arguments.Byte, 4, 'C','A','L','L',          // CALL + byte
        0x6A, Arguments.None, 3, 'R','E','T',              // RET
        0x6C, Arguments.Char, 3, 'B','R','A',              // BRA + sbyte
        0x6E, Arguments.Int,  4, 'B','R','A','2',          // BRA2 + int
        0x70, Arguments.Char, 2, 'J','Z',                  // JZ + sbyte
        0x72, Arguments.Int,  3, 'J','Z','2',              // JZ2 + int
        0x74, Arguments.Char, 3, 'J','N','Z',              // JNZ + sbyte
        0x76, Arguments.Int,  4, 'J','N','Z','2',          // JNZ2 + int
        
        // Zero Page Operations
        0x78, Arguments.Byte, 6, 'P','U','S','H','Z','B',  // PUSHZB + byte
        0x7A, Arguments.Byte, 6, 'P','U','S','H','Z','W',  // PUSHZW + byte
        0x7C, Arguments.Byte, 5, 'P','O','P','Z','B',      // POPZB + byte
        0x7E, Arguments.Byte, 5, 'P','O','P','Z','W',      // POPZW + byte
        
        // System Operations
        0x80, Arguments.Byte, 7, 'S','Y','S','C','A','L','L', // SYSCALL + byte
        0x82, Arguments.None, 4, 'H','A','L','T',          // HALT
        
        // Register Operations
        0x84, Arguments.None, 4, 'P','O','P','A',          // POPA
        0x86, Arguments.None, 4, 'P','O','P','Y',          // POPY
        0x88, Arguments.None, 5, 'P','U','S','H','A',      // PUSHA
        0x8A, Arguments.None, 5, 'P','U','S','H','C',      // PUSHC
        0x8C, Arguments.None, 5, 'P','U','S','H','Z',      // PUSHZ
        
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

