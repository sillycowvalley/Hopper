unit Dasm
{
    // Disassemble complete function body's opcode stream
    // Input: ZP.IDX = function node
    // Output: Disassembly printed to serial, one line per opcode
    // Modifies: ZP.STR
    DisassembleFunctionOpCodes()
    {
#ifdef DEBUG
        PHA
        PHX
        PHY
        
        // Save registers we'll use
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop // Single exit block
        {
            Objects.GetTokens();
            LDA ZP.IDYL
            STA ZP.XIDL 
            LDA ZP.IDYH
            STA ZP.XIDH 
            
            NL(); Space(); Space(); Space(); // indent
            LDA #'[' COut(); LDA ZP.IDXH HOut();LDA ZP.IDXL HOut(); LDA #']' COut();
            LDA #'[' COut(); LDA ZP.XIDH HOut();LDA ZP.XIDL HOut(); LDA #']' COut();
            NL();
            
            // Get opcode stream pointer from function
            Functions.GetOpCodes(); // Input: ZP.IDX, Output: ZP.IDY = opcode stream, C if compiled
            if (NC)
            {
                break;
                // Function not compiled - no opcodes to disassemble
                Functions.Compile();
                Functions.GetOpCodes();
                if (NC)
                {
                    break; // compile failed (syntax error for example);
                }
            }
            
            // Check for null opcode stream
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                // No opcodes
                break;
            }
            
            // Iterate through opcode stream
            loop
            {
                Space(); Space();Space();Space(); // indent
                
                // Print address (4 hex digits)
                LDA ZP.IDYH HOut(); LDA ZP.IDYL HOut(); LDA #':' COut(); Space();
                
                // Get opcode byte
                LDA [ZP.IDY]
                TAX  // Save opcode in X for ToString()
                IncIDY();
                
                // Print hex value of opcode
                TXA
                HOut(); Space();
                
                TXA  // Get opcode back
                AND #0xC0  // Isolate bits 7-6
                CMP #0x00  
                switch (A)
                {
                    case 0x00: // no operands
                    {
                        Space(); Space(); Space(); Space(); Space();
                    }
                    case 0x40:  // one byte operand
                    {
                        LDA [ZP.IDY]
                        STA ZP.ACCL
                        HOut(); Space();
                        IncIDY();
                        
                        Space(); Space();
                    }
                    case 0x80:  // two byte operands
                    {
                        LDA [ZP.IDY]
                        STA ZP.ACCL
                        HOut(); Space();
                        IncIDY();
                        
                        LDA [ZP.IDY]
                        STA ZP.ACCH
                        HOut();
                        IncIDY();
                    }
                    default:
                    {
                        CLC
                        break; // undefined?
                    }
                }
                
                OpCodes.ToString(); // X = opcode, returns ZP.STR
                Space(); Space(); Tools.PrintStringSTR();
                LDY # 12 // pad to width
                loop
                {
                    LDA [ZP.STR]
                    if (Z) { break; }
                    IncSTR();
                    DEY
                }
                loop
                {
                    DEY
                    if (Z) { break; }
                    Space();
                }
                
                TXA  // Get opcode back
                AND #0xC0  // Isolate bits 7-6
                CMP #0x00  
                switch (A)
                {
                    case 0x00: // no operands
                    {
                        CPX # OpCode.HALT
                        if (Z) { SEC break; }
                        CPX # OpCode.INVALID
                        if (Z) { CLC break; }
                    }
                    case 0x40:  // one byte operand
                    {
                        Space(); LDA ZP.ACCL HOut();Space();
                        switch (X)
                        {
                            case OpCode.SYSCALL:
                            {
                                // TODO
                            }
                        }
                    }
                    case 0x80:  // two byte operands
                    {
                        Space(); LDA ZP.ACCH HOut(); LDA ZP.ACCL HOut(); Space();
                        switch (X)
                        {
                            case OpCode.CALL:
                            {
                                CLC
                                LDA ZP.ACCL
                                ADC ZP.XIDL
                                STA ZP.TOPL
                                LDA ZP.ACCH
                                ADC ZP.XIDH
                                STA ZP.TOPH
                                Functions.Find();
                                if (C)
                                {
                                    Functions.GetName();
                                    LDA #'(' COut();
                                    PrintStringSTR();
                                    LDA #')' COut();
                                }
                            }
                            case OpCode.CALLF:
                            {
                                LDA ZP.ACCL
                                STA ZP.IDXL
                                LDA ZP.ACCH
                                STA ZP.IDXH
                                Functions.GetName();
                                LDA #'(' COut();
                                PrintStringSTR();
                                LDA #')' COut();
                            }
                            case OpCode.PUSHGLOBAL:
                            case OpCode.POPGLOBAL:
                            {
                                LDA #'(' COut();
                                LDA ZP.ACCL
                                STA ZP.IDXL
                                LDA ZP.ACCH
                                STA ZP.IDXH
                                Variables.GetName();
                                Tools.PrintStringSTR();
                                LDA #')' COut();
                            }
                            case OpCode.PUSHCSTRING:
                            {
                                CLC
                                LDA ZP.XIDL
                                ADC ZP.ACCL
                                STA ZP.ACCL
                                LDA ZP.XIDH
                                ADC ZP.ACCH
                                STA ZP.ACCH
                                 
                                LDA #'(' COut();
                                PrintStringACC();
                                LDA #')' COut();
                            }
                            case OpCode.JUMPW:
                            case OpCode.JUMPZW:
                            case OpCode.JUMPNZW:
                            {
                                LDA #'(' COut();
                                LDA #BASICType.INT
                                STA ZP.TOPT
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.ACCH
                                STA ZP.TOPH
                                if (PL)
                                {
                                    LDA #'+' COut();
                                }
                                Tools.PrintDecimalWord();
                                
                                LDA #' ' COut(); LDA #'-' COut(); LDA #'>' COut(); LDA #' ' COut();
                                
                                // relative jump
                                CLC
                                LDA ZP.ACCL
                                ADC IDYL
                                STA ZP.ACCL
                                LDA ZP.ACCH
                                ADC IDYH
                                STA ZP.ACCH
                                HOut(); LDA ZP.ACCL HOut();   
                                
                                LDA #')' COut();
                            }
                        }
                        
                    }
                }
                        
                // Special decode for SYSCALL
                /*
                CPX #OpCode.SYSCALL
                if (Z)
                {
                    LDA #' '
                    Serial.WriteChar();
                    LDA #'('
                    Serial.WriteChar();
                    
                    // Decode SYSCALL ID bits
                    LDA [ZP.IDY], Y
                    LSR LSR LSR  // Shift right 3 to get function ID (bits 7-3)
                    AND #0x1F    // Mask to 5 bits
                    
                    // Print function ID
                    Serial.HexOut();
                    LDA #')'
                    Serial.WriteChar();
                }
                */
                Tools.NL();  // line break on regular line
            } // Inner loop
            Tools.NL(); // line break on exit line
            break;
        } // Single exit block
        
        // Restore registers
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
#endif
    }

}
