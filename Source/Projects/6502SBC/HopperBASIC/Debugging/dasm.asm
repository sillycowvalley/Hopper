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
            LDA #'[' COut(); LDA ZP.IDXH HOut();LDA ZP.IDXL HOut(); LDA #']' COut(); // opcode stream
            LDA #'[' COut(); LDA ZP.XIDH HOut();LDA ZP.XIDL HOut(); LDA #']' COut(); // token stream
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
                        Space(); Space(); Space();
                    }
                    case 0x40:  // one byte operand
                    {
                        LDA [ZP.IDY]
                        STA ZP.ACCL
                        HOut(); Space();
                        IncIDY();
                        
                        Space(); Space(); 
                        Space(); Space(); Space();
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
                        Space(); Space(); Space();
                    }
                    case 0xC0:  // two byte operands
                    {
                        LDA [ZP.IDY]
                        STA ZP.ACCT
                        HOut(); Space();
                        IncIDY();
                        
                        LDA [ZP.IDY]
                        STA ZP.ACCL
                        HOut();
                        IncIDY(); Space();
                        
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
                LDY # 13 // pad to width
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
                                LDA ZP.ACCL
                                BASICSysCalls.ToString();
                            }
                            case OpCode.PUSHBIT:
                            {
                                // Show TRUE or FALSE
                                LDA #'(' COut();
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                STZ ZP.TOPH
                                LDA # BASICType.BIT
                                STA ZP.TOPT
                                
                                BASICTypes.PrintValue();
                                LDA #')' COut();
                            }
                            case OpCode.PUSHCHAR:
                            {
                                // Show TRUE or FALSE
                                LDA #'(' COut();
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                STZ ZP.TOPH
                                LDA # BASICType.CHAR
                                STA ZP.TOPT
                                
                                SEC // quotes for CHAR
                                BASICTypes.PrintValue();
                                LDA #')' COut();
                            }
                            case OpCode.PUSHGLOBAL:
                            case OpCode.POPGLOBAL:
                            {
                                
                                LDA #'(' COut();
                                // TODO
                                //LDA ZP.ACCL
                                //STA ZP.IDXL
                                //LDA ZP.ACCH
                                //STA ZP.IDXH
                                //Variables.GetName();
                                //Tools.PrintStringSTR();
                                LDA #')' COut();
                                 
                            }
                            case OpCode.PUSHLOCAL:
                            case OpCode.POPLOCAL:
                            {
                                // Format BP offset nicely: [BP-2] or [BP+1]
                                LDA #'[' COut();
                                LDA #'B' COut();
                                LDA #'P' COut();
                                
                                // Sign extend the byte to 16-bit word
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                if (MI) // Negative
                                {
                                    LDA #0xFF
                                    STA ZP.TOPH  // Sign extend for negative
                                }
                                else // Positive or zero
                                {
                                    STZ ZP.TOPH  // Zero high byte for positive
                                    LDA #'+' COut();
                                }
                                
                                LDA #BASICType.INT
                                STA ZP.TOPT
                                Tools.PrintDecimal();  // Handles negative numbers correctly
                                
                                LDA #']' COut();
                            }
                            case OpCode.JUMPB:
                            case OpCode.JUMPZB:
                            case OpCode.JUMPNZB:
                            {
                                // Format jump offset nicely
                                LDA #'(' COut();
                                
                                // Sign extend to 16-bit for printing
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                if (PL)
                                {
                                    STZ ZP.TOPH
                                    if (NZ)
                                    {
                                        LDA #'+' COut();
                                    }
                                }
                                else
                                {
                                    LDA #0xFF
                                    STA ZP.TOPH
                                }
                                
                                LDA #BASICType.INT
                                STA ZP.TOPT
                                Tools.PrintDecimal();
                                
                                LDA #' ' COut(); 
                                LDA #'-' COut(); 
                                LDA #'>' COut(); 
                                LDA #' ' COut();
                                
                                // Calculate target address
                                CLC
                                LDA ZP.IDYL
                                ADC ZP.ACCL
                                STA ZP.ACCL
                                LDA ZP.IDYH
                                if (MI)  // Original offset was negative
                                {
                                    ADC #0xFF  // Sign extend
                                }
                                else
                                {
                                    ADC #0
                                }
                                STA ZP.ACCH
                                
                                LDA ZP.ACCH HOut();
                                LDA ZP.ACCL HOut();
                                
                                LDA #')' COut();
                            }
                        }
                    }
                    case 0x80:  // two byte operands
                    {
                        Space(); LDA ZP.ACCH HOut(); LDA ZP.ACCL HOut(); Space();
                        switch (X)
                        {
                            case OpCode.SETITEMGG:
                            case OpCode.SETITEMGL:
                            case OpCode.SETITEMLG:
                            case OpCode.SETITEMLL:
                            
                            case OpCode.GETITEMGG:
                            case OpCode.GETITEMGL:
                            case OpCode.GETITEMLG:
                            case OpCode.GETITEMLL:
                            {
                                // Show as two separate byte operands
                                Space(); LDA ZP.ACCL HOut(); Space(); LDA ZP.ACCH HOut(); Space();
                            }
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
                            case OpCode.PUSHINT:
                            {
                                // Show unsigned decimal value in parentheses
                                LDA #'(' COut();
                                
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.ACCH
                                STA ZP.TOPH
                                LDA # BASICType.INT
                                STA ZP.TOPT
                                BASICTypes.PrintValue();
                                
                                LDA #')' COut();
                            }
                            case OpCode.PUSHWORD:
                            {
                                // Show unsigned decimal value in parentheses
                                LDA #'(' COut();
                                
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.ACCH
                                STA ZP.TOPH
                                LDA # BASICType.WORD
                                STA ZP.TOPT
                                BASICTypes.PrintValue();
                                
                                LDA #')' COut();
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
                            case OpCode.PUSHCSTRING:
                            {
                                CLC
                                LDA ZP.XIDL
                                ADC ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.XIDH
                                ADC ZP.ACCH
                                STA ZP.TOPH
                                LDA # BASICType.STRING
                                STA ZP.TOPT
                                
                                SEC // quotes
                                BASICTypes.PrintValue();
                            }
                            case OpCode.JUMPW:
                            case OpCode.JUMPZW:
                            case OpCode.JUMPNZW:
                            {
                                LDA #'(' COut();
                                LDA # BASICType.INT
                                STA ZP.TOPT
                                LDA ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.ACCH
                                STA ZP.TOPH
                                if (PL)
                                {
                                    LDA #'+' COut();
                                }
                                Tools.PrintDecimal();
                                
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
                    case 0xC0:  // three byte operands
                    {
                        Space(); LDA ZP.ACCT HOut(); Space(); LDA ZP.ACCH HOut(); LDA ZP.ACCL HOut(); Space();
                        switch (X)
                        {
                            case OpCode.FORCHK:
                            {
                                // Format: FORCHK (iterator_offset, forward_jump)
                                // ZP.ACCT = iterator_offset
                                // ZP.ACCL = forward_offset_lsb  
                                // ZP.ACCH = forward_offset_msb
                                
                                LDA #'(' COut();
                                
                                // Show iterator offset
                                LDA #'[' COut();
                                 // Sign extend the byte to 16-bit word
                                LDA ZP.ACCT
                                STA ZP.TOPL
                                if (PL)  // Positive or zero
                                {
                                    STZ ZP.TOPH  // Zero high byte for positive
                                    if (NZ)      // Don't print + for zero
                                    {
                                        LDA #'+' COut();
                                    }
                                }
                                else  // Negative
                                {
                                    LDA #0xFF
                                    STA ZP.TOPH  // Sign extend for negative
                                }
                                
                                LDA #BASICType.INT
                                STA ZP.TOPT
                                Tools.PrintDecimal();  // Handles negative numbers correctly
                                LDA #']' COut();
                                
                                Space();
                                
                                // Calculate and show forward jump target
                                CLC
                                LDA ZP.IDYL
                                ADC ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.IDYH
                                ADC ZP.ACCH
                                STA ZP.TOPH
                                
                                LDA #' ' COut(); LDA #'-' COut(); LDA #'>' COut(); LDA #' ' COut();
                                
                                LDA ZP.TOPH HOut();
                                LDA ZP.TOPL HOut();
                                
                                LDA #')' COut();
                            }
                            case OpCode.FORIT:
                            case OpCode.FORITF:
                            {
                                // Format: FORIT (iterator_offset, backward_jump)
                                // ZP.ACCT = iterator_offset
                                // ZP.ACCL = backward_offset_lsb
                                // ZP.ACCH = backward_offset_msb
                                
                                LDA #'(' COut();
                                
                                // Show iterator offset
                                LDA #'[' COut();
                                 // Sign extend the byte to 16-bit word
                                LDA ZP.ACCT
                                STA ZP.TOPL
                                if (PL)  // Positive or zero
                                {
                                    STZ ZP.TOPH  // Zero high byte for positive
                                    if (NZ)      // Don't print + for zero
                                    {
                                        LDA #'+' COut();
                                    }
                                }
                                else  // Negative
                                {
                                    LDA #0xFF
                                    STA ZP.TOPH  // Sign extend for negative
                                }
                                
                                LDA #BASICType.INT
                                STA ZP.TOPT
                                Tools.PrintDecimal();  // Handles negative numbers correctly
                                LDA #']' COut();
                                
                                Space();
                                
                                // Calculate and show backward jump target
                                // Note: This is typically a negative offset
                                CLC
                                LDA ZP.IDYL
                                ADC ZP.ACCL
                                STA ZP.TOPL
                                LDA ZP.IDYH
                                ADC ZP.ACCH
                                STA ZP.TOPH
                                
                                LDA #' ' COut(); LDA #'-' COut(); LDA #'>' COut(); LDA #' ' COut();
                                
                                LDA ZP.TOPH HOut();
                                LDA ZP.TOPL HOut();
                                
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
