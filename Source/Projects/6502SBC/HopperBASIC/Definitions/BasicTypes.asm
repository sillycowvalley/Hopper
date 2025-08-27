unit BASICTypes // BASICTypes.asm
{
    // Symbol types: only top 2 bits
    flags SymbolType
    {
        VARIABLE = 0x40,   // Mutable values
        CONSTANT = 0x80,   // Immutable values  
        ARGUMENT = 0x40,   // Function parameters (negative BP offset)
        LOCAL    = 0x80,   // Local variables (positive BP offset)
        
        MASK     = 0xC0,   // Top 2 bits only
    }
    
    enum BASICType
    {
        VOID     = 0x00, // Hopper VM Types.Undefined (function return type indicating no return value (internal))
        CHAR     = 0x01, // Hopper VM Types.Char
        INT      = 0x02, // Hopper VM Types.Int
        BYTE     = 0x03, // Hopper VM Types.Byte
        WORD     = 0x04, // Hopper VM Types.UInt
        STRING   = 0x05, // Hopper VM Types.String
        BIT      = 0x06, // Hopper VM Types.Bool - must 0x06!
        
        LONG     = 0x08, // Bit 3 - LONG is the only type to use this bit!
        
        
        
        VAR      = 0x10, // Bit 4 - runtime-determined type
        ARRAY    = 0x20, // Bit 5 - array 
        
        
        TYPEMASK = 0x0F,   // Bottom 4 bits for base type
        FLAGMASK = 0x30,   // Bits 4-5 for flags
        MASK     = 0x3F,   // Bottom 6 bits total
    }
    
    
    
    
    // Input:   TOP0..3, BYTE, WORD, INT
    // Output:  TOP0..3, TOPT -> LONG, C if OK, NC if unsupported type
    Promote()
    {
        loop
        {
            LDA ZP.TOPT
            switch(A)
            {
                case BASICType.BYTE:
                {
                    Long.ZeroTop3();
                }
                case BASICType.INT:
                {
                    if (BBS7, ZP.TOP1)
                    {
                        LDA #0x0FF
                        STA ZP.TOP2
                        STA ZP.TOP3
                    }
                    else
                    {
                        STZ ZP.TOP2
                        STZ ZP.TOP3
                    }
                }
                case 0:
                case BASICType.WORD:
                {
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                }
                case BASICType.LONG:
                {
                }
                default:
                {
                    CLC
                    break;
                }
            }
            LDA # BASICType.LONG
            STA ZP.TOPT
            SEC
            break;
        } // single exit
    }
    
    // Input:   LONG: TOP0-3, TOPT, desired type ACCT
    // Output:  C, or NC if out of range
    Coerce()
    {
        loop
        {
            LDA #BASICType.LONG
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BYTE:
                case BASICType.CHAR:
                {
                    LDA ZP.TOP1
                    ORA ZP.TOP2
                    ORA ZP.TOP3 // x3
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                case BASICType.WORD:
                {
                    LDA ZP.TOP2
                    ORA ZP.TOP3 // x2
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                case BASICType.INT:
                {
                    LDA ZP.TOP2
                    ORA ZP.TOP3 // x2
                    if (NZ)
                    {
                        LDA #0xFF
                        CMP ZP.TOP3
                        if (NZ)
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                        CMP ZP.TOP2
                        if (NZ)
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                        if (BBR7, ZP.TOP1) // not negative
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                    }
                }
                default:
                {
                    Error.RangeError(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            SEC
            break;
        } // single exit
    }
    
    // Input:   LONG: NEXT0-3, NEXT, desired type ACCT
    // Output:  C, or NC if out of range
    CoerceNext()
    {
        loop
        {
            LDA #BASICType.LONG
            CMP ZP.NEXTT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            LDA ZP.ACCT
            switch (A)
            {
                case BASICType.BYTE:
                case BASICType.CHAR:
                {
                    LDA ZP.NEXT1
                    ORA ZP.NEXT2
                    ORA ZP.NEXT3 // x2
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                case BASICType.WORD:
                {
                    LDA ZP.NEXT2
                    ORA ZP.NEXT3 // x2
                    if (NZ)
                    {
                        Error.RangeError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                case BASICType.INT:
                {
                    LDA ZP.NEXT2
                    ORA ZP.NEXT3 // x2
                    if (NZ)
                    {
                        LDA #0xFF
                        CMP ZP.NEXT3
                        if (NZ)
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                        CMP ZP.NEXT2
                        if (NZ)
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                        if (BBR7, ZP.NEXT1) // not negative
                        {
                            Error.RangeError(); BIT ZP.EmulatorPCL
                            break;
                        }
                    }
                }
                default:
                {
                    Error.RangeError(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            SEC
            break;
        } // single exit
    }
    
    // Print BasicType enum value as readable string
    // Input: A = BasicType enum value
    // Output: Type name printed to serial
    // Preserves: Everything
    PrintType()
    {
        PHA
        PHX
        TAX
        
        loop
        {
            AND # SymbolType.MASK
            if (NZ)
            {
                TXA
                AND # SymbolType.VARIABLE
                if (NZ)
                {
                    LDA # Token.VAR
                    Tokens.PrintKeyword(); // preserves X and Y
                    LDA #'|' Print.Char();
                }
                TXA
                AND # SymbolType.CONSTANT
                if (NZ)
                {
                    LDA # Token.CONST
                    Tokens.PrintKeyword(); // preserves X and Y
                    LDA #'|' Print.Char();
                }
            }
            
            TXA
            AND # BASICType.VAR
            if (NZ)
            {
                LDA #Token.VAR
                Tokens.PrintKeyword(); // preserves X and Y
                TXA
                AND # BASICType.TYPEMASK
                if (NZ)
                {
                    // VAR contains a type
                    LDA #'(' Print.Char();
                    TXA
                    AND # BASICType.TYPEMASK
                    PrintType();
                    LDA #')' Print.Char();
                }    
            }
            else
            {
                TXA
                AND # BASICType.ARRAY
                if (NZ)
                {
                    LDA #Token.ARRAY
                    Tokens.PrintKeyword(); // preserves X and Y
                    TXA
                    AND # BASICType.TYPEMASK
                    if (NZ)
                    {
                        // ARRAY contains elements of type
                        LDA #'(' Print.Char();
                        TXA
                        AND # BASICType.TYPEMASK
                        PrintType();
                        LDA #')' Print.Char();
                    }    
                }
                else
                {
                    // Convert BasicType to corresponding Token and use keyword table
                    TXA
                    AND # BASICType.TYPEMASK
                    if (Z)
                    {
                        // 0 = BASICType.VOID:
                        LDA # ErrorWord.VOID
                        Error.PrintWord();
                    }
                    else
                    {
                        switch (A)
                        {
                            case BASICType.INT:
                            {
                                LDA #Token.INT
                            }
                            case BASICType.WORD:
                            {
                                LDA #Token.WORD
                            }
                            case BASICType.LONG:
                            {
                                LDA #Token.LONG
                            }
                            case BASICType.BIT:
                            {
                                LDA #Token.BIT
                            }
                            case BASICType.BYTE:
                            {
                                LDA #Token.BYTE
                            }
                            case BASICType.CHAR:
                            {
                                LDA #Token.CHAR
                            }
                            case BASICType.STRING:
                            {
                                LDA #Token.STRING
                            }
                            default:
                            {
                                // Unknown type
                                Error.InternalError(); BIT ZP.EmulatorPCL
                                break;
                            }
                        } // switch
                        Tokens.PrintKeyword(); // preserves X and Y
                    }
                }
            }
            break;
        } // single exit
        PLX
        PLA
    }
    // Input: X
    // Output: C set if token is a type keyword, NC if not a type keyword, A = BASICType
    FromToken()
    {
        switch (X)
        {
            case Token.INT:
            {
                LDA # BASICType.INT
                SEC
            }
            case Token.WORD:
            {
                LDA # BASICType.WORD
                SEC
            }
            case Token.BYTE:
            {
                LDA # BASICType.BYTE
                SEC
            }
            case Token.CHAR:
            {
                LDA # BASICType.CHAR
                SEC
            }
            case Token.BIT:
            {
                LDA # BASICType.BIT
                SEC
            }
            case Token.STRING:
            {
                LDA # BASICType.STRING
                SEC
            }
            case Token.VAR:
            {
                LDA # BASICType.VAR
                SEC
            }
            case Token.ARRAY:
            {
                LDA # BASICType.ARRAY
                SEC
            }
            case Token.LONG:
            {
                LDA # BASICType.LONG
                SEC
            }
            default:
            {
                CLC
            }
        }
    }
    
    // Convert BASICType base type to corresponding Token
    // Input: A = BASICType base type (e.g., BASICType.INT)
    // Output: A = corresponding Token (e.g., Token.INT), C if success, NC if not
    ToToken()
    {
        switch (A)
        {
            case BASICType.INT:
            {
                LDA #Token.INT
                SEC
            }
            case BASICType.WORD:
            {
                LDA #Token.WORD
                SEC
            }
            case BASICType.BYTE:
            {
                LDA #Token.BYTE
                SEC
            }
            case BASICType.CHAR:
            {
                LDA #Token.CHAR
                SEC
            }
            case BASICType.STRING:
            {
                LDA #Token.STRING
                SEC
            }
            case BASICType.BIT:
            {
                LDA #Token.BIT
                SEC
            }
            case BASICType.VAR:
            {
                LDA #Token.VAR
                SEC
            }
            case BASICType.ARRAY:
            {
                LDA #Token.ARRAY
                SEC
            }
            case BASICType.LONG:
            {
                LDA #Token.LONG
                SEC
            }
            default:
            {
                // Unknown type
                CLC
            }
        }
    }
    
    // Print variable value with proper type formatting
    // Input: ZP.TOP = value, ZP.TOPT = type, C =  quote strings, NC = no quotes
    // Output: Value printed to serial (TRUE/FALSE for BIT, numeric for others)
    // Preserves: Everything
    PrintValue()
    {
        PHA
        PHX
        PHP
        
        // Special handling for BIT type - print TRUE/FALSE instead of 1/0
        LDX ZP.TOPT
        switch(X)
        {
            case BASICType.BIT:
            {
                LDA ZP.TOPL
                CMP #0
                if (Z)
                {
                    LDA #Token.FALSE
                    Tokens.PrintKeyword();
                }
                else
                {
                    LDA #Token.TRUE
                    Tokens.PrintKeyword();
                }
            }
            case BASICType.CHAR:
            {
                // Print the character itself, not its numeric value
                PLP
                if (C)
                {
                    LDA #'\'' Serial.WriteChar();
                    LDA ZP.TOPL
                    Tools.IsPrintable();
                    if (C)
                    {
                        LDA ZP.TOPL
                        Serial.WriteChar();
                    }
                    else
                    {
                        LDA #'\\' Serial.WriteChar(); 
                        LDX ZP.TOPL
                        switch (X)
                        {
                            case 0x00:
                            {
                                LDA #'0' Serial.WriteChar();
                            }
                            case 0x07:
                            {
                                LDA #'a' Serial.WriteChar();
                            }
                            case 0x08:
                            {
                                LDA #'b' Serial.WriteChar();
                            }
                            case 0x09:
                            {
                                LDA #'t' Serial.WriteChar();
                            }
                            case 0x0A:
                            {
                                LDA #'n' Serial.WriteChar();
                            }
                            case 0x0C:
                            {
                                LDA #'f' Serial.WriteChar();
                            }
                            case 0x0D:
                            {
                                LDA #'r' Serial.WriteChar();
                            }
                            case 0x27:
                            {
                                LDA #'e' Serial.WriteChar();
                            }
                            default:
                            {
                                LDA #'x' Serial.WriteChar(); TXA Serial.HexOut();
                            }
                        }
                    }
                    LDA #'\'' Serial.WriteChar();
                }
                else
                {
                    LDA ZP.TOPL
                    Serial.WriteChar();
                }
                PHP
                
            }
            case BASICType.STRING:
            {
                PLP
                LDA ZP.TOPL
                STA ZP.STRL
                LDA ZP.TOPH
                STA ZP.STRH
                if (C)
                {
                    LDA #'"' Serial.WriteChar();
                    Print.String();  // Print the actual string content
                    LDA #'"' Serial.WriteChar();
                }
                else
                {
                    Print.String();  // Print the actual string content
                }
                PHP
            }
            case BASICType.LONG:
            {
                Print.Decimal(); // Numeric types
            }
            default:
            {
#ifdef DEBUG
Debug.NL(); TLOut();
#endif
                Error.InternalError(); BIT ZP.EmulatorPCL
                // Print.Decimal(); // Numeric types
            }
        }
        
        PLP
        PLX
        PLA
    }
}
