unit BASICTypes // BASICTypes.asm
{
    enum BASICType
    {
        VOID     = 0x00, // Hopper VM Types.Undefined (function return type indicating no return value (internal))
        CHAR     = 0x01, // Hopper VM Types.Char
        INT      = 0x02, // Hopper VM Types.Int
        BYTE     = 0x03, // Hopper VM Types.Byte
        WORD     = 0x04, // Hopper VM Types.UInt
        
        BIT      = 0x06, // Hopper VM Types.Bool - must 0x06!
        
        STRING   = 0x0F, // Hopper VM Types.String
        
        VAR      = 0x10, // Bit 4 - runtime-determined type
        ARRAY    = 0x20, // Bit 5 - array 
        
        
        TYPEMASK = 0x0F,   // Bottom 4 bits for base type
        FLAGMASK = 0x30,   // Bits 4-5 for flags
        MASK     = 0x3F,   // Bottom 6 bits total
    }
    
    const string voidName = "VOID";
    
    // Print BasicType enum value as readable string
    // Input: A = BasicType enum value
    // Output: Type name printed to serial
    // Preserves: Everything
    PrintType()
    {
        PHA
        PHX
        TAX
        
        AND #SymbolType.MASK
        if (NZ)
        {
            TXA
            AND #SymbolType.VARIABLE
            if (NZ)
            {
                LDA # Token.VAR
                Tokens.PrintKeyword();
                LDA #'|' COut();
            }
            TXA
            AND #SymbolType.CONSTANT
            if (NZ)
            {
                LDA # Token.CONST
                Tokens.PrintKeyword();
                LDA #'|' COut();
            }
        }
        
        TXA
        AND # BASICType.VAR
        if (NZ)
        {
            LDA #Token.VAR
            Tokens.PrintKeyword();
            TXA
            AND # BASICType.TYPEMASK
            if (NZ)
            {
                // VAR contains a type
                LDA #'(' COut();
                TXA
                AND # BASICType.TYPEMASK
                PrintType();
                LDA #')' COut();
            }    
        }
        else
        {
            TXA
            AND # BASICType.ARRAY
            if (NZ)
            {
                LDA #Token.ARRAY
                Tokens.PrintKeyword();
                TXA
                AND # BASICType.TYPEMASK
                if (NZ)
                {
                    // ARRAY contains elements of type
                    LDA #'(' COut();
                    TXA
                    AND # BASICType.TYPEMASK
                    PrintType();
                    LDA #')' COut();
                }    
            }
            else
            {
                // Convert BasicType to corresponding Token and use keyword table
                TXA
                AND # BASICType.TYPEMASK
                switch (A)
                {
                    case BASICType.INT:
                    {
                        LDA #Token.INT
                        Tokens.PrintKeyword();
                    }
                    case BASICType.WORD:
                    {
                        LDA #Token.WORD
                        Tokens.PrintKeyword();
                    }
                    case BASICType.BIT:
                    {
                        LDA #Token.BIT
                        Tokens.PrintKeyword();
                    }
                    case BASICType.BYTE:
                    {
                        LDA #Token.BYTE
                        Tokens.PrintKeyword();
                    }
                    case BASICType.CHAR:
                    {
                        LDA #Token.CHAR
                        Tokens.PrintKeyword();
                    }
                    case BASICType.STRING:
                    {
                        LDA #Token.STRING
                        Tokens.PrintKeyword();
                    }
                    case BASICType.VOID:
                    {
                        LDA #(voidName % 256)
                        STA ZP.STRL
                        LDA #(voidName / 256)
                        STA ZP.STRH
                        PrintStringSTR();
                    }
                    default:
                    {
                        // Unknown type
                        Serial.HexOut();
                    }
                }
            }
        }
        
        PLX
        PLA
    }
    // Input: X
    // Output: C set if token is a type keyword, NC if not a type keyword, A = BASICType
    FromToken()
    {
        switch (X)
        {
            case #Token.INT:
            {
                LDA # BASICType.INT
                SEC
            }
            case #Token.WORD:
            {
                LDA # BASICType.WORD
                SEC
            }
            case #Token.BYTE:
            {
                LDA # BASICType.BYTE
                SEC
            }
            case #Token.CHAR:
            {
                LDA # BASICType.CHAR
                SEC
            }
            case #Token.BIT:
            {
                LDA # BASICType.BIT
                SEC
            }
            case #Token.STRING:
            {
                LDA # BASICType.STRING
                SEC
            }
            case #Token.VAR:
            {
                LDA # BASICType.VAR
                SEC
            }
            case #Token.ARRAY:
            {
                LDA # BASICType.ARRAY
                SEC
            }
            default:
            {
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
                if (C)
                {
                    LDA #'"' Serial.WriteChar();
                    PrintStringTOP();  // Print the actual string content
                    LDA #'"' Serial.WriteChar();
                }
                else
                {
                    PrintStringTOP();  // Print the actual string content
                }
                PHP
            }
            default:
            {
                PrintDecimalWord(); // Numeric types
            }
        }
        
        PLP
        PLX
        PLA
    }
}
