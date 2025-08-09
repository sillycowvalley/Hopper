unit BASICTypes // BASICTypes.asm
{
    enum BASICType
    {
        VOID     = 0x00, // Hopper VM Types.Undefined (function return type indicating no return value (internal))
        
        INT      = 0x02, // Hopper VM Types.Int
        BYTE     = 0x03, // Hopper VM Types.Byte
        WORD     = 0x04, // Hopper VM Types.UInt
        
        BIT      = 0x06, // Hopper VM Types.Bool - must 0x06!
        ARRAY    = 0x07,
        
        STRING   = 0x0F, // Hopper VM Types.String
        
        VAR      = 0x10, // bit that makes a variable type specifiable at runtime (like arguments)
        
        TYPEMASK = 0x0F,
        MASK     = 0x1F,
    }
    
    const string voidName = "VOID";
    
    // Print BasicType enum value as readable string
    // Input: A = BasicType enum value, ZP.TOP contains value (or pointer)
    // Output: Type name printed to serial
    // Preserves: Everything
    PrintType()
    {
        PHA
        
        // Convert BasicType to corresponding Token and use keyword table
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
            case BASICType.STRING:
            {
                LDA #Token.STRING
                Tokens.PrintKeyword();
            }
            case BASICType.VAR:
            {
                LDA #Token.VAR
                Tokens.PrintKeyword();
            }
            /*
            case BASICType.ARRAY:
            {
                LDA #Token.ARRAY
                Tokens.PrintKeyword();
            }
            */
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
            /*
            case #Token.ARRAY:
            {
                LDA # BASICType.ARRAY
                SEC
            }
            */
            
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
