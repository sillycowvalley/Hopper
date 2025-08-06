unit BASICTypes // BASICTypes.asm
{
    enum BASICType
    {
        VOID   = 0x00, // function return type indicating no return value (internal)
        INT    = 0x02, // Types.Int
        BYTE   = 0x03, // Types.Byte
        WORD   = 0x04, // Types.UInt
        BIT    = 0x06, // Types.Bool
        STRING = 0x0F, // Types.String
        ARRAY  = 0x12, // Types.Array
    }
    
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
            default:
            {
                // Unknown type
                Serial.HexOut();
            }
        }
        
        PLA
    }
}
