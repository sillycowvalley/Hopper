unit BASICTypes // BASICTypes.asm
{
    enum BasicType
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
            case BasicType.INT:
            {
                LDA #Token.INT
                Tokenizer.PrintKeyword();
            }
            case BasicType.WORD:
            {
                LDA #Token.WORD
                Tokenizer.PrintKeyword();
            }
            case BasicType.BIT:
            {
                LDA #Token.BIT
                Tokenizer.PrintKeyword();
            }
            case BasicType.BYTE:
            {
                LDA #Token.BYTE
                Tokenizer.PrintKeyword();
            }
            case BasicType.STRING:
            {
                LDA #Token.STRING
                Tokenizer.PrintKeyword();
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
