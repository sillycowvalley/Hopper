unit Types
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
}
