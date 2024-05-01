unit Type
{
    enum Types
    {
        Undefined,
        Char,   // char (for now)
        Int,    // 16 bit signed
        Byte,   // unsigned char
        UInt,   // internal type for unsigned 16 bit int (tFlags and tEnum)
        Reference,  // internal type for "ref" addresses (tUInt)
        Bool,
        
        Type       = 0x0C,
        
        ReferenceType = 0x0D, // (type >= ReferenceType) -> reference type
        
        Float      = 0x0D,
        Long       = 0x0E,
        String     = 0x0F,
        Pair       = 0x10,
        Array      = 0x12,
        Dictionary = 0x13,
        Variant    = 0x14,
        File       = 0x15,
        Directory  = 0x16,
        List       = 0x19,
    }
}
