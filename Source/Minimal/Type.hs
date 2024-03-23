unit Type
{
    enum Type
    {
        Undefined  = 0x00,
        Char       = 0x01,
        Int        = 0x02,
        Byte       = 0x03,
        UInt       = 0x04,
        Reference  = 0x05,
        Bool       = 0x06,
        
        Type       = 0x0C,
        
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
        ListItem   = 0x1A, /// testing only
    }
    
    bool IsReferenceType(Type htype)
    {
        return (byte(htype) >= 0x0D);
    }
}
