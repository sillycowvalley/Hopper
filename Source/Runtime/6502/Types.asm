unit Type // Types.asm
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
    
    IsReferenceType()
    {
        // Assumes type is in A
        // Returns C=1 if type is a reference type, C=0 otherwise
    
        CMP # Types.ReferenceType
        if (C)
        {
            // If type is >= ReferenceType, it's a reference type
            SEC
            return;
        }
    
        CLC
    }
    
    TypeOf()
    {
        Stacks.PopTop();
        LDA TOPT
        IsReferenceType();
        if (C)
        {
            LDA TOPL
            STA IDXL
            LDA TOPH
            STA IDXH
            GC.Release(); 
        }
        LDA TOPT
        STA TOPL
        LDA # 0
        STA TOPH
        LDA # Types.Type
        Stacks.PushTop();
    }
    BoxTypeOf()
    {
        Stacks.PopTop();
        LDA TOPT
        IsReferenceType();
        if (C)
        {
            LDY # 0
            LDA [TOP], Y
            STA TOPT
            CMP # Types.Variant
            if (Z)
            {
                LDY # 2
                LDA [TOP], Y
                STA TOPT
            }
            LDA TOPL
            STA IDXL
            LDA TOPH
            STA IDXH
            GC.Release();
        }
        LDA TOPT
        STA TOPL
        LDA # 0
        STA TOPH
        LDA # Types.Type
        Stacks.PushTop();
    }
    
}
