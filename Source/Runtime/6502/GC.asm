unit GC
{
    uses "ZeroPage"
    uses "Memory"
    
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
    
    Create()
    {
        // create new object on heap (reference count 1)
        // type in A
        // size is in FSIZE
        // return address in IDX
        
        //phx
        TAX
        
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        LDA ACCL
        PHA
        LDA ACCH
        PHA
        
        CLC
        LDA FSIZEL  // LSB
        ADC # 0x05    // 4 + 1, ok do that, but also round up to the nearest 4 byte boundary to reduce fragmentation
        STA ACCL
        LDA FSIZEH  // MSB
        ADC # 0
        STA ACCH
        
        LDA ACCL
        AND # 0xFC
        STA ACCL
        
        // size is in ACC
        // return address in IDX
        Allocate.allocate();
        
        LDY # 0
        TXA
        STA [IDX], Y // type
        INY
        LDA # 1
        STA [IDX], Y // reference count starts at 1
        
        PLA
        STA ACCH
        PLA
        STA ACCL
        
        PLA
        STA IDYH
        PLA
        STA IDYL
        
        //plx
    }
    
    AddReference()
    {
        TYA PHA
        
        // increment the reference count by 1
        // address in IDX
        LDY # 1
        LDA [IDX], Y
        CLC
        ADC # 1
        STA [IDX], Y
        
        PLA TAY
    }
    Release()
    {
        TYA PHA
        TXA PHA
        
        // decrement reference count, if zero, free
        
        // address in IDX
        LDY # 1
        LDA [IDX], Y // reference count
        
#ifdef CHECKED
        if (Z)
        {
            LDA 0x0B BRK // reference count already zero?
        }  
#endif        
        SEC
        SBC # 1
        STA [IDX], Y
        
        // if zero, free
        CMP #0
        if (Z)
        {       
            LDY # 0
            LDA [IDX], Y // type
            switch (A)
            {
                case Types.String:
                case Types.Array:
                {
                    Free.free();        
                }
                default:
                {
                    LDA # 0x0B BRK // unsupported reference type
                }
            }
        }
        
        PLA TAX
        PLA TAY
    }
    Clone()
    {
        // type is in A
        // reference type to clone is at IDY, resulting clone in IDX
        TAX
        TYA PHA
        
        // Clone.List and Clone.Dictionary can go recursive:       preserve lCURRENT, lNEXT, IDY
        LDA LCURRENTL
        PHA
        LDA LCURRENTH
        PHA
        LDA LNEXTL
        PHA
        LDA LNEXTH
        PHA
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        switch (X)
        {
            case Types.String:
            case Types.Array:
            case Types.Long:
            {
                genericClone();
            }
            default:
            {
                LDA # 0x0B BRK // unsupported reference type
            }
        }
        PLA
        STA IDYH
        PLA
        STA IDYL
        PLA
        STA LNEXTH
        PLA
        STA LNEXTL
        PLA
        STA LCURRENTH
        PLA
        STA LCURRENTL
        
        PLA TAY
    }
    
    // IDY -> source, returns cloned object in IDX
    genericClone()
    {
        /*
        LDA FSOURCEADDRESSH
        PHA
        LDA FSOURCEADDRESSL
        PHA
        */
        // get the memory block size  
        LDA IDYL
        STA IDXL      
        LDA IDYH
        STA IDXH
        DecIDX();
        DecIDX();
        LDY # 0 
        LDA [IDX], Y
        STA ACCL
        INY 
        LDA [IDX], Y
        STA ACCH
        
        // size = size - size word of block
        DecACC();
        DecACC();
        
        LDA ACCL
        STA FLENGTHL
        LDA ACCH
        STA FLENGTHH
        
        // initialized from source at IDY (before it gets munted in Allocate)
        LDA IDYL
        STA FSOURCEADDRESSL
        LDA IDYH
        STA FSOURCEADDRESSH
        
        // size is in ACC
        // return address in IDX
        Allocate.allocate();
        
        // initialized from destination at IDX
        LDA IDXL
        STA FDESTINATIONADDRESSL
        LDA IDXH
        STA FDESTINATIONADDRESSH
        
        // SOURCEADDRESS -> DESTINATIONADDRESS
        LDY # 0
        loop
        {
            LDA FLENGTHL
            if (Z)
            {
                LDA FLENGTHH
                if (Z) { break; }
            }
            
            LDA [FSOURCEADDRESS], Y
            STA [FDESTINATIONADDRESS], Y
            
            IncDESTINATIONADDRESS();
            IncSOURCEADDRESS();
            DecLENGTH();
        }
        
        // exact clone : make sure reference count is only one
        LDY # 1
        LDA # 1
        STA [IDX], Y
        
        /*
        PLA
        STA FSOURCEADDRESSL
        PLA
        STA FSOURCEADDRESSH
        */
    }
}
