unit GC
{
    uses "ZeroPage"
    uses "Memory"
    uses "Type"
    
    
    Create()
    {
        // create new object on heap (reference count 1)
        // type in A
        // size is in FSIZE
        // return address in IDX
        
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
        
    }
    
    AddReference()
    {
#ifdef CPU_65C02S
        PHY
#else
        TYA PHA
#endif
        
        // increment the reference count by 1
        // address in IDX
        LDY # 1
        LDA [IDX], Y
        CLC
        ADC # 1
        STA [IDX], Y
#ifdef CPU_65C02S
        PLY
#else        
        PLA TAY
#endif
    }
    Release()
    {
#ifdef CPU_65C02S
        PHY PHX
#else
        TYA PHA
        TXA PHA
#endif
        
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
        //CMP #0
        if (Z)
        {       
#ifdef CPU_65C02S
            LDA [IDX] // type
#else            
            LDY # 0
            LDA [IDX], Y // type
#endif
            switch (A)
            {
                case Types.String:
                case Types.Array:
                case Types.Long:
                case Types.Float:
                {
                    Free.free();        
                }
                default:
                {
#ifdef CHECKED
                    LDA # 0x0B BRK // unsupported reference type
#endif
                }
            }
        }
#ifdef CPU_65C02S
        PLX PLY
#else
        PLA TAX
        PLA TAY
#endif
    }
    Clone()
    {
        // type is in A
        // reference type to clone is at IDY, resulting clone in IDX
        TAX
#ifdef CPU_65C02S
        PHY
#else
        TYA PHA
#endif
        
        // Clone.List and Clone.Dictionary can go recursive:       preserve lCURRENT, lNEXT, IDY
        /*
        LDA LCURRENTL
        PHA
        LDA LCURRENTH
        PHA
        LDA LNEXTL
        PHA
        LDA LNEXTH
        PHA
        */
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        switch (X)
        {
            case Types.String:
            case Types.Array:
            case Types.Long:
            case Types.Float:
            {
                genericClone();
            }
            default:
            {
#ifdef CHECKED
                LDA # 0x0B BRK // unsupported reference type
#endif
            }
        }
        PLA
        STA IDYH
        PLA
        STA IDYL
        /*
        PLA
        STA LNEXTH
        PLA
        STA LNEXTL
        PLA
        STA LCURRENTH
        PLA
        STA LCURRENTL
        */
#ifdef CPU_65C02S
        PLY
#else        
        PLA TAY
#endif
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
        
        // IDX = IDY - 2
        SEC
        LDA IDYL
        SBC # 2
        STA IDXL
        LDA IDYH
        SBC # 0
        STA IDXH
        
        LDY # 0 
        LDA [IDX], Y
        STA ACCL
        INY 
        LDA [IDX], Y
        STA ACCH
        
        // size = size - size word of block
        DecACCx2();
        
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
            
            LDA FLENGTHL
            if (Z)
            {
                DEC FLENGTHH
            }
            DEC FLENGTHL
        }
        
        // exact clone : make sure reference count is only one
        INY 
        TYA // 1 -> A
        STA [IDX], Y
        
        /*
        PLA
        STA FSOURCEADDRESSL
        PLA
        STA FSOURCEADDRESSH
        */
    }
}
