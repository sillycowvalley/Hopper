unit GC
{
    uses "ZeroPage"
    uses "Memory"
    uses "Types"
    
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

        // add a byte for type and a byte for reference count        
        CLC
        LDA FSIZEL
        ADC # 2
        STA ACCL
        LDA FSIZEH
        ADC # 0
        STA ACCH
        
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
            LDA # 0x0B Diagnostics.die(); // reference count already zero?
        }  
#endif        
        SEC
        SBC # 1
        STA [IDX], Y
        
        // if zero, free
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
                case Types.Variant:
                {
                    // no reference members to clear
                }
                case Types.List:
                {
#ifdef LISTS
                    List.clear();
#endif
                }
                default:
                {
#ifdef CHECKED
                    LDA # 0x0B BRK // unsupported reference type
#endif
                }
            }
            Free.free();
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
        
        // Preserve LCURRENT, LNEXT, FSOURCEADDRESS, IDY
        LDA LCURRENTL       // F10
        PHA
        LDA LCURRENTH       // F11
        PHA
        LDA LNEXTL          // F8
        PHA
        LDA LNEXTH          // F9
        PHA
        LDA FSOURCEADDRESSL // F3
        PHA
        LDA FSOURCEADDRESSH // F4
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
            case Types.Float:
            {
                genericClone();  // uses ACC, FSOURCEADDRESS (F3..F4) , FDESTINATIONADDRESS (F5..F6), FLENGTH (F8..F9)
            }
            case Types.List:
            {
#ifdef LISTS
                List.clone();    // uses ACC, FSIZE (F1..F2), LTYPE (F3), LNEXT (F8..F9), LCURRENT (F10..F11)
#endif
            }
            case Types.Variant:
            {
#ifdef LISTS
                Variant.clone(); // uses FSIZE (F1..F2), FVALUE (F10..F11)
#endif
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
        PLA
        STA FSOURCEADDRESSH // F4
        PLA
        STA FSOURCEADDRESSL // F3
        PLA
        STA LNEXTH          // F9
        PLA
        STA LNEXTL         // F8
        PLA
        STA LCURRENTH      // F11
        PLA
        STA LCURRENTL      // F10
#ifdef CPU_65C02S
        PLY
#else        
        PLA TAY
#endif
    }

    genericClone()
    {
        // IDY -> source, returns cloned object in IDX
        //    uses ACC, FSOURCEADDRESS (F3..F4) , FDESTINATIONADDRESS (F5..F6), FLENGTH (F8..F9)
        //
        
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
    }
    
    CompareEqual()
    {
        // called by List.Contains which uses LCURRENT (F10..F11)
        //   uses:
        //      String.compareEqual() : FSOURCEADDRESS (F3..F4), FDESTINATIONADDRESS (F5..F6), LCOUNT (F14..F15)
        //      Long.compareEqual(), String.compareEqual() : munt Y
        TAY PHA
        loop
        {
            // compare the reference types in IDX and IDY for equality
            //    return X=1 for equal, X=0 for not equal
            LDA IDXL
            CMP IDYL
            if (Z)
            {
                LDA IDXH
                CMP IDYH
                if (Z)
                {
                    // trivial case: IDX and IDY are the same reference
                    LDX # 1
                    break;
                }
            }
            LDY #0
            LDA [IDX], Y
            CMP [IDY], Y
            if (NZ)
            {
                // types are not the same
                LDX # 0
                break;
            }
            
            // types are the same
            TAX
            switch (X)
            {
                case Types.String:
                {
                    String.compareEqual(); // munts Y, sets X, uses FSOURCEADDRESS (F3..F4), FDESTINATIONADDRESS (F5..F6), LCOUNT (F14..F15)
                }
                case Types.Long:
                case Types.Float:
                {
                    Long.compareEqual(); // munts Y, sets X
                }
                case Types.Array:
                {
                    LDA # 0x0A
                    Diagnostics.die();    
                }
                case Types.List:
                {
    #ifdef LISTS
                    LDA # 0x0A
                    Diagnostics.die();
    #endif
                }
                case Types.Variant:
                {
    #ifdef LISTS
                    LDA # 0x0A
                    Diagnostics.die();
    #endif
                }
                default:
                {
                    LDA # 0x0B
                    Diagnostics.die();
                }
            }
            break;
        } // loop
        
        PLA TAY
    }

}

