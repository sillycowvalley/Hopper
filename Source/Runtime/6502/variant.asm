unit Variant
{
    uses "Types"
    uses "GC"
    
    friend List, GC;
    
    // Variant memory map:
    //   0000 heap allocator size
    //   14   type = tVariant
    //   00   reference count
    //   xx   actual type for item
    //   xxxx data for value types, pData for reference types
    
    const uint ivType  = 2;
    const uint ivValue = 3; 
    
    Box()
    {
        Stacks.PopNext(); // NEXT = type 
        Stacks.PopTop();  // TOP = value
        
#ifdef CHECKED
        // Check if vtype is a reference type
        LDA NEXTL
        IsReferenceType();
        if (C) 
        {
            // don't ever box a reference type
            LDA # 0x0B
            Diagnostics.die();
            return;
        }
#endif
        LDA TOPL
        STA FVALUEL
        LDA TOPH
        STA FVALUEH
        
        LDA NEXTL // type
        // type in A, value in FVALUE
        //     return tVariant in IDX
        createValueVariant();
        
        LDA IDXL
        STA TOPL
        LDA IDXH
        STA TOPH
        LDA  # Types.Variant
        STA TOPT
        PushTop();
    }
    
    UnBox()
    {
        Stacks.PopIDX();
        
        LDY # ivType
        LDA [IDX], Y
        STA FTYPE
        
        LDY # ivValue
        LDA [IDX], Y
        STA TOPL
        INY
        LDA [IDX], Y
        STA TOPH
        
        GC.Release();
        
        LDA FTYPE
        Stacks.PushTop();
    }
    
    createValueVariant()
    {
        // type in A, value in FVALUE
        //     return tVariant in IDX
        // uses FSIZE
        
    
    #ifdef CHECKED
        // Check if A is a reference type
        IsReferenceType();
        if (C) 
        {
            LDA # 0x0B
            Diagnostics.die();
            return;
        }
    #endif
    
        PHA // type
        
        LDA # 3
        STA ZP.FSIZEL
        LDA # 0
        STA ZP.FSIZEH
        
        LDA # Types.Variant
        // type in A
        // size is in FSIZE
        // return address in IDX
        GC.Create();
    
        PLA //  type
        LDY # ivType
        STA [IDX], Y
    
        //variantItem = Variant_Box(type, value);
        INY
        LDA FVALUEL
        STA [IDX], Y
        INY
        LDA FVALUEH
        STA [IDX], Y
    }
    clone()
    {
        // variant type to clone is at IDY, 
        //    resulting clone in IDX
        //    uses FSIZE (F1..F2), FVALUE (F10..F11)
        LDY #4
        LDA [IDY], Y
        STA FVALUEH
        DEY
        LDA [IDY], Y
        STA FVALUEL
        DEY
        LDA [IDY], Y // type
        createValueVariant();
    }
}

