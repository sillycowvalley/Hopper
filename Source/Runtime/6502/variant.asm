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
        Stacks.PopNext();  
        LDA TOPL
        STA LTYPE
        Stacks.PopTop(); // TOP = value
        
        // value in TOP, vtype in LTYPE
        // Returns address in IDX
        CreateValueVariant();
        
        LDA IDXL
        STA TOPL
        LDA IDXH
        STA TOPH
        LDA # Types.Variant
        STA TOPT
        PushTop();
    }
    
    CreateValueVariant()
    {
        // value in TOP, vtype in LTYPE
        // Returns address in IDX
    
    #ifdef CHECKED
        // Check if vtype is a reference type
        LDA LTYPE
        IsReferenceType();
        if (C) 
        {
            LDA # 0x0B
            BRK
        }
    #endif
    
        // Allocate memory for the Variant (3 bytes)
        LDA # 3
        STA ZP.FSIZEL
        LDA # 0
        STA ZP.FSIZEH
        
        LDA # Types.Variant
        // type in A
        // size is in FSIZE
        // return address in IDX
        GC.Create();
    
        // Store vtype in ivType
        LDY # ivType
        LDA LTYPE
        STA [IDX], Y
    
        // Store value in ivValue
        LDY # ivValue
        LDA TOPL
        STA [IDX], Y
        INY
        LDA TOPH
        STA [IDX], Y
    }
    clone()
    {
        // variant type to clone is at IDY, resulting clone in IDX
        LDY #0
        LDA [IDY], Y
        STA LTYPE
        INY
        LDA [IDY], Y
        STA TOPL
        INY
        LDA [IDY], Y
        STA TOPH
        CreateValueVariant();
    }
#ifdef CHECKED
    clear()
    {
        // address in IDX
        LDY # ivType
        LDA [IDX], Y
        LDA LTYPE

        // Check if vtype is a reference type
        IsReferenceType();
        if (C) 
        {
            LDA # 0x0B // variant should never contain a reference type
            BRK
        }
    }
#endif    
    getValue()
    {
        // pData in TOP
        // Returns value in IDX and updates LTYPE
        
        // Get the type of the item in the variant
        LDY # ivType
        LDA [TOP], Y
        STA LITYPE
        
        // Get the value from the variant
        LDY # ivValue
        LDA [TOP], Y
        STA FITEML
        INY
        LDA [TOP], Y
        STA FITEMH
        
#ifdef CHECKED        
        // Check if the value is a reference type
        LDA LITYPE
        IsReferenceType();
        if (C)
        {
            LDA # 0x0B // variant should never contain a reference type
            BRK
        }
#endif
        
        // If it's a value type, return the data directly
        LDA FITEML
        STA IDXL
        LDA FITEMH
        STA IDXH
    }
    
}

