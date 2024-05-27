unit Variant
{
    uses "Types"
    
    friend List;
    
    // Variant memory map:
    //   0000 heap allocator size
    //   14   type = tVariant
    //   00   reference count
    //   xx   actual type for item
    //   xxxx data for value types, pData for reference types
    
    const uint ivType  = 2;
    const uint ivValue = 3; 
    
    CreateValueVariant()
    {
        // value in TOP, vtype in LTYPE
        // Returns address in IDX
    
    #ifdef CHECKED
        // Check if vtype is a reference type
        LDA LTYPE
        IsReferenceType();
        if (NZ) 
        {
            LDA # 0x0B
            BRK
        }
    #endif
    
        // Allocate memory for the Variant (3 bytes)
        LDA #3
        LDA # Types.Variant
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
        
        // Check if the value is a reference type
        LDA LITYPE
        IsReferenceType();
        if (C)
        {
            // Clone the reference type data
            LDA FITEML
            STA IDYL
            LDA FITEMH
            STA IDYH
            
            // type is in A
            LDA LITYPE
            // reference type to clone is at IDY, resulting clone in IDX
            GC.Clone();
            
            // Update the type to the cloned reference type
            LDY #0
            LDA [IDX], Y
            STA LITYPE
            return;
        }
        
        // If it's a value type, return the data directly
        LDA FITEML
        STA IDXL
        LDA FITEMH
        STA IDXH
    }
    
    
}

