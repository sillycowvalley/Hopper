unit BASICArray
{
    // Array memory map:
    //   0000 number of elements
    //   xx   type = WORD|INT|BYTE|BIT
    //   0000 first element in array
    //   ..
    //   <nn>  last element in array
    
    const uint aiCount    = 0;
    const uint aiType     = 2;
    const uint aiElements = 3;
    
    const byte[] bitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
    new()
    {
        // element type in FTYPE, number of elements in FSIZE
        //    returns array at IDX and length in bytes in LCOUNT
        LDA FTYPE
        switch (A)
        {
            case BASICType.BIT:
            {
                // size = number of elements / 8 + 1
                LDA # 0
                STA ACARRY
                LDA FSIZEL
                AND # 0x07
                if (NZ)
                {
                    INC ACARRY
                }
                
                LSR FSIZEH
                ROR FSIZEL
                LSR FSIZEH
                ROR FSIZEL
                LSR FSIZEH
                ROR FSIZEL
                CLC
                LDA FSIZEL
                ADC ACARRY
                STA FSIZEL
                if (C)
                {
                    INC FSIZEH
                }
            }
            case BASICType.BYTE:
            {
                // size = number of elements == size
            }
            default:
            {
                // size = number of elements x 2
                ASL FSIZEL
                ROL FSIZEH
            }
        }
        // keep the size in bytes for zero initialization later
        LDA FSIZEL
        STA LCOUNTL
        LDA FSIZEH
        STA LCOUNTH
        
        // add 2 bytes for number of elements field and 1 byte for type of element field
        CLC
        LDA FSIZEL  // LSB
        ADC #3
        STA FSIZEL
        LDA FSIZEH  // MSB
        ADC #0
        STA FSIZEH
        
        // type in A
        // size is in FSIZE
        // return address in IDX
        LDA # Types.Array
        GC.Create();
        
        
        LDY # aiCount
        LDA NEXTL
        STA [IDX], Y
        INY
        LDA NEXTH
        STA [IDX], Y
        INY
        LDA FTYPE
        STA [IDX], Y
        
    }
    
    New()
    {
        Stacks.PopTopNext();  // element type, number of elements
        
        LDA NEXTL
        STA FSIZEL
        LDA NEXTH
        STA FSIZEH
        
        LDA TOPL 
        STA FTYPE
        
        // element type in FTYPE, number of elements in FSIZE
        //    returns array at IDX and length in bytes in LCOUNT
        new();
        
        LDA IDXL
        STA TOPL
        LDA IDXH
        STA TOPH
        
        // zero initialize
        LDY  # aiElements
        loop
        {
            LDA LCOUNTL
            if (Z)
            {
                LDA LCOUNTH
                if (Z)
                {
                    break;
                }
            }
            
            LDA # 0
            STA [IDX], Y
            IncIDX();
            
            LDA LCOUNTL
            if (Z)
            {
                DEC LCOUNTH
            }
            DEC LCOUNTL
        }
        
        LDA # BASICType.ARRAY
        Stacks.PushTop();
    }
    
    GetCount()
    {
        Stacks.PopIDX(); // this
        
        LDY # aiCount
        LDA [IDX], Y
        STA ZP.NEXTL
        INY
        LDA [IDX], Y
        STA ZP.NEXTH
        
        GC.Release();
        
        LDA # Types.UInt
        Stacks.PushNext();
    }
    GetItemType()
    {
        Stacks.PopIDX(); // this
        
        LDY # aiType
        LDA [IDX], Y
        STA ZP.NEXTL
        LDA # 0
        STA ZP.NEXTH
        
        GC.Release();
        
        LDA # Types.Type
        Stacks.PushNext();
    }
    
    getIndexAndMask()
    {
        // element type in FTYPE, index in IDY and this in IDX
        //   returns byte ptr in IDY and mask in ABITMASK
        switch (A)
        {
            case BASICType.BIT:
            {
                // capture the bit
                LDA IDYL
                AND # 0x07
                TAX
                
                // divide offset by 8
                LSR IDYH
                ROR IDYL
                LSR IDYH
                ROR IDYL
                LSR IDYH
                ROR IDYL
            }
            case BASICType.BYTE:
            {
            }
            default:
            {
                // two byte elements : IDY << 1
                ASL IDYL
                ROL IDYH
            }
        }
        
        CLC
        LDA IDXL  // LSB
        ADC IDYL
        STA IDYL
        LDA IDXH  // MSB
        ADC IDYH
        STA IDYH
    }
    GetItem()
    {
        Stacks.PopIDY();  // index
        Stacks.PopIDX();  // this
        
        // index < aiCount?
        LDY # aiCount+1
        LDA ZP.IDYH        // index MSB
        CMP [IDX], Y       // aiCount MSB
        if (Z)
        {
            DEY
            LDA ZP.IDYL    // index LSB
            CMP [IDX], Y   // aiCount LSB
        }
        if (C) // index < aiCount?
        {
            // index >= aiCount
            LDA # 0x02 // array index out of range
            Diagnostics.die();
        }
        LDY # aiType
        LDA [IDX], Y
        STA FTYPE
        
        getIndexAndMask(); // returns index in IDY and bit # in X
                
        LDY # aiElements
        LDA # 0
        STA NEXTH
                      
        LDA FTYPE
        switch (A)
        {
            case BASICType.BIT:
            {
                LDA [IDY], Y           
                AND bitMasks, X
                if (Z)
                {
                    STA NEXTL
                }
                else
                {
                    LDA # 1
                    STA NEXTL   
                }
            }
            case BASICType.BYTE:
            {
                LDA [IDY], Y
                STA NEXTL
            }
            default:
            {
                LDA [IDY], Y
                STA NEXTL
                INY
                LDA [IDY], Y
                STA NEXTH
            }
        }      
        LDA FTYPE
        Stacks.PushNext();  
    }
    SetItem()
    {
        Stacks.PopTop(); // value
        Stacks.PopIDY(); // index
        Stacks.PopIDX(); // this
        
        // index < aiCount?
        LDY # aiCount+1
        LDA ZP.IDYH        // index MSB
        CMP [IDX], Y       // aiCount MSB
        if (Z)
        {
            DEY
            LDA ZP.IDYL    // index LSB
            CMP [IDX], Y   // aiCount LSB
        }
        if (C) // index < aiCount?
        {
            // index >= aiCount
            LDA # 0x02 // array index out of range
            Diagnostics.die();
        }
        
        LDY # aiType
        LDA [IDX], Y
        STA FTYPE
        
        getIndexAndMask(); // returns index in IDY and bit # in X
                
        LDY # aiElements
        LDA # 0
        STA NEXTH
                      
        LDA FTYPE
        switch (A)
        {
            case BASICType.BIT:
            {
                LDA TOPL
                if (NZ)
                {
                    // set the bit
                    LDA bitMasks, X
                    ORA [IDY], Y    
                    STA [IDY], Y
                }
                else
                {
                    // clear the bit
                    LDA bitMasks, X
                    EOR # 0xFF
                    AND [IDY], Y    
                    STA [IDY], Y       
                }
            }
            case BASICType.BYTE:
            {
                LDA TOPL
                STA [IDY], Y
            }
            default:
            {
                LDA TOPL
                STA [IDY], Y
                INY
                LDA TOPH
                STA [IDY], Y
            }
        }      
    }
}
    
