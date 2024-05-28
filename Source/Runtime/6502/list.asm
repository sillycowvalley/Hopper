unit List
{
    uses "GC"
    uses "Variant"
    
    friend GC;
    
    // List memory map:
    //   0000 heap allocator size
    //   19   type = tList
    //   00   GC reference count
    //   0000 current number of items
    //   xx   type of items
    //   xxxx pFirst
    //   xxxx pRecent
    //   xxxx iRecent
    
    const uint lsCount   = 2;
    const uint lsType    = 4;
    const uint lsFirst   = 5;
    const uint lsRecent  = 7;
    const uint lsiRecent = 9;
    
    // ListItem memory map:
    //   0000 heap allocator size
    //   xxxx inline for value types, pData for reference types and when item type is variant
    //   0000 pNext
            
    const uint liData = 0;
    const uint liNext = 2;
    
    
    // Available zero page variables:
    //
    // F1-F2   FSIZE - used (call to gcCreate)
    // F3      LTYPE
    // F4-F5   LLENGTH
    // F6-F7   LPREVIOUS
    // F8-F9   LNEXT        preserved during recursive clone calls
    // F10-F11 LCURRENT     preserved during recursive clone calls      F10-F11   FVALUE (only used in call to createValueVariant)
    // F12-F13 FITEM
    // F14-F15 LCOUNT
    
    popType()
    {
        Stacks.PopA();
        STA LTYPE
    }
    popValueToIDXandTYPE()
    {
        Stacks.PopIDX(); // correctly loads Y for type..
        LDA Address.TypeStackLSB, Y
        STA LTYPE
    }
    zeroFields()
    {
        // IDX -> list : set length, pFirst, pRecent and iRecent to zero
        LDA # 0
        // length
        LDY # lsCount
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        
        INY // skip type
        
        // pFirst
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        // pRecent
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        // iRecent
        STA [IDX], Y
        INY
        STA [IDX], Y
    }
    createList()
    {
        // returns new list as IDX (zeroes fields too)
        LDA # 9
        STA FSIZEL
        LDA # 0
        STA FSIZEH
        
        // type in A
        // size is in fSIZE
        // return address in IDX
        LDA # Types.List
        GC.Create();
        zeroFields();
    }
    loadLengthFromIDX()
    {
        // IDX -> length, load LLENGTH
        LDY # lsCount
        LDA [IDX], Y
        STA LLENGTHL
        INY
        LDA [IDX], Y
        STA LLENGTHH
    }
    
    appendReferenceType()
    {
        // item is reference type
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        // IDX -> IDX   variantItem = Variant_Clone(variantOriginal)) and release original
        
        // type is in A
        // reference type to clone is at IDY, resulting clone in IDX
        LDA IDXL
        STA IDYL
        LDA IDXH
        STA IDYH
        GC.Clone();
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        LDA IDYL
        STA IDXL
        LDA IDYH
        STA IDXH
        GC.Release(); // release original
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        PLA
        STA IDYH
        PLA
        STA IDYL
        // list is in IDY, item to append is in IDX
        
        // pData value is in IDX
        // returns new listItem in FITEM
        itemCreate();
    }
        
    appendItem()
    {
        LDA # 0
        STA LNEXTL
        STA LNEXTL
        STA LCURRENTL
        STA LCURRENTH
        STA LPREVIOUSL
        STA LPREVIOUSH
        
        // append it to the end of the list
        //  List.Append(list, variantItem);
        
        LDA # 0x0A
        Serial.WriteChar();
        LDA IDYH
        Serial.HexOut();
        LDA IDYL
        Serial.HexOut();
        
        // pRecent
        LDY # lsRecent
        LDA [IDY], Y
        STA LNEXTL
        INY
        LDA [IDY], Y
        STA LNEXTH
        ORA LNEXTL
        if (Z)
        {
            // pFirst
            LDY # lsFirst
            LDA [IDY], Y
            STA LNEXTL
            INY
            LDA [IDY], Y
            STA LNEXTH
        }
        
        LDA LNEXTH
        Serial.HexOut();
        LDA LNEXTL
        Serial.HexOut();
        
        LDA LNEXTL
        ORA LNEXTH
        if (Z)
        {
            // NEXT == 0, special case for first item
        
            // pFirst = IDX
            LDY # lsFirst
            LDA FITEML
            STA [IDY], Y
            INY
            LDA FITEMH
            STA [IDY], Y
        }
        else
        {
           loop
           {
                LDA LCURRENTL
                STA LPREVIOUSL
                LDA LCURRENTH
                STA LPREVIOUSH
                LDA LNEXTL
                STA LCURRENTL
                LDA LNEXTH
                STA LCURRENTH
                
                LDY # liNext
                LDA [LCURRENT], Y
                STA LNEXTL
                INY
                LDA [LCURRENT], Y
                STA LNEXTH
                
                // NEXT == 0?
                LDA LNEXTL
                if (NZ) { continue; }
                
                LDA LNEXTH
                if (Z) { break; }
            }
            // NEXT == 0 
            // CURRENT.pData = fITEM
            LDY # liData
            LDA FITEML
            STA [LCURRENT], Y
            INY
            LDA FITEMH
            STA [LCURRENT], Y
            
            // PREVIOUS.pNext = CURRENT
            LDY # liNext
            LDA LCURRENTL
            STA [LPREVIOUS], Y
            INY
            LDA LCURRENTH
            STA [LPREVIOUS], Y
        }
        
        // pRecent = IDX  
        LDY # lsRecent
        LDA FITEML
        STA [IDY], Y
        INY
        LDA FITEMH
        STA [IDY], Y
        
        // previous length
        LDY # lsCount
        LDA [IDY], Y
        STA LLENGTHL
        INY
        LDA [IDY], Y
        STA LLENGTHH
        
        // iRecent = previous length
        LDY # lsiRecent
        LDA LLENGTHL
        STA [IDY], Y
        INY
        LDA LLENGTHH
        STA [IDY], Y
        
        // length: increment the item count in the list
        LDY # lsCount
        LDA LLENGTHL
        INC
        STA [IDY], Y
        if (Z)
        {
            INY
            LDA LLENGTHH
            INC
            STA [IDY], Y
        }
    }
          
    itemCreate()
    {
        // pData value is in IDX
        // uses fSIZE
        // sets pNext = 0
        // returns listItem in FITEM
        
        LDA IDYH
        PHA
        LDA IDYL
        PHA
        
        LDA IDXH
        PHA
        LDA IDXL
        PHA
        
        LDA # 4
        STA ACCL
        LDA # 0
        STA ACCH
        // size is in ACC
        // return address in IDX
        Allocate.allocate();
        
        LDA IDXH
        STA FITEMH
        LDA IDXL
        STA FITEML
        
        LDY # liData
        PLA            // IDXL
        STA [FITEM], Y // pData LSB
        INY
        PLA            // IDXH 
        STA [FITEM], Y // pData MSB
        
        LDY # liNext
        LDA # 0
        STA [FITEM], Y // pNext LSB
        INY
        STA [FITEM], Y // pNext MSB
        
        PLA
        STA IDYL
        PLA
        STA IDYH
    }
    createValueItem()
    {
        // type in A, value in FVALUEL, resulting item in listItem in FITEM
        LDA FVALUEL
        STA IDXL
        LDA FVALUEH
        STA IDXH
        
        // pData value is in IDX
        //   uses FSIZE
        // sets pNext = 0
        itemCreate();
        // returns listItem in FITEM
    }
    moveToItem()
    {
        // IDX has list, IDY is index of interest, returns lCURRENT (and updates iRecent and pRecent)
        //   used by Insert, Remove, GetItem, SetItem
        // IDX is reference of list
        // IDY is index of interest
        // LCURRENT is reference item on return
        // iRecent and pRecent are updated
        
        // pFirst
        LDY # lsFirst
        LDA [IDX], Y
        STA LCURRENTL
        INY
        LDA [IDX], Y
        STA LCURRENTH
        
#ifdef CHECKED
        LDA LCURRENTL
        ORA LCURRENTH
        if (Z)
        {
            // pFirst == 0 : empty list
            
            // list index out of range
            LDA # 0x01
            Diagnostics.die();
        }
#endif
        
        // LCOUNT = iRecent
        LDY # lsiRecent
        LDA [IDX], Y
        STA LCOUNTL
        INY
        LDA [IDX], Y
        STA LCOUNTH
        
        loop
        {
            // iRecent == 0?
            LDA LCOUNTL
            if (Z)
            {
                LDA LCOUNTH
                if (Z)
                {
                    LDA # 0
                    STA LCOUNTL
                    STA LCOUNTH
                    break; // moveToItem : NotRecent
                }
            }
            
            // iRecent <= index?
            //   lCOUNT <= IDY?
            LDA LCOUNTH
            CMP IDYH
            if (Z)
            {
                LDA LCOUNTL
                CMP IDYL
            } 
    
            // http://6502.org/tutorials/compare_instructions.html
            if (NZ)  // LCOUNT == IDY (not >)
            {
                if (C) // LCOUNT <  IDY (not >)
                {
                    LDA # 0
                    STA LCOUNTL
                    STA LCOUNTH
                    break; // moveToItem : NotRecent // lCOUNT > IDY
                }
            }
            
            // iRecent <= index
            
            LDY # lsiRecent
            LDA [IDX], Y
            STA LCURRENTL
            INY
            LDA [IDX], Y
            STA LCURRENTH
        
#ifdef CHECKED
            LDA LCURRENTL
            if (Z)
            {
                LDA LCURRENTH
                if (Z)
                {
                    // pFirst == 0 : empty list
                     
                    // list index out of range
                    LDA # 0x01 
                    Diagnostics.die();
                }
            }
#endif
            break;
        } // loop
        
        loop
        {
            LDA LCOUNTL
            CMP IDYL
            if (Z)
            {
                LDA LCOUNTH
                CMP IDYH
                if (Z)
                {
                    // lCOUNT == IDY
                    break;
                }
            }
        
            // CURRENT = CURRENT.pNext
            LDY # liNext
            LDA [LCURRENT], Y
            TAX
            INY
            LDA [LCURRENT], Y
            STA LCURRENTH
            TXA
            STA LCURRENTL
        
#ifdef CHECKED
            LDA LCURRENTL
            ORA LCURRENTH
            if (Z)
            {
                // list index out of range
                LDA # 0x01 
                Diagnostics.die();
            }
#endif
        
            // LCOUNT++ 
            INC LCOUNTL
            if (NZ) { continue; }
            INC LCOUNTH
        } // loop
        
        // ListItem memory map:
        //   0000 heap allocator size
        //   xxxx variant box for value types, pData for reference types
        //   0000 pNext
        
        // update iRecent and pRecent
        // pRecent = lCURRENT
        LDY # lsRecent
        LDA LCURRENTL
        STA [IDX], Y
        INY
        LDA LCURRENTH
        STA [IDX], Y
        
        // iRecent = index
        LDY # lsiRecent
        LDA IDYL
        STA [IDX], Y
        INY
        LDA IDYH
        STA [IDX], Y
    }
    
    clear()
    {
        LDA # 0x0A
        Diagnostics.die();
    }
    
    clone()
    {
        LDA # 0x0A
        Diagnostics.die();
        return;
    }
    
    New()
    {
        popType();    // LTYPE
        createList(); // returns new list as IDX (zeroes fields too)
        
        LDY # lsType
        LDA LTYPE
        STA [IDX], Y
        
        LDA # Types.List
        PushIDX(); 
    }
    
    // uint Count { get system; }
    CountGet()
    {
        Stacks.PopIDX();
        loadLengthFromIDX();
        
        GC.Release();
        
        LDA LLENGTHL
        STA TOPL  
        LDA LLENGTHH
        STA TOPH
        
        LDA # Types.UInt
        Stacks.PushTop(); // type is in A
    }
    
    // Append(<V> this, V value) system;
    Append()
    {
        popValueToIDXandTYPE(); // value and type
        Stacks.PopIDY();        // list
        
        // list for GC.Release
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        
        //list item type
        LDY # lsType
        LDA [IDY], Y
        IsReferenceType();
        if (C)
        {
            // reference type
            appendReferenceType();
        }
        else
        {
            // value type
            // save the value
            LDA IDXH
            STA FVALUEH
            LDA IDXL
            STA FVALUEL
            
            // list item type
            LDY # lsType
            LDA [IDY], Y
            createValueItem();
        }
        appendItem();
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        GC.Release();
    }
    
    GetItem()
    {
        // V GetItem(<V> this, uint index) system;
        
        Stacks.PopIDY(); // pop uint argument -> IDY
        Stacks.PopIDX(); // this -> IDX
        
        // for GC.Release();
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
        
        // IDX is reference of list
        // IDY is index of interest
        moveToItem(); // -> LCURRENT
        
        LDY # liData
        LDA [LCURRENT], Y
        STA IDYL
        INY
        LDA [LCURRENT], Y
        STA IDYH
        
        LDA LTYPE
        IsReferenceType();
        if (C)
        {
            // reference type from pData
            LDY # 0
            LDA [IDY], Y
            CMP # Types.Variant
            if (Z)
            {
                // variant which implies value type in variant
                LDY # 3 // ivValue
                LDA [IDY], Y
                STA TOPL
                INY
                LDA [IDY], Y
                STA TOPH
                LDY # 0
                LDA [TOP], Y // type
                STA TOPT
            }
            else
            {
                LDY # 0
                LDA [IDY], Y // type
                
                // type is in A
                // reference type to clone is at IDY, resulting clone in IDX
                GC.Clone();
                
                LDA IDXL
                STA TOPL
                LDA IDXH
                STA TOPH
                LDY # 0
                LDA [TOP], Y // type
                STA TOPT
            }
        }
        else
        {
            LDA IDYL
            STA TOPL
            LDA IDYH
            STA TOPH
            LDA LTYPE
            STA TOPT
        }
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        GC.Release(); // we popped 'this', decrease reference count
        
        Stacks.PushTop();
    }
    
    Clear()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
        
    Insert()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
    
    GetItemAsVariant()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
    
    SetItem()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
    
    Remove()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
    
    Contains()
    {
        LDA # 0x0A // LibCall not Implemented!
        Diagnostics.die();
        return;
    }
}
