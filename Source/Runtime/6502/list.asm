unit List
{
    uses "GC"
    uses "Variant"
    uses "Diagnostics"
    
    friend GC, Diagnostics;
    
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
    
    dump()
    {
        PHA
        PHY
        
        // List in IDX
        LDA # ' '
        Serial.WriteChar();
        
        // count
        LDY # (lsCount+2)
        INY
        LDA [IDX], Y
        Serial.HexOut();
        DEY
        LDA [IDX], Y
        TAX
        Serial.HexOut();
        
        LDA # ':'
        Serial.WriteChar();
        
        LDY # (lsType+2)
        LDA [IDX], Y
        Serial.HexOut();
        
        LDA FITEML
        PHA
        LDA FITEMH
        PHA
        
        LDY # (lsFirst+2)
        LDA [IDX], Y
        STA FITEML
        INY
        LDA [IDX], Y
        STA FITEMH
        
        loop
        {
            CPX #0
            if (Z) { break; }
            PHX
            
            LDA # ' '
            Serial.WriteChar();
            
            LDA FITEMH
            Serial.HexOut();
            LDA FITEML
            Serial.HexOut();
            
            LDA IDXL
            PHA
            LDA IDXH
            PHA
            
            SEC
            LDA FITEML
            SBC # 2
            STA IDXL
            LDA FITEMH
            SBC # 0
            STA IDXH
            Diagnostics.validateHeapIDX();
            
            PLA
            STA IDXH
            PLA
            STA IDXL
            
            LDY # liNext
            LDA [FITEM], Y
            TAX
            INY
            LDA [FITEM], Y
            STA FITEMH
            STX FITEML
            
            PLX
            DEX
        }
        
        PLA
        STA FITEMH
        PLA
        LDA FITEML
        
        PLY
        PLA
    }
    
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
        
        LDY # 0
        LDA [IDY], Y // type in A
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
        // list is IDY, item is FITEM
        LDA # 0
        STA LNEXTL
        STA LNEXTL
        STA LCURRENTL
        STA LCURRENTH
        STA LPREVIOUSL
        STA LPREVIOUSH
        
        // append it to the end of the list
        //  List.Append(list, variantItem);
        
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
            // NEXT == 0:
            // CURRENT.pNext = FITEM
            LDY # liNext
            LDA FITEML
            STA [LCURRENT], Y
            INY
            LDA FITEMH
            STA [LCURRENT], Y
            
            /*            
            // PREVIOUS.pNext = CURRENT
            LDY # liNext
            LDA LCURRENTL
            STA [LPREVIOUS], Y
            INY
            LDA LCURRENTH
            STA [LPREVIOUS], Y
            */
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
#ifdef CPU_65C02S
        INC
#else
        CLC
        ADC # 1
#endif
        STA [IDY], Y
        if (Z)
        {
            INY
            LDA LLENGTHH
#ifdef CPU_65C02S
            INC
#else
            CLC
            ADC # 1
#endif
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
        Allocate.Allocate();
        
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
    releaseItemIDY()
    {
        // IDY -> listItem : frees the listItem (but not the contents of pData even if reference type)
        //   used by Remove and Clear
        
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        LDA IDYL
        STA IDXL
        LDA IDYH
        STA IDXH
        Free.Free();
        
        PLA
        STA IDXH
        PLA
        STA IDXL
    }
    
    releaseItemValue()
    {
        // preserves LCURRENT, LTYPE
        // type in LTYPE, reference in IDY (works for listItem.pData)
  
        LDA LTYPE
        IsReferenceType();
        if (C)
        {
            LDA LCURRENTL
            PHA
            LDA LCURRENTH
            PHA
            
            LDA IDYL
            STA IDXL
            LDA IDYH
            STA IDXH
            
            GC.Release(); // address in IDX
            
            PLA
            STA LCURRENTH
            PLA
            STA LCURRENTL
        }
    }
    rangeCheckTOP()
    {
        // list -> IDY, index -> TOP
        LDY # lsCount
        LDA [IDY], Y
        STA LLENGTHL
        INY
        LDA [IDY], Y
        STA LLENGTHH
        
        // TOP <= lLENGTH?
        LDA TOPH
        CMP LLENGTHH
        if (Z)
        {
            LDA TOPL
            CMP LLENGTHL
        }

        // http://6502.org/tutorials/compare_instructions.html
        if (NZ) // TOP != LLENGTH
        {
            if (C) // TOP > LLENGTH
           {
                // list index out of range
                LDA # 0x01 
                Diagnostics.die();
            }
        }
        // TOP <= LLENGTH
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
        // IDX has list, IDY is index of interest, returns LCURRENT (and updates iRecent and pRecent)
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
            ORA LCOUNTH
            if (Z)
            {
                break; // moveToItem : NotRecent
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
            
            LDY # lsRecent
            LDA [IDX], Y
            STA LCURRENTL
            INY
            LDA [IDX], Y
            STA LCURRENTH
        
#ifdef CHECKED
            ORA LCURRENTL
            if (Z)
            {
                // pFirst == 0 : empty list
                 
                // list index out of range
                LDA # 0x01 
                Diagnostics.die();
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
                    // LCOUNT == IDY
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
        // IDX -> list to clear : disposes items in list (but not list itself)
        
        // called from syscallListClear and gcRelease
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        LDA LNEXTL
        PHA
        LDA LNEXTH
        PHA
        
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
        
        // pFirst
        LDY # lsFirst
        LDA [IDX], Y
        STA LNEXTL
        INY
        LDA [IDX], Y
        STA LNEXTH
        
        loop
        {
            LDA LNEXTL
            ORA LNEXTH
            if (Z) { break; }
            
            // IDY = LNEXT.pData
            LDY # liData
            LDA [LNEXT], Y
            STA IDYL
            INY
            LDA [LNEXT], Y
            INY
            STA IDYH
            
            // release the data memory
            releaseItemValue(); // release pData
            
            LDA LNEXTL  
            STA IDYL
            LDA LNEXTH
            STA IDYH
            
            // LNEXT = LNEXT.pNext
            LDY # liNext
            LDA [LNEXT], Y
            TAX
            INY
            LDA [LNEXT], Y
            INY
            STA LNEXTH
            STX LNEXTL
            
            // release the listitem memory
            releaseItemIDY(); // release IDY (previous pNext)
            
        } // loop        
        
        PLA
        STA LNEXTH
        PLA
        STA LNEXTL
        PLA
        STA IDYH
        PLA
        STA IDYL
        PLA
        STA IDXH
        PLA
        STA IDXL
    }
    
    clone()
    {
        // IDY -> sourceList, returns cloned list in IDX (LCURRENT, LNEXT and IDY preserved in recursive calls)
        //   called from GC.Clone()
        //   uses FSIZE (F1..F2), LTYPE (F3), LNEXT (F8..F9), LCURRENT (F10..F11)
        
        createList(); // IDX
        
        // preserve for return value
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        // number of items
        LDY # lsCount
        LDA [IDY], Y
        STA [IDX], Y
        INY
        LDA [IDY], Y
        STA [IDX], Y
        
        // item type
        LDY # lsType
        LDA [IDY], Y
        STA [IDX], Y
        STA LTYPE
        
        // CURRENT : location to put the pointer to new list item (tList.pFirst)
        CLC
        LDA IDXL
        ADC # lsFirst
        STA LCURRENTL
        LDA IDXH
        ADC # 0
        STA LCURRENTH
        
        // NEXT : list item to clone (tList.pFirst)
        LDY # lsFirst
        LDA [IDY], Y
        STA LNEXTL
        INY
        LDA [IDY], Y
        STA LNEXTH
        
        loop
        {
            LDA LNEXTL
            ORA LNEXTH
            if (Z)
            {
                break;
            }
        
            LDA # 4
            STA ACCL
            LDA # 0
            STA ACCH
            // size is in ACC
            // return address in IDX
            Allocate.Allocate();
            
            LDY # 0
            LDA IDXL
            STA [LCURRENT], Y
            INY
            LDA IDXH
            STA [LCURRENT], Y
            
            // CURRENT : location to put the pointer to new list item (tListItem.pNext)
            CLC
            LDA IDXL
            ADC # liNext
            STA LCURRENTL
            LDA IDXH
            ADC # 0
            STA LCURRENTH
            
            // active listItem
            LDA IDXL
            PHA
            LDA IDXH
            PHA
            
            // pData from existing listItem
            LDY # liData
            LDA [LNEXT], Y
            STA IDYL
            INY
            LDA [LNEXT], Y
            STA IDYH
            
            LDA LTYPE
            IsReferenceType();
            if (NC)
            {
                // item is value type
                LDY # liData
                LDA IDYL
                STA [IDX], Y
                INY
                LDA IDYH
                STA [IDX], Y
                
                // active listItem
                PLA
                STA IDYH
                PLA
                STA IDYL
            }
            else
            {
                // item is reference type
                LDY # 0
                LDA [IDY], Y
                // type is in A
                // reference type to clone is at IDY
                //   (preserves LCURRENT, LNEXT and IDY for recursive calls)
                GC.Clone();
            
                // active listItem
                PLA
                STA IDYH
                PLA
                STA IDYL
                                
                // pData
                LDY # liData
                LDA IDXL
                STA [IDY], Y
                INY
                LDA IDXH
                STA [IDY], Y
            }
            
            // NEXT : list item to clone
            LDY # liNext
            LDA [LNEXT], Y
            TAX
            INY
            LDA [LNEXT], Y
            STA LNEXTH
            STX LNEXTL
        } // loop
        PLA
        STA IDXH
        PLA
        STA IDXL
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
        
        LDA IDYH
        STA IDXH
        LDA IDYL
        STA IDXL
        
        GC.Release();
    }
    
    getItem()
    {
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
                LDY # Variant.ivValue
                LDA [IDY], Y
                STA TOPL
                INY
                LDA [IDY], Y
                STA TOPH
                LDY # Variant.ivType
                LDA [IDY], Y
                STA LTYPE
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
                STA LTYPE
            }
        }
        else
        {
            LDA IDYL
            STA TOPL
            LDA IDYH
            STA TOPH
            LDA LTYPE
        }
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        LDA LTYPE
        PHA
        
        GC.Release(); // we popped 'this', decrease reference count
        
        PLA
        STA LTYPE
    }
    GetItem()
    {
        // V GetItem(<V> this, uint index) system;
        
        //  returns item in TOP, type in LTYPE
        getItem();
        LDA LTYPE
        Stacks.PushTop();
    }
    GetItemAsVariant()
    {
        //  returns item in TOP, type in LTYPE
        getItem();
        
        LDA LTYPE
        IsReferenceType();
        if (NC)
        {
            //value type
            
            // type in A, value in FVALUE
            //     return tVariant in IDX
            // uses FSIZE
            
            LDA TOPL
            STA FVALUEL
            LDA TOPH
            STA FVALUEH
        
            LDA LTYPE // type in A, value in FVALUE
            Variant.createValueVariant();
            
            LDA # Types.Variant
            Stacks.PushIDX();
        }
        else
        {
            // type is in A
            Stacks.PushTop();
        }
    }
    
    Clear()
    {
        // Clear(<V> this) system;
        Stacks.PopIDX(); // this -> IDX
        clear();
        zeroFields();
        GC.Release();
    }
    SetItem()
    {
        // SetItem(<V> this, uint index, V value) system;
        
        Stacks.PopTop();        // value -> TOP and LTYPE
        LDA TOPT
        STA LTYPE
        Stacks.PopIDY();        // UInt index -> IDY
        Stacks.PopIDX();        // this -> IDX
        
        LDA LTYPE
        STA NEXTL // type of value
        
        LDY # lsType
        LDA [IDX], Y
        STA NEXTH // type of list items
        
        // IDX is reference of list
        // IDY is index of interest
        moveToItem(); // -> LCURRENT
        
        LDA NEXTH // type of list items
        IsReferenceType();
        if (NC)
        {
            // value types - just reset in item, no need to release
            LDY # liData
            LDA TOPL
            STA [LCURRENT], Y
            INY
            LDA TOPH
            STA [LCURRENT], Y
        }
        else
        {
            LDA IDXL
            PHA
            LDA IDXH
            PHA
            
            LDA LTYPE
            PHA
        
            // LCURRENT.pData -> IDY
            LDY # liData
            LDA [LCURRENT], Y
            STA IDYL
            INY
            LDA [LCURRENT], Y
            STA IDYH
            
            LDY # 0
            LDA [IDY], Y
            STA LTYPE
            
            // type in LTYPE, reference in IDY
            //   munts IDX, IDY
            releaseItemValue(); // release pData 
            
            // clone value and put it into LCURRENT/NEXT
            LDA TOPL
            STA IDYL
            LDA TOPH
            STA IDYH
            
            PLA // LTYPE
            // type is in A
            // reference type to clone is at IDY, resulting clone in IDX
            GC.Clone(); // cloneIDY
            
            /*
            LDY # 2
            LDA IDXL
            STA [NEXT], Y
            INY
            LDA IDXH
            STA [NEXT], Y
            */
            
        
            // IDX -> LCURRENT.pData
            LDY # liData
            LDA IDXL
            STA [LCURRENT], Y
            INY
            LDA IDXH
            STA [LCURRENT], Y
            
            LDA TOPL
            STA IDXL
            LDA TOPH
            STA IDXH
            GC.Release(); // we consumed 'value', decrease reference count
            
            PLA
            STA IDXH
            PLA
            STA IDXL
        }
        
        GC.Release(); // we popped 'this' (IDX), decrease reference count
    }
    
    Remove()
    {
        // Remove(<V> this, uint index) system;
        Stacks.PopIDY(); // pop uint argument -> IDY
        Stacks.PopIDX(); // this -> IDX
        
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
        
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
#ifdef CHECKED
        LDY # lsCount
        LDA [IDX], Y
        STA LLENGTHL
        INY
        LDA [IDX], Y
        STA LLENGTHH
        
        // IDY < LLENGTH?
        LDA IDYH
        CMP LLENGTHH
        if (Z)
        {
            LDA IDYL
            CMP LLENGTHL
        }

        // http://6502.org/tutorials/compare_instructions.html
        if (C)
        {
            // IDY >= lLENGTH
        
            // list index out of range
            LDA # 0x01 
            Diagnostics.die();
        }
#endif
        
        // IDY < lLENGTH
        
        LDA IDYL
        ORA IDYH
        if (Z)
        {
            // IDY is zero
            
            // 'fake' LCURRENT so that listItem.pNext offset works below
            CLC
            LDA IDXL
            ADC # (lsFirst-liNext)
            STA LCURRENTL
            LDA IDXH
            ADC # 0
            STA IDXH
            STA LCURRENTH
        }
        else
        {
            DecIDY();
            // IDX is reference of list
            // IDY+1 is index of interest
            
            moveToItem(); // -> LCURRENT
        }
        
        // LNEXT  = LCURRENT.pNext
        LDY # liNext
        LDA [LCURRENT], Y
        STA LNEXTL
        INY
        LDA [LCURRENT], Y
        STA LNEXTH
        
        // IDY  = LNEXT.pData
        LDY # liData
        LDA [LNEXT], Y
        STA IDYL
        INY
        LDA [LNEXT], Y
        STA IDYH
        
        // reference in IDY, type in LTYPE (works for tListItem.pData)
        //    preserves LCURRENT, LTYPE
        releaseItemValue(); // release pData
        
        // IDY  = LCURRENT.pNext
        LDY # liNext
        LDA [LCURRENT], Y
        STA IDYL
        INY
        LDA [LCURRENT], Y
        STA IDYH
        
        // LNEXT  = IDY.pNext
        LDY # liNext
        LDA [IDY], Y
        STA LNEXTL
        INY
        LDA [IDY], Y
        STA LNEXTH
        
        // LCURRENT.pNext = lNEXT
        LDY # liNext
        LDA LNEXTL
        STA [LCURRENT], Y
        INY
        LDA LNEXTH
        STA [LCURRENT], Y
        
        releaseItemIDY(); // release IDY (previous LCURRENT)
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        // count  
        LDY # lsCount
        SEC
        LDA [IDX], Y
        SBC # 1
        STA [IDX], Y
        INY
        LDA [IDX], Y
        SBC # 0
        STA [IDX], Y
        
        // pRecent
        LDY # lsRecent
        LDA # 0
        STA [IDX], Y
        INY
        STA [IDX], Y
        
        // iRecent
        LDY # lsiRecent
        LDA # 0
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        
        GC.Release(); // we popped 'this', decrease reference count
    }
        
    Insert()
    {
        // Insert(<V> this, uint index, V value) system;
        popValueToIDXandTYPE(); // value to IDX, LTYPE
        Stacks.PopTop();        // index in TOP
        Stacks.PopIDY();        // 'this' list in IDY
        
        LDY # lsType
        LDA [IDY], Y
        STA LTYPE // type of list items
        
#ifdef CHECKED
        rangeCheckTOP(); // list -> IDY, index -> TOP
#endif
        
        LDA TOPL
        ORA TOPH
        if (Z)
        {
            // IDY is zero
            
            // 'fake' LCURRENT so that #4 offset works below
            CLC
            LDA IDYL
            ADC # (lsFirst-liNext)
            STA LCURRENTL
            LDA IDYH
            ADC # 0
            STA LCURRENTH
        }
        else
        {
            LDA IDXH
            PHA
            LDA IDXL
            PHA
            LDA IDYH
            STA IDXH
            PHA
            LDA IDYL
            STA IDXL
            PHA
            LDA TOPH
            STA IDYH
            LDA TOPL
            STA IDYL
            DecIDY();
            
            // IDX is reference of list
            // IDY is index of item before
            moveToItem(); // LCURRENT
            
            PLA
            STA IDYL
            PLA
            STA IDYH
            PLA
            STA IDXL
            PLA
            STA IDXH
        }
        
        // save 'LCURRENT'
        LDA LCURRENTH
        PHA
        LDA LCURRENTL
        PHA
        
        LDA LTYPE
        IsReferenceType();
        if (NC)
        {
            // item is value type
            
            // save the value
            LDA IDXH
            STA FVALUEH
            LDA IDXL
            STA FVALUEL
            
            // get the list item type
            LDA LTYPE
            // type in A, value in FVALUEL, resulting item in tListItem in FITEM
            createValueItem();
        }
        else
        {
            // item is reference type
            
            // value to IDX
            // 'this' list in IDY
            LDA IDYL
            PHA
            LDA IDYH
            PHA
            
            // value: IDX -> IDY
            LDA IDXL
            STA IDYL
            LDA IDXH
            STA IDYH
            
            // clone value, and release original
            
            // type is in A
            LDY # 0
            LDA [IDY], Y
            // reference type to clone is at IDY, resulting clone in IDX
            //    (preserves LCURRENT, LNEXT and IDY for recursive calls)
            GC.Clone();
            
            LDA IDYL
            PHA
            LDA IDYH
            PHA
                 
            // pData value is in IDX
            // returns new listItem in FITEM
            itemCreate();
            
            PLA
            STA IDXH
            PLA
            STA IDXL
            
            GC.Release(); // release original value item
            
            // restore 'this' list
            PLA
            STA IDYH
            PLA
            STA IDYL
        }
        
        PLA
        STA LCURRENTL
        PLA
        STA LCURRENTH
        
        LDY # liNext
        LDA [LCURRENT], Y
        STA LNEXTL
        INY
        LDA [LCURRENT], Y
        STA LNEXTH
        
        LDY # liNext
        LDA LNEXTL
        STA [FITEM], Y // pNext LSB
        INY
        LDA LNEXTH
        STA [FITEM], Y // pNext MSB
        
        
        LDY # liNext
        LDA FITEML
        STA [LCURRENT], Y
        INY
        LDA FITEMH
        STA [LCURRENT], Y
        
        // pRecent = 0  
        LDY #7
        LDA #0
        STA [IDY], Y
        INY
        STA [IDY], Y
        // iRecent
        INY
        STA [IDY], Y
        INY
        STA [IDY], Y
        
        // previous length
        LDY # lsCount
        LDA [IDY], Y
        STA LLENGTHL
        INY
        LDA [IDY], Y
        STA LLENGTHH
        
        // length: increment the item count in the list
        LDY # lsCount
        LDA LLENGTHL
#ifdef CPU_65C02S
        INC
#else
        CLC
        ADC #1
#endif
        STA [IDY], Y
        if (Z)
        {
            INY
            LDA LLENGTHH
#ifdef CPU_65C02S
            INC
#else
            CLC
            ADC #1
#endif
            STA [IDY], Y
        }
        
        LDA IDYL
        STA IDXL
        LDA IDYH
        STA IDXH
        GC.Release(); // we popped 'this'
    }
    
    Contains()
    {
        // Pop the value to search for (V) and list reference (this)
        popValueToIDXandTYPE(); // value and type
        Stacks.PopIDY();        // list reference -> IDY
        
        // Load the type of the list items
        LDY # lsType
        LDA [IDY], Y
        STA LTYPE // type of list items
    
        // Get the first item in the list (pFirst)
        LDY # lsFirst
        LDA [IDY], Y
        STA LCURRENTL
        INY
        LDA [IDY], Y
        STA LCURRENTH
        
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        // value to search for is in IDX
             
        // Loop through the list items
        loop
        {
            // Check if the current item pointer is zero (end of list)
            LDA LCURRENTL
            ORA LCURRENTH
            if (Z) 
            { 
                // End of list, item not found
                LDX # 0
                break; 
            } 
    
            // Get the current item's data (pData)
            LDY # liData
            LDA [LCURRENT], Y
            STA IDYL
            INY
            LDA [LCURRENT], Y
            STA IDYH
    
            // Compare the current item with the target value
            LDA LTYPE
            IsReferenceType();
            if (C)
            {
                // compare the reference types in IDX and IDY for equality
                //    return X=1 for equal, X=0 for not equal
                GC.CompareEqual();
                CPX # 1
                if (Z)
                {
                    // Item found, return true           
                    break;
                }
            }
            else
            {
                // If the item is a value type, compare the values
                LDA IDXL
                CMP IDYL
                if (Z)
                {
                    LDA IDXH
                    CMP IDYH
                    if (Z)
                    {
                        LDX #1 // Item found, return true
                        break;
                    }
                }
            }
    
            // Move to the next item (pNext)
            LDY # liNext
            LDA [LCURRENT], Y
            TAX
            INY
            LDA [LCURRENT], Y
            STA LCURRENTH
            STX LCURRENTL
        } // loop
        
        LDA LTYPE
        IsReferenceType();
        if (C)
        {
            GC.Release(); // 'value' in IDX, preserves X
        }
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        GC.Release(); // 'this' in IDX, preserves X
        
        // bool result in X
        Stacks.PushX();
    }
    
        
    
}
