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
    
    const uint lsCount = 2;
    const uint lsType   = 4;
    const uint lsFirst  = 5;
    const uint lsRecent = 7;
    
    // ListItem memory map:
    //   0000 heap allocator size
    //   xxxx inline for value types, pData for reference types and when item type is variant
    //   0000 pNext
            
    const uint liData = 0;
    const uint liNext = 2;
    
    new()
    {
        // element type in FTYPE
        // returns list at IDX
        
        // Add space for number of items, item type, pFirst, pRecent, and iRecent
        // total = 2 + 1 + 2 + 2 + 2 = 9 bytes for the list header
        CLC
        LDA #9
        STA FSIZEL
        LDA #0
        STA FSIZEH
        // type in A
        // size is in FSIZE
        // return address in IDX
        LDA # Types.List
        GC.Create();
        
        // Initialize the list structure in memory
        LDY # lsCount
        
        // Set current number of items to 0
        LDA #0
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        
        // Set the item type
        LDA LTYPE
        STA [IDX], Y
        INY
        
        // Set pFirst, pRecent, iRecent to 0 (initially null pointers)
        LDA #0
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        
        // Store the list pointer on the stack
        LDA IDXL
        STA TOPL
        LDA IDXH
        STA TOPH
        LDA # Types.List
        Stacks.PushTop();
    }
    
    New()
    {
        // Extract the element type from the stack and store it in FTYPE
        Stacks.PopTop();   // Get the element type
        LDA TOPL
        STA LTYPE
        // Call the private new method to handle the rest
        new();
    }
    
    clone()
    {
        // Original list pointer in IDY, cloned list pointer in IDX
        
        // Get the type of items in the original list
        LDY # lsType
        LDA [IDY], Y
        STA LTYPE
        
        // Create a new list with the same item type
        LDA LTYPE
        new();
        
        // Store the cloned list pointer
        LDA IDXL
        STA FITEML
        LDA IDXH
        STA FITEMH
        
        // Get the first item in the original list
        LDY # lsFirst
        LDA [IDY], Y
        STA LCURRENTL
        INY
        LDA [IDY], Y
        STA LCURRENTH
    
        // Loop to clone each item in the original list
        loop
        {
            // Check if we reached the end of the list
            LDA LCURRENTL
            ORA LCURRENTH
            if (Z) { break; }
            
            // Get item data
            LDY # liData
            LDA [LCURRENT], Y
            STA LNEXTL
            INY
            LDA [LCURRENT], Y
            STA LNEXTH
            
            // Determine the item type
            LDA LTYPE
            IsReferenceType();
            if (NC)
            {
                // Value type
                LDA LTYPE
                STA NEXTH
            }
            else
            {
                // Reference type
                LDY # 0
                LDA [LNEXT], Y
                STA NEXTH
            }
            
            // Append the item to the clone
            LDA FITEML
            STA IDXL
            LDA FITEMH
            STA IDXH
            LDA LNEXTL
            STA TOPL
            LDA LNEXTH
            STA TOPH
            append();
            
            // Move to the next item
            LDY # liNext
            LDA [LCURRENT], Y
            PHA
            INY
            LDA [LCURRENT], Y
            STA LCURRENTH
            PLA
            STA LCURRENTL
        }
    
        // Return the cloned list pointer
        LDA FITEML
        STA IDXL
        LDA FITEMH
        STA IDXH
    }
    
    
    CountGet()
    {
        Stacks.PopIDX(); // Get the 'this' pointer
        
        LDY # lsCount   // Load the offset for the count field
        LDA [IDX], Y    // Load the low byte of the count
        STA NEXTL       // Store it in NEXTL
        INY
        LDA [IDX], Y    // Load the high byte of the count
        STA NEXTH       // Store it in NEXTH
        
        GC.Release();   // Release the 'this' pointer
        
        LDA # Types.UInt
        Stacks.PushNext(); // Push the count value onto the stack
    }
    
    createItem()
    {
        // itemData in TOP, etype in LTYPE
        // returns pointer to the new item in IDX
        
        // Initialize pData with itemData
        LDA TOPL
        STA FITEML
        LDA TOPH
        STA FITEMH
    
        // Check if etype is a reference type
        LDA LTYPE
        IsReferenceType();
        if (NC)
        {
            // etype is a value type
            // proceed with itemData as is
            // allocate item memory
            LDA #4
            STA ACCL
            LDA #0
            STA ACCH
            Allocate.allocate(); // size is in ACC, return address in IDX
    
            // Write pData to liData
            LDY #0
            LDA FITEML
            STA [IDX], Y
            INY
            LDA FITEMH
            STA [IDX], Y
            // Write 0 to liNext
            INY
            LDA #0
            STA [IDX], Y
            INY
            STA [IDX], Y
            return;
        }
        
        // If etype is a reference type, check if itype is also a reference type
        LDY # 0          // first byte of any reference type is its type
        LDA [TOP], Y
        IsReferenceType();
        if (NC)
        {
            // etype is reference type and itype is value type, create a value variant
            LDA TOPL
            STA ACCL
            LDA TOPH
            STA ACCH
            LDA LTYPE
            
            // value in TOP, vtype in LTYPE
            // Returns address in IDX
            CreateValueVariant();
    
            // move the new item pointer to IDY
            LDA IDXL
            STA IDYL
            LDA IDXH
            STA IDYH
        }
        else
        {        
            // Both are reference types, clone itemData
            LDA TOPL
            PHA
            STA IDYL
            
            LDA TOPH
            PHA
            STA IDYH
            
            // type is in A
            // reference type to clone is at IDY, resulting clone in IDX
            GC.Clone();
            
            // move the new item pointer to IDY
            LDA IDXL
            STA IDYL
            LDA IDXH
            STA IDYH
            
            PLA
            STA IDXH
            PLA
            STA IDXL
            GC.Release(); // argument was reference type, needs Release
        }
        
        // Allocate memory for the ListItem (4 bytes)
        LDA #4
        STA ACCL
        LDA #0
        STA ACCH
        Allocate.allocate(); // size is in ACC, return address in IDX
        
        // Write pData to liData
        LDY #0
        LDA IDYL
        STA [IDX], Y
        INY
        LDA IDYH
        STA [IDX], Y
        // Write 0 to liNext
        INY
        LDA #0
        STA [IDX], Y
        INY
        STA [IDX], Y
    }
    
    Append()
    {
        // Pop item and list from the stack
        Stacks.PopTop(); // item in TOP
        Stacks.PopIDX(); // list in IDX
        append();
        GC.Release(); // release list ptr IDX
    }
    
    append()
    {
        // takes this in IDX, item to append in TOP
        
        // Get the type of items in the list
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
        
        // Save the list pointer
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        // Call createItem to create the new item
        //   itemData in TOP, etype in LTYPE
        //   returns pointer to the new item in IDX
        createItem();
        
        // Store the new item pointer in FITEM
        LDA IDXL
        STA FITEML
        LDA IDXH
        STA FITEMH
        
        // Restore the list pointer
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        loop
        {
            // Get the first item pointer
            LDY # lsFirst
            LDA [IDX], Y
            STA ACCL
            INY
            LDA [IDX], Y
            STA ACCH
            
            // Check if the list is empty (first item pointer is 0)
            LDA ACCL
            ORA ACCH
            if (Z)
            {
                // List is empty, add the new item as the first item
                LDY # lsFirst
                LDA FITEML
                STA [IDX], Y
                INY
                LDA FITEMH
                STA [IDX], Y
                break;
            }
            
            // List is not empty, find the last item
            LDA ACCL
            STA LCURRENTL
            LDA ACCH
            STA LCURRENTH
            LDA LCURRENTL
            ORA LCURRENTH
            if (Z) { break; }
            loop
            {
                // Get the next item pointer
                LDY # liNext
                LDA [LCURRENT], Y
                STA LNEXTL
                INY
                LDA [LCURRENT], Y
                STA LNEXTH
                
                // Check if the next item pointer is 0
                LDA LNEXTL
                ORA LNEXTH
                if (Z) { break; }
                
                // Move to the next item
                LDA LNEXTL
                STA LCURRENTL
                LDA LNEXTH
                STA LCURRENTH
            }
            
            // Add the new item after the last item
            LDY # liNext
            LDA FITEML
            STA [LCURRENT], Y
            INY
            LDA FITEMH
            STA [LCURRENT], Y
            break;
        } // loop
        
        // Update the element count
        LDY # lsCount
        LDA [IDX], Y
        STA LCOUNTL
        CLC
        ADC #1
        STA [IDX], Y
        INY
        LDA [IDX], Y
        STA LCOUNTH
        ADC #0
        STA [IDX], Y
        
        // Update pRecent to be the last item added
        LDY # lsRecent
        LDA FITEML
        STA [IDX], Y
        INY
        LDA FITEMH
        STA [IDX], Y
        
        // update iRecent to be current element count before it was incremented (count-1) 
        INY
        LDA LCOUNTL
        STA [IDX], Y
        INY
        LDA LCOUNTH
        STA [IDX], Y
    }
    
    clearAllItems()
    {
        // List item pointer in IDY, etype in LTYPE
        // Clears all items in the list
        
        loop
        {
            // Check if we reached the end of the list
            LDA IDYL
            ORA IDYH
            if (Z) { break; }
    
            // Get the next item pointer
            LDY # liNext
            LDA [IDY], Y
            STA LNEXTL
            INY
            LDA [IDY], Y
            STA LNEXTH
    
            // Clear the current item
            LDA LTYPE
            IsReferenceType();
            if (C)
            {
                // Reference type, release the reference (IDX)
                LDY # liData
                LDA [IDY], Y
                STA IDXL
                INY
                LDA [IDY], Y
                STA IDXH
                GC.Release(); // IDX
            }
            
            // free the item
            LDA IDYL
            STA IDXL
            LDA IDYH
            STA IDXH
            Free.free(); // IDX
            
            // Move to the next item
            LDA LNEXTL
            STA IDYL
            LDA LNEXTH
            STA IDYH
        }
    }
    
    Clear()
    {
        Stacks.PopIDX(); // Get the 'this' pointer
        clear();
        GC.Release();
    }
    
    clear()
    {
        // this in IDX
        // Clears all items in the list
        
        // Get the type of items in the list
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
    
        // Get the first item in the list
        LDY # lsFirst
        LDA [IDX], Y
        STA LCURRENTL
        INY
        LDA [IDX], Y
        STA LCURRENTH
    
        // Check if the list is not empty
        LDA LCURRENTL
        ORA LCURRENTH
        if (Z) { return; }
        
        LDA IDXL
        PHA
        LDA IDXH
        PHA
    
        // Clear all items
        LDA LCURRENTL
        STA IDXL
        LDA LCURRENTH
        STA IDXH
        clearAllItems();
    
        // Reset list metadata
        LDY # lsCount
        LDA # 0
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        INY
        STA [IDX], Y
        
        PLA
        STA IDXH
        PLA
        STA IDXL
    }
        
    Insert()
    {
        TXA // LibCall not Implemented!
        Diagnostics.Die();
    }
    
    GetItem()
    {
        // Get the 'this' pointer and index from the stack
        Stacks.PopIDY(); // index in IDY
        Stacks.PopIDX(); // 'this' pointer in IDX
        // Debug: Starting GetItem
        LDA # 0x0A // NewLine
        Serial.WriteChar();
        LDA # 'I'
        Serial.WriteChar();
        LDA IDXL
        Serial.HexOut();
        LDA IDXH
        Serial.HexOut();
        LDA IDYL
        Serial.HexOut();
        LDA IDYH
        Serial.HexOut();
        // Get the item type
        LDY # lsType
        LDA [IDX], Y
        STA LTYPE
        // Debug: Item type
        LDA # 'T'
        Serial.WriteChar();
        LDA LTYPE
        Serial.HexOut();
        // Get the count of items in the list
        LDY # lsCount
        LDA [IDX], Y
        STA LCOUNTL
        INY
        LDA [IDX], Y
        STA LCOUNTH
        // Debug: Count of items
        LDA # 'C'
        Serial.WriteChar();
        LDA LCOUNTL
        Serial.HexOut();
        LDA LCOUNTH
        Serial.HexOut();
        // Check if the index is out of range
        LDA IDYL
        CMP LCOUNTL
        if (Z)
        {
            LDA IDYH
            SBC LCOUNTH
        }
        if (C)
        {
            // Error: index out of range
            LDA # 'E'
            Serial.WriteChar();
            LDA # 0x01
            BRK
        }
        // Initialize iteration variables
        LDY #0
        STY FITEML
        STY FITEMH
        LDY # lsFirst
        LDA [IDX], Y
        STA LCURRENTL
        INY
        LDA [IDX], Y
        STA LCURRENTH
        // Debug: First item pointer
        LDA # 'F'
        Serial.WriteChar();
        LDA LCURRENTL
        Serial.HexOut();
        LDA LCURRENTH
        Serial.HexOut();
        LDY # lsRecent
        LDA [IDX], Y
        STA LPREVIOUSL
        INY
        LDA [IDX], Y
        STA LPREVIOUSH
        // If pRecent is not 0, use it to start the search
        LDA LPREVIOUSL
        ORA LPREVIOUSH
        if (NZ)
        {
            LDY # lsRecent + 2
            LDA [IDX], Y
            CMP IDYL
            if (NC)
            {
                INY
                LDA [IDX], Y
                SBC IDYH
                if (NC)
                {
                    // Use recent index to start search
                    LDY # lsRecent + 2
                    LDA [IDX], Y
                    STA FITEML
                    INY
                    LDA [IDX], Y
                    STA FITEMH
                    LDY # lsRecent
                    LDA [IDX], Y
                    STA LCURRENTL
                    INY
                    LDA [IDX], Y
                    STA LCURRENTH
                }
            }
        }
        loop
        {
            LDA IDYL
            CMP FITEML
            if (Z)
            {
                LDA IDYH
                CMP FITEMH
                if (Z) { break; }
            }
            // Move to the next item
            LDY # liNext
            LDA [LCURRENT], Y
            PHA
            INY
            LDA [LCURRENT], Y
            STA LCURRENTH
            PLA
            STA LCURRENTL
            INC FITEML
            if (Z)
            {
               INC FITEMH
            }
        }
        // Get the item data pointer
        LDY # liData
        LDA [LCURRENT], Y
        STA FITEML
        INY
        LDA [LCURRENT], Y
        STA FITEMH
        // Check if the item is a Variant and call Variant.getValue if necessary
        LDA LTYPE
        CMP # Types.Variant
        if (Z)
        {
            LDA IDXL
            PHA
            LDA IDXH
            PHA
            // pData in FITEM
            LDA FITEML
            STA TOPL
            LDA FITEMH
            STA TOPH
            // Variant.getValue takes pData in TOP
            //    returns value in IDX and updates LITYPE
            Variant.getValue();
            LDA IDXL
            STA FITEML
            LDA IDXH
            STA FITEMH
            // Update the type to the type stored in the variant
            LDA LITYPE
            STA LTYPE
            PLA
            STA IDXH
            PLA
            STA IDXL
        }
        // Update recent pointers
        LDY # lsRecent
        LDA LCURRENTL
        STA [IDX], Y
        INY
        LDA LCURRENTH
        STA [IDX], Y
        INY
        LDA IDYL
        STA [IDX], Y
        INY
        LDA IDYH
        STA [IDX], Y
        // Debug: Updated recent pointers
        LDA # 'U'
        Serial.WriteChar();
        GC.Release(); // Release the 'this' pointer in IDX
        // Return item data pointer
        LDA FITEML
        STA NEXTL
        LDA FITEMH
        STA NEXTH
        LDA LTYPE
        STA NEXTT
        Stacks.PushNext();
    }
    
    GetItemAsVariant()
    {
        TXA // LibCall not Implemented!
        Diagnostics.Die();
    }
    
    SetItem()
    {
        TXA // LibCall not Implemented!
        Diagnostics.Die();
    }
    
    Remove()
    {
        TXA // LibCall not Implemented!
        Diagnostics.Die();
    }
    
    Contains()
    {
        TXA // LibCall not Implemented!
        Diagnostics.Die();
    }
}

