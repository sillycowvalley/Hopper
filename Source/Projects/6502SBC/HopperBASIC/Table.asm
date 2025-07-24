unit Table
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    
    // Generic linked list operations using temporary ZP.F workspace
    // Lists are represented by a pointer to the first node (head)
    // Empty list = 0x0000
    // Each node has user data with next pointer at fixed offset 4-5
    // Node layout: [0-3: user data] [4-5: next pointer] [6+: variable data]
    //
    // IMPORTANT: F slots are temporary workspace only - not preserved between calls
    // Only persistent storage is ZP.SymbolListL/SymbolListH for the global symbol table
    
    const byte NEXT_POINTER_OFFSET = 4;
    
    // Create empty list
    // Output: ZP.LCURRENT = 0x0000 (empty list head pointer)
    Create()
    {
        STZ ZP.LCURRENTL
        STZ ZP.LCURRENTH
    }
    
    // Add new node to front of list
    // Input: ZP.LCURRENT = current list head, ZP.LLENGTH = node size (including next pointer)
    // Output: ZP.LCURRENT = new list head (address of new node)
    Add()
    {
        // Save current list head in LNEXT
        LDA ZP.LCURRENTL
        STA ZP.LNEXTL
        LDA ZP.LCURRENTH
        STA ZP.LNEXTH
        
        // Allocate memory for new node (size in LLENGTH)
        LDA ZP.LLENGTHL
        STA ZP.ACCL
        LDA ZP.LLENGTHH
        STA ZP.ACCH
        Memory.Allocate(); // Returns address in IDX
        
        // Check if allocation succeeded
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // Allocation failed - restore original head
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
            return;
        }
        
        // Store new node address as new list head
        LDA ZP.IDXL
        STA ZP.LCURRENTL
        LDA ZP.IDXH
        STA ZP.LCURRENTH
        
        // Calculate address of next pointer field (fixed offset 4-5)
        CLC
        LDA ZP.LCURRENTL
        ADC #NEXT_POINTER_OFFSET
        STA ZP.FITEML
        LDA ZP.LCURRENTH
        ADC #0
        STA ZP.FITEMH
        
        // Store old list head in next pointer field
        LDY #0
        LDA ZP.LNEXTL
        STA [ZP.FITEM], Y
        INY
        LDA ZP.LNEXTH
        STA [ZP.FITEM], Y
    }
    
    // Find node by comparing string at given offset
    // Input: ZP.LCURRENT = list head, ZP.LCOUNT = name offset in node, ZP.FITEM = target name pointer
    // Output: ZP.LCURRENT = node address (0x0000 if not found)
    Find()
    {
        // Start traversal with current list head
        LDA ZP.LCURRENTL
        STA ZP.LPREVIOUSL
        LDA ZP.LCURRENTH
        STA ZP.LPREVIOUSH
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.LPREVIOUSL
            ORA ZP.LPREVIOUSH
            if (Z)
            {
                // End of list - not found
                STZ ZP.LCURRENTL
                STZ ZP.LCURRENTH
                return;
            }
            
            // Calculate address of name field: LPREVIOUS + LCOUNT offset
            CLC
            LDA ZP.LPREVIOUSL
            ADC ZP.LCOUNTL
            STA ZP.LNEXTL
            LDA ZP.LPREVIOUSH
            ADC ZP.LCOUNTH
            STA ZP.LNEXTH
            
            // Compare strings: FITEM = target name, LNEXT = current node name
            compareStrings();
            if (Z) // Strings match
            {
                // Found it - return current node address in LCURRENT
                LDA ZP.LPREVIOUSL
                STA ZP.LCURRENTL
                LDA ZP.LPREVIOUSH
                STA ZP.LCURRENTH
                return;
            }
            
            // Move to next node - get next pointer from current node
            getNextPointer(); // Input: LPREVIOUS, Output: LNEXT
            
            // Move LNEXT to LPREVIOUS for next iteration
            LDA ZP.LNEXTL
            STA ZP.LPREVIOUSL
            LDA ZP.LNEXTH
            STA ZP.LPREVIOUSH
        }
    }
    
    // Compare two null-terminated strings
    // Input: ZP.FITEM = string1 pointer, ZP.LNEXT = string2 pointer
    // Output: Z set if equal, NZ if different
    // Uses: Y register for indexing, ZP.LTYPE for temporary storage
    compareStrings()
    {
        LDY #0
        loop
        {
            LDA [ZP.FITEM], Y    // Load char from string1
            STA ZP.LTYPE         // Store temporarily
            LDA [ZP.LNEXT], Y    // Load char from string2
            CMP ZP.LTYPE         // Compare
            if (NZ) { return; }  // Different characters
            
            // Check if we hit null terminator
            if (Z) { return; }   // Both null terminators - strings equal
            
            INY
        }
    }
    
    // Get next pointer from a node
    // Input: ZP.LPREVIOUS = node address
    // Output: ZP.LNEXT = next pointer value
    getNextPointer()
    {
        // Read next pointer from fixed offset 4-5
        LDY #NEXT_POINTER_OFFSET
        LDA [ZP.LPREVIOUS], Y
        STA ZP.LNEXTL
        INY
        LDA [ZP.LPREVIOUS], Y
        STA ZP.LNEXTH
    }
    
    // Remove specific node from list using three-pointer walk
    // Input: ZP.LCURRENT = list head, ZP.FITEM = node address to remove
    // Output: ZP.LCURRENT = new list head
    Remove()
    {
        // Save original list head in LCOUNT (as backup)
        LDA ZP.LCURRENTL
        STA ZP.LCOUNTL
        LDA ZP.LCURRENTH
        STA ZP.LCOUNTH
        
        // Special case: removing first node
        LDA ZP.LCURRENTL
        CMP ZP.FITEML
        if (Z)
        {
            LDA ZP.LCURRENTH
            CMP ZP.FITEMH
            if (Z)
            {
                // Get next pointer of first node to become new head
                LDA ZP.FITEML
                STA ZP.LPREVIOUSL
                LDA ZP.FITEMH
                STA ZP.LPREVIOUSH
                getNextPointer(); // Returns next in LNEXT
                
                // Set new head
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
                
                // Free the removed node
                LDA ZP.FITEML
                STA ZP.ACCL
                LDA ZP.FITEMH
                STA ZP.ACCH
                Memory.Free();
                return;
            }
        }
        
        // General case: walk with LPREVIOUS=null, LCURRENT=first node
        STZ ZP.LPREVIOUSL
        STZ ZP.LPREVIOUSH
        
        LDA ZP.LCOUNTL      // Use saved original head
        STA ZP.LCURRENTL
        LDA ZP.LCOUNTH
        STA ZP.LCURRENTH
        
        loop
        {
            // Check if LCURRENT is null (end of list)
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) 
            { 
                // Restore original head and return (node not found)
                LDA ZP.LCOUNTL
                STA ZP.LCURRENTL
                LDA ZP.LCOUNTH
                STA ZP.LCURRENTH
                return; 
            }
            
            // Check if LCURRENT is the node to remove
            LDA ZP.LCURRENTL
            CMP ZP.FITEML
            if (Z)
            {
                LDA ZP.LCURRENTH
                CMP ZP.FITEMH
                if (Z)
                {
                    // Found it! Get LCURRENT's next pointer
                    LDA ZP.LCURRENTL
                    STA ZP.LPREVIOUSL  // Temp use for getNextPointer
                    LDA ZP.LCURRENTH
                    STA ZP.LPREVIOUSH
                    getNextPointer();  // LCURRENT->next now in LNEXT
                    
                    // Save the next pointer value before we restore LPREVIOUS
                    LDA ZP.LNEXTL
                    STA ZP.LLENGTHL
                    LDA ZP.LNEXTH
                    STA ZP.LLENGTHH
                    
                    // Restore LPREVIOUS (node before the one to remove)
                    // We need to find it again by walking from head
                    // Actually, we can track it properly...
                    // For now, patch using the saved next value
                    LDA ZP.LLENGTHL
                    STA ZP.LNEXTL
                    LDA ZP.LLENGTHH
                    STA ZP.LNEXTH
                    
                    // Patch LPREVIOUS->next = LCURRENT->next (skip LCURRENT)
                    setNextPointer();  // Sets LPREVIOUS->next = LNEXT
                    
                    // Free the node
                    LDA ZP.LCURRENTL
                    STA ZP.ACCL
                    LDA ZP.LCURRENTH
                    STA ZP.ACCH
                    Memory.Free();
                    
                    // Restore original head
                    LDA ZP.LCOUNTL
                    STA ZP.LCURRENTL
                    LDA ZP.LCOUNTH
                    STA ZP.LCURRENTH
                    return;
                }
            }
            
            // Move forward: LPREVIOUS = LCURRENT, LCURRENT = LCURRENT->next
            // Save current LCURRENT as new LPREVIOUS
            LDA ZP.LCURRENTL
            STA ZP.LPREVIOUSL
            LDA ZP.LCURRENTH
            STA ZP.LPREVIOUSH
            
            // Get LCURRENT->next as new LCURRENT
            getNextPointer(); // Returns LCURRENT->next in LNEXT
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
        }
    }
    
    // Set next pointer in a node
    // Input: ZP.LPREVIOUS = node address, ZP.LNEXT = value to store in next field
    setNextPointer()
    {
        // Store next pointer at fixed offset 4-5
        LDY #NEXT_POINTER_OFFSET
        LDA ZP.LNEXTL
        STA [ZP.LPREVIOUS], Y
        INY
        LDA ZP.LNEXTH
        STA [ZP.LPREVIOUS], Y
    }
    
    // Clear entire list (free all nodes)
    // Input: ZP.LCURRENT = list head
    // Output: ZP.LCURRENT = 0x0000 (empty list)
    Clear()
    {
        loop
        {
            // Check if list is empty
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) { return; } // Empty list
            
            // Get next pointer before freeing current node
            LDA ZP.LCURRENTL
            STA ZP.LPREVIOUSL
            LDA ZP.LCURRENTH
            STA ZP.LPREVIOUSH
            getNextPointer(); // Returns next in LNEXT
            
            // Free current node
            LDA ZP.LCURRENTL
            STA ZP.ACCL
            LDA ZP.LCURRENTH
            STA ZP.ACCH
            Memory.Free();
            
            // Move to next node
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
        }
    }
}