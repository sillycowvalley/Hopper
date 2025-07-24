unit Table
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    
    // Generic linked list operations using ZP.Lxx workspace
    // Lists are represented by a pointer stored at a ZP address
    // Empty list = 0x0000
    // Each node has next pointer at fixed offset 0-1 followed by arbitrary user data
    // Node layout: [0-1: next pointer] [user data]
    
    // Get first node in list
    // Input: X = ZP address of list head pointer
    // Output: ZP.IDX = first node (0x0000 if empty list), C set if found, NC if empty
    // Preserves: A, X, Y, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    GetFirst()
    {
        PHA
        
        // Read the node pointer value stored at that head address
        LDA 0x00, X
        STA ZP.IDXL
        LDA 0x01, X
        STA ZP.IDXH
        
        // Set carry based on whether we found a node
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // Empty list
        }
        else
        {
            SEC  // Found node
        }
        
        PLA
    }
    
    // Get next node in traversal
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next node (0x0000 if end of list), C set if found, NC if end
    // Preserves: A, X, Y, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    GetNext()
    {
        PHA
        
        // Check if current node is null
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) 
        { 
            CLC  // Already at end
            PLA
            return; 
        }
        
        PHY
        
        // Load next pointer: [IDX] -> IDX
        LDY #0
        LDA [ZP.IDX], Y
        PHA
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Set carry based on whether we found a next node
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // End of list
        }
        else
        {
            SEC  // Found next node
        }
        
        PLY
        PLA
    }
    
    // Add new node to list
    // Input: X = ZP address of list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address, C set if successful, NC if allocation failed
    // Preserves: A, Y, ZP.TOP, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add()
    {
        PHA
        PHX
        PHY
        
        STX ZP.LHEADX
        
        // Read current list head value into LCURRENT
        LDA 0x00, X
        STA ZP.LCURRENTL
        LDA 0x01, X
        STA ZP.LCURRENTH
        
        // Allocate memory (size already in ZP.ACC)
        Memory.Allocate(); // Returns address in IDX, probably munts everything (except ZP.Lx)
        
        // Check if allocation succeeded
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // Allocation failed
            CLC  // Failure
            PLY
            PLX
            PLA
            return;
        }
        
        // Store old list head in next pointer field of new node
        LDY #0
        LDA ZP.LCURRENTL
        STA [ZP.IDX], Y
        INY
        LDA ZP.LCURRENTH
        STA [ZP.IDX], Y
        
        // Update list head to point to new node
        LDX ZP.LHEADX
        LDA ZP.IDXL
        STA 0x00, X
        LDA ZP.IDXH
        STA 0x01, X
        
        // Success
        SEC
        PLY
        PLX
        PLA
    }
    
    // Delete specific node from list
    // Input: X = ZP address of list head pointer, ZP.IDX = node to delete
    // Output: C set if successful, NC if node not found
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Memory.Free calls)
    // Uses: ZP.Lxx variables as temporary workspace
    Delete()
    {
        PHA
        PHX
        PHY
        
        STX ZP.LHEADX
        
        // load first node from list head location
        LDA 0x00, X
        STA ZP.LCURRENTL
        LDA 0x01, X
        STA ZP.LCURRENTH
        
        // Check if LCURRENT is null (end of empty list)
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z) 
        { 
            CLC  // Node not found
            PLY
            PLX
            PLA
            return; 
        }
        
        // Special case: deleting first node
        LDA ZP.LCURRENTL
        CMP ZP.IDXL
        if (Z)
        {
            LDA ZP.LCURRENTH
            CMP ZP.IDXH
            if (Z)
            {
                // Update list head VALUE from the next pointer from first node (could be null)
                LDX LHEADX
                LDY #0
                LDA [ZP.LCURRENT], Y
                STA 0x00, X
                INY
                LDA [ZP.LCURRENT], Y
                STA 0x01, X
                
                // Free the node
                LDA ZP.IDXL
                STA ZP.ACCL
                LDA ZP.IDXH
                STA ZP.ACCH
                Memory.Free(); // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
                
                SEC  // Success
                PLY
                PLX
                PLA
                return;
            }
        }
        
        // General case: walk the list
        STZ ZP.LPREVIOUSL
        STZ ZP.LPREVIOUSH
        
        loop
        {
            // Check if LCURRENT is null (end of list)
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) 
            { 
                CLC  // Node not found
                PLY
                PLX
                PLA
                return; 
            }
            
            LDY #0
            LDA [ZP.LCURRENT], Y
            STA ZP.LNEXTL
            INY
            LDA [ZP.LCURRENT], Y
            STA ZP.LNEXTH
            
            // Check if LCURRENT is the node to delete (never first node because of special case above)
            LDA ZP.LCURRENTL
            CMP ZP.IDXL
            if (Z)
            {
                LDA ZP.LCURRENTH
                CMP ZP.IDXH
                if (Z)
                {
                    // Update previous node's next pointer to skip deleted node
                    LDY #0
                    LDA ZP.LNEXTL
                    STA [ZP.LPREVIOUS], Y
                    INY
                    LDA ZP.LNEXTH
                    STA [ZP.LPREVIOUS], Y
                    
                    // Free the node
                    LDA ZP.LCURRENTL
                    STA ZP.ACCL
                    LDA ZP.LCURRENTH
                    STA ZP.ACCH
                    Memory.Free(); // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
                    
                    SEC  // Success
                    PLY
                    PLX
                    PLA
                    return;
                }
            }
            
            // Move forward: 
            // LPREVIOUS <- LCURRENT
            LDA ZP.LCURRENTL
            STA ZP.LPREVIOUSL
            LDA ZP.LCURRENTH
            STA ZP.LPREVIOUSH
            
            // LCURRENT <- LNEXTL
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
        } // loop
    }
    
    // Clear entire list (free all nodes)
    // Input: X = ZP address of list head pointer
    // Output: C set (always succeeds)
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Memory.Free calls via Delete)
    // Uses: ZP.Lxx variables as temporary workspace
    Clear()
    {
        PHA
        PHX
        PHY
        
        STX LHEADX
               
        loop
        {
            // load first node from list head location
            LDX LHEADX
            LDA 0x00, X
            STA ZP.IDXL
            LDA 0x01, X
            STA ZP.IDXH
            
            // delete it
            Delete();
            if (NC)
            {
                // done
                break;
            }
        }
        
        SEC  // Always succeeds
        PLY
        PLX
        PLA
    }
}
