unit Table // Table.asm
{
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Uses ZP.L* scratch space for legitimate Table operations (does not leak beyond method boundaries)
    
    // Legitimate scratch space for Table operations:
    // ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX - Internal linked list traversal
    
    // Generic linked list operations using ZP.Lxx workspace
    // Lists are represented by a pointer stored at a ZP address
    // Empty list = 0x0000
    // Each node has next pointer at fixed offset 0-1 followed by arbitrary user data
    // Node layout: [0-1: next pointer] [user data]
    
    // Get first node in list
    // Input: X = ZP address of list head pointer
    // Output: ZP.IDX = first node (0x0000 if empty list), C set if found, NC if empty
    GetFirst()
    {
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
    }
    
    // Get next node in traversal
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next node (0x0000 if end of list), C set if found, NC if end
    GetNext()
    {
        PHY
        
        loop
        {
            // Check if current node is null
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) 
            { 
                CLC  // Already at end
                break;
            }
            
            // Load next pointer: [IDX] -> IDX
            LDA [ZP.IDX]
            PHA
            LDY #1
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
            break;
        } // single exit
        
        PLY
    }
    
    // Add new node to end of list
    // Input: X = ZP address of list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address, C set if successful, NC if allocation failed
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Add()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            Memory.Allocate();       // Table.Add(): Input: ZP.ACC = size, Munts: ZP.M*, ZP.FREELIST, ZP.ACCL, -> ZP.IDX
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            // IDX->NEXT = null
            LDA #0
            STA [IDX]
            LDY #1
            STA [IDX], Y
            
            // load first node from head (X)
            LDA 0x00, X             // Get low byte of pointer to first node
            STA ZP.LCURRENTL
            LDA 0x01, X             // Get high byte of pointer to first node
            STA ZP.LCURRENTH
            
            ORA ZP.LCURRENTL
            if (Z)
            {
                // special case : empty list so HEAD = IDX
                LDA ZP.IDXL
                STA 0x00, X
                LDA ZP.IDXH
                STA 0x01, X
                SEC  // Success
                break;
            }
            
            // we need to walk to find the end
            loop
            {
                
                LDA [ZP.LCURRENT]
                STA ZP.LNEXTL
                LDY #1
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
            
                ORA ZP.LNEXTL
                if (Z)
                {
                    // LCURRENT is the last node so CURRENT-NEXT = IDX
                    LDA ZP.IDXL
                    STA [ZP.LCURRENT]
                    LDY #1
                    LDA ZP.IDXH
                    STA [ZP.LCURRENT], Y
                    break;
                }
                // CURRENT = CURRENT->NEXT
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
            } // loop
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Delete specific node from list
    // Input: X = ZP address of list head pointer, ZP.IDX = node to delete
    // Output: C set if successful, NC if node not found
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Delete()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop // start of single exit block
        {
            STX ZP.LHEADX
            
            // load first node from list head location
            LDA 0x00, X
            STA ZP.LCURRENTL
            LDA 0x01, X
            STA ZP.LCURRENTH
            
            // Check if LCURRENT is null (empty list)
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) 
            { 
                CLC  // Node not found
                break; 
            }
            
            // Check if we're deleting the first node
            LDA ZP.LCURRENTL
            CMP ZP.IDXL
            if (Z)
            {
                LDA ZP.LCURRENTH
                CMP ZP.IDXH
                if (Z)
                {
                    // Special case: deleting first node
                    // Update list head VALUE from the next pointer from first node (could be null)
                    LDX ZP.LHEADX
                    LDA [ZP.LCURRENT]
                    STA 0x00, X
                    LDY #1
                    LDA [ZP.LCURRENT], Y
                    STA 0x01, X
                    
                    // Free the node - restore IDX temporarily for Memory.Free
                    PLA
                    STA ZP.ACCH
                    PLA
                    STA ZP.ACCL
                    PLA
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    
                    Memory.Free(); // Input: ZP.IDX, Munts: ZP.IDX, ZP.M* -> C on success
                    
                    // Push dummy values back for consistent exit
                    LDA #0
                    PHA
                    PHA
                    PHA
                    PHA
                    break;
                }
            }
            
            // General case: walk the list to find the node
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
                    break; // exits inner loop, which will break outer loop
                }
                
                LDA [ZP.LCURRENT]
                STA ZP.LNEXTL
                LDY #1
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
                
                // Check if LCURRENT is the node to delete
                LDA ZP.LCURRENTL
                CMP ZP.IDXL
                if (Z)
                {
                    LDA ZP.LCURRENTH
                    CMP ZP.IDXH
                    if (Z)
                    {
                        // Found the node to delete
                        // Update previous node's next pointer to skip deleted node
                        LDA ZP.LNEXTL
                        STA [ZP.LPREVIOUS]
                        LDY #1
                        LDA ZP.LNEXTH
                        STA [ZP.LPREVIOUS], Y
                        
                        // Free the node - restore IDX temporarily for Memory.Free
                        PLA
                        STA ZP.ACCH
                        PLA
                        STA ZP.ACCL
                        PLA
                        STA ZP.IDXH
                        PLA
                        STA ZP.IDXL
                        
                        Memory.Free(); // Input: ZP.IDX, Munts: ZP.IDX, ZP.M* -> C on success
                        
                        // Push dummy values back for consistent exit
                        LDA #0
                        PHA
                        PHA
                        PHA
                        PHA
                        break; // exits inner loop with success flag set
                    }
                }
                
                // Not this node, continue searching
                // Move forward: LPREVIOUS <- LCURRENT
                LDA ZP.LCURRENTL
                STA ZP.LPREVIOUSL
                LDA ZP.LCURRENTH
                STA ZP.LPREVIOUSH
                
                // LCURRENT <- LNEXT
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
            } // inner loop
            
            // Break out of outer loop (whether success or failure)
            break;
        } // end of single exit block
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY    
        PLX
        PLA
    }
    
    // Clear entire list (free all nodes)
    // Input: X = ZP address of list head pointer
    // Output: C set (always succeeds), list head set to 0x0000
    // Modifies: ZP.L* scratch space (internal to Table operations)
    Clear()
    {
        PHA
        PHX
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        STX ZP.LHEADX
               
        loop
        {
            // load first node from list head location
            LDX ZP.LHEADX
            LDA 0x00, X
            STA ZP.IDXL
            LDA 0x01, X
            STA ZP.IDXH
            
            // Check if list is empty
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // List is empty, we're done
                break;
            }
            
            // delete first node
            Delete();
            // Delete always succeeds when node exists, no need to check carry
        }
        
        SEC  // Always succeeds
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLX
        PLA
    }
}
