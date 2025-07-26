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
        
        // Check if current node is null
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) 
        { 
            CLC  // Already at end
            PLY
            return;
        }
        
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
    }
    
    // Add new node to end of list
    // Input: X = ZP address of list head pointer, ZP.ACC = node size (16-bit)
    // Output: ZP.IDX = new node address, C set if successful, NC if allocation failed
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Add()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Save inputs in ZP.Lxx slots
            STX ZP.LHEADX           // ZP address of list head pointer
            
            Memory.Allocate();      // Returns address in ZP.IDX, munts ZP.IDY, ZP.TOP, ZP.NEXT
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // Allocation failed
                break;
            }
            
            // IDX->NEXT = null
            LDY #0
            LDA #0
            STA [IDX], Y
            INY
            STA [IDX], Y
            
            // load first node from head
            LDX ZP.LHEADX
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
                LDY #0
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTL
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
            
                ORA ZP.LNEXTL
                if (Z)
                {
                    // LCURRENT is the last node so CURRENT-NEXT = IDX
                    LDY #0
                    LDA ZP.IDXL
                    STA [ZP.LCURRENT], Y
                    INY
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
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
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
            
            // Special case: deleting first node
            LDA ZP.LCURRENTL
            CMP ZP.IDXL
            if (NZ)
            {
                // Not the first node, jump to general case
                JMP deleteGeneralCase
            }
            
            LDA ZP.LCURRENTH
            CMP ZP.IDXH
            if (NZ)
            {
                // Not the first node, jump to general case
                JMP deleteGeneralCase
            }
            
            // Update list head VALUE from the next pointer from first node (could be null)
            LDX ZP.LHEADX
            LDY #0
            LDA [ZP.LCURRENT], Y
            STA 0x00, X
            INY
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
            
            Memory.Free(); // munts ZP.IDY, ZP.TOP, ZP.NEXT
            
            // Push dummy values back for consistent exit
            LDA #0
            PHA
            PHA
            PHA
            PHA
            
            SEC  // Success
            break;
            
        deleteGeneralCase:
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
                    JMP deleteExit
                }
                
                LDY #0
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTL
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
                
                // Check if LCURRENT is the node to delete
                LDA ZP.LCURRENTL
                CMP ZP.IDXL
                if (NZ)
                {
                    // Not this node, continue searching
                    JMP deleteNotFound
                }
                
                LDA ZP.LCURRENTH
                CMP ZP.IDXH
                if (NZ)
                {
                    // Not this node, continue searching
                    JMP deleteNotFound
                }
                
                // Found the node to delete
                // Update previous node's next pointer to skip deleted node
                LDY #0
                LDA ZP.LNEXTL
                STA [ZP.LPREVIOUS], Y
                INY
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
                
                Memory.Free(); // munts ZP.IDY, ZP.TOP, ZP.NEXT
                
                // Push dummy values back for consistent exit
                LDA #0
                PHA
                PHA
                PHA
                PHA
                
                SEC  // Success
                JMP deleteExit
                
            deleteNotFound:
                // Move forward: 
                // LPREVIOUS <- LCURRENT
                LDA ZP.LCURRENTL
                STA ZP.LPREVIOUSL
                LDA ZP.LCURRENTH
                STA ZP.LPREVIOUSH
                
                // LCURRENT <- LNEXT
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
            } // loop
            
        deleteExit:
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
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
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
