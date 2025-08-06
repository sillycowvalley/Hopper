unit Arguments
{
    // Argument table management - arguments list head stored directly in function node
    // No separate "table head storage" - function node field points directly to first argument
    
    // Argument Node Structure:
    // Offset 0-1: next pointer
    // Offset 2+:  null-terminated argument name
    
    const byte argOverhead = 2;     // Fixed fields before name
    const byte anNext = 0;          // Next pointer offset (2 bytes)
    const byte anName = 2;          // Name field offset (variable length)
    
    // Add argument to function's arguments list at the end for correct order
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: C set if successful, NC if allocation failed
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT, ZP.LPREVIOUS, 
    //        ZP.SymbolType, ZP.SymbolNameL/H, ZP.SymbolLength
    Add()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Save function node address IMMEDIATELY (before it gets munted)
            LDA ZP.IDXL
            STA ZP.LHEADL           // Function node address
            LDA ZP.IDXH
            STA ZP.LHEADH
            
            LDA ZP.TOPL
            STA ZP.SymbolNameL      // Argument name pointer
            LDA ZP.TOPH
            STA ZP.SymbolNameH
            
            // 1. Allocate and initialize new node
            calculateNodeSize();       // Returns size in ZP.ACC
            Memory.Allocate();         // Returns address in ZP.IDX
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // Allocation failed
                break;
            }
            
            LDA ZP.IDXL
            STA ZP.LCURRENTL        // New argument node
            LDA ZP.IDXH
            STA ZP.LCURRENTH
            
            initializeNode();
            
            // New node's next = NULL
            LDY #anNext
            LDA #0
            STA [ZP.LCURRENT], Y
            INY
            STA [ZP.LCURRENT], Y
            
            // 2. Find insertion point (pointer-to-pointer pattern)
            // Start with address of function's arguments field
            CLC
            LDA ZP.LHEADL
            ADC #Objects.snArguments
            STA ZP.LPREVIOUSL       // Points to insertion location
            LDA ZP.LHEADH
            ADC #0
            STA ZP.LPREVIOUSH
            
            loop
            {
                // Check if *LPREVIOUS is NULL (insertion point found)
                LDY #0
                LDA [ZP.LPREVIOUS], Y
                STA ZP.LNEXTL
                INY
                LDA [ZP.LPREVIOUS], Y
                STA ZP.LNEXTH
                
                LDA ZP.LNEXTL
                ORA ZP.LNEXTH
                if (Z) { break; }    // Found insertion point
                
                // LPREVIOUS = &(LNEXT->next)
                CLC
                LDA ZP.LNEXTL
                ADC #anNext
                STA ZP.LPREVIOUSL
                LDA ZP.LNEXTH
                ADC #0
                STA ZP.LPREVIOUSH
            }
            
            // 3. Insert: *LPREVIOUS = LCURRENT
            LDY #0
            LDA ZP.LCURRENTL
            STA [ZP.LPREVIOUS], Y
            INY
            LDA ZP.LCURRENTH
            STA [ZP.LPREVIOUS], Y
            
            SEC  // Success
            break;
        } // end of single exit block
        
        // RESTORE original IDX before exit
        LDA ZP.LHEADL
        STA ZP.IDXL
        LDA ZP.LHEADH
        STA ZP.IDXH
        
        PLY
        PLX
        PLA
    }
    
    // Find argument by name in function's arguments list
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: ZP.IDY = argument node address, ZP.ACCL = argument index, C set if found, NC if not found
    // Munts: ZP.LCURRENT, ZP.LNEXT
    Find()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Get arguments list head from function node (IDX preserved)
            LDY #Objects.snArguments
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            STZ ZP.ACCL  // Argument index counter
            
            loop
            {
                // Check if we've reached end of list
                LDA ZP.LCURRENTL
                ORA ZP.LCURRENTH
                if (Z)
                {
                    CLC  // Not found
                    break;
                }
                
                // Compare argument name
                compareNames();
                if (C)  // Names match
                {
                    // Copy node address to IDY
                    LDA ZP.LCURRENTL
                    STA ZP.IDYL
                    LDA ZP.LCURRENTH
                    STA ZP.IDYH
                    break;
                }
                
                // Move to next argument - use LCURRENT throughout
                LDY #anNext
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTL
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
                
                // LCURRENT = LNEXT (never touch IDX)
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
                
                INC ZP.ACCL  // Increment argument index
            }
            break;  // Only one path through outer loop
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Get argument name pointer from argument node
    // Input: ZP.IDY = argument node address
    // Output: ZP.TOP = argument name pointer, C set (always succeeds)
    // Munts: -
    GetName()
    {
        PHA
        
        // Calculate address of name field in argument node
        CLC
        LDA ZP.IDYL
        ADC #anName
        STA ZP.STRL
        LDA ZP.IDYH
        ADC #0
        STA ZP.STRH
        
        PLA
    }
    
    // Find argument by index for BP offset calculation
    // Input: ZP.IDX = function node address, ZP.ACCL = argument index
    // Output: ZP.IDY = argument node address, C set if found, NC if index out of range
    // Munts: ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0
    FindByIndex()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Save target index
            LDA ZP.ACCL
            STA ZP.SymbolTemp0
            
            // Get arguments list head from function node (IDX preserved)
            LDY #Objects.snArguments
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            STZ ZP.ACCL  // Current index counter
            
            loop
            {
                // Check if we've reached end of list
                LDA ZP.LCURRENTL
                ORA ZP.LCURRENTH
                if (Z)
                {
                    CLC  // Not found
                    break;
                }
                
                // Check if current index matches target
                LDA ZP.ACCL
                CMP ZP.SymbolTemp0
                if (Z)  // Found the target index
                {
                    // Copy node address to IDY
                    LDA ZP.LCURRENTL
                    STA ZP.IDYL
                    LDA ZP.LCURRENTH
                    STA ZP.IDYH
                    
                    SEC  // Found
                    break;
                }
                
                // Move to next argument - use LCURRENT throughout
                LDY #anNext
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTL
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.LNEXTH
                
                // LCURRENT = LNEXT (never touch IDX)
                LDA ZP.LNEXTL
                STA ZP.LCURRENTL
                LDA ZP.LNEXTH
                STA ZP.LCURRENTH
                
                INC ZP.ACCL  // Increment current index
            }
            break;  // Only one path through outer loop
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Get argument count in function's arguments list
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = argument count, C set (always succeeds)
    // Munts: ZP.LCURRENT, ZP.LNEXT
    GetCount()
    {
        PHA
        PHX
        
        // Get arguments list head from function node (IDX preserved)
        LDY #Objects.snArguments
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTL
        INY
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTH
        
        STZ ZP.ACCL  // Argument count
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z) { break; }  // End of list
            
            INC ZP.ACCL  // Increment count
            
            // Move to next argument (use LCURRENT, not IDX)
            LDY #anNext
            LDA [ZP.LCURRENT], Y
            STA ZP.LNEXTL        // Use LNEXT as temp
            INY
            LDA [ZP.LCURRENT], Y
            STA ZP.LNEXTH
            
            // LCURRENT = LNEXT (never touch IDX)
            LDA ZP.LNEXTL
            STA ZP.LCURRENTL
            LDA ZP.LNEXTH
            STA ZP.LCURRENTH
        }
        
        SEC  // Always succeeds
        
        PLX
        PLA
        // IDX unchanged!
    }
    
    // Start iteration over arguments in function's list
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = first argument node, C set if found, NC if no arguments
    // Munts: -
    IterateStart()
    {
        PHA
        
        // Get arguments list head from function node (IDX preserved)
        LDY #Objects.snArguments
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        // Set carry based on result
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            CLC  // No arguments
        }
        else
        {
            SEC  // Found first argument
        }
        
        PLA
        // IDX unchanged - still points to function node
    }
    
    // Continue argument iteration
    // Input: ZP.IDY = current argument node
    // Output: ZP.IDY = next argument node, C set if found, NC if end of arguments
    // Munts: ZP.LCURRENT
    IterateNext()
    {
        PHA
        
        // Get next pointer from current argument node
        LDY #anNext
        LDA [ZP.IDY], Y
        STA ZP.LCURRENTL
        INY
        LDA [ZP.IDY], Y
        STA ZP.LCURRENTH
        
        // Copy to IDY
        LDA ZP.LCURRENTL
        STA ZP.IDYL
        LDA ZP.LCURRENTH
        STA ZP.IDYH
        
        // Set carry based on result
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            CLC  // End of arguments
        }
        else
        {
            SEC  // Found next argument
        }
        
        PLA
    }
    
    // Clear all arguments in function's list with proper memory cleanup
    // Input: ZP.IDX = function node address
    // Output: Function's arguments field set to null, all argument nodes freed, C set (always succeeds)
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
    Clear()
    {
        PHA
        PHX
        PHY
        
        // Save function node address since Memory.Free() will munt ZP.IDX
        LDA ZP.IDXL
        STA ZP.SymbolTemp0
        LDA ZP.IDXH
        STA ZP.SymbolTemp1
        
        loop
        {
            // Get first argument from function node (use saved address)
            LDY #Objects.snArguments
            LDA [ZP.SymbolTemp0], Y
            STA ZP.IDXL
            INY
            LDA [ZP.SymbolTemp0], Y
            STA ZP.IDXH
            
            // Check if arguments list is empty
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Get next pointer from first argument (use IDY instead of IDX)
            LDY #anNext
            LDA [ZP.IDX], Y
            STA ZP.LNEXTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LNEXTH
            
            // Store the next pointer as the new head argument
            LDY #Objects.snArguments
            LDA ZP.LNEXTL
            STA [ZP.SymbolTemp0], Y
            INY
            LDA ZP.LNEXTH
            STA [ZP.SymbolTemp0], Y
            
            // Free the current argument node
            Memory.Free();       // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
        }
        
        // Restore function node address for caller
        LDA ZP.SymbolTemp0
        STA ZP.IDXL
        LDA ZP.SymbolTemp1
        STA ZP.IDXH
        
        SEC  // Always succeeds
        
        PLY
        PLX
        PLA
    }
    
    // Internal helper: Calculate required argument node size
    // Input: ZP.SymbolName = argument name pointer
    // Output: ZP.ACC = total node size (16-bit), ZP.SymbolLength = name length including null
    // Munts: -
    calculateNodeSize()
    {
        // Start with fixed overhead
        LDA #argOverhead
        STA ZP.ACCL
        STZ ZP.ACCH
        
        // Calculate and store name length + null terminator
        LDY #0
        loop
        {
            LDA [ZP.SymbolName], Y
            if (Z) { break; } // Hit null terminator
            INY
        }
        
        // Y now contains string length, add 1 for null terminator
        INY
        STY ZP.SymbolLength  // Store total name length (including null) for reuse
        
        // Add name length to total size
        TYA
        CLC
        ADC ZP.ACCL
        STA ZP.ACCL
        if (C) { INC ZP.ACCH }
    }
    
    // Internal helper: Initialize fields in newly allocated argument node
    // Input: ZP.IDX = node address
    //        ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    // Note: Next pointer will be set by caller
    // Munts: -
    initializeNode()
    {
        // Copy name string starting at anName
        copyNameToNode();
    }
    
    // Internal helper: Copy argument name string to node
    // Input: ZP.IDX = node address, ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    // Munts: ZP.FSOURCEADDRESSL/H, ZP.FDESTINATIONADDRESSL/H, ZP.FLENGTHL/H
    copyNameToNode()
    {
        // Set up source address (name pointer)
        LDA ZP.SymbolNameL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.SymbolNameH
        STA ZP.FSOURCEADDRESSH
        
        // Set up destination address (node + anName)
        CLC
        LDA ZP.IDXL
        ADC #anName
        STA ZP.FDESTINATIONADDRESSL
        LDA ZP.IDXH
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // Use pre-calculated length
        LDA ZP.SymbolLength
        STA ZP.FLENGTHL
        STZ ZP.FLENGTHH
        
        // Copy the string using Tools.CopyBytes
        Tools.CopyBytes();
    }
    
    // Internal helper: Compare argument names
    // Input: ZP.LCURRENT = argument node address, ZP.TOP = target name
    // Output: C if names match, NC if different
    // Munts: ZP.NEXT (temporarily)
    compareNames()
    {
        PHA
        
        // Save ZP.NEXT since StringCompare uses it
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        
        // Calculate address of name field in argument node
        CLC
        LDA ZP.LCURRENTL
        ADC #anName
        STA ZP.NEXTL         // Use NEXT for string2 pointer
        LDA ZP.LCURRENTH
        ADC #0
        STA ZP.NEXTH
        
        // Compare strings using Tools utility
        Tools.StringCompare();  // Returns C set if match, NC if different
        
        // Restore ZP.NEXT
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        
        PLA
    }
}
