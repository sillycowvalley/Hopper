unit Locals
{
    uses "./Objects/Objects"
    
    friend Debug;
    
    // In Locals.asm
    // Unified node structure for both arguments and locals
    // Offset 0-1: next pointer
    // Offset 2:   symbolType|dataType (packed byte like Objects)
    // Offset 3+:  null-terminated name

    const byte localOverhead = 3;      // Fixed fields before name
    const byte lnNext = 0;             // Next pointer (2 bytes)
    const byte lnType = 2;             // symbolType|dataType packed byte
    const byte lnName = 3;             // Name field start
    
    // Add local to function's locals list at the end for correct order
    // Input: ZP.IDX = function node address, ZP.TOP = local name
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
            Memory.Allocate();         // Locals.Add(): Input: ZP.ACC = size, Munts: ZP.M*, ZP.FREELIST, ZP.ACCL, -> ZP.IDX
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            LDA ZP.IDXL
            STA ZP.LCURRENTL        // New local node
            LDA ZP.IDXH
            STA ZP.LCURRENTH
            
            initializeNode();
            
            LDY #lnType
            LDA ZP.SymbolType  // set by caller
            STA [ZP.LCURRENT], Y
            
            // New node's next = NULL
            LDY # lnNext
            LDA #0
            STA [ZP.LCURRENT], Y
            INY
            STA [ZP.LCURRENT], Y
            
            // 2. Find insertion point (pointer-to-pointer pattern)
            // Start with address of function's locals field
            CLC
            LDA ZP.LHEADL
            ADC # Objects.snLocals
            STA ZP.LPREVIOUSL       // Points to insertion location
            LDA ZP.LHEADH
            ADC #0
            STA ZP.LPREVIOUSH
            
            loop
            {
                // Check if *LPREVIOUS is NULL (insertion point found)
                LDA [ZP.LPREVIOUS]
                STA ZP.LNEXTL
                LDY #1
                LDA [ZP.LPREVIOUS], Y
                STA ZP.LNEXTH
                
                LDA ZP.LNEXTL
                ORA ZP.LNEXTH
                if (Z) { break; }    // Found insertion point
                
                // LPREVIOUS = &(LNEXT->next)
                CLC
                LDA ZP.LNEXTL
                ADC # lnNext
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
    
    // Find local variable or argument by name in function
    // Input: ZP.IDX = function node address
    //        ZP.TOP = name string pointer to find
    // Output: C set if found, NC if not found
    //         If found: ZP.IDY = node address
    //                   ZP.ACCL = BP offset (signed: negative for args, positive for locals)
    // Modifies: ZP.IDY, ZP.ACCL, ZP.LCURRENT
    Find()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit
        {
            // Get locals/arguments list head from function node
            LDY #Objects.snLocals
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            LDX #0       // Index counter for arguments
            STZ ZP.ACCH  // Index counter for locals
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
                
                // Compare names using existing compareNames helper
                compareNames();  // Uses ZP.LCURRENT and ZP.TOP
                if (C)  // Names match
                {
                    // Found - determine BP offset based on type
                    LDY #lnType
                    LDA [ZP.LCURRENT], Y
                    AND # SymbolType.MASK
                    CMP # SymbolType.ARGUMENT
                    if (Z)  // It's an argument
                    {
                        // For arguments: BP offset = -(arg_count - index)
                        STX ZP.ACCL  // Save index first
                        LDA (Compiler.compilerFuncArgs + 0)  // Get arg_count
                        SEC
                        SBC ZP.ACCL  // arg_count - index
                        EOR #0xFF
                        CLC
                        ADC #1  // Two's complement for negative
                        STA ZP.ACCL  // Negative BP offset
                    }
                    else
                    {
                        // For locals, BP offset is positive (0-based)
                        LDA ZP.ACCH
                        STA ZP.ACCL
                    }
                    
                    // Copy node address to IDY
                    LDA ZP.LCURRENTL
                    STA ZP.IDYL
                    LDA ZP.LCURRENTH
                    STA ZP.IDYH
                    
                    SEC  // Found
                    break;
                }
                
                // count the arguments and locals that we skip
                LDY #lnType
                LDA [ZP.LCURRENT], Y
                AND #SymbolType.MASK
                CMP #SymbolType.ARGUMENT
                if (Z)
                {
                    INX  // Increment local counter
                }
                else
                {
                    INC ZP.ACCH
                }
                
                // Move to next node
                LDY #lnNext
                LDA [ZP.LCURRENT], Y
                PHA
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.LCURRENTH
                PLA
                STA ZP.LCURRENTL
                
                
            }
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Get local name pointer from local node
    // Input: ZP.IDY = local node address
    // Output: ZP.STR = local name pointer
    // Munts: -
    GetName()
    {
        PHA
        
        // Calculate address of name field in local node
        CLC
        LDA ZP.IDYL
        ADC # lnName
        STA ZP.STRL
        LDA ZP.IDYH
        ADC #0
        STA ZP.STRH
        
        PLA
    }
    
    // Get local type from local node
    // Input: ZP.IDY = local node address
    // Output: ZP.ACCT = (SymbolType.LOCAL or SymbolType.ARGUMENT) | BASICType
    // Munts: -
    GetType()
    {
        PHA
        PHY
        
        LDY # lnType
        LDA [ZP.IDY], Y 
        STA ZP.ACCT
                   
        PLY
        PLA
    }
    
    // Find local by index for BP offset calculation
    // Input: ZP.IDX = function node address, ZP.ACCL = local index
    // Output: ZP.IDY = local node address, C set if found, NC if index out of range
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
            
            // Get locals list head from function node (IDX preserved)
            LDY #Objects.snLocals
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
                
                // Move to next local - use LCURRENT throughout
                LDY # lnNext
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
    
    // Get local count in function's locals list
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = local count, C set (always succeeds)
    // Munts: ZP.LCURRENT, ZP.LNEXT, A, X, Y
    GetCount()
    {
        // NOTE: optimizations in this method are hideous: be careful
        
        STZ ZP.ACCL  // Argument count
        
        // Get locals list head from function node (IDX preserved)
        LDY #( Objects.snLocals + 1)
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTH
        DEY
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTL
        loop
        {
            // Check if we've reached end of list
            // LDA ZP.LCURRENTL - pre-loaded on loop entry and continue
            ORA ZP.LCURRENTH
            if (Z) { break; }  // End of list
            
            INC ZP.ACCL  // Increment count
            
            // Move to next local (use LCURRENT, not IDX)
            LDY # lnNext
            LDA [ZP.LCURRENT], Y
            TAX
            INY
            LDA [ZP.LCURRENT], Y
            STA ZP.LCURRENTH
            STX ZP.LCURRENTL
        }
        SEC  // Always succeeds
        // IDX unchanged!
    }
    
    // Start iteration over locals in function's list
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = first local node, C set if found, NC if no locals
    // Munts: -
    IterateStart()
    {
        PHA
        
        // Get locals list head from function node (IDX preserved)
        LDY # Objects.snLocals
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        // Set carry based on result
        ORA ZP.IDYL // or with ZP.IDYH from above
        if (Z)
        {
            CLC  // No locals
        }
        else
        {
            SEC  // Found first local
        }
        
        PLA
        // IDX unchanged - still points to function node
    }
    
    // Continue local iteration
    // Input: ZP.IDY = current local node
    // Output: ZP.IDY = next local node, C set if found, NC if end of locals
    // Munts: ZP.LCURRENT
    IterateNext()
    {
        // Get next pointer from current local node
        LDY # lnNext
        LDA [ZP.IDY], Y
        STA ZP.LCURRENTL
        INY
        LDA [ZP.IDY], Y
        STA ZP.IDYH
        
        LDA ZP.LCURRENTL
        STA ZP.IDYL
        
        // Set carry based on result
        ORA ZP.IDYH // or with ZP.IDYL from above
        if (Z)
        {
            CLC  // End of locals
        }
        else
        {
            SEC  // Found next local
        }
    }
    
    // Clear all locals in function's list with proper memory cleanup
    // Input: ZP.IDX = function node address
    // Output: Function's locals field set to null, all local nodes freed, C set (always succeeds)
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
            // Get first local from function node (use saved address)
            LDY #Objects.snLocals
            LDA [ZP.SymbolTemp0], Y
            STA ZP.IDXL
            INY
            LDA [ZP.SymbolTemp0], Y
            STA ZP.IDXH
            
            // Check if locals list is empty
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Get next pointer from first local (use IDY instead of IDX)
            LDY # lnNext
            LDA [ZP.IDX], Y
            STA ZP.LNEXTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LNEXTH
            
            // Store the next pointer as the new head local
            LDY #Objects.snLocals
            LDA ZP.LNEXTL
            STA [ZP.SymbolTemp0], Y
            INY
            LDA ZP.LNEXTH
            STA [ZP.SymbolTemp0], Y
            
            // Free the current local node
            Memory.Free(); // Input: ZP.IDX, Munts: ZP.IDX, ZP.M* -> C on success
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
    
    // Internal helper: Calculate required local node size
    // Input: ZP.SymbolName = local name pointer
    // Output: ZP.ACC = total node size (16-bit), ZP.SymbolLength = name length including null
    // Munts: -
    calculateNodeSize()
    {
        // Start with fixed overhead
        LDA # localOverhead
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
    
    // Internal helper: Initialize fields in newly allocated local node
    // Input: ZP.IDX = node address
    //        ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    // Note: Next pointer will be set by caller
    // Munts: -
    initializeNode()
    {
        // Copy name string starting at anName
        copyNameToNode();
    }
    
    // Internal helper: Copy local name string to node
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
        ADC # lnName
        STA ZP.FDESTINATIONADDRESSL
        LDA ZP.IDXH
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // Use pre-calculated length
        LDA ZP.SymbolLength
        STA ZP.FLENGTHL
        STZ ZP.FLENGTHH
        
        // Copy the string using Tools.CopyBytes
        Memory.Copy();
    }
    
    // Internal helper: Compare local names
    // Input: ZP.LCURRENT = local node address, ZP.TOP = target name
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
        ADC # lnName
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
    
    // Resolve local/argument by name in current function context
    // Input: ZP.TOP = name pointer
    // Output: C = found, ZP.IDX = node address if found, BP offset in ZP.ACCL (signed byte), type in ACCT
    // Modifies: ZP.IDX, ZP.LCURRENT
    Resolve()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            InFunction();     // Are we in a function?
            if (NC)
            {
                CLC  // Not found - not in function context
                break;
            }
            
            // Get the function node being compiled
            LDA (Compiler.compilerSavedNodeAddrL + 0)
            STA ZP.IDXL
            LDA (Compiler.compilerSavedNodeAddrH + 0)
            STA ZP.IDXH
            
            // Look for local/argument by name
            Find();  // Returns BP offset in ACCL directly!
            if (C)
            {
                // Found - ZP.ACCL already has BP offset
                // ZP.IDY has node address, copy to IDX
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                
                LDA #IdentifierType.Local  // Works for both locals and arguments
                STA ZP.ACCT
                SEC  // Found
                break;
            }
            
            CLC  // Not found
            break;
        }
        
        PLY
        PLX
        PLA
    }
    // Remove the last local/argument from the function's list
    // Input: ZP.IDX = function node address
    // Output: C set if successful, NC if list was empty
    // Munts: ZP.LCURRENT, ZP.LPREVIOUS, ZP.IDYL, ZP.IDYH
    RemoveLast()
    {
        PHA
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // Single exit
        {
            // Get locals/arguments list head from function node
            LDY #Objects.snLocals
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            // Check if list is empty
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z)
            {
                CLC  // List was empty
                break;
            }
            
            // Initialize previous pointer to point to head pointer in function node
            CLC
            LDA ZP.IDXL
            ADC #Objects.snLocals
            STA ZP.LPREVIOUSL
            LDA ZP.IDXH
            ADC #0
            STA ZP.LPREVIOUSH
            
            // Walk to the end of the list
            loop
            {
                // Check if current node has a next
                LDY #lnNext
                LDA [ZP.LCURRENT], Y
                STA ZP.IDYL  // Save next pointer low
                INY
                LDA [ZP.LCURRENT], Y
                STA ZP.IDYH  // Save next pointer high
                
                // Is this the last node?
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z)
                {
                    // This is the last node - remove it
                    // Update previous->next to NULL
                    LDY #0
                    LDA #0
                    STA [ZP.LPREVIOUS], Y
                    INY
                    STA [ZP.LPREVIOUS], Y
                    
                    // Free the node
                    LDA ZP.LCURRENTL
                    STA ZP.IDXL
                    LDA ZP.LCURRENTH
                    STA ZP.IDXH
                    Memory.Free(); // Input: ZP.IDX, Munts: ZP.IDX, ZP.M* -> C on success
                    break;
                }
                
                // Not the last - move forward
                LDA ZP.LCURRENTL
                STA ZP.LPREVIOUSL
                LDA ZP.LCURRENTH
                STA ZP.LPREVIOUSH
                
                LDA ZP.IDYL
                STA ZP.LCURRENTL
                LDA ZP.IDYH
                STA ZP.LCURRENTH
            }
            break;
        }
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        
        PLY
        PLA
    }
    
    // Count the number of locals/arguments in the function's list
    // Input: ZP.IDX = function node address
    // Output: A = count, C set (always succeeds)
    // Munts: ZP.LCURRENT
    Count()
    {
        PHX
        PHY
        
        // Get locals/arguments list head from function node
        LDY #Objects.snLocals
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTL
        INY
        LDA [ZP.IDX], Y
        STA ZP.LCURRENTH
        
        // Count nodes
        LDX #0  // Counter
        loop
        {
            // Check if we've reached end of list
            LDA ZP.LCURRENTL
            ORA ZP.LCURRENTH
            if (Z)
            {
                TXA  // Return count in A
                SEC  // Success
                break;
            }
            
            // Move to next node
            INX  // Increment count
            LDY #lnNext
            LDA [ZP.LCURRENT], Y
            PHA
            INY
            LDA [ZP.LCURRENT], Y
            STA ZP.LCURRENTH
            PLA
            STA ZP.LCURRENTL
        }
        
        PLY
        PLX
    }
}
