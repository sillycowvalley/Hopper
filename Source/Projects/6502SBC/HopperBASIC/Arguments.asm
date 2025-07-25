unit Arguments
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "BasicTypes"
    uses "Tools"
    
    // Argument table management - arguments list head stored directly in function node
    // No separate "table head storage" - function node field points directly to first argument
    
    // Argument Node Structure:
    // Offset 0-1: next pointer
    // Offset 2:   argument type (BasicType.INT, WORD, BIT, etc.)
    // Offset 3+:  null-terminated argument name
    
    const byte argOverhead = 3;     // Fixed fields before name
    const byte argTypeOffset = 2;   // Offset to argument type field
    const byte argNameOffset = 3;   // Offset to name field in argument node
    
    // Add argument to function's arguments list
    // Input: ZP.IDX = function node address, ZP.TOP = argument name, ZP.ACCL = argument type
    // Output: C set if successful, argument added to function's list
    Add()
    {
        PHA
        PHX
        PHY
        
        // Save input parameters
        LDA ZP.ACCL
        STA ZP.SymbolType     // Argument type
        
        LDA ZP.TOPL
        STA ZP.SymbolNameL    // Argument name pointer
        LDA ZP.TOPH
        STA ZP.SymbolNameH
        
        LDA ZP.IDXL
        STA ZP.SymbolTemp0    // Function node address
        LDA ZP.IDXH
        STA ZP.SymbolTemp1
        
        // Calculate argument node size
        calculateArgumentNodeSize();  // Returns size in ZP.ACC, sets ZP.SymbolLength
        
        // Allocate new argument node
        Memory.Allocate();  // Returns address in ZP.IDX
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            PLY
            PLX
            PLA
            CLC  // Allocation failed
            return;
        }
        
        // Initialize the new argument node
        initializeArgumentNode();
        
        // Get current arguments list head from function node (offset 3-4)
        LDA ZP.SymbolTemp0
        STA ZP.LHEADL
        LDA ZP.SymbolTemp1
        STA ZP.LHEADH
        
        LDY #3  // Arguments field offset in function node
        LDA [ZP.LHEAD], Y
        STA ZP.LCURRENTL  // Current first argument
        INY
        LDA [ZP.LHEAD], Y
        STA ZP.LCURRENTH
        
        // Set new node's next pointer to current first argument
        LDY #0
        LDA ZP.LCURRENTL
        STA [ZP.IDX], Y
        INY
        LDA ZP.LCURRENTH
        STA [ZP.IDX], Y
        
        // Update function node's arguments field to point to new node
        LDY #3
        LDA ZP.IDXL
        STA [ZP.LHEAD], Y
        INY
        LDA ZP.IDXH
        STA [ZP.LHEAD], Y
        
        PLY
        PLX
        PLA
        SEC  // Success
    }
    
    // Find argument by name in function's arguments list
    // Input: ZP.IDX = function node address, ZP.TOP = argument name
    // Output: ZP.IDY = argument node address, ZP.ACCL = argument index, C set if found
    Find()
    {
        PHA
        PHX
        PHY
        
        // Get arguments list head from function node (offset 3-4)
        LDY #3
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
                PLY
                PLX
                PLA
                CLC  // Not found
                return;
            }
            
            // Compare argument name
            compareArgumentNames();
            if (Z)  // Names match
            {
                // Copy node address to IDY
                LDA ZP.LCURRENTL
                STA ZP.IDYL
                LDA ZP.LCURRENTH
                STA ZP.IDYH
                
                PLY
                PLX
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next argument
            LDA ZP.LCURRENTL
            STA ZP.IDXL
            LDA ZP.LCURRENTH
            STA ZP.IDXH
            
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            INC ZP.ACCL  // Increment argument index
        }
    }
    
    // Get argument type from node
    // Input: ZP.IDY = argument node address
    // Output: ZP.ACCL = argument type
    GetType()
    {
        LDY #argTypeOffset
        LDA [ZP.IDY], Y
        STA ZP.ACCL
    }
    
    // Get argument name from node
    // Input: ZP.IDY = argument node address
    // Output: ZP.TOP = argument name pointer
    GetName()
    {
        // Calculate address of name field in argument node
        CLC
        LDA ZP.IDYL
        ADC #argNameOffset
        STA ZP.TOPL
        LDA ZP.IDYH
        ADC #0
        STA ZP.TOPH
    }
    
    // Find argument by index (for BP offset calculation)
    // Input: ZP.IDX = function node address, ZP.ACCL = argument index
    // Output: ZP.IDY = argument node address, C set if found
    FindByIndex()
    {
        PHA
        PHX
        PHY
        
        // Save target index
        LDA ZP.ACCL
        STA ZP.SymbolTemp0
        
        // Get arguments list head from function node
        LDY #3
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
                PLY
                PLX
                PLA
                CLC  // Not found
                return;
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
                
                PLY
                PLX
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next argument
            LDA ZP.LCURRENTL
            STA ZP.IDXL
            LDA ZP.LCURRENTH
            STA ZP.IDXH
            
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
            
            INC ZP.ACCL  // Increment current index
        }
    }
    
    // Get argument count in function's arguments list
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = argument count
    GetCount()
    {
        PHA
        PHX
        
        // Get arguments list head from function node
        LDY #3
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
            
            // Move to next argument
            LDA ZP.LCURRENTL
            STA ZP.IDXL
            LDA ZP.LCURRENTH
            STA ZP.IDXH
            
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTL
            INY
            LDA [ZP.IDX], Y
            STA ZP.LCURRENTH
        }
        
        PLX
        PLA
    }
    
    // Start iteration over arguments in function's list
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = first argument node, C set if found
    IterateStart()
    {
        PHA
        
        // Get arguments list head from function node
        LDY #3
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
    }
    
    // Continue argument iteration
    // Input: ZP.IDY = current argument node
    // Output: ZP.IDY = next argument node, C set if found
    IterateNext()
    {
        PHA
        
        // Get next pointer from current argument node
        LDY #0
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
    
    // Clear all arguments in function's list
    // Input: ZP.IDX = function node address
    // Output: Function's arguments field set to null, all argument nodes freed
    // Preserves: ZP.IDX (function node address)
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
            LDY #3
            LDA [ZP.SymbolTemp0], Y
            STA ZP.ACCL
            INY
            LDA [ZP.SymbolTemp1], Y
            STA ZP.ACCH
            
            // Check if arguments list is empty
            LDA ZP.ACCL
            ORA ZP.ACCH
            if (Z) { break; }  // No more arguments
            
            // Get next pointer from first argument (use IDY instead of IDX)
            LDY #0
            LDA [ZP.ACC], Y
            STA ZP.LNEXTL
            INY
            LDA [ZP.ACC], Y
            STA ZP.LNEXTH
            
            // Store the next pointer as the new head argument
            LDY #3
            LDA ZP.LNEXTL
            STA [ZP.SymbolTemp0], Y
            INY
            LDA ZP.LNEXTH
            STA [ZP.SymbolTemp1], Y
            
            // Free the current argument node
            Memory.Free();  // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
        }
        
        // Restore function node address for caller
        LDA ZP.SymbolTemp0
        STA ZP.IDXL
        LDA ZP.SymbolTemp1
        STA ZP.IDXH
        
        PLY
        PLX
        PLA
    }
    
    // Internal helper: Calculate required argument node size
    // Input: ZP.SymbolName = argument name pointer
    // Output: ZP.ACC = total node size (16-bit), ZP.SymbolLength = name length including null
    calculateArgumentNodeSize()
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
    // Input: ZP.IDX = node address, ZP.SymbolType = argument type,
    //        ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    // Note: Next pointer will be set by caller
    initializeArgumentNode()
    {
        // Set argument type (offset argTypeOffset)
        LDY #argTypeOffset
        LDA ZP.SymbolType
        STA [ZP.IDX], Y
        
        // Copy name string starting at argNameOffset
        copyArgumentNameToNode();
    }
    
    // Internal helper: Copy argument name string to node
    // Input: ZP.IDX = node address, ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    copyArgumentNameToNode()
    {
        // Set up source address (name pointer)
        LDA ZP.SymbolNameL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.SymbolNameH
        STA ZP.FSOURCEADDRESSH
        
        // Set up destination address (node + argNameOffset)
        CLC
        LDA ZP.IDXL
        ADC #argNameOffset
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
    // Output: Z set if equal, NZ if different
    compareArgumentNames()
    {
        // Calculate address of name field in argument node
        CLC
        LDA ZP.LCURRENTL
        ADC #argNameOffset
        STA ZP.LPREVIOUSL    // Use LPREVIOUS for argument name pointer
        LDA ZP.LCURRENTH
        ADC #0
        STA ZP.LPREVIOUSH
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y      // Target name character
            STA ZP.LNEXTL        // Temporary storage for comparison
            LDA [ZP.LPREVIOUS], Y // Argument name character
            CMP ZP.LNEXTL        // Compare
            if (NZ) { return; }  // Different characters
            
            // Check if we hit null terminator
            if (Z) { return; }   // Both null terminators - strings equal
            
            INY
        }
    }
}
