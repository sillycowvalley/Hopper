unit Arguments
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Table"
    uses "BasicTypes"
    uses "Tools"
    
    // Argument table management - independent of Functions unit
    // Arguments use simplified node structure optimized for argument data
    
    // Argument Node Structure:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   argument type (BasicType.INT, WORD, BIT, etc.)
    // Offset 3+:  null-terminated argument name
    
    const byte argOverhead = 3;     // Fixed fields before name
    const byte argTypeOffset = 2;   // Offset to argument type field
    const byte argNameOffset = 3;   // Offset to name field in argument node
    
    // Create new arguments table
    // Output: ZP.IDX = arguments table head address, C set if successful
    Create()
    {
        // Allocate space for table head pointer (2 bytes)
        LDA #2
        STA ZP.ACCL
        STZ ZP.ACCH
        
        Memory.Allocate();  // Returns address in ZP.IDX
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // Allocation failed
            return;
        }
        
        // Initialize table head to empty (0x0000)
        LDY #0
        LDA #0
        STA [ZP.IDX], Y
        INY
        STA [ZP.IDX], Y
        
        SEC  // Success
    }
    
    // Add argument to arguments table
    // Input: ZP.IDX = arguments table head address, ZP.TOP = argument name, ZP.ACCL = argument type
    // Output: C set if successful, argument added to table
    Add()
    {
        PHA
        PHX
        PHY
        
        // Save input parameters
        LDA ZP.ACCL
        STA ZP.SymbolType     // Reuse for argument type
        
        LDA ZP.TOPL
        STA ZP.SymbolNameL
        LDA ZP.TOPH
        STA ZP.SymbolNameH
        
        // Save table head address
        LDA ZP.IDXL
        STA ZP.SymbolTokensL  // Reuse for table head
        LDA ZP.IDXH
        STA ZP.SymbolTokensH
        
        // Calculate argument node size
        calculateArgumentNodeSize();  // Returns size in ZP.ACC, sets ZP.SymbolLength
        
        // Add to arguments table
        LDX #ZP.SymbolTokens  // Address of table head pointer
        Table.Add();  // Returns new node in IDX, C set if successful
        if (NC)  // Allocation failed
        {
            PLY
            PLX
            PLA
            return;  // C already clear
        }
        
        // Initialize the new argument node
        initializeArgumentNode();
        
        PLY
        PLX
        PLA
        SEC  // Success
    }
    
    // Find argument by name in arguments table
    // Input: ZP.IDX = arguments table head address, ZP.TOP = argument name
    // Output: ZP.IDY = argument node address, ZP.ACCL = argument index, C set if found
    Find()
    {
        PHA
        PHX
        PHY
        
        // Save table head address
        LDA ZP.IDXL
        STA ZP.SymbolTokensL
        LDA ZP.IDXH
        STA ZP.SymbolTokensH
        
        // Start iteration through arguments table
        LDX #ZP.SymbolTokens
        Table.GetFirst();  // Returns first argument node in IDX
        
        STZ ZP.ACCL  // Argument index counter
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
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
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                PLY
                PLX
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next argument
            Table.GetNext();  // Updates IDX
            INC ZP.ACCL       // Increment argument index
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
    // Input: ZP.IDX = arguments table head address, ZP.ACCL = argument index
    // Output: ZP.IDY = argument node address, C set if found
    FindByIndex()
    {
        PHA
        PHX
        PHY
        
        // Save target index
        LDA ZP.ACCL
        STA 0x7A  // Temporary storage
        
        // Start iteration
        LDX #ZP.IDX  // Table head address already in IDX
        Table.GetFirst();
        
        STZ ZP.ACCL  // Current index counter
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
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
            CMP 0x7A
            if (Z)  // Found the target index
            {
                // Copy node address to IDY
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                PLY
                PLX
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next argument
            Table.GetNext();
            INC ZP.ACCL  // Increment current index
        }
    }
    
    // Get argument count in table
    // Input: ZP.IDX = arguments table head address
    // Output: ZP.ACCL = argument count
    GetCount()
    {
        PHA
        PHX
        
        // Start iteration
        LDX #ZP.IDX  // Table head address already in IDX
        Table.GetFirst();
        
        STZ ZP.ACCL  // Argument count
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }  // End of list
            
            INC ZP.ACCL  // Increment count
            Table.GetNext();
        }
        
        PLX
        PLA
    }
    
    // Start iteration over arguments in table
    // Input: ZP.IDX = arguments table head address
    // Output: ZP.IDY = first argument node, C set if found
    IterateStart()
    {
        PHA
        
        // Save table head address
        LDA ZP.IDXL
        STA ZP.SymbolTokensL
        LDA ZP.IDXH
        STA ZP.SymbolTokensH
        
        LDX #ZP.SymbolTokens
        Table.GetFirst();
        
        // Copy result to IDY
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
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
        
        // Copy IDY to IDX for Table.GetNext()
        LDA ZP.IDYL
        STA ZP.IDXL
        LDA ZP.IDYH
        STA ZP.IDXH
        
        Table.GetNext();
        
        // Copy result back to IDY
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
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
    
    // Clear all arguments in table
    // Input: ZP.IDX = arguments table head address
    // Output: Empty arguments table
    Clear()
    {
        LDX #ZP.IDX  // Table head address already in IDX
        Table.Clear();
    }
    
    // Destroy arguments table (free table head storage)
    // Input: ZP.IDX = arguments table head address
    // Output: Table head freed
    Destroy()
    {
        PHA
        
        // First clear all arguments
        Clear();
        
        // Then free the table head storage itself
        LDA ZP.IDXL
        STA ZP.ACCL
        LDA ZP.IDXH
        STA ZP.ACCH
        Memory.Free();
        
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
    // Note: Next pointer at offset 0-1 already initialized by Table.Add()
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
    // Input: ZP.IDX = argument node address, ZP.TOP = target name
    // Output: Z set if equal, NZ if different
    compareArgumentNames()
    {
        // Calculate address of name field in argument node
        CLC
        LDA ZP.IDXL
        ADC #argNameOffset
        STA 0x7B            // Temporary storage for argument name pointer low
        LDA ZP.IDXH
        ADC #0
        STA 0x7C            // Temporary storage for argument name pointer high
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y     // Target name character
            STA 0x7D            // Temporary storage for comparison
            LDA [0x7B], Y       // Argument name character (using temp pointer)
            CMP 0x7D            // Compare
            if (NZ) { return; } // Different characters
            
            // Check if we hit null terminator
            if (Z) { return; }  // Both null terminators - strings equal
            
            INY
        }
    }
}