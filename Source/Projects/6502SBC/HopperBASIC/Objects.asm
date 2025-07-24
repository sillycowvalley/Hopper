unit Objects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "BasicTypes"
    uses "Table"
    
    // Symbol table implementation using Table foundation
    // ZP.SymbolListL/H stores the global symbol table head pointer
    
    // Symbol types
    enum SymbolType
    {
        VARIABLE = 0x01,   // Mutable values
        CONSTANT = 0x02,   // Immutable values  
        FUNCTION = 0x03    // Executable code blocks
    }
    
    // Node layout (offsets from node start):
    // Offset 0: symbolType|dataType (packed byte)
    // Offset 1: unused (padding)
    // Offset 2-3: value/address (16-bit)
    // Offset 4-5: next pointer (managed by Table unit)
    // Offset 6+: null-terminated name string
    
    const byte SYMBOL_OVERHEAD = 6;    // Fixed fields before name
    const byte NAME_OFFSET = 6;        // Offset to name field in node
    
    // Initialize empty symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: All registers and ZP variables
    Initialize()
    {
        STZ ZP.SymbolListL
        STZ ZP.SymbolListH
    }
    
    // Add new symbol to table
    // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
    //        ZP.NEXT = value (16-bit)  
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y, ZP.TOP, ZP.ACC, ZP.NEXT
    // Uses: ZP.Lxx variables as temporary workspace
    Add()
    {
        // Calculate node size
        calculateNodeSize(); // Returns size in ZP.LLENGTH
        
        // Move size to ACC for Table.Add()
        LDA ZP.LLENGTHL
        STA ZP.ACCL
        LDA ZP.LLENGTHH
        STA ZP.ACCH
        
        // Add to table
        LDX #ZP.SymbolListL  // Address of list head pointer
        Table.Add(); // Returns new node in IDX, Z set if successful
        if (NZ)  // Allocation failed
        {
            CLC  // Signal failure
            return;
        }
        
        // Initialize the new node
        initializeNode();
        
        SEC  // Signal success
    }
    
    // Find symbol by name
    // Input: ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Find()
    {
        // Start iteration
        LDX #ZP.SymbolListL
        Table.GetFirst(); // Returns first node in IDX
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // Not found
                return;
            }
            
            // Compare name at offset 6
            compareNames();
            if (Z) // Names match
            {
                SEC  // Found
                return;
            }
            
            // Move to next node
            Table.GetNext(); // Updates IDX
        }
    }
    
    // Remove symbol by node pointer
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: None (thin wrapper around Table.Delete)  
    // Preserves: A, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Remove()
    {
        LDX #ZP.SymbolListL
        Table.Delete();
    }
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACC = symbolType|dataType (packed), ZP.NEXT = value
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP
    GetData()
    {
        // Get symbolType|dataType (offset 0)
        LDY #0
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        STZ ZP.ACCH  // Clear high byte
        
        // Get value (offset 2-3)
        LDY #2
        LDA [ZP.IDX], Y
        STA ZP.NEXTL
        INY
        LDA [ZP.IDX], Y
        STA ZP.NEXTH
    }
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.NEXT = new value
    // Output: C set if successful, NC if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.ACC
    SetValue()
    {
        // Get symbolType (high nibble of packed byte at offset 0)
        LDY #0
        LDA [ZP.IDX], Y
        AND #0xF0  // Extract high nibble
        LSR LSR LSR LSR  // Shift to low nibble
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            CLC  // Not a variable
            return;
        }
        
        // Update value (offset 2-3)
        LDY #2
        LDA ZP.NEXTL
        STA [ZP.IDX], Y
        INY
        LDA ZP.NEXTH
        STA [ZP.IDX], Y
        
        SEC  // Success
    }
    
    // Start iteration for specific symbol type
    // Input: ZP.ACC = symbol type filter (0 = all types)
    // Output: ZP.IDX = first matching symbol, C set if found, NC if none
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateStart()
    {
        // Start at beginning of list
        LDX #ZP.SymbolListL
        Table.GetFirst();
        
        // Find first matching symbol
        findNextMatch();
    }
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.ACC = type filter
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext()
    {
        // Move to next node
        Table.GetNext();
        
        // Find next matching symbol
        findNextMatch();
    }
    
    // Destroy entire symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    Destroy()
    {
        LDX #ZP.SymbolListL
        Table.Clear();
    }
    
    // Internal helper: Calculate required node size
    // Input: ZP.TOP = name pointer
    // Output: ZP.LLENGTH = total node size (16-bit)
    calculateNodeSize()
    {
        // Start with fixed overhead
        LDA #SYMBOL_OVERHEAD
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        
        // Add name length + null terminator
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y
            if (Z) { break; } // Hit null terminator
            
            INC ZP.LLENGTHL
            if (Z) { INC ZP.LLENGTHH }
            
            INY
        }
        
        // Add 1 for null terminator
        INC ZP.LLENGTHL
        if (Z) { INC ZP.LLENGTHH }
    }
    
    // Internal helper: Initialize fields in newly allocated node
    // Input: ZP.IDX = node address, ZP.ACC = packed symbolType|dataType, ZP.NEXT = value, ZP.TOP = name
    initializeNode()
    {
        // Set symbolType|dataType (offset 0)
        LDY #0
        LDA ZP.ACCL
        STA [ZP.IDX], Y
        
        // Clear padding byte (offset 1)
        INY
        LDA #0
        STA [ZP.IDX], Y
        
        // Set value (offset 2-3)
        INY
        LDA ZP.NEXTL
        STA [ZP.IDX], Y
        INY
        LDA ZP.NEXTH
        STA [ZP.IDX], Y
        
        // Next pointer (offset 4-5) already set by Table.Add()
        
        // Copy name string starting at offset 6
        copyNameToNode();
    }
    
    // Internal helper: Copy name string to node
    copyNameToNode()
    {
        LDY #NAME_OFFSET  // Destination index
        STZ ZP.LPREVIOUSL // Source index counter
        
        loop
        {
            // Get character from name
            PHY  // Save destination offset
            LDY ZP.LPREVIOUSL
            LDA [ZP.TOP], Y
            PLY  // Restore destination offset
            
            // Store at destination
            STA [ZP.IDX], Y
            if (Z) { break; } // Copied null terminator
            
            INC ZP.LPREVIOUSL // Increment source index
            INY              // Increment destination index
        }
    }
    
    // Internal helper: Compare names
    // Input: ZP.IDX = node address, ZP.TOP = target name
    // Output: Z set if equal, NZ if different
    compareNames()
    {
        // Calculate address of name field in node
        CLC
        LDA ZP.IDXL
        ADC #NAME_OFFSET
        STA ZP.LNEXTL
        LDA ZP.IDXH
        ADC #0
        STA ZP.LNEXTH
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y     // Target name character
            STA ZP.LTYPE        // Store temporarily
            LDA [ZP.LNEXT], Y   // Node name character
            CMP ZP.LTYPE        // Compare
            if (NZ) { return; } // Different characters
            
            // Check if we hit null terminator
            if (Z) { return; }  // Both null terminators - strings equal
            
            INY
        }
    }
    
    // Internal helper: Find next matching symbol in iteration
    // Input: ZP.IDX = current position, ZP.ACC = type filter (0 = all)
    // Output: ZP.IDX = next matching symbol, C set if found, NC if none
    findNextMatch()
    {
        loop
        {
            // Check if we've reached end
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // End of list
                return;
            }
            
            // Check type filter
            LDA ZP.ACCL
            if (Z)  // No filter
            {
                SEC  // Return current
                return;
            }
            
            // Get symbol type from current node (high nibble of offset 0)
            LDY #0
            LDA [ZP.IDX], Y
            AND #0xF0  // Extract high nibble
            LSR LSR LSR LSR  // Shift to low nibble
            CMP ZP.ACCL
            if (Z)
            {
                SEC  // Found matching type
                return;
            }
            
            // Move to next node
            Table.GetNext();
        }
    }
}