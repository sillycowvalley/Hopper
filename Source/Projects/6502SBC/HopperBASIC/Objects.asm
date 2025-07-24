unit Objects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "BasicTypes"
    uses "Table"
    uses "Tools"
    
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
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2: symbolType|dataType (packed byte)
    // Offset 3-4: value/address (16-bit)
    // Offset 5+: null-terminated name string
    
    const byte symbolOverhead = 5;       // Fixed fields before name (including Table's next pointer)
    const byte typeOffset = 2;           // Offset to symbolType|dataType field
    const byte valueOffset = 3;          // Offset to value field
    const byte nameOffset = 5;           // Offset to name field in node
    
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
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Add call)
    // Uses: ZP.Lxx variables as temporary workspace
    Add()
    {
        PHA
        PHX
        PHY
        
        // Save input parameters before Table.Add munts them
        LDA ZP.ACCL
        STA ZP.LTYPE        // Save symbolType|dataType
        
        LDA ZP.NEXTL
        STA ZP.LNEXTL       // Save value low
        LDA ZP.NEXTH
        STA ZP.LNEXTH       // Save value high
        
        LDA ZP.TOPL
        STA ZP.LPREVIOUSL   // Save name pointer low
        LDA ZP.TOPH
        STA ZP.LPREVIOUSH   // Save name pointer high
        
        // Calculate node size
        calculateNodeSize(); // Returns size in ZP.LLENGTH, uses name at ZP.LPREVIOUS
        
        // Move size to ACC for Table.Add()
        LDA ZP.LLENGTHL
        STA ZP.ACCL
        LDA ZP.LLENGTHH
        STA ZP.ACCH
        
        // Add to table
        LDX # ZP.SymbolList  // Address of list head pointer
        Table.Add(); // Returns new node in IDX, C set if successful
        if (NC)  // Allocation failed
        {
            PLY
            PLX
            PLA
            return;  // C already clear
        }
        
        // Initialize the new node
        initializeNode();
        
        
        PLY
        PLX
        PLA
        SEC  // Signal success
    }
    
    // Find symbol by name
    // Input: ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Find()
    {
        PHA
        PHX
        PHY
        
        // Start iteration
        LDX # ZP.SymbolList
        Table.GetFirst(); // Returns first node in IDX
        
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
            
            // Compare name at nameOffset
            compareNames();
            if (Z) // Names match
            {
                PLY
                PLX
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next node
            Table.GetNext(); // Updates IDX
        }
    }
    
    // Remove symbol by node pointer
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Delete call)
    // Uses: ZP.Lxx variables as temporary workspace
    Remove()
    {
        LDX # ZP.SymbolList
        Table.Delete();
    }
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACC = symbolType|dataType (packed), ZP.NEXT = value
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP
    GetData()
    {
        // Get symbolType|dataType (offset typeOffset)
        LDY #typeOffset
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        STZ ZP.ACCH  // Clear high byte
        
        // Get value (offset valueOffset to valueOffset+1)
        LDY #valueOffset
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
        // Get symbolType (high nibble of packed byte at typeOffset)
        LDY #typeOffset
        LDA [ZP.IDX], Y
        AND #0xF0  // Extract high nibble
        LSR LSR LSR LSR  // Shift to low nibble
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            CLC  // Not a variable
            return;
        }
        
        // Update value (offset valueOffset to valueOffset+1)
        LDY #valueOffset
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
        PHA
        PHX
        PHY
        
        // Start at beginning of list
        LDX # ZP.SymbolList
        Table.GetFirst();
        
        // Find first matching symbol
        findNextMatch();
        
        PLY
        PLX
        PLA
    }
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.ACC = type filter
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext()
    {
        PHA
        PHX
        PHY
        
        // Move to next node
        Table.GetNext();
        
        // Find next matching symbol
        findNextMatch();
        
        PLY
        PLX
        PLA
    }
    
    // Destroy entire symbol table
    // Output: ZP.SymbolListL/H = 0x0000
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Clear call)
    Destroy()
    {
        LDX # ZP.SymbolList
        Table.Clear();
    }
    
    // Internal helper: Calculate required node size
    // Input: ZP.LPREVIOUS = name pointer
    // Output: ZP.LLENGTH = total node size (16-bit)
    calculateNodeSize()
    {
        // Start with fixed overhead
        LDA #symbolOverhead
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        
        // Add name length + null terminator
        LDY #0
        loop
        {
            LDA [ZP.LPREVIOUS], Y
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
    // Input: ZP.IDX = node address, ZP.LTYPE = packed symbolType|dataType, 
    //        ZP.LNEXT = value, ZP.LPREVIOUS = name pointer
    // Note: Next pointer at offset 0-1 already initialized by Table.Add()
    initializeNode()
    {
        // Set symbolType|dataType (offset typeOffset)
        LDY # typeOffset
        LDA ZP.LTYPE
        STA [ZP.IDX], Y
        
        // Set value (offset valueOffset to valueOffset+1)
        LDY # valueOffset
        LDA ZP.LNEXTL
        STA [ZP.IDX], Y
        INY
        LDA ZP.LNEXTH
        STA [ZP.IDX], Y
        
        // Copy name string starting at nameOffset
        copyNameToNode();
    }
    
    // Internal helper: Copy name string to node
    // Input: ZP.IDX = node address, ZP.LPREVIOUS = name pointer
    copyNameToNode()
    {
        LDY # nameOffset     // Destination index
        STZ ZP.LCOUNTL       // Source index counter
        
        loop
        {
            // Get character from name
            PHY  // Save destination offset
            LDY ZP.LCOUNTL
            LDA [ZP.LPREVIOUS], Y
            PLY  // Restore destination offset
            
            // Store at destination
            STA [ZP.IDX], Y
            if (Z) { break; } // Copied null terminator
            
            INC ZP.LCOUNTL       // Increment source index
            INY                  // Increment destination index
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
        ADC #nameOffset
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
            
            // Get symbol type from current node (high nibble of typeOffset)
            LDY #typeOffset
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
