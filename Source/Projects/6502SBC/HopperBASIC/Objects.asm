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
        FUNCTION = 0x03,   // Executable code blocks
        ARGUMENT = 0x04    // Function parameters
    }
    
    // Node layout (offsets from node start):
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2: symbolType|dataType (packed byte)
    // Offset 3-4: value/address (16-bit)
    // Offset 5-6: tokens pointer (16-bit pointer to initialization token stream)
    // Offset 7+: null-terminated name string
    
    const byte symbolOverhead = 7;       // Fixed fields before name (including Table's next pointer)
    const byte typeOffset = 2;           // Offset to symbolType|dataType field
    const byte valueOffset = 3;          // Offset to value field
    const byte tokensOffset = 5;         // Offset to tokens pointer field
    const byte nameOffset = 7;           // Offset to name field in node
    
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
    //        ZP.NEXT = value (16-bit), ZP.IDY = tokens pointer (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, X, Y
    // Munts: ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT, ZP.IDY (due to Table.Add call)
    // Uses: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName, ZP.SymbolLength for temporary storage
    Add()
    {
        PHA
        PHX
        PHY
        
        // Save input parameters in dedicated ZP locations that survive Memory.Allocate()
        LDA ZP.ACCL
        STA ZP.SymbolType   // Save symbolType|dataType
        
        LDA ZP.NEXTL
        STA ZP.SymbolValueL // Save value low
        LDA ZP.NEXTH
        STA ZP.SymbolValueH // Save value high
        
        LDA ZP.TOPL
        STA ZP.SymbolNameL  // Save name pointer low
        LDA ZP.TOPH
        STA ZP.SymbolNameH  // Save name pointer high
        
        // Save tokens pointer using dedicated ZP locations
        LDA ZP.IDYL
        STA ZP.SymbolTokensL
        LDA ZP.IDYH
        STA ZP.SymbolTokensH
        
        // Calculate node size
        calculateNodeSize(); // Returns size in ZP.ACC, sets ZP.SymbolLength
        
        // Add to table (size already in ZP.ACC)
        LDX #ZP.SymbolList  // Address of list head pointer
        Table.Add(); // Returns new node in IDX, C set if successful
        if (NC)  // Allocation failed
        {
            PLY
            PLX
            PLA
            return;  // C already clear
        }
        
        // Initialize the new node using saved parameters
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
        LDX #ZP.SymbolList
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
        LDX #ZP.SymbolList
        Table.Delete();
    }
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACC = symbolType|dataType (packed), ZP.NEXT = value, ZP.IDY = tokens pointer
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
        
        // Get tokens pointer (offset tokensOffset to tokensOffset+1)
        LDY #tokensOffset
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
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
    
    // Get tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address
    // Output: ZP.IDY = tokens pointer
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    GetTokens()
    {
        // Get tokens pointer (offset tokensOffset to tokensOffset+1)
        LDY #tokensOffset
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
    }
    
    // Set tokens pointer in symbol node
    // Input: ZP.IDX = symbol node address, ZP.IDY = new tokens pointer
    // Output: C set if successful
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    SetTokens()
    {
        // Set tokens pointer (offset tokensOffset to tokensOffset+1)
        LDY #tokensOffset
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
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
        LDX #ZP.SymbolList
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
        LDX #ZP.SymbolList
        Table.Clear();
    }
    
    // Internal helper: Calculate required node size
    // Input: ZP.SymbolName = name pointer
    // Output: ZP.ACC = total node size (16-bit), ZP.SymbolLength = name length including null
    calculateNodeSize()
    {
        // Start with fixed overhead
        LDA #symbolOverhead
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
    
    // Internal helper: Initialize fields in newly allocated node
    // Input: ZP.IDX = node address, ZP.SymbolType = packed symbolType|dataType, 
    //        ZP.SymbolValue = value, ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    //        ZP.SymbolTokens = tokens pointer
    // Note: Next pointer at offset 0-1 already initialized by Table.Add()
    initializeNode()
    {
        // Set symbolType|dataType (offset typeOffset)
        LDY #typeOffset
        LDA ZP.SymbolType
        STA [ZP.IDX], Y
        
        // Set value (offset valueOffset to valueOffset+1)
        LDY #valueOffset
        LDA ZP.SymbolValueL
        STA [ZP.IDX], Y
        INY
        LDA ZP.SymbolValueH
        STA [ZP.IDX], Y
        
        // Set tokens pointer (offset tokensOffset to tokensOffset+1)
        LDY #tokensOffset
        LDA ZP.SymbolTokensL
        STA [ZP.IDX], Y
        INY
        LDA ZP.SymbolTokensH
        STA [ZP.IDX], Y
        
        // Copy name string starting at nameOffset
        copyNameToNode();
    }
    
    // Internal helper: Copy name string to node
    // Input: ZP.IDX = node address, ZP.SymbolName = name pointer, ZP.SymbolLength = name length
    copyNameToNode()
    {
        // Set up source address (name pointer)
        LDA ZP.SymbolNameL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.SymbolNameH
        STA ZP.FSOURCEADDRESSH
        
        // Set up destination address (node + nameOffset)
        CLC
        LDA ZP.IDXL
        ADC #nameOffset
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
    
    // Internal helper: Compare names
    // Input: ZP.IDX = node address, ZP.TOP = target name
    // Output: Z set if equal, NZ if different
    compareNames()
    {
        // Use available ZP slots from our 0x77-0x7F range for temporary storage
        // Calculate address of name field in node and store in 0x77-0x78
        CLC
        LDA ZP.IDXL
        ADC #nameOffset
        STA 0x77            // Temporary storage for node name pointer low
        LDA ZP.IDXH
        ADC #0
        STA 0x78            // Temporary storage for node name pointer high
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y     // Target name character
            STA 0x79            // Temporary storage for comparison
            LDA [0x77], Y       // Node name character (using temp pointer)
            CMP 0x79            // Compare
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