unit Objects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "BasicTypes"
    uses "Table"
    uses "Tools"
    
    friend Variables, Functions, Arguments;
    
    // Symbol table implementation using Table foundation
    // ZP.VariableListL/H stores the variables/constants table head pointer
    // ZP.FunctionsListL/H stores the functions table head pointer
    
    // Symbol types
    enum SymbolType
    {
        VARIABLE = 0x01,   // Mutable values
        CONSTANT = 0x02,   // Immutable values  
        FUNCTION = 0x03,   // Executable code blocks
        ARGUMENT = 0x04    // Function parameters
    }
    
    // Symbol node memory map:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   symbolType|dataType (packed byte)
    // Offset 3-4: tokens pointer (16-bit pointer to initialization/body token stream)
    // Offset 5-6: value/address (16-bit - value for variables/constants, args list for functions)
    // Offset 7+:  null-terminated name string
    
    const byte symbolOverhead = 7;       // Fixed fields before name (including Table's next pointer)
    const byte snNext = 0;               // Next pointer offset (2 bytes)
    const byte snType = 2;               // symbolType|dataType field offset
    const byte snTokens = 3;             // Tokens pointer field offset (2 bytes)
    const byte snValue = 5;              // Value/args field offset (2 bytes)
    const byte snName = 7;               // Name field offset (variable length)
    
    // Initialize empty symbol tables
    // Output: ZP.VariableListL/H = 0x0000, ZP.FunctionsListL/H = 0x0000
    // Preserves: All registers and ZP variables
    Initialize()
    {
        STZ ZP.VariablesListL
        STZ ZP.VariablesListH
        STZ ZP.FunctionsListL
        STZ ZP.FunctionsListH
    }
    
    // Add new symbol to table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
    //        ZP.IDY = tokens pointer (16-bit), ZP.NEXT = value/args (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Preserves: A, Y
    // Munts: X, ZP.IDX, ZP.ACC, ZP.TOP, ZP.NEXT, ZP.IDY (due to Table.Add call)
    // Uses: ZP.SymbolType, ZP.SymbolValue, ZP.SymbolName, ZP.SymbolLength for temporary storage
    Add()
    {
        PHA
        PHY
        
        // Save table head address
        STX ZP.SymbolTemp0
        
        // Save input parameters in dedicated ZP locations that survive Memory.Allocate()
        LDA ZP.ACCL
        STA ZP.SymbolType   // Save symbolType|dataType
        
        LDA ZP.NEXTL
        STA ZP.SymbolValueL // Save value/args low
        LDA ZP.NEXTH
        STA ZP.SymbolValueH // Save value/args high
        
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
        LDX ZP.SymbolTemp0  // Restore table head address
        Table.Add(); // Returns new node in IDX, C set if successful
        if (NC)  // Allocation failed
        {
            PLY
            PLA
            return;  // C already clear
        }
        
        // Initialize the new node using saved parameters
        initializeNode();
        
        PLY
        PLA
        SEC  // Signal success
    }
    
    // Find symbol by name
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Preserves: A, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    // Uses: ZP.Lxx variables as temporary workspace
    Find()
    {
        PHA
        PHY
        
        // Start iteration
        Table.GetFirst(); // Returns first node in IDX
        
        loop
        {
            // Check if we've reached end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                PLY
                PLA
                CLC  // Not found
                return;
            }
            
            // Compare name at snName offset
            compareNames();
            if (Z) // Names match
            {
                PLY
                PLA
                SEC  // Found
                return;
            }
            
            // Move to next node
            Table.GetNext(); // Updates IDX
        }
    }
    
    // Remove symbol by node pointer
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Preserves: A, Y
    // Munts: X, ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Delete call)
    // Uses: ZP.Lxx variables as temporary workspace
    Remove()
    {
        Table.Delete();
    }
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACC = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value/args
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP
    GetData()
    {
        // Get symbolType|dataType (offset snType)
        LDY #snType
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        STZ ZP.ACCH  // Clear high byte
        
        // Get tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
        LDA [ZP.IDX], Y
        STA ZP.NEXTL
        INY
        LDA [ZP.IDX], Y
        STA ZP.NEXTH
        
        // Get value/args (offset snValue to snValue+1)
        LDY #snValue
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
    }
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.IDY = new value/args
    // Output: C set if successful, NC if not a variable
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.ACC
    SetValue()
    {
        // Get symbolType (high nibble of packed byte at snType)
        LDY #snType
        LDA [ZP.IDX], Y
        AND #0xF0  // Extract high nibble
        LSR LSR LSR LSR  // Shift to low nibble
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            CLC  // Not a variable
            return;
        }
        
        // Update value (offset snValue to snValue+1)
        LDY #snValue
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
        STA [ZP.IDX], Y
        
        SEC  // Success
    }
    
    // Get tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address
    // Output: ZP.IDY = tokens pointer
    // Preserves: A, X, Y, ZP.IDX, ZP.TOP, ZP.NEXT, ZP.ACC
    GetTokens()
    {
        // Get tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
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
        // Set tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
        STA [ZP.IDX], Y
        
        SEC  // Success
    }
    
    // Start iteration for specific symbol type
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.ACCL = symbol type filter (0 = all types)
    // Output: ZP.IDX = first matching symbol, C set if found, NC if none
    // Preserves: A, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateStart()
    {
        PHA
        PHY
        
        // Start at beginning of list
        Table.GetFirst();
        
        // Find first matching symbol
        findNextMatch();
        
        PLY
        PLA
    }
    
    // Continue iteration
    // Input: ZP.IDX = current symbol, ZP.ACCL = type filter
    // Output: ZP.IDX = next matching symbol, C set if found, NC if done
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT, ZP.ACC
    IterateNext()
    {
        PHA
        PHY
        
        // Move to next node
        Table.GetNext();
        
        // Find next matching symbol
        findNextMatch();
        
        PLY
        PLA
    }
    
    // Destroy entire symbol table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList)
    // Output: Table head set to 0x0000
    // Preserves: A, Y
    // Munts: X, ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT (due to Table.Clear call)
    Destroy()
    {
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
        // Set symbolType|dataType (offset snType)
        LDY #snType
        LDA ZP.SymbolType
        STA [ZP.IDX], Y
        
        // Set tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
        LDA ZP.SymbolTokensL
        STA [ZP.IDX], Y
        INY
        LDA ZP.SymbolTokensH
        STA [ZP.IDX], Y
        
        // Set value (offset snValue to snValue+1)
        LDY #snValue
        LDA ZP.SymbolValueL
        STA [ZP.IDX], Y
        INY
        LDA ZP.SymbolValueH
        STA [ZP.IDX], Y
        
        // Copy name string starting at snName
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
        
        // Set up destination address (node + snName)
        CLC
        LDA ZP.IDXL
        ADC #snName
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
        ADC #snName
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
            
            // Get symbol type from current node (high nibble of snType)
            LDY #snType
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