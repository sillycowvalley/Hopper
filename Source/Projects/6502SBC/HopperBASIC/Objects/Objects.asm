unit Objects
{
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
    // Offset 7-8: opcode stream pointer (16-bit - for functions, unused for variables/constants)
    // Offset 9+:  null-terminated name string
    
    const byte symbolOverhead = 9;       // Fixed fields before name (increased from 7)
    const byte snNext = 0;               // Next pointer offset (2 bytes)
    const byte snType = 2;               // symbolType|dataType field offset
    const byte snFlags = 2;              // flags for functions
    const byte snTokens = 3;             // Tokens pointer field offset (2 bytes)
    const byte snValue = 5;              // Value/args field offset (2 bytes)
    const byte snArguments = 5;          // same slot as Values, better name for Function arguments
    const byte snOpCodes = 7;            // OpCode stream pointer offset (2 bytes) - NEW!
    const byte snName = 9;               // Name field offset (variable length) - shifted by 2
    
    // Initialize empty symbol tables
    // Output: ZP.VariableListL/H = 0x0000, ZP.FunctionsListL/H = 0x0000
    Initialize()
    {
        STZ ZP.VariablesListL
        STZ ZP.VariablesListH
        STZ ZP.FunctionsListL
        STZ ZP.FunctionsListH
    }
    
    // Add new symbol to table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.IDY = tokens pointer (16-bit), ZP.NEXT = value/args (16-bit)
    // Output: ZP.IDX = new symbol node address, C set if successful, NC if allocation failed
    // Munts: ZP.LCURRENT, ZP.LHEADX
    Add()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA ZP.NEXTH
        PHA
        LDA ZP.NEXTL
        PHA
        
        loop // start of single exit block
        {
            // Save table head address
            STX ZP.SymbolTemp0
            
            // Save input parameters in dedicated ZP locations that survive Memory.Allocate()
            LDA ZP.ACCT
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
                CLC  // Already clear
                break;
            }
            
            // Initialize the new node using saved parameters
            initializeNode();
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.NEXTH
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLY
        PLX
        PLA
    }
    
    // Find symbol by name
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.TOP = name pointer to search for
    // Output: ZP.IDX = symbol node address, C set if found, NC if not found
    // Munts: ZP.LCURRENT
    Find()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.NEXTH
        PHA
        LDA ZP.NEXTL
        PHA
        
        loop // start of single exit block
        {
            // Start iteration
            Table.GetFirst(); // Returns first node in IDX
            loop
            {
                // Check if we've reached end of list
                LDA ZP.IDXL
                ORA ZP.IDXH
                if (Z)
                {
                    CLC  // Not found
                    break;
                }
                
                // Compare name at snName offset
                compareNames();
                if (C) // Names match
                {
                    break;
                }
                
                // Move to next node
                Table.GetNext(); // Updates IDX
            }
            
            break;
        } // end of single exit block
        
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.NEXTH
        
        PLY
        PLX
        PLA
    }
    
    // Remove symbol by node pointer
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
    //        ZP.IDX = symbol node address (from Find)
    // Output: C set if successful, NC if node not found
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
    Remove()
    {
        Table.Delete();
    }
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value/args
    GetData()
    {
        PHY
        
        // Get symbolType|dataType (offset snType)
        LDY #snType
        LDA [ZP.IDX], Y
        STA ZP.ACCT
        
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
        
        PLY
    }
    
    // Set symbol value (variables only)
    // Input: ZP.IDX = symbol node address, ZP.IDY = new value/args
    // Output: C set if successful, NC if not a variable
    SetValue()
    {
        PHA
        PHY
        
        loop // start of single exit block
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
                break;
            }
            
            // Update value (offset snValue to snValue+1)
            LDY #snValue
            LDA ZP.IDYL
            STA [ZP.IDX], Y
            INY
            LDA ZP.IDYH
            STA [ZP.IDX], Y
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLY
        PLA
    }
    
    // Get tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address
    // Output: ZP.IDY = tokens pointer (16-bit)
    GetTokens()
    {
        PHY
        
        // Get tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        PLY
    }
    
    // Set tokens pointer for symbol node
    // Input: ZP.IDX = symbol node address, ZP.IDY = new tokens pointer
    SetTokens()
    {
        PHA
        PHY
        
        // Store tokens pointer (offset snTokens to snTokens+1)
        LDY #snTokens
        LDA ZP.IDYL
        STA [ZP.IDX], Y
        INY
        LDA ZP.IDYH
        STA [ZP.IDX], Y
        
        PLY
        PLA
    }
    
    // Destroy entire symbol table
    // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList)
    // Output: C set (always succeeds)
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
    Destroy()
    {
        Table.Clear();
    }
    
    // Start iteration with type filter
    // Input: X = ZP address of table head, ZP.SymbolIteratorFilter = filter (0 = all, or specific symbolType)
    // Output: ZP.IDX = first matching symbol (0x0000 if none), C set if found
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateStart()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop // start of single exit block
        {
            // Get first node
            Table.GetFirst(); // Returns first node in IDX
            
            // Check if list is empty
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // Empty list
                break;
            }
            // Check if first node matches filter (Filter is already in ZP.SymbolIteratorFilter)
            findNextMatch();
            break;
        } // end of single exit block
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }
    
    // Continue iteration with filter
    // Input: None (uses saved filter from IterateStart)
    // Output: ZP.IDX = next matching symbol (0x0000 if no more), C set if found
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateNext()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop // start of single exit block
        {
            // Check if we're already at end
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // Already at end
                break;
            }
            
            // Move to next node
            Table.GetNext(); // Updates IDX
            if (NC)
            {
                CLC  // End of list
                break;
            }
            /*
            // Check if we've reached end
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // End of list
                break;
            }
            */
            
            // Check if node matches filter (Filter is already in ZP.SymbolIteratorFilter)
            findNextMatch();
            break;
        } // end of single exit block
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }
    
    
    // Private: Calculate required node size
    // Input: ZP.SymbolNameL/H = name pointer
    // Output: ZP.ACC = total node size (16-bit), ZP.SymbolLength = name length
    calculateNodeSize()
    {
        PHX
        PHY
        
        // Get name length
        LDX ZP.SymbolNameL
        LDY ZP.SymbolNameH
        Tools.StringLength(); // Returns length in A, preserves X,Y
        STA ZP.SymbolLength
        
        // Add name length to fixed overhead
        CLC
        ADC #symbolOverhead
        STA ZP.ACCL
        
        LDA #0
        ADC #0  // Add carry
        STA ZP.ACCH
        
        // Add 1 for null terminator
        INC ZP.ACCL
        if (Z)
        {
            INC ZP.ACCH
        }
        
        PLY
        PLX
    }
    
    // Private: Initialize node fields
    // Input: ZP.IDX = new node address, ZP.Symbol* = saved parameters
    initializeNode()
    {
        PHY
        
        // Store symbolType|dataType (offset snType)
        LDA ZP.SymbolType
        LDY #snType
        STA [ZP.IDX], Y
        
        // Store tokens pointer (offset snTokens to snTokens+1)
        LDA ZP.SymbolTokensL
        LDY #snTokens
        STA [ZP.IDX], Y
        LDA ZP.SymbolTokensH
        INY
        STA [ZP.IDX], Y
        
        // Store value/args (offset snValue to snValue+1)
        LDA ZP.SymbolValueL
        LDY #snValue
        STA [ZP.IDX], Y
        LDA ZP.SymbolValueH
        INY
        STA [ZP.IDX], Y
        
        // Copy name to node
        copyNameToNode();
        
        PLY
    }
    
    // Private: Copy name string to node
    // Input: ZP.IDX = node address, ZP.SymbolNameL/H = source name, ZP.SymbolLength = length
    copyNameToNode()
    {
        PHA
        PHX
        PHY
        
        // Set up for copy
        LDA ZP.SymbolNameL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.SymbolNameH
        STA ZP.FSOURCEADDRESSH
        
        // Calculate destination address (node + snName)
        CLC
        LDA ZP.IDXL
        ADC #snName
        STA ZP.FDESTINATIONADDRESSL
        LDA ZP.IDXH
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // Copy length (already in ZP.SymbolLength)
        LDA ZP.SymbolLength
        STA ZP.FLENGTHL
        STZ ZP.FLENGTHH
        
        // Add 1 for null terminator
        INC ZP.FLENGTHL
        if (Z)
        {
            INC ZP.FLENGTHH
        }
        
        Tools.CopyBytes(); // FSOURCEADDRESS=src, FDESTINATIONADDRESS=dst, FLENGTH=count
        
        PLY
        PLX
        PLA
    }
    
    // Private: Compare symbol name with search name
    // Input: ZP.IDX = current node, ZP.TOP = search name
    // Output: C if names match, NC if different
    compareNames()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        
        // Calculate address of name in node (node + snName)
        CLC
        LDA ZP.IDXL
        ADC #snName
        STA ZP.NEXTL
        LDA ZP.IDXH
        ADC #0
        STA ZP.NEXTH
        
        // Compare strings
        Tools.StringCompare(); // TOP vs NEXT, sets C if equal
        
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    // Private: Find next node matching filter
    // Input: ZP.IDX = current node, ZP.SymbolIteratorFilter = filter
    // Output: ZP.IDX = matching node or next node, C set if match found
    findNextMatch()
    {
        PHY
        
        loop
        {
            // Check if filter is 0 (match all)
            LDA ZP.SymbolIteratorFilter
            if (Z)
            {
                SEC  // Match all
                break;
            }
            
            // Get symbol type from current node
            LDY #snType
            LDA [ZP.IDX], Y
            AND #0xF0  // Extract high nibble
            LSR LSR LSR LSR  // Shift to low nibble
            
            // Compare with filter
            CMP ZP.SymbolIteratorFilter
            if (Z)
            {
                SEC  // Match found
                break;
            }
            
            // No match, try next node
            Table.GetNext();
            
            // Check if we've reached end
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                CLC  // End of list
                break;
            }
            
            // Continue searching
        }
        
        PLY
    }
}
