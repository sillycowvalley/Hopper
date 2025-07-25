unit Functions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "Arguments"
    uses "BasicTypes"
    uses "Messages"
    
    // Function management building on Objects foundation with separate Arguments unit
    // Functions reuse the existing Objects node structure:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   symbolType|returnType (FUNCTION | INT/WORD/BIT/etc)
    // Offset 3-4: arguments table head pointer (reuses value field)
    // Offset 5-6: function body tokens pointer
    // Offset 7+:  null-terminated function name string
    
    // Declare new function
    // Input: ZP.TOP = name pointer, ZP.ACC = FUNCTION|returnType (packed),
    //        ZP.NEXT = arguments table head pointer, ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, C set if successful
    // Note: Arguments table created separately via Arguments.Create()
    Declare()
    {
        PHA
        PHX
        PHY
        
        // Check if function already exists
        LDX #ZP.FunctionsList
        Objects.Find();
        if (C)  // Function already exists
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            CLC  // Error
            PLY
            PLX
            PLA
            return;
        }
        
        // Function doesn't exist, add it
        LDX #ZP.FunctionsList
        Objects.Add();
        
        PLY
        PLX
        PLA
    }
    
    // Find function by name
    // Input: ZP.TOP = name pointer
    // Output: ZP.IDX = function node address, C set if found and is function
    // Error: Sets LastError if found but wrong type
    Find()
    {
        PHA
        PHX
        PHY
        
        // Find the symbol
        LDX #ZP.FunctionsList
        Objects.Find();
        if (NC)  // Not found
        {
            PLY
            PLX
            PLA
            CLC
            return;
        }
        
        // Check if it's a function
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        LDA ZP.ACCL
        AND #0xF0  // Extract symbol type (high nibble)
        LSR LSR LSR LSR  // Shift to low nibble
        
        CMP #SymbolType.FUNCTION
        if (Z)  // It's a function
        {
            PLY
            PLX
            PLA
            SEC  // Found and correct type
            return;
        }
        
        // Wrong type
        LDA #(Messages.TypeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TypeMismatch / 256)
        STA ZP.LastErrorH
        
        PLY
        PLX
        PLA
        CLC  // Error
    }
    
    // Get function signature info
    // Input: ZP.IDX = function node address
    // Output: ZP.ACCL = returnType, ZP.NEXT = arguments table head, ZP.IDY = function body tokens
    GetSignature()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Extract return type (low nibble)
        LDA ZP.ACCL
        AND #0x0F
        STA ZP.ACCL
        
        // ZP.NEXT already contains arguments table head pointer
        // ZP.IDY already contains function body tokens pointer
    }
    
    // Get function body tokens
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = function body tokens pointer
    GetBody()
    {
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
    }
    
    // Get function name
    // Input: ZP.IDX = function node address
    // Output: ZP.TOP = name pointer (points into node data)
    GetName()
    {
        // Calculate address of name field in node
        CLC
        LDA ZP.IDXL
        ADC #Objects.nameOffset
        STA ZP.TOPL
        LDA ZP.IDXH
        ADC #0
        STA ZP.TOPH
    }
    
    // Set arguments table head pointer in function node
    // Input: ZP.IDX = function node address, ZP.NEXT = arguments table head
    // Output: C set if successful
    SetArguments()
    {
        Objects.SetValue();  // Uses ZP.NEXT to set the value field (which stores arguments table head)
    }
    
    // Get arguments table head pointer from function node
    // Input: ZP.IDX = function node address
    // Output: ZP.NEXT = arguments table head pointer, C set if has arguments table
    GetArguments()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Check if arguments table head is non-zero
        LDA ZP.NEXTL
        ORA ZP.NEXTH
        if (Z)
        {
            CLC  // No arguments table
        }
        else
        {
            SEC  // Has arguments table
        }
    }
    
    // Remove function by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful
    // Note: Caller must clear arguments table first via Arguments.Clear()
    Remove()
    {
        // Find the function first
        Find();
        if (NC)  // Not found
        {
            return;  // C already clear
        }
        
        // Get arguments table head before removing function
        GetArguments();
        if (C)  // Has arguments table
        {
            // Save arguments table head
            LDA ZP.NEXTL
            STA ZP.SymbolTokensL
            LDA ZP.NEXTH
            STA ZP.SymbolTokensH
            
            // Clear and destroy arguments table
            LDA ZP.SymbolTokensL
            STA ZP.IDXL
            LDA ZP.SymbolTokensH
            STA ZP.IDXH
            Arguments.Destroy();
            
            // Restore function node address for removal
            LDX #ZP.FunctionsList
            Find();  // Find function again since IDX was changed
        }
        
        // Remove the function
        LDX #ZP.FunctionsList
        Objects.Remove();
    }
    
    // Start iteration over functions only
    // Output: ZP.IDX = first function node, C set if found
    IterateFunctions()
    {
        LDA #SymbolType.FUNCTION
        STA ZP.ACCL
        LDX #ZP.FunctionsList
        Objects.IterateStart();
    }
    
    // Continue function iteration
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next function node, C set if found
    IterateNext()
    {
        Objects.IterateNext();
    }
    
    // Clear all functions
    // Output: Empty function table
    // Note: Caller must clear all arguments tables first
    Clear()
    {
        // Iterate through all functions and clear their arguments tables
        IterateFunctions();
        
        loop
        {
            if (NC) { break; }  // No more functions
            
            // Clear arguments table for this function
            GetArguments();
            if (C)  // Has arguments table
            {
                // Save function node address
                LDA ZP.IDXL
                STA ZP.SymbolTokensL
                LDA ZP.IDXH
                STA ZP.SymbolTokensH
                
                // Get and destroy arguments table
                LDA ZP.NEXTL
                STA ZP.IDXL
                LDA ZP.NEXTH
                STA ZP.IDXH
                Arguments.Destroy();
                
                // Restore function node address
                LDA ZP.SymbolTokensL
                STA ZP.IDXL
                LDA ZP.SymbolTokensH
                STA ZP.IDXH
            }
            
            IterateNext();
        }
        
        // Now clear all function nodes
        LDX #ZP.FunctionsList
        Objects.Destroy();
    }
}