unit Functions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "Arguments"
    uses "BasicTypes"
    uses "Messages"
    
    // Function management building on Objects foundation
    // Functions use the existing Objects node structure:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   symbolType|returnType (FUNCTION | INT/WORD/BIT/etc)
    // Offset 3-4: arguments list head pointer (points directly to first argument node)
    // Offset 5-6: function body tokens pointer
    // Offset 7+:  null-terminated function name string
    //
    // Arguments list is stored separately and managed by Arguments unit
    
    // Declare new function
    // Input: ZP.TOP = name pointer, ZP.ACC = FUNCTION|returnType (packed),
    //        ZP.NEXT = arguments list head pointer, ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, C set if successful
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
    // Output: ZP.ACCL = returnType, ZP.NEXT = arguments list head, ZP.IDY = function body tokens
    GetSignature()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Extract return type (low nibble)
        LDA ZP.ACCL
        AND #0x0F
        STA ZP.ACCL
        
        // ZP.IDY already contains function body tokens pointer
        // ZP.NEXT already contains arguments list head pointer
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
    
    // Set arguments list head pointer in function node
    // Input: ZP.IDX = function node address, ZP.NEXT = arguments list head
    // Output: C set if successful
    SetArguments()
    {
        Objects.SetValue();  // Uses ZP.NEXT to set the value field (which stores arguments list head)
    }
    
    // Get arguments list head pointer from function node
    // Input: ZP.IDX = function node address
    // Output: ZP.NEXT = arguments list head pointer, C set if has arguments
    GetArguments()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Check if arguments list head is non-zero
        LDA ZP.NEXTL
        ORA ZP.NEXTH
        if (Z)
        {
            CLC  // No arguments
        }
        else
        {
            SEC  // Has arguments
        }
    }
    
    // Remove function by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful
    // Note: This automatically clears all arguments since they're referenced from the function node
    Remove()
    {
        // Find the function first
        Find();
        if (NC)  // Not found
        {
            return;  // C already clear
        }
        
        // Clear all arguments before removing function
        Arguments.Clear();
        
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
    // Note: This clears all arguments and function body tokens before destroying function nodes
    Clear()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            LDX # ZP.FunctionsList
            Table.GetFirst();
            
            if (NC) { break; }  // No more functions
            
            // Clear all arguments for this function (NOP if arguments list head pointer is null)
            Arguments.Clear();
            
            // Get function body tokens pointer and free it if non-zero
            Functions.GetBody();  // Returns tokens pointer in ZP.IDY (correct for function nodes!)
            
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Non-zero tokens pointer
            {
                LDA ZP.IDYL
                STA ZP.ACCL
                LDA ZP.IDYH
                STA ZP.ACCH
                
                // Before Memory.Free()
                LDA #'T'
                Serial.WriteChar();
                LDA #':'
                Serial.WriteChar();
                LDA ZP.ACCH
                Serial.HexOut();
                LDA ZP.ACCL
                Serial.HexOut();
                LDA #' '
                Serial.WriteChar();
                
                Memory.Free();  // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
                               
                // Re-establish function node address after Memory.Free munts everything
                LDX #ZP.FunctionsList
                Table.GetFirst();
            }
            
            // Delete the function node
            LDX #ZP.FunctionsList
            Table.Delete();
        }
        
        PLY
        PLX
        PLA
    }
}
