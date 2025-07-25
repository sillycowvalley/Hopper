unit Variables
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Objects"
    uses "BasicTypes"
    uses "Messages"
    
    // Variable management using Objects foundation
    // Two-stage approach: Find name to address, then operate on address
    
    // Declare new variable or constant
    // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
    //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Uses: Objects.Add() internally with extended node layout
    Declare()
    {
        PHA
        PHX
        PHY
        
        // Check if symbol already exists
        Objects.Find();
        if (C)  // Symbol already exists
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
        
        // Symbol doesn't exist, add it
        Objects.Add();
        
        PLY
        PLX
        PLA
    }
    
    // Find variable/constant name to address
    // Input: ZP.TOP = name pointer, ZP.ACCL = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, C set if found and correct type, NC if not found or wrong type
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Sets LastError if found but wrong type
    Find()
    {
        PHA
        PHX
        PHY
        
        // Find the symbol
        Objects.Find();
        if (NC)  // Not found
        {
            PLY
            PLX
            PLA
            CLC
            return;
        }
        
        // Check if type filtering is requested
        LDA ZP.ACCL
        if (Z)  // No type filter
        {
            PLY
            PLX
            PLA
            SEC  // Found
            return;
        }
        
        // Get symbol type and check
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        LDA ZP.ACCL
        AND #0xF0  // Extract symbol type (high nibble)
        LSR LSR LSR LSR  // Shift to low nibble
        
        // Save expected type for comparison
        LDA ZP.ACCL  // This was the expected type passed in
        STA 0x7A     // Temporary storage
        
        // Get actual type
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        
        CMP 0x7A     // Compare with expected type
        if (Z)       // Types match
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
    
    // Get variable/constant value by address
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetValue()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Check if it's a variable or constant
        LDA ZP.ACCL
        AND #0xF0  // Extract symbol type (high nibble)
        LSR LSR LSR LSR  // Shift to low nibble
        
        CMP #SymbolType.VARIABLE
        if (Z)
        {
            // Copy value to TOP and extract data type
            LDA ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.NEXTH
            STA ZP.TOPH
            
            LDA ZP.ACCL
            AND #0x0F  // Extract data type (low nibble)
            STA ZP.TOPT
            
            SEC  // Success
            return;
        }
        
        CMP #SymbolType.CONSTANT
        if (Z)
        {
            // Copy value to TOP and extract data type
            LDA ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.NEXTH
            STA ZP.TOPH
            
            LDA ZP.ACCL
            AND #0x0F  // Extract data type (low nibble)
            STA ZP.TOPT
            
            SEC  // Success
            return;
        }
        
        // Not a variable or constant
        LDA #(Messages.TypeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TypeMismatch / 256)
        STA ZP.LastErrorH
        CLC  // Error
    }
    
    // Set variable value by address (variables only)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value
    // Output: C set if successful, NC if error (not a variable, type mismatch)
    // Preserves: A, X, Y, ZP.ACC, ZP.NEXT
    // Error: Fails if node is not a variable (constants rejected)
    SetValue()
    {
        PHA
        
        // Get current symbol info
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Check if it's a variable
        LDA ZP.ACCL
        AND #0xF0  // Extract symbol type (high nibble)
        LSR LSR LSR LSR  // Shift to low nibble
        
        CMP #SymbolType.VARIABLE
        if (NZ)  // Not a variable
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            PLA
            CLC  // Error
            return;
        }
        
        // It's a variable, set the new value
        LDA ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.TOPH
        STA ZP.NEXTH
        
        Objects.SetValue();  // This will use ZP.NEXT
        
        PLA
    }
    
    // Get type info by address
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACC = symbolType|dataType (packed), C set if successful, NC if error
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    // Error: Fails if node is not variable or constant
    GetType()
    {
        Objects.GetData();  // Returns type in ZP.ACC, value in ZP.NEXT, tokens in ZP.IDY
        
        // Check if it's a variable or constant
        LDA ZP.ACCL
        AND #0xF0  // Extract symbol type (high nibble)
        LSR LSR LSR LSR  // Shift to low nibble
        
        CMP #SymbolType.VARIABLE
        if (Z)
        {
            SEC  // Success
            return;
        }
        
        CMP #SymbolType.CONSTANT
        if (Z)
        {
            SEC  // Success
            return;
        }
        
        // Not a variable or constant
        LDA #(Messages.TypeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TypeMismatch / 256)
        STA ZP.LastErrorH
        CLC  // Error
    }
    
    // Get name from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACC = name pointer (points into node data)
    // Preserves: A, X, Y, ZP.TOP, ZP.NEXT
    GetName()
    {
        // Calculate address of name field in node
        CLC
        LDA ZP.IDXL
        ADC #Objects.nameOffset
        STA ZP.ACCL
        LDA ZP.IDXH
        ADC #0
        STA ZP.ACCH
    }
     
    // Get initialization tokens from current node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.NEXT = tokens pointer (points to initialization token stream)
    // Preserves: A, X, Y, ZP.TOP, ZP.ACC
    GetTokens()
    {
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
        
        // Copy to ZP.NEXT for consistency with interface
        LDA ZP.IDYL
        STA ZP.NEXTL
        LDA ZP.IDYH
        STA ZP.NEXTH
    }
    
    // Remove variable or constant by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Uses: Objects.Remove() internally
    // Note: Also frees token stream if non-zero
    Remove()
    {
        PHA
        PHX
        PHY
        
        // Find the symbol first
        STZ ZP.ACCL  // Accept any type
        Find();
        if (NC)  // Not found
        {
            PLY
            PLX
            PLA
            return;  // C already clear
        }
        
        // Get tokens pointer before removing symbol
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
        
        // Save tokens pointer for freeing
        LDA ZP.IDYL
        STA 0x7A  // Temporary storage
        LDA ZP.IDYH
        STA 0x7B
        
        // Remove the symbol node
        Objects.Remove();  // This munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
        if (NC)
        {
            PLY
            PLX
            PLA
            return;  // C already clear
        }
        
        // Free tokens if non-zero
        LDA 0x7A
        ORA 0x7B
        if (NZ)  // Non-zero tokens pointer
        {
            LDA 0x7A
            STA ZP.ACCL
            LDA 0x7B
            STA ZP.ACCH
            Memory.Free();  // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
        }
        
        PLY
        PLX
        PLA
        SEC  // Success
    }
    
    // Start iteration over variables only (for VARS command)
    // Output: ZP.IDX = first variable node, C set if found, NC if none
    // Sets up iteration state for IterateNext()
    IterateVariables()
    {
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        Objects.IterateStart();
    }
    
    // Start iteration over constants only (for CONSTS command if added)
    // Output: ZP.IDX = first constant node, C set if found, NC if none
    IterateConstants()
    {
        LDA #SymbolType.CONSTANT
        STA ZP.ACCL
        Objects.IterateStart();
    }
    
    // Start iteration over all symbols (for general enumeration)
    // Output: ZP.IDX = first symbol node, C set if found, NC if none
    IterateAll()
    {
        STZ ZP.ACCL  // No filter
        Objects.IterateStart();
    }
    
    // Continue iteration (use after any Iterate* method)
    // Input: ZP.IDX = current node, ZP.ACCL = type filter from previous call
    // Output: ZP.IDX = next matching node, C set if found, NC if done
    IterateNext()
    {
        Objects.IterateNext();
    }
    
    // Clear all variables and constants (for NEW command)
    // Output: Empty symbol table
    // Note: Frees all token streams before destroying symbols
    Clear()
    {
        PHA
        PHX
        PHY
        
        // Walk through all symbols and free their token streams
        STZ ZP.ACCL  // No filter - get all symbols
        Objects.IterateStart();
        
        loop
        {
            if (NC) { break; }  // No more symbols
            
            // Get tokens pointer
            Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
            
            // Free tokens if non-zero
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Non-zero tokens pointer
            {
                LDA ZP.IDYL
                STA ZP.ACCL
                LDA ZP.IDYH
                STA ZP.ACCH
                Memory.Free();  // munts ZP.IDX, ZP.IDY, ZP.ACC, ZP.TOP, ZP.NEXT
                
                // Re-establish iteration state after Memory.Free munts everything
                STZ ZP.ACCL  // No filter
                Objects.IterateStart();  // Restart iteration
                continue;
            }
            
            // Move to next symbol
            Objects.IterateNext();
        }
        
        // Now destroy the symbol table itself
        Objects.Destroy();
        
        PLY
        PLX
        PLA
    }
}