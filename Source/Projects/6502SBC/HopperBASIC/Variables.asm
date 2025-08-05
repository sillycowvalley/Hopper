unit Variables
{
    uses "Objects"
    uses "BasicTypes"
    uses "Messages"
    uses "Error"
    uses "Table"
    
    // Variable management using Objects foundation
    // Two-stage approach: Find name to address, then operate on address
    
    // Declare new variable or constant
    // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Declare()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Check if symbol already exists
            LDX #ZP.VariablesList
            Objects.Find();
            
            if (C)  // Symbol already exists
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA ZP.ACCT
            AND #0x0F
            CMP #BasicType.STRING
            if (Z)
            {
                // save the source string address and clear 'value'
                LDA ZP.NEXTL
                PHA
                LDA ZP.NEXTH
                PHA
                STZ ZP.NEXTL
                STZ ZP.NEXTH
            }
            
            // Symbol doesn't exist, add it
            LDX #ZP.VariablesList
            Objects.Add();
            
            LDA ZP.ACCT
            AND #0x0F
            CMP #BasicType.STRING
            if (Z)
            {
                // restore source string pointer
                PLA
                STA ZP.TOPH
                PLA
                STA ZP.TOPL
            }
            // set by Objects.Add() - C for success, NC for failure
            if (NC)
            {
                break;
            }
            
            LDA ZP.ACCT
            AND #0x0F
            CMP #BasicType.STRING
            if (Z)
            {
                SetValue();
            }
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Find variable/constant by name with optional type filtering
    // Input: ZP.TOP = name pointer, ZP.SymbolIteratorFilter = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, C set if found and correct type, NC if not found or wrong type
    // Munts: ZP.LCURRENT, ZP.SymbolTemp0
    Find()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCT
        PHA
        
        loop // start of single exit block
        {
            // Find the symbol
            LDX #ZP.VariablesList
            Objects.Find();
            if (NC)  // Not found
            {
                CLC
                break;
            }
            
            // Check if type filtering is requested
            LDA ZP.SymbolIteratorFilter
            if (Z)  // No type filter
            {
                SEC  // Found
                break;
            }
            
            // Save expected type for comparison
            LDA ZP.SymbolIteratorFilter
            STA ZP.SymbolTemp0  // Temporary storage
            
            // Get symbol type and check
            Variables.GetType();
            
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            LSR LSR LSR LSR  // Shift to low nibble
            
            CMP ZP.SymbolTemp0  // Compare with expected type
            if (Z)              // Types match
            {
                SEC  // Found and correct type
                break;
            }
            
            // Wrong type
            Error.TypeMismatch(); BIT ZP.EmulatorPCL
            break;
        } // end of single exit block
        
        PLA
        STA ZP.ACCT
        
        PLY
        PLX
        PLA
    }
    
    // Get variable/constant value and type
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    // Munts: -
    GetValue()
    {
        PHA
        
        loop // start of single exit block
        {
            Objects.GetData();  // Returns type in ZP.ACCT, tokens in ZP.NEXT, value in ZP.IDY
            
            // Check if it's a variable or constant
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            CMP # (SymbolType.VARIABLE << 4)
            if (NZ)
            { 
                CMP # (SymbolType.CONSTANT << 4)
                if (NZ)
                {
                    // Not a variable or constant
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            // Copy value to TOP and extract data type - value is now in ZP.IDY
            LDA ZP.IDYL
            STA ZP.TOPL
            LDA ZP.IDYH
            STA ZP.TOPH
            
            LDA ZP.ACCT
            AND #0x0F  // Extract data type (low nibble)
            STA ZP.TOPT
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLA
    }
    
    // Set variable value (variables only, constants are immutable)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value
    // Output: C set if successful, NC if error (not a variable)
    // Munts: -
    SetValue()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Save ZP.ACC (contains iteration filter)
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH  
        PHA
        
        loop // start of single exit block
        {
            // Get current symbol info
            Objects.GetData();  // Returns type in ZP.ACCT, tokens in ZP.NEXT, value in ZP.IDY
            
            // Check if it's a variable
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            LSR LSR LSR LSR  // Shift to low nibble
            
            CMP #SymbolType.VARIABLE
            if (NZ)  // Not a variable
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Check if this is a STRING variable needing memory management
            LDA ZP.ACCT  // symbolType|dataType from Objects.GetData()
            AND #0x0F    // Extract data type (low nibble)
            CMP #BasicType.STRING
            if (Z)
            {
                // STRING variable - need to free old string and allocate new
                FreeStringValue(); // Free existing string memory
                Error.CheckError();
                if (NC) { break; }
                // Allocate and copy new string (ZP.TOP has source string pointer)
                AllocateAndCopyString(); // Returns new string pointer in ZP.IDY
                Error.CheckError();
                if (NC) { break; }
            }
            else
            {
                // Non-STRING variable - use existing logic
                LDA ZP.TOPL
                STA ZP.IDYL
                LDA ZP.TOPH
                STA ZP.IDYH
            }
           // Set the new string pointer as the variable's value
            Objects.SetValue(); // Uses ZP.IDY, C = success, NC = failure
            break;
        } // end of single exit block
        
        // Restore ZP.ACC
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    // Get type information for variable/constant
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), C set if successful, NC if error
    // Munts: -
    GetType()
    {
        PHA
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH  
        PHA
        
        Objects.GetData();  // Returns type in ZP.ACCT, tokens in ZP.NEXT, value in ZP.IDY
        
        // Check if it's a variable or constant
        LDA ZP.ACCT
        AND #0xF0  // Extract symbol type (high nibble)
        switch (A)
        {
            case (SymbolType.VARIABLE << 4):
            {
                SEC  // Success
            }
            case (SymbolType.CONSTANT << 4):
            {
                SEC  // Success
            }
            default:
            {
                // Not a variable or constant
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
            }
        }
        
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLA
    }
    
    // Get variable/constant signature info
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.IDY = value, C set if successful, NC if error
    // Munts: -
    GetSignature()
    {
        PHA
        
        loop // start of single exit block
        {
            Objects.GetData();  // Returns type in ZP.ACCT, tokens in ZP.NEXT, value in ZP.IDY
            
            // Check if it's a variable or constant
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            LSR LSR LSR LSR  // Shift to low nibble
            
            CMP #SymbolType.VARIABLE
            if (Z)
            {
                SEC  // Success - ZP.ACC, ZP.NEXT, ZP.IDY already set by Objects.GetData()
                break;
            }
            
            CMP #SymbolType.CONSTANT
            if (Z)
            {
                SEC  // Success - ZP.ACC, ZP.NEXT, ZP.IDY already set by Objects.GetData()
                break;
            }
            
            // Not a variable or constant
            Error.TypeMismatch(); BIT ZP.EmulatorPCL
            break;
        } // end of single exit block
        
        PLA
    }    
    // Get name pointer from symbol node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.STR = name pointer (points into node data), always succeeds
    // Munts: -
    GetName()
    {
        PHA
        
        // Calculate address of name field in node
        CLC
        LDA ZP.IDXL
        ADC #Objects.snName
        STA ZP.STRL
        LDA ZP.IDXH
        ADC #0
        STA ZP.STRH
        
        PLA
    }
     
    // Get initialization tokens pointer from symbol node
    // Input: ZP.IDX = symbol node address (from Find or iteration)
    // Output: ZP.NEXT = tokens pointer, C set (always succeeds)
    // Munts: -
    GetTokens()
    {
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
        
        // Copy to ZP.NEXT for consistency with interface
        LDA ZP.IDYL
        STA ZP.NEXTL
        LDA ZP.IDYH
        STA ZP.NEXTH
        
        PLA
        STA ZP.IDYH
        PLA   
        STA ZP.IDYL
        
        SEC  // Always succeeds
    }
    
    // Remove variable or constant by name with token cleanup
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Munts: ZP.IDY, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Remove()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
        
        loop // start of single exit block
        {
            // Find the symbol first
            STZ ZP.SymbolIteratorFilter  // Accept any type
            Find();
            if (NC)  // Not found
            {
                CLC  // Not found
                break;
            }
            
            // This checks type internally and only frees STRING types (that are not null)
            FreeStringValue(); 
            
            // Get tokens pointer before removing symbol
            Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
            
            // Save tokens pointer for freeing
            LDA ZP.IDYL
            STA ZP.SymbolTemp0  // Temporary storage
            LDA ZP.IDYH
            STA ZP.SymbolTemp1
            
            // Remove the symbol node
            LDX #ZP.VariablesList
            Objects.Remove();  // This munts ZP.IDY, ZP.TOP, ZP.NEXT
            if (NC)
            {
                CLC  // Failed to remove
                break;
            }
            
            // Free tokens if non-zero
            LDA ZP.SymbolTemp0
            ORA ZP.SymbolTemp1
            if (NZ)  // Non-zero tokens pointer
            {
                LDA ZP.SymbolTemp0
                STA ZP.IDXL
                LDA ZP.SymbolTemp1
                STA ZP.IDXH
                Memory.Free();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
            }
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLA
        STA ZP.TOPH
        PLA   
        STA ZP.TOPL
        PLA
        STA ZP.IDXH
        PLA   
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    // Start iteration over variables only (for VARS command)
    // Output: ZP.IDX = first variable node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateVariables()
    {
        PHA
        PHX
        
        LDA #SymbolType.VARIABLE
        STA ZP.SymbolIteratorFilter
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        PLX
        PLA
    }
    
    // Start iteration over constants only (for CONSTS command)
    // Output: ZP.IDX = first constant node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateConstants()
    {
        PHA
        PHX
        
        LDA #SymbolType.CONSTANT
        STA ZP.SymbolIteratorFilter
        LDX #ZP.VariablesList
        Objects.IterateStart(); 
               
        PLX
        PLA
    }
    
    // Start iteration over all symbols (for LIST command)
    // Output: ZP.IDX = first symbol node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateAll()
    {
        PHA
        PHX
        
        STZ ZP.SymbolIteratorFilter  // No filter
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        PLX
        PLA
    }
    
    // Continue iteration (use after any Iterate* method)
    // Output: ZP.IDX = next matching node, C set if found, NC if done
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateNext()
    {
        Objects.IterateNext();
    }
    
    // Clear all variables and constants with token cleanup (for NEW command)
    // Output: Empty symbol table, C set (always succeeds)
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
    Clear()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            LDX #ZP.VariablesList
            Table.GetFirst();
        
            if (NC) 
            { 
                break; // No more symbols
            }
            
            // This checks type internally and only frees STRING types (that are not null)
            FreeStringValue(); 
            
            // Get tokens pointer
            Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
            
            // Save tokens pointer for later freeing
            LDA ZP.IDYL
            PHA
            LDA ZP.IDYH
            PHA
            
            // Delete the node from the table
            LDX #ZP.VariablesList
            Table.Delete();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
            
            // Restore tokens pointer and free if non-zero
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (NZ)  // Non-zero tokens pointer
            {
                Memory.Free();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
            }
        }
        
        PLY
        PLX
        PLA
        SEC  // Always succeeds
    }
    
    // Empty string constant for default STRING values
    const string EmptyString = "";

    // Allocate and copy string from source
    // Input: ZP.TOP = source string pointer
    // Output: ZP.IDY = pointer to allocated copy, C set if successful, NC if allocation failed
    // Modifies: ZP.ACC, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, ZP.FLENGTH
    // Preserves: ZP.IDX (variable node address)
    AllocateAndCopyString()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Save ZP.IDX (variable node) 
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // single exit
        {
            // Get source string length
            LDX ZP.TOPL
            LDY ZP.TOPH
            Tools.StringLength(); // Returns length in A
            
            // Allocate length + 1 for null terminator
            CLC
            ADC #1
            STA ZP.ACCL
            LDA #0
            ADC #0  // Add carry
            STA ZP.ACCH
            
            Memory.Allocate(); // Returns address in ZP.IDX (temporarily)
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                Error.OutOfMemory(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Move allocated pointer to ZP.IDY for return
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Set up copy parameters
            LDA ZP.TOPL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TOPH
            STA ZP.FSOURCEADDRESSH
            
            LDA ZP.IDYL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDYH
            STA ZP.FDESTINATIONADDRESSH
            
            LDA ZP.ACCL  // Length + 1 (includes null terminator)
            STA ZP.FLENGTHL
            LDA ZP.ACCH
            STA ZP.FLENGTHH
            
            // Copy string including null terminator
            Tools.CopyBytes();
            
            SEC // Success
            break;
        }
        
        // Restore ZP.IDX (variable node)
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }

    // Free string memory for STRING variable
    // Input: ZP.IDX = variable node address (must be STRING type)
    // Output: C set if successful, NC if error or not a STRING variable  
    // Modifies: ZP.ACCT, ZP.IDY, ZP.NEXT
    // Preserves: ZP.IDX (variable node address)
    FreeStringValue()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        
        // Save ZP.IDX (variable node)
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // single exit
        {
            // Get variable data
            Objects.GetData(); // Returns type in ZP.ACCT, value in ZP.IDY
            
            // Check if it's a STRING variable
            LDA ZP.ACCT
            AND #0x0F  // Extract data type (low nibble)
            CMP #BasicType.STRING
            if (NZ)
            {
                // Not a STRING variable - nothing to free
                SEC // Success (no-op)
                break;
            }
            
            // Check if string pointer is non-zero
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                // Null pointer - nothing to free
                SEC // Success (no-op)
                break;
            }
            
            // Free the string memory (use different register than IDX)
            LDA ZP.IDYL
            STA ZP.ACCL  // Temporarily use ACC for Memory.Free
            LDA ZP.IDYH
            STA ZP.ACCH
            
            LDA ZP.ACCL
            STA ZP.IDXL  // Memory.Free expects address in IDX
            LDA ZP.ACCH
            STA ZP.IDXH
            
            Memory.Free(); // munts ZP.IDY, ZP.TOP, ZP.NEXT
            
            STZ ZP.IDYL  // Zero out for Objects.SetValue
            STZ ZP.IDYH
            Objects.SetValue(); // Set variable's string pointer to 0x0000
            
            SEC // Success
            break;
        }
        
        // Restore ZP.IDX (variable node)
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }


}
