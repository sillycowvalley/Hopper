unit Variables
{
    uses "Objects"
    uses "Table"
    
    // Variable management using Objects foundation
    // Two-stage approach: Find name to address, then operate on address
    
    // Declare new variable or constant
    // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
    //        ZP.NEXT = initial value (16-bit or 32 bit), number of elements for ARRAY
    //        ZP.IDY = tokens pointer (16-bit)
    // Output: C set if successful, NC if error (name exists, out of memory)
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    const string strDeclare= "VarDecl";
    Declare()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
       LDA #(strDeclare % 256) STA ZP.TraceMessageL LDA #(strDeclare / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        
        loop // start of single exit block
        {
            LDA ZP.IDYL
            PHA
            LDA ZP.IDYH
            PHA
            
            // Check if symbol already exists
            LDX #ZP.VariablesList
            Objects.Find();
            
            PLA
            STA ZP.IDYH
            PLA
            STA ZP.IDYL
            
            if (C)  // Symbol already exists
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA ZP.ACCT
            AND # BASICType.TYPEMASK
            CMP # BASICType.STRING
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
            
            if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
            {
                // Array declaration
                // ZP.NEXT contains size, extract element type
                LDA ZP.ACCT
                AND #BASICType.TYPEMASK
                STA ZP.ACCT  // Element type for BASICArray.New
                
                // ZP.ACC = size (from ZP.NEXT)
                LDA ZP.NEXTL
                STA ZP.ACCL
                LDA ZP.NEXTH
                STA ZP.ACCH
                
                BASICArray.New();  // Returns pointer in ZP.IDX
                CheckError();
                if (NC)
                {
                    break;
                }
                
                // Store array pointer as variable value
                LDA ZP.IDXL
                STA ZP.NEXTL
                LDA ZP.IDXH
                STA ZP.NEXTH

                // put ARRAY back  
                LDA ZP.ACCT
                ORA # BASICType.ARRAY 
                ORA # SymbolType.VARIABLE             
                STA ZP.ACCT
            }
            
            // Symbol doesn't exist, add it
            LDX #ZP.VariablesList
            
            // Input: X = ZP address of table head (ZP.VariableList or ZP.FunctionsList),
            //        ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
            //        ZP.IDY = tokens pointer (16-bit), ZP.NEXT = value/args (16-bit or 32 bit)
            
            Objects.Add();
            if (NC)
            {
                States.SetFailure();
            }
            
            LDA ZP.ACCT
            AND # BASICType.TYPEMASK
            CMP # BASICType.STRING
            if (Z)
            {
                // restore source string pointer
                PLA
                STA ZP.TOPH
                PLA
                STA ZP.TOPL
                LDA ZP.ACCT
                AND # BASICType.MASK // keep VAR
                STA ZP.TOPT
            }
            // set by Objects.Add() - C for success, NC for failure
            States.IsFailure();
            if (C)
            {
                CLC
                break;
            }
            LDA ZP.ACCT
            AND # BASICType.TYPEMASK
            CMP # BASICType.STRING
            if (Z)
            {
                Variables.SetValue(); // TOP->
            }
            if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
            {
                LDA ZP.NEXTL
                STA ZP.TOPL
                LDA ZP.NEXTH
                STA ZP.TOPH
                Variables.SetValue(); // TOP->
            }
            SEC // success
            break;
        } // end of single exit block

#ifdef TRACE
        PHP LDA #(strDeclare % 256) STA ZP.TraceMessageL LDA #(strDeclare / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLP
#endif

        PLY
        PLX
        PLA
    }
    
    // Find variable/constant by name with optional type filtering
    // Input: ZP.TOP = name pointer, ZP.SymbolIteratorFilter = expected symbolType (VARIABLE or CONSTANT, 0 = any)
    // Output: ZP.IDX = symbol node address, ZP.IDY = index in list, C set if found and correct type, NC if not found or wrong type
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
            AND #SymbolType.MASK
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
    
    
    // Get symbol data from found node
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.ACCT = symbolType|dataType (packed), ZP.NEXT = tokens pointer, ZP.TOP = value/args
    // Munts: A, ZP.NEXT, ZP.ACCT
    getData()
    {
        PHY
        
        // Get symbolType|dataType (offset snType)
        LDY # Objects.snType
        LDA [ZP.IDX], Y
        STA ZP.ACCT
        
        // Get tokens pointer (offset snTokens to snTokens+1)
        LDY # Objects.snTokens
        LDA [ZP.IDX], Y
        STA ZP.NEXTL
        INY
        LDA [ZP.IDX], Y
        STA ZP.NEXTH
        
        // Get value/args (offset snValue to snValue+1)
        LDY # Objects.snValue
        LDA [ZP.IDX], Y
        STA ZP.TOP0
        INY
        LDA [ZP.IDX], Y
        STA ZP.TOP1
        INY
        LDA [ZP.IDX], Y
        STA ZP.TOP2
        INY
        LDA [ZP.IDX], Y
        STA ZP.TOP3
        
        PLY
    }
    
    // Get variable/constant value and type
    // Input: ZP.IDX = symbol node address (from Find)
    // Output: ZP.TOP = value, ZP.TOPT = dataType, C set if successful, NC if error
    // Munts: -
    GetValue()
    {
        PHA
        PHY
        
        loop // start of single exit block
        {
            Variables.getData();  // Returns type in ZP.ACCT, tokens in ZP.NEXT, value in ZP.TOP
            
            // Check if it's a variable or constant
            LDA ZP.ACCT
            AND #SymbolType.MASK
            CMP #SymbolType.VARIABLE
            if (NZ)
            { 
                CMP #SymbolType.CONSTANT
                if (NZ)
                {
                    // Not a variable or constant
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            
            LDA ZP.ACCT
            AND # BASICType.TYPEMASK   // masks off VAR bit (0x10)
            STA ZP.TOPT
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLY
        PLA
    }
    
    // Set variable value (variables only, constants are immutable)
    // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new value, ZP.TOPT = new value type
    // Output: C set if successful, NC if error (not a variable)
    // Munts: -
    const string strSetValue = "VarSet";
    SetValue()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(strSetValue % 256) STA ZP.TraceMessageL LDA #(strSetValue / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

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
            // Get symbolType|dataType (offset snType)
            LDY # Objects.snType
            LDA [ZP.IDX], Y
            STA ZP.ACCT

            // Check if it's a variable
            LDA ZP.ACCT
            AND #SymbolType.MASK
            CMP #SymbolType.VARIABLE
            if (NZ)  // Not a variable
            {
                CMP #SymbolType.CONSTANT
                if (NZ)  // Not a constant
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            
            LDA ZP.ACCT // symbolType|dataType
            if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
            {
                // ARRAY management happens elsewhere, just overwrite ptr              
                // Non-STRING - use value (TOP)
            }
            else
            {
                // Check if the current value is a STRING variable needing memory management
                LDA ZP.ACCT // symbolType|dataType
                AND # BASICType.TYPEMASK
                CMP # BASICType.STRING
                if (Z)
                {
                    LDA ZP.TOPT
                    AND # BASICType.TYPEMASK
                    CMP # BASICType.STRING
                    if (Z)
                    {
                        // If the new value is the same string as the old value, it will do nothing.
                        LDY #Objects.snValue
                        LDA [ZP.IDX], Y
                        CMP ZP.TOP0
                        if (Z)
                        {
                            INY
                            LDA [ZP.IDX], Y
                            CMP ZP.TOP1
                            if (Z)
                            {
                                SEC // same pointer, nothing to do
                                break;
                            }
                        }    
                    }

                    // If the old value was a string, not the same as the new one, we'll free it
                    SEC
                    // STRING variable - need to free old string and allocate new
                    FreeCompoundValue(); // frees node IDX snValue field, munts A
                    CheckError();
                    if (NC) { break; }
                }
                
                LDA ZP.TOPT
                AND # BASICType.TYPEMASK
                CMP # BASICType.STRING
                if (Z)
                {
                    // if we got to here, we have a new string to allocate
                    // Allocate and copy new string (ZP.TOP has source string pointer)
                    AllocateAndCopyString(); // Returns new string pointer in ZP.TOP -> ZP.IDY, preserves IDX, ACCT
                    CheckError();
                    if (NC) { break; }
                    LDA ZP.IDYL
                    STA ZP.TOP0
                    LDA ZP.IDYH
                    STA ZP.TOP1
                }
                                
                // Update type if it's a VAR variable : Variable type (packed)
                if (BBS4, ZP.ACCT) // Bit 4 - VAR
                {
                    // VAR variable - update type in symbol table
                    LDA ZP.ACCT
                    AND # (SymbolType.MASK | BASICType.VAR) // preserve VARIABLE|CONSTANT and VARness
                    ORA ZP.TOPT
                    STA ZP.ACCT
                    
                    // Set symbolType|dataType (offset snType)
                    LDY # Objects.snType // SetValue : store
                    LDA ZP.ACCT
                    STA [ZP.IDX], Y
                }
                
            } // not ARRAY
                        
            // Set the new string pointer as the variable's value
            Objects.SetValue(); // Uses ZP.TOP for value, C = success, NC = failure
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

#ifdef TRACE
        LDA #(strSetValue % 256) STA ZP.TraceMessageL LDA #(strSetValue / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    

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
        PHY
        
        // Get symbolType|dataType (offset snType)
        LDY # Objects.snType
        LDA [ZP.IDX], Y
        STA ZP.ACCT
        
        // Check if it's a variable or constant
        LDA ZP.ACCT
        AND # SymbolType.MASK
        switch (A)
        {
            case SymbolType.VARIABLE:
            {
                SEC  // Success
            }
            case SymbolType.CONSTANT:
            {
                SEC  // Success
            }
            default:
            {
                // Not a variable or constant
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
            }
        }
        
        PLY
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
        
        Objects.GetTokens(); // node address in IDX, -> tokens pointer in ZP.IDY, Munts: A
        
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
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
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
            
            // This checks type internally and only frees STRING and ARRAY types (that are not null)
            FreeCompoundValue(); // frees node IDX snValue field, munts A
            
            // Get tokens pointer before removing symbol
            Objects.GetTokens(); // node address in IDX, -> snTokens pointer in ZP.IDY, Munts: A
            
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
                Memory.Free();  // Input: ZP.IDX, Munts: A, ZP.IDX, ZP.M* -> C on success
            }
            break;
        } // end of single exit block
        
        PLA
        STA ZP.TOPH
        PLA   
        STA ZP.TOPL
        PLA
        STA ZP.IDYH
        PLA   
        STA ZP.IDYL
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
        PHX
        
        LDX #ZP.VariablesList // for Table.GetFirst() and Table.Delete()
        loop
        {
            Table.GetFirst();
            if (NC) { break; }// No more symbols
            
            // This checks type internally and only frees STRING and ARRAY types (that are not null)
            FreeCompoundValue(); // frees node IDX snValue field, munts A
            
            // Get tokens pointer
            Objects.GetTokens(); // node address in IDX, -> snTokens pointer in ZP.IDY, Munts: A
            
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Non-zero tokens pointer
            {
                Memory.FreeIDY();  // Input: ZP.IDY, Munts: A, ZP.M* -> C on success
            }

            // Delete the node from the table
            Table.Delete();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
        }
        
        PLX
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
        LDA ZP.ACCT
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
           
            Memory.Allocate(); // Variables.AllocateAndCopyString(): Input: ZP.ACC = size, Munts: ZP.M*, ZP.FREELIST, ZP.ACCL, -> ZP.IDX
           if (NC) { BIT ZP.EmulatorPCL break; }
            
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
            Memory.Copy();
            
            SEC // Success
            break;
        }
        
        // Restore ZP.IDX (variable node)
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLA
        STA ZP.ACCT
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }

    // Free string snValue memory: string for STRING variable, BASICArray for ARRAY variable
    // Input: ZP.IDX = variable node address (must be STRING type)
    // Output: C set if successful, NC if error or not a STRING variable  
    // Munts: A
    // Preserves: ZP.IDX (variable node address), ZP.ACCT, ZP.IDY, ZP.NEXT
    FreeCompoundValue()
    {
        LDA ZP.ACCT
        PHA
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        loop // single exit
        {
            // Get variable data
            Variables.getData(); // Input: IDX, Returns type in ZP.ACCT, value in ZP.IDY, tokens in ZP.NEXT
            
            // Check if it's a ARRAY variable
            if (BBR5, ZP.ACCT) // Bit 5 - ARRAY
            {
                // not ARRAY
                LDA ZP.ACCT
                AND # BASICType.TYPEMASK
                CMP # BASICType.STRING
                if (NZ)
                {
                    // Not a STRING or ARRAY variable - nothing to free
                    SEC // Success (nop)
                    break;
                }
            }
            
            // Check if string pointer is non-zero
            LDA ZP.TOP0
            ORA ZP.TOP1
            if (Z)
            {
                // Null pointer - nothing to free
                SEC // Success (no-op)
                break;
            }
            LDA ZP.TOP0
            STA ZP.IDYL
            LDA ZP.TOP1
            STA ZP.IDYH
            
            // Free the string memory (use different register than IDX)
            Memory.FreeIDY();  // Input: ZP.IDY, Munts: A, ZP.M* -> C on success
                 
            STZ ZP.TOP0  // Zero out for Objects.SetValue
            STZ ZP.TOP1
            STZ ZP.TOP2
            STZ ZP.TOP3
            Objects.SetValue(); // Set variable's string pointer to 0x0000
            
            SEC // Success
            break;
        }
        
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.ACCT
    }
    
    // Resolve variable or constant by name
    // Input: ZP.TOP = name pointer  
    // Output: C = found, ZP.ACCT = IdentifierType, ZP.IDX = node address if found
    // Modifies: ZP.IDX, ZP.ACCT
    Resolve()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        
        loop
        {
            STZ ZP.SymbolIteratorFilter  // Accept any symbol type
            Find(); // ZP.IDX = symbol node address
            if (NC) // Not found
            {
                CLC
                break;
            }
            
            // Get symbol type and determine identifier type
            GetType(); // Output: ZP.ACCT = symbolType|dataType
            
            LDA ZP.ACCT
            AND #SymbolType.MASK
            CMP #SymbolType.VARIABLE
            if (Z)
            { 
                LDA #IdentifierType.Global
                STA ZP.ACCT
                SEC  // Found
                break;
            }
            
            CMP #SymbolType.CONSTANT
            if (Z)
            { 
                LDA #IdentifierType.Constant
                STA ZP.ACCT
                SEC  // Found
                break;
            }
            
            // Unknown symbol type - shouldn't happen
            Error.InternalError(); BIT ZP.EmulatorPCL
            CLC
            break;
        }
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        
        PLY
        PLX
        PLA
    }
    
}
