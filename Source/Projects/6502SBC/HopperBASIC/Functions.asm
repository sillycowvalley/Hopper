unit Functions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "Arguments"
    uses "BasicTypes"
    uses "Messages"
    uses "Error"
        
    flags FunctionFlags
    {
        None            = 0x00,
        Compiled        = 0x01,    // Bit 0: 1 = opcodes compiled, 0 = tokens only
        NotCompiledMask = 0xFE,
        // Bits 1-7: Available for future use (optimization flags, etc.)
    }
    
    // Function management building on Objects foundation
    // Functions use the existing Objects node structure:
    // Offset 0-1: next pointer (managed by Table unit)
    // Offset 2:   function flags byte (was unused)
    // Offset 3-4: function body tokens pointer / compiled opcodes pointer (dual purpose)
    // Offset 5-6: arguments list head pointer (points directly to first argument node)
    // Offset 7-8: opcode stream pointer (16-bit - for functions, unused for variables/constants)
    // Offset 9+:  null-terminated name string
    
    // Declare new function
    // Input: ZP.TOP = name pointer
    //        ZP.NEXT = arguments list head pointer, ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, C set if successful, NC if error
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Declare()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Check if function already exists
            LDX #ZP.FunctionsList
            Objects.Find();
            if (C)  // Function already exists
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Function doesn't exist, add it
            STZ ZP.ACCT // flags = 0
            LDX #ZP.FunctionsList
            Objects.Add();
            
            // FunctionFlags = FunctionFlags.None
            LDY # Objects.snFlags
            LDA # 0
            STA [ZP.IDX], Y
            
            // clear opCodes
            LDY # Objects.snOpCodes
            STA [ZP.IDX], Y
            INY
            STA [ZP.IDX], Y
            
            
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Find function by name
    // Input: ZP.TOP = name pointer
    // Output: ZP.IDX = function node address, C set if found and is function, NC if not found or wrong type
    // Munts: ZP.LCURRENT, ZP.SymbolTemp0
    Find()
    {
        PHA
        PHX
        PHY
        
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
        
        loop // start of single exit block
        {
            // Find the symbol
            LDX #ZP.FunctionsList
            Objects.Find();
            if (NC)  // Not found
            {
                CLC  // Not found
                break;
            }
            break;
        } // end of single exit block
        
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
        
        PLY
        PLX
        PLA
    }
    
    // Get function body tokens
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = function body tokens pointer, C set (always succeeds)
    // Munts: -
    GetBody()
    {
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
        SEC  // Always succeeds
    }
    
    // Get function name
    // Input: ZP.IDX = function node address
    // Output: ZP.TOP = name pointer (points into node data), C set (always succeeds)
    // Munts: -
    GetName()
    {
        PHA
        
        // Calculate address of name field in node
        CLC
        LDA ZP.IDXL
        ADC #Objects.snName
        STA ZP.TOPL
        LDA ZP.IDXH
        ADC #0
        STA ZP.TOPH
        
        SEC  // Always succeeds
        
        PLA
    }
    
    // Set arguments list head pointer in function node
    // Input: ZP.IDX = function node address, ZP.IDY = arguments list head
    // Output: C set if successful
    // Munts: ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
    SetArguments()
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
        
        loop // start of single exit block
        {
            // Clear existing arguments first
            Arguments.Clear();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
            // Restore function node address and new arguments list head
            PLA
            STA ZP.IDYH
            PLA
            STA ZP.IDYL
            
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Set new arguments list head directly in function node
            LDY #Objects.snArguments
            LDA ZP.IDYL
            STA [ZP.IDX], Y
            INY
            LDA ZP.IDYH
            STA [ZP.IDX], Y
            
            SEC  // Success
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Get arguments list head pointer from function node
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = arguments list head pointer, C set if has arguments, NC if no arguments
    // Munts: -
    GetArguments()
    {
        PHA
        
        LDA ZP.ACCT
        PHA
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        
        Objects.GetData();  // Returns tokens in ZP.NEXT, value/args in ZP.IDY
        
        // Check if arguments list head is non-zero
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            CLC  // No arguments
        }
        else
        {
            SEC  // Has arguments
        }
        
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.ACCT
        
        PLA
    }
    
    // Remove function by name
    // Input: ZP.TOP = name pointer
    // Output: C set if successful, NC if not found
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Remove()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // start of single exit block
        {
            // Find the function first
            Find(); //returns IDX
            if (NC)  // Not found
            {
                CLC  // Not found
                break;
            }
            
            // Clear all arguments before removing function
            Arguments.Clear();  // preserves IDX munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
            // Free function body tokens if they exist
            Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Non-zero tokens pointer
            {
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Memory.Free();  // munts ZP.IDX, ZP.IDY, ZP.TOP, ZP.NEXT
                
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            
            freeOpCodes();
            
            LDX #ZP.FunctionsList
            Objects.Remove();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
            break;
        } // end of single exit block
    
        PLA
        STA ZP.IDXH
        PLA   
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    // function node IDX
        
    // Start iteration over functions only
    // Output: ZP.IDX = first function node, C set if found, NC if none
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateFunctions()
    {
        PHA
        PHX
        
        // 'all' because this list only has FUNCTIONs
        LDA # 0 
        STA ZP.SymbolIteratorFilter
        
        LDX #ZP.FunctionsList
        Objects.IterateStart();
        
        PLX
        PLA
    }
    
    // Continue function iteration
    // Input: ZP.IDX = current node
    // Output: ZP.IDX = next function node, C set if found, NC if done
    // Munts: ZP.LCURRENT, ZP.LNEXT
    IterateNext()
    {
        Objects.IterateNext();
    }
    
    // Clear all functions
    // Output: Empty function table, C set (always succeeds)
    // Munts: ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX, ZP.SymbolTemp0, ZP.SymbolTemp1
    Clear()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            LDX #ZP.FunctionsList
            Table.GetFirst();
            
            if (NC) { break; }  // No more functions
            
            // Clear all arguments for this function (NOP if arguments list head pointer is null)
            Arguments.Clear();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
            // Get function body tokens pointer and free it if non-zero
            Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
            
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)  // Non-zero tokens pointer
            {
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                Memory.Free();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
                               
                // Re-establish function node address after Memory.Free munts everything
                LDX #ZP.FunctionsList
                Table.GetFirst();
            }
            
            // Delete the function node
            LDX #ZP.FunctionsList
            Table.Delete();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LPREVIOUS, ZP.LNEXT, ZP.LHEADX
        }
        
        SEC  // Always succeeds
        
        PLY
        PLX
        PLA
    }
    
    // Set function body tokens
    // Input: ZP.IDX = function node address, ZP.IDY = new function body tokens pointer
    // Output: C set if successful
    // Munts: ZP.TOP, ZP.NEXT
    SetBody()
    {
        PHA
        PHX
        PHY
        
        // Save new tokens pointer
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        // Get current tokens pointer to free old allocation
        Objects.GetTokens();  // Returns tokens pointer in ZP.IDY
        
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)  // Non-zero tokens pointer
        {
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Free old tokens
            LDA ZP.IDYL
            STA ZP.IDXL
            LDA ZP.IDYH
            STA ZP.IDXH
            Memory.Free();  // munts ZP.IDY, ZP.TOP, ZP.NEXT
            
            // Restore function node address
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
        
        // Restore new tokens pointer
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        // Set new tokens
        Objects.SetTokens();  // Uses ZP.IDX and ZP.IDY
        
        SEC  // Always succeeds
        
        PLY
        PLX
        PLA
    }
    
    freeOpCodes()
    {
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
         
            
        // clear opCodes stream
        LDY # Objects.snOpCodes
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        STA ZP.IDYH
        ORA ZP.IDYL
        if (NZ)
        {
            LDA ZP.IDYL
            STA ZP.IDXL
            LDA ZP.IDYH
            STA ZP.IDXH
            Memory.Free(); // ZP.IDX
        }
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // set to null
        LDY # Objects.snOpCodes
        LDA # 0
        STA [ZP.IDX], Y
        INY
        STA [ZP.IDX], Y
    }
    
    // Free all compiled opcode streams and mark functions as tokens-only
    // Called when environment changes (variables/constants/functions modified)
    // and JITed code becomes stale.
    //
    // Input: None
    // Output: All functions marked as uncompiled, opcode memory freed
    // Munts: ZP.IDX, ZP.IDY, iteration state
    FreeAllOpCodes()
    {
        PHA
        PHX
        PHY
        
        // Save all ZP registers that will be munted
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
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Iterate through all functions
        IterateFunctions(); // ZP.IDX = first function
        
        loop
        {
            if (NC) { break; } // No more functions
            
            // Free opcodes for current function (ZP.IDX)
            freeOpCodes(); // Frees opcode stream and nulls pointer
            ClearCompiled(); // Mark as tokens-only
            
            // Move to next function
            IterateNext(); // ZP.IDX = next function
        }
        
        // Restore all ZP registers
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
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
    
    // Check if function is compiled
    // Input: ZP.IDX = function node address  
    // Output: C set if compiled, NC if tokens only
    IsCompiled()
    {
        LDY # Objects.snFlags
        LDA [ZP.IDX], Y
        AND #FunctionFlags.Compiled
        if (NZ) { SEC } else { CLC }
    }
    
    // Mark function as compiled  
    // Input: ZP.IDX = function node address
    SetCompiled()
    {
        LDY # Objects.snFlags
        LDA [ZP.IDX], Y
        ORA #FunctionFlags.Compiled
        STA [ZP.IDX], Y
    }
    
    // Mark function as tokens only (clear compiled flag)
    // Input: ZP.IDX = function node address  
    ClearCompiled()
    {
        LDY # Objects.snFlags
        LDA [ZP.IDX], Y
        AND #FunctionFlags.NotCompiledMask
        STA [ZP.IDX], Y
    }
    
    // Retrieve opcode stream for execution
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = opcode stream pointer, C set if compiled, NC if not compiled
    GetOpCodes()
    {
        PHA
        PHY
        
        // Check if function is compiled
        LDY # Objects.snFlags 
        LDA [ZP.IDX], Y
        AND # FunctionFlags.Compiled
        if (Z) // Not compiled
        {
            // Return null pointer
            STZ ZP.IDYL
            STZ ZP.IDYH
            CLC // Not compiled
        }
        else
        {
            // Function is compiled - return opcode stream pointer
            LDY # Objects.snOpCodes
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            
            SEC // Compiled
        }
        
        PLY
        PLA
    }
    
    // fast unprotected version for CALLF
    JumpToOpCodes()
    {
        // assume we are compiled and good
        Executor.FetchOperandWord();
        LDA Executor.executorOperandL
        STA ZP.IDXL
        LDA Executor.executorOperandH
        STA ZP.IDXH
        
        Stacks.PushPC(); // after FetchOperandWord
        
#ifdef DEBUG
        LDA #' ' Debug.COut(); LDA #'[' Debug.COut();
        LDA ZP.IDXH Debug.HOut(); LDA ZP.IDXL Debug.HOut();
        LDA #']' Debug.COut(); LDA #' ' Debug.COut();
#endif        
        
        LDY #Objects.snOpCodes
        LDA [ZP.IDX], Y
        STA ZP.PCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.PCH
    
#ifdef DEBUG
        LDA #' ' Debug.COut(); LDA #'-' Debug.COut();LDA #'>' Debug.COut(); LDA #' ' Debug.COut();
        LDA ZP.PCH Debug.HOut(); LDA ZP.PCL Debug.HOut();
#endif        
        SEC
    }   
    
    
    Compile()
    {
        PHA
        PHX
        PHY
        
        // Save main execution PC
        LDA ZP.PCL
        PHA
        LDA ZP.PCH
        PHA
        
        // Save current tokenizer state
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        LDA ZP.TokenBufferLengthL
        PHA
        LDA ZP.TokenBufferLengthH
        PHA
        
        loop // Single exit block
        {
            freeOpCodes(); // Free existing opcode stream if not null
                                    
            // Get function body tokens
            Functions.GetBody(); // Input: ZP.IDX = function node, Output: ZP.IDY = tokens pointer
            Compiler.SetLiteralBase(); 
            
            // Check if function has a body
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                Error.InternalError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Set up tokenizer to read from function's token stream (ZP.IDY)
            // We need to temporarily redirect the tokenizer to use the function's allocated token stream
            // instead of BasicTokenizerBuffer
            
            // Calculate length of function token stream by scanning for end marker
            LDY #0
            STZ ZP.ACCL
            STZ ZP.ACCH
            
            loop // Scan for length
            {
                LDA [ZP.IDY], Y
                CMP #Tokens.ENDFUNC
                if (Z) { break; }
                CMP #Tokens.EOF  
                if (Z) { break; }
                
                INY
                if (Z) { INC ZP.IDYH }
                
                INC ZP.ACCL
                if (Z) { INC ZP.ACCH }
            }
            
            // Copy function's token stream to BasicTokenizerBuffer for compilation
            Functions.GetBody(); // Input: ZP.IDX = function node, Output: ZP.IDY = tokens pointer
            LDA ZP.IDYL
            STA ZP.FSOURCEADDRESSL        // Source: function's token stream
            LDA ZP.IDYH
            STA ZP.FSOURCEADDRESSH
            
            LDA #(Address.BasicTokenizerBuffer % 256)
            STA ZP.FDESTINATIONADDRESSL   // Destination: BasicTokenizerBuffer
            LDA #(Address.BasicTokenizerBuffer / 256)  
            STA ZP.FDESTINATIONADDRESSH
            
            LDA ZP.ACCL                   // Length of function tokens
            STA ZP.FLENGTHL
            LDA ZP.ACCH
            STA ZP.FLENGTHH
           
            Tools.CopyBytes();            // Copy function tokens to tokenizer buffer
            
            // Set up tokenizer to read copied function tokens
            LDA ZP.ACCL                   // Length of function tokens
            STA ZP.TokenBufferLengthL
            LDA ZP.ACCH
            STA ZP.TokenBufferLengthH
            
            STZ ZP.TokenizerPosL          // Start at beginning of copied tokens
            STZ ZP.TokenizerPosH 
            
            STZ ZP.ACCL // 0 arguments
            GetArguments(); // ZP.IDY = arguments list head pointer
            if (C)
            {
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH 
                PHA
                
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                
                // has arguments
                Arguments.GetCount(); // ZP.ACCL = argument count
                
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH 
            PHA
            
            // Use Compiler.CompileExpression() to compile function body
            Compiler.CompileFunction();
            Error.CheckError();
            State.GetState();
            switch (A)
            {
                case SystemState.Success:
                {
                    // continue
                }
                case SystemState.Failure:
                { 
                    // handle compilation error
                    break;
                }
                case SystemState.Exiting:   
                { 
                    // should not happen in compilation
                }
            }
            
            
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            if (NC) { break; }
            
            // Copy opcodes from BasicOpCodeBuffer to permanent function storage
            copyOpCodesToFunction();
            Error.CheckError();
            if (NC) { break; }
            
            // Mark function as compiled
            SetCompiled();
            
            SEC // Success
            break;
        }
        
        // Restore tokenizer state
        PLA
        STA ZP.TokenBufferLengthH
        PLA
        STA ZP.TokenBufferLengthL
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
        
        // Restore main execution PC  
        PLA
        STA ZP.PCH
        PLA
        STA ZP.PCL
        
        PLY
        PLX
        PLA
    }
    
    

    // Helper method to copy opcodes to permanent storage
    // Input: ZP.IDX = function node address, opcodes in BasicOpCodeBuffer
    // Output: OpCodes copied to allocated memory and stored in function node
    // Modifies: ZP.ACC, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, ZP.FLENGTH
    copyOpCodesToFunction()
    {
        PHA
        PHX
        PHY
        
        // Save function node address
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // Single exit block
        {
            // Check if we have any opcodes to copy
            LDA ZP.OpCodeBufferLengthL
            ORA ZP.OpCodeBufferLengthH
            if (Z)
            {
                // No opcodes - this shouldn't happen but handle gracefully
                Error.InternalError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Allocate memory for opcode storage
            LDA ZP.OpCodeBufferLengthL
            STA ZP.ACCL
            LDA ZP.OpCodeBufferLengthH
            STA ZP.ACCH
            
            Memory.Allocate(); // Returns address in ZP.IDX
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                Error.OutOfMemory(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Save allocated opcode storage address
            LDA ZP.IDXL
            STA ZP.ACCL  // Temporarily store opcode storage address
            LDA ZP.IDXH
            STA ZP.ACCH
            
            // Set up copy parameters
            LDA #(Address.BasicOpCodeBuffer % 256)
            STA ZP.FSOURCEADDRESSL        // Source: BasicOpCodeBuffer
            LDA #(Address.BasicOpCodeBuffer / 256)
            STA ZP.FSOURCEADDRESSH
            
            LDA ZP.ACCL
            STA ZP.FDESTINATIONADDRESSL   // Destination: allocated storage
            LDA ZP.ACCH
            STA ZP.FDESTINATIONADDRESSH
            
            LDA ZP.OpCodeBufferLengthL    // Length: opcode buffer length
            STA ZP.FLENGTHL
            LDA ZP.OpCodeBufferLengthH
            STA ZP.FLENGTHH
            
            // Copy opcodes to permanent storage
            Tools.CopyBytes();
            
            // Restore function node address
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Store opcode storage address in function node
            LDY #Objects.snOpCodes
            LDA ZP.ACCL  // OpCode storage address LSB
            STA [ZP.IDX], Y
            INY
            LDA ZP.ACCH  // OpCode storage address MSB
            STA [ZP.IDX], Y
            
            SEC // Success
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
}
