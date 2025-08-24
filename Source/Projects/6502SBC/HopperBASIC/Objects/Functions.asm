unit Functions
{
    uses "Objects"
    uses "Locals"
        
    // Function management building on Objects foundation
    // Functions use the existing Objects node structure:
    // Offset 0-1:  next pointer (managed by Table unit)
    // Offset 2:    function flags byte (was unused)
    // Offset 3-4:  function body tokens pointer / compiled opcodes pointer (dual purpose)
    // Offset 5-6:  arguments list head pointer (points directly to first argument node)
    // Offset 9-10: opcode stream pointer (16-bit - for functions, unused for variables/constants)
    // Offset 11+:  null-terminated name string
    
    // Declare new function
    // Input: ZP.TOP = name pointer
    //        ZP.NEXT = arguments list head pointer, ZP.IDY = function body tokens pointer
    // Output: ZP.IDX = function node address, ZP.IDY = index in list, C set if successful, NC if error
    // Munts: ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT
    Declare()
    {
        PHA
        PHX
        PHY
        
        loop // start of single exit block
        {
            // Save the tokens pointer passed in IDY
            LDA ZP.IDYL
            PHA
            LDA ZP.IDYH
            PHA
            
            // Check if function already exists
            LDX #ZP.FunctionsList
            Objects.Find();
            if (C)  // Function already exists
            {
                // stack balance
                PLA
                STA ZP.IDYH
                PLA
                STA ZP.IDYL
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Restore the tokens pointer before calling Add
            PLA
            STA ZP.IDYH
            PLA
            STA ZP.IDYL
            
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
            
            SEC // success
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Find function by name
    // Input: ZP.TOP = name pointer
    // Output: ZP.IDX = function node address, ZP.IDY = index in list, C set if found and is function, NC if not found or wrong type
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
    // Output: ZP.STR = name pointer (points into node data), C set (always succeeds)
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
        
        SEC  // Always succeeds
        
        PLA
    }
    
    // Set arguments list head pointer in function node
    // Input: ZP.IDX = function node address, ZP.IDY = arguments list head
    // Output: C set if successful
    // Munts: ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
    
    /* Never Called? SetData used instead?
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
            Locals.Clear();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
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
    */
    
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
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
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
            Locals.Clear();  // preserves IDX munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
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
        
        // clear all FUNC opcodes before calling removing functions
        FreeAllOpCodes(); 
        
        loop
        {
            LDX #ZP.FunctionsList
            Table.GetFirst();
            
            if (NC) { break; }  // No more functions
            
            // Clear all arguments for this function (NOP if arguments list head pointer is null)
            Locals.Clear();  // munts ZP.IDY, ZP.TOP, ZP.NEXT, ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0, ZP.SymbolTemp1
            
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
            //ClearCompiled(); // Mark as tokens-only
            
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
        LDY # Objects.snOpCodes
        LDA [ZP.IDX], Y
        INX
        ORA [ZP.IDX], Y
        if (NZ)
        { SEC } else { CLC }
    }
    
    
    // Retrieve opcode stream for execution
    // Input: ZP.IDX = function node address
    // Output: ZP.IDY = opcode stream pointer, C set if compiled, NC if not compiled
    GetOpCodes()
    {
        PHA
        PHY
        
        // Check if function is compiled
        IsCompiled();
        if (NC) // Not compiled
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
        
//Debug.NL(); LDA #'>' COut(); Space(); XIOut();        

        Stacks.PushXID();
                
        LDY #Objects.snOpCodes
        LDA [ZP.IDX], Y
        STA ZP.PCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.PCH
        
        LDY #Objects.snTokens
        LDA [ZP.IDX], Y
        STA ZP.XIDL
        INY
        LDA [ZP.IDX], Y
        STA ZP.XIDH
        
//Debug.NL(); LDA #':' COut(); Space(); XIOut();        
    
        SEC
    }   
    
    // Helper method to copy function tokens to ZP.TokenBuffer and set up tokenizer
    // Input: ZP.IDX = function node address
    // Output: Tokenizer configured with function's tokens copied to ZP.TokenBuffer
    //         ZP.ACCL/H = length of function tokens
    // Modifies: ZP.IDY, ZP.ACC, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, ZP.FLENGTH
    // Preserves: ZP.IDX (function node address)
    /*
    copyFunctionTokensToBuffer()
    {
        PHA
        PHX
        PHY
        loop
        {
            // Get function body tokens
            Functions.GetBody(); // Input: ZP.IDX = function node, Output: ZP.IDY = tokens pointer
            
            // Check if function has a body
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                Error.InternalError(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA ZP.TokenBufferL
            STA ZP.FDESTINATIONADDRESSL   // Destination: ZP.TokenBuffer
            LDA ZP.TokenBufferH
            STA ZP.FDESTINATIONADDRESSH
            
            // Length of function tokens
            STZ ZP.FLENGTHL
            STZ ZP.FLENGTHH
            
            // Copy function's token stream to ZP.TokenBuffer for compilation
            loop
            {
                IncLENGTH();
                LDA [ZP.IDY]
                STA [ZP.FDESTINATIONADDRESS]
                CMP #Token.EOF  
                if (Z) { break; }
                IncIDY();
                IncDESTINATIONADDRESS();
            }
            
            // Configure tokenizer to read copied function tokens
            LDA ZP.FLENGTHL                   // Length of function tokens
            STA ZP.TokenBufferContentSizeL
            LDA ZP.FLENGTHH
            STA ZP.TokenBufferContentSizeH
            
            STZ ZP.TokenizerPosL          // Start at beginning of copied tokens
            STZ ZP.TokenizerPosH
            
            break;
        } // single exit
        
        PLY
        PLX
        PLA
    }
    */

    const string functionCompile = "FuncComp";
    Compile()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACE
        LDA #(functionCompile % 256) STA ZP.TraceMessageL LDA #(functionCompile / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
    
        // Save current tokenizer state
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        LDA ZP.TokenBufferContentLengthL
        PHA
        LDA ZP.TokenBufferContentLengthH
        PHA
        LDA ZP.CurrentToken
        PHA
        
        // Save current buffers  (they may be REPL or BASIC)
        LDA ZP.TokenBufferL
        PHA
        LDA ZP.TokenBufferH
        PHA
        LDA ZP.OpCodeBufferL
        PHA
        LDA ZP.OpCodeBufferH
        PHA
        
        
        
        loop // Single exit block
        {
            freeOpCodes(); // Free existing opcode stream if not null
                             
            // Copy function tokens to ZP.TokenBuffer and configure tokenizer
            //copyFunctionTokensToBuffer();
            //CheckError();
            //if (NC) { break; }
            
            STZ ZP.ACCL // 0 arguments
            GetArguments(); // ZP.IDY = arguments list head pointer
            if (C)
            {
                // has arguments
                Locals.GetCount(); // ZP.ACCL = argument count
            }
           
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH 
            PHA
            
            Stacks.PushXID();
            
            BufferManager.UseFunctionBuffers(); // IDX -> function node
            
            GetTokens();
            LDA ZP.IDYL
            STA ZP.XIDL
            LDA ZP.IDYH
            STA ZP.XIDH

            // Use Compiler.CompileExpression() to compile function body
            Compiler.CompileFunction();
            CheckError();
            
            Stacks.PopXID();
            
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            

            States.GetState();
            switch (A)
            {
                case State.Success:
                {
                    // continue
                }
                case State.Failure:
                { 
                    // handle compilation error
                    States.SetFailure();
                    break;
                }
                case State.Exiting:   
                { 
                    BRK // should not happen in compilation
                }
            }
            
            // Copy opcodes from BasicOpCodeBuffer to permanent function storage
            copyOpCodesToFunction();
            CheckError();
            if (NC) { break; }
            
            States.SetSuccess(); // should already be the case
            break;
        }
        
        loop
        {
            Trace.IsTracing();
            if (C)
            {
                break;
            }
            CheckError();
            if (NC)
            {
                LDA ZP.TokenBufferContentLengthH
                CMP ZP.TokenizerPosH
                if (Z)
                {
                    LDA ZP.TokenBufferContentLengthL
                    CMP ZP.TokenizerPosL
                    if (Z)
                    {
                        // POS == SIZE -> not much use
                        break;
                    }
                }
                // POS != SIZE
                
                LDA ZP.TokenBufferH
                STA ZP.IDYH
                LDA ZP.TokenBufferL
                STA ZP.IDYL
                
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (NZ)
                {
                    LDA ZP.TokenizerPosH
                    STA ZP.TOKERRORH
                    LDA ZP.TokenizerPosL
                    STA ZP.TOKERRORL
                    
                    Print.NewLine();
                    Commands.DisplayFunctionSignature(); // Input: ZP.IDX = function node
                    TokenIterator.RenderTokenStream();   // Input: ZP.IDY = tokens pointer
                    Commands.DisplayFunctionSuffix();    // Input: ZP.IDX = function node, munts IDY
                    Print.NewLine();
                }
            }
            break;
        }
        
        // restore buffers (they may be REPL or Function)
        PLA
        STA ZP.OpCodeBufferH
        PLA
        STA ZP.OpCodeBufferL
        PLA
        STA ZP.TokenBufferH
        PLA
        STA ZP.TokenBufferL
        
        // Restore tokenizer state
        PLA
        STA ZP.CurrentToken
        PLA
        STA ZP.TokenBufferContentLengthH
        PLA
        STA ZP.TokenBufferContentLengthL
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
        
        //BufferManager.DetectBufferState();
        
    #ifdef TRACE
        LDA #(functionCompile % 256) STA ZP.TraceMessageL LDA #(functionCompile / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
        
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
            LDA ZP.OpCodeBufferContentLengthL
            ORA ZP.OpCodeBufferContentLengthH
            if (Z)
            {
                // No opcodes - this shouldn't happen but handle gracefully
                Error.InternalError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Allocate memory for opcode storage
            LDA ZP.OpCodeBufferContentLengthL
            STA ZP.ACCL
            LDA ZP.OpCodeBufferContentLengthH
            STA ZP.ACCH
            
            Memory.Allocate(); // copyOpCodesToFunction(): Returns address in ZP.IDX
            
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
            LDA ZP.OpCodeBufferL
            STA ZP.FSOURCEADDRESSL        // Source: BasicOpCodeBuffer
            LDA ZP.OpCodeBufferH
            STA ZP.FSOURCEADDRESSH
            
            LDA ZP.ACCL
            STA ZP.FDESTINATIONADDRESSL   // Destination: allocated storage
            LDA ZP.ACCH
            STA ZP.FDESTINATIONADDRESSH
            
            LDA ZP.OpCodeBufferContentLengthL    // Length: opcode buffer length
            STA ZP.FLENGTHL
            LDA ZP.OpCodeBufferContentLengthH
            STA ZP.FLENGTHH
            
            // Copy opcodes to permanent storage
            Memory.Copy();
            
            // Restore function node address 
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            // push it again to maintain stack balance on exit
            PHA
            LDA ZP.IDXH
            PHA
            
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
        
        // Restore function node address (needs to be here in case of failure exit)
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
}
