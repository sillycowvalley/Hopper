unit FunctionDeclaration // FunctionDeclaration.asm
{
    uses "Tokenizer"
    uses "Instructions"
    
    
    stmtObjectPtrToIDX()
    {
        LDA (Statement.stmtObjectPtr + 0)
        STA ZP.IDXL
        LDA (Statement.stmtObjectPtr + 1)
        STA ZP.IDXH
    }
    IDXtoStmtObjectPtr()
    {
        LDA ZP.IDXL
        STA (Statement.stmtObjectPtr + 0)
        LDA ZP.IDXH
        STA (Statement.stmtObjectPtr + 1)
    }
    // Execute BEGIN declaration statement (main program)
    // Input: ZP.CurrentToken = BEGIN token
    // Output: Main program declared as special "BEGIN" function
    //         ZP.CurrentToken = token after BEGIN declaration
    // Modifies: Functions table, memory allocation, all statement buffer locations
    // Error: Sets ZP.LastError if syntax error, name conflict, or memory allocation fails
    const string executeBeginDeclarationTrace = "ExecBegDecl";
    ExecuteBeginDeclaration()
    {
#ifdef TRACE
        LDA #(executeBeginDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(executeBeginDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
        loop // Single exit block for clean error handling
        {
            // Check if "BEGIN" function already exists and remove it
            Messages.Main(); // point ZP.TOP -> "$MAIN"
            Functions.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // BEGIN function exists - remove it (name pointer already in ZP.TOP)
                Functions.Remove();
                CheckError();
                if (NC) { break; }
                
                // Restore the name pointer for declaration
                //Messages.Main(); // point ZP.TOP -> "$MAIN"
            }
            
            // Create empty "BEGIN" function with no arguments
            //Messages.Main(); // point ZP.TOP -> "$MAIN"
            
            // Initialize empty arguments list and body tokens
            STZ ZP.NEXTL // Arguments list head = null (no arguments)
            STZ ZP.NEXTH
            STZ ZP.IDYL  // Function body tokens = null (will be set later)
            STZ ZP.IDYH
            
            // Declare the function
            Functions.Declare(); // Input: ZP.TOP = name, ZP.NEXT = args head, ZP.IDY = body tokens
            CheckError();
            if (NC) { break; }
            
            // Save function node address
            IDXtoStmtObjectPtr();
            
            // Get next token after BEGIN
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Check if this is an incomplete BEGIN block (ends with EOL)
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (Z)
            {
                // Incomplete BEGIN block - set up for capture mode
                SEC // Success - incomplete function ready for capture
                break;
            }
            
            // Complete BEGIN block on same line - capture body from current position to END
            captureBeginBody();
            CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
#ifdef TRACE
        LDA #(executeBeginDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(executeBeginDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Execute function declaration statement
    // Input: ZP.CurrentToken = FUNC token
    // Output: Function declared and added to symbol table with arguments and body
    //         ZP.CurrentToken = token after function declaration
    // Modifies: Stack, ZP.CurrentToken, symbol tables, memory allocation,
    //          all statement buffer locations, all parsing variables
    // Error: Sets ZP.LastError if syntax error, name conflict, or memory allocation fails
    const string executeFunctionDeclarationTrace = "ExecFuncDecl";
    ExecuteFunctionDeclaration()
    {
#ifdef TRACE
        LDA #(executeFunctionDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(executeFunctionDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
        loop // Single exit block for clean error handling
        {
            // Get next token - should be function name
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP #Token.IDENTIFIER
            if (NZ)
            {
                Tokens.IsKeyword();
                if (C)
                {
                    Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                }
                else
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                }
                break;
            }
            
            // Get the function name
            Tokenizer.GetTokenString(); // Returns pointer in ZP.TOP
            CheckError();
            if (NC) { break; }
            
            // Save function name pointer in statement storage
            LDA ZP.TOPL
            STA (Statement.stmtNamePtr + 0)
            LDA ZP.TOPH
            STA (Statement.stmtNamePtr + 1)
            
            // Check if function with this name already exists and remove it
            Functions.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // Function exists - remove it (name pointer already in ZP.TOP)
                Functions.Remove();
                CheckError();
                if (NC) { break; }
            }
            
            // Check if variable/constant with this name exists - this is an error
            STZ ZP.SymbolIteratorFilter // Accept any symbol type
            Variables.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // Variable/constant exists - determine type and show appropriate error
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                CheckError();
                if (NC) { break; }
                
                LDA ZP.ACCT
                AND #SymbolType.MASK
                CMP #SymbolType.CONSTANT
                if (Z)
                {
                    Error.ConstantExistsTOP(); BIT ZP.EmulatorPCL // name is in ZP.TOP
                }
                else
                {
                    Error.VariableExistsTOP(); BIT ZP.EmulatorPCL  // name is in ZP.TOP
                }
                break;
            }
            
            // Restore function name pointer after potential corruption from Remove operations
            LDA (Statement.stmtNamePtr + 0)
            STA ZP.TOPL
            LDA (Statement.stmtNamePtr + 1)
            STA ZP.TOPH
            
            // Get next token - should be opening parenthesis
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Token.LPAREN
            if (NZ)
            {
                Error.ExpectedLeftParen(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Parse parameter list - create empty function first, then add arguments
            // Restore function name to ZP.TOP
            LDA (Statement.stmtNamePtr + 0)
            STA ZP.TOPL
            LDA (Statement.stmtNamePtr + 1)
            STA ZP.TOPH
            
            // Initialize empty arguments list and body tokens for now
            STZ ZP.NEXTL // Arguments list head = null
            STZ ZP.NEXTH
            STZ ZP.IDYL  // Function body tokens = null (will be set later)
            STZ ZP.IDYH
            
            // Declare the function with empty arguments and body
            Functions.Declare(); // Input: ZP.TOP = name, ZP.NEXT = args head, ZP.IDY = body tokens
            CheckError();
            if (NC) { break; }
            
            // Save function node address for adding arguments
            IDXtoStmtObjectPtr();
            
            // Parse parameters
            parseParameterList();
            CheckError();
            if (NC) { break; }
            
            // Expect closing parenthesis (should be current token after parseParameterList)
            LDA ZP.CurrentToken
            CMP #Token.RPAREN
            if (NZ)
            {
                Error.ExpectedRightParen(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Get next token - start of function body or EOL
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Check if this is an incomplete function (ends with EOL)
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (Z)
            {
                // Incomplete function - set up for capture mode
                // Function is already declared, Console will handle capture mode
                SEC // Success - incomplete function ready for capture
                break;
            }

            // Complete function on same line - capture function body from current position to ENDFUNC
            captureFunctionBody();
            CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
#ifdef TRACE
        LDA #(executeFunctionDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(executeFunctionDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Parse parameter list and add arguments to function
    // Input: ZP.CurrentToken = token after opening parenthesis
    //        Function node address in stmtObjectPtr
    // Output: Arguments added to function, ZP.CurrentToken = closing parenthesis or error
    // Modifies: ZP.CurrentToken, Arguments list, ZP.IDX, ZP.TOP
    const string parseParameterListTrace = "ParseParams";
    parseParameterList()
    {
#ifdef TRACE
        LDA #(parseParameterListTrace % 256) STA ZP.TraceMessageL LDA #(parseParameterListTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
        loop // Single exit block
        {
            // Get next token after opening parenthesis
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            // Check for empty parameter list
            LDA ZP.CurrentToken
            CMP #Token.RPAREN
            if (Z)
            {
                SEC // Success - empty parameter list
                break;
            }
            
            // Parse parameter list
            loop
            {
                // Expect identifier
                LDA ZP.CurrentToken
                CMP #Token.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get parameter name
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                CheckError();
                if (NC) { break; }
                
                // Add argument to function
                // Restore function node address to ZP.IDX
                stmtObjectPtrToIDX();
                
                LDA #SymbolType.ARGUMENT
                ORA #BASICType.VAR 
                STA ZP.SymbolType // argument for Locals.Add()
                
                Locals.Add(); // Input: ZP.IDX = function node, ZP.TOP = argument name
                CheckError();
                if (NC) { break; }
                
                // Get next token
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
                
                // Check what comes next
                LDA ZP.CurrentToken
                CMP #Token.RPAREN
                if (Z)
                {
                    SEC // Success - end of parameter list
                    break;
                }
                
                // Expect comma for more parameters
                CMP #Token.COMMA
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get next token after comma
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
                
                // Continue parsing parameters
            }
            
            break; // Exit outer loop
        } // Single exit block
        
#ifdef TRACE
        LDA #(parseParameterListTrace % 256) STA ZP.TraceMessageL LDA #(parseParameterListTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Capture BEGIN body tokens from current position to END
    // Input: ZP.CurrentToken = first token of BEGIN body
    //        Function node address in stmtObjectPtr
    // Output: BEGIN body tokens captured and stored in function node
    //         ZP.CurrentToken = token after END
    // Modifies: Memory allocation, function node, ZP.CurrentToken, tokenizer position
    const string captureBeginBodyTrace = "CapBegBody";
    captureBeginBody()
    {
#ifdef TRACE
        LDA #(captureBeginBodyTrace % 256) STA ZP.TraceMessageL LDA #(captureBeginBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
        loop // Single exit block
        {
            // Save tokenizer position at start of BEGIN body
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Scan to find END token
            loop
            {
                LDA ZP.CurrentToken
                CMP #Token.EOF
                if (Z)
                {
                    // Hit end of input without finding END
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                CMP #Token.END
                if (Z)
                {
                    // Found END - we're done
                    SEC
                    break;
                }
                
                // Get next token
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
            }
            
            if (NC) { break; } // Error during scanning
            
            // Calculate length of BEGIN body tokens
            Statement.calculateTokensLength();
            
            // - don't miss the first token
            // - do skip the END (by decreasing FLENGTH)
            DecSOURCEADDRESS(); 
            
            // Create token stream copy
            Tools.CreateTokenStream(); // captureBeginBody() : Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, -> ZP.IDY
            CheckError();
            if (NC) { break; }
            
            // Set function body tokens in function node
            stmtObjectPtrToIDX();
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
            // Get next token after END
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
#ifdef TRACE
        LDA #(captureBeginBodyTrace % 256) STA ZP.TraceMessageL LDA #(captureBeginBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Capture function body tokens from current position to ENDFUNC
    // Input: ZP.CurrentToken = first token of function body
    //        Function node address in stmtObjectPtr
    // Output: Function body tokens captured and stored in function node
    //         ZP.CurrentToken = token after ENDFUNC
    // Modifies: Memory allocation, function node, ZP.CurrentToken, tokenizer position
    const string captureFunctionBodyTrace = "CapFuncBody";
    captureFunctionBody()
    {
#ifdef TRACE
        LDA #(captureFunctionBodyTrace % 256) STA ZP.TraceMessageL LDA #(captureFunctionBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Save tokenizer position at start of function body
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Scan to find ENDFUNC token (no nesting support)
            loop
            {
                LDA ZP.CurrentToken
                CMP #Token.EOF
                if (Z)
                {
                    // Hit end of input without finding ENDFUNC
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                CMP #Token.ENDFUNC
                if (Z)
                {
                    // Found ENDFUNC - we're done
                    SEC
                    break;
                }
                
                // Get next token
                Tokenizer.NextTokenCheck();
                if (NC) 
                { 
                    CLC
                    break; 
                }
            }
            
            if (NC) { break; } // Error during scanning
            
            // Calculate length of function body tokens
            // Current position is at ENDFUNC, subtract start position
            Statement.calculateTokensLength();
            
            // - don't miss the first token
            // - do skip the ENDFUNC (by not increasing FLENGTH)
            DecSOURCEADDRESS(); 
            
            // Create token stream copy using existing pattern
            Tools.CreateTokenStream(); // captureFunctionBody(): Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, -> ZP.IDY
            CheckError();
            if (NC) { break; }
            
            // Set function body tokens in function node
            // Restore function node address to ZP.IDX
            stmtObjectPtrToIDX();
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
            // Get next token after ENDFUNC
            Tokenizer.NextTokenCheck();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
#ifdef TRACE
        LDA #(captureFunctionBodyTrace % 256) STA ZP.TraceMessageL LDA #(captureFunctionBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Complete a partial function that was captured in multiple lines
    // Input: Token buffer contains complete function from FUNC to ENDFUNC
    // Output: Function body tokens extracted and attached to existing function node
    // Modifies: Function node, memory allocation, tokenizer position
    // Error: Sets ZP.LastError if function not found, syntax error, or memory allocation fails
    const string completePartialFunctionTrace = "CompPartFunc";
    CompletePartialFunction()
    {
    #ifdef TRACE
        LDA #(completePartialFunctionTrace % 256) STA ZP.TraceMessageL LDA #(completePartialFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif

        loop // Single exit block for error handling
        {
            // Reset to start of function buffer
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            
            // Skip FUNC token
            Tokenizer.NextTokenCheck(); // Gets FIRST token (should be FUNC)
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Token.FUNC
            if (Z)
            {
                // Get function name
                Tokenizer.NextTokenCheck(); // Gets SECOND token (should be function name)
                if (NC) { break; }
                
                LDA ZP.CurrentToken
                CMP #Token.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get the function name string
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                CheckError();
                if (NC) { break; }
                
                // Skip to end of function signature (past closing parenthesis)
                loop
                {
                    Tokenizer.NextTokenCheck();
                    if (NC) { break; }
                    
                    LDA ZP.CurrentToken
                    CMP #Token.RPAREN
                    if (Z) 
                    { 
                        Tokenizer.NextTokenCheck(); // Move past RPAREN to start of body
                        if (NC) { break; }
                        break; 
                    }
                    
                    CMP #Token.EOF
                    if (Z)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                if (NC) { break; }
            }
            else
            {
                CMP #Token.BEGIN
                if (NZ)
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    break;
                }
                // "BEGIN" function name
                Messages.Main(); // point ZP.TOP -> "$MAIN"
                
                // Skip EOL after BEGIN if present (from multi-line capture)
                Tokenizer.NextTokenCheck();
                if (NC) { break; }
            }
            
            // Skip EOL after RPAREN if present (from multi-line capture)
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (NZ)
            {
                // Not EOL - back up tokenizer so we don't skip real body token
                DEC ZP.TokenizerPosL
                if (Z)
                {
                    DEC ZP.TokenizerPosH
                }
            }
            // Now positioned at actual body start (past any EOL)
            
            // Find the function that was already declared
            Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = function node
            if (NC)
            {
                Error.UndefinedIdentifierTOP(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Save function node address in statement storage
            IDXtoStmtObjectPtr();
            
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Save position BEFORE each token so we can stop BEFORE ENDFUNC/END
            loop
            {
                // Save position BEFORE getting next token
                LDA ZP.TokenizerPosL
                PHA
                LDA ZP.TokenizerPosH
                PHA
                
                Tokenizer.NextTokenCheck();
                if (NC) 
                { 
                    PLA  // Clean up stack
                    PLA
                    break; 
                }
                
                LDA ZP.CurrentToken
                CMP #Token.ENDFUNC
                if (Z) 
                { 
                    // Found ENDFUNC - restore position to BEFORE it
                    PLA
                    STA ZP.TokenizerPosH
                    PLA
                    STA ZP.TokenizerPosL
                    break; 
                }
                
                CMP #Token.END
                if (Z) 
                { 
                    // Found END - restore position to BEFORE it
                    PLA
                    STA ZP.TokenizerPosH
                    PLA
                    STA ZP.TokenizerPosL
                    break; 
                }
                
                CMP #Token.EOF
                if (Z)
                {
                    PLA  // Clean up stack
                    PLA
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Not a terminator, discard saved position and continue
                PLA
                PLA
            }
            if (NC) { break; }
            
            // Calculate body length (position BEFORE terminator - start pos)
            Statement.calculateTokensLength();
            
            // Create token stream for function body (without ENDFUNC/END)
            Tools.CreateTokenStream(); // CompletePartialFunction(): Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, -> ZP.IDY
            CheckError();
            if (NC) { break; }
            
            // Restore function node address
            stmtObjectPtrToIDX();
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
            SEC // Success
            break;
        }
        
    #ifdef TRACE
        LDA #(completePartialFunctionTrace % 256) STA ZP.TraceMessageL LDA #(completePartialFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
}
