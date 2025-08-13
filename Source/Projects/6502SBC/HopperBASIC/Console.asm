unit Console // Console.asm
{
    uses "Tokenizer"
    uses "Statement"
    uses "Commands"
    
    // Initialize console system
    Initialize()
    {
        // Initialize tokenizer
        Tokenizer.Initialize();
        
        // Initialize symbol tables
        Objects.Initialize();
        
        LDA #CaptureMode.Off
        Statement.SetCaptureMode();
        
        // Initialize state system
        States.SetSuccess();
    }
    
    // Read a line of input and tokenize it
    ReadLine()
    {
        Tokenizer.ReadLine();    // Read into BasicInputBuffer, sets ZP.BasicInputLength
        
        // Tokenize based on current mode
        Statement.IsCaptureModeOn();
        if (C)
        {
            // FUNC or BEGIN capture mode - append to existing buffer
            Tokenizer.TokenizeAndAppendLine();
        }
        else
        {
            // Normal mode - replace buffer
            Tokenizer.TokenizeLine();
        }
        
        Error.CheckError();
        if (NC) { States.SetFailure(); }  // Return if tokenization failed
        else { States.SetSuccess(); }
    }
    
    // Enhanced ProcessLine() to handle function capture mode
    ProcessLine()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            // FUNC or BEGIN capture mode processing
            processLineFunctionCapture();
        }
        else
        {
            // Normal mode processing
            processLineNormal();
        }
        
        // Check SystemState and propagate
        States.GetState();
        switch (A)
        {
            case State.Success:   { /* continue */ }
            case State.Failure:   { /* return */ }
            case State.Exiting:   { /* return */ }
        }
    }
    
    // Normal mode processing
    const string processLineNormalTrace = "ProcNorm";
    processLineNormal()
    {
#ifdef TRACECONSOLE
        LDA #(processLineNormalTrace % 256) STA ZP.TraceMessageL LDA #(processLineNormalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop // Single exit pattern
        {
            // Check for tokenization errors
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Check for empty line (just EOL token)
            LDA ZP.TokenBufferContentSizeL
            CMP #1
            if (NZ) 
            { 
                // More than one token, process normally
                processTokensAndCheckFunction();
                break;
            }
            
            LDA ZP.TokenBufferContentSizeH
            if (NZ)
            {
                // Definitely more than one token
                processTokensAndCheckFunction();
                break;
            }
            
            // Exactly one token - check if it's EOL by getting first token
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            Tokenizer.NextToken();
            LDA ZP.CurrentToken
            CMP #Token.EOL
            if (Z)
            {
                States.SetSuccess(); // Continue (empty line)
                break;
            }
            
            // Single non-EOL token, reset position and process it
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            processTokensAndCheckFunction();
            break;
        }
        
#ifdef TRACECONSOLE
        LDA #(processLineNormalTrace % 256) STA ZP.TraceMessageL LDA #(processLineNormalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Process tokens and check if we're starting a function
    processTokensAndCheckFunction()
    {
        // Always process the tokens first (this creates the function node)
        processTokens();
        States.IsExiting();
        if (NC)
        {
            // not exiting
            Error.CheckError();
            if (NC)
            {
                States.SetFailure();
            }
            else
            {
                // After processing, check if we just processed an incomplete function
                detectIncompleteFunction();
                States.SetSuccess(); // Continue
            }
        }
    }

    // Detect if current line starts FUNC but doesn't end with ENDFUNC
    detectIncompleteFunction()
    {
        PHA
        PHX
        PHY
        
        // Save current position
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        
        // Reset to start of token buffer
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        // Get first token
        Tokenizer.NextToken();
        
        LDA ZP.CurrentToken
        CMP #Token.FUNC
        if (NZ)
        {
            CMP #Token.BEGIN
            if (NZ)
            {
                // Restore position and exit - not a function
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
            }
            else
            {
                // BEGIN case - scan for END
                loop
                {
                    Tokenizer.NextToken();
                    LDA ZP.CurrentToken
                    CMP #Token.EOL
                    if (Z) 
                    { 
                        // Restore position
                        PLA
                        STA ZP.TokenizerPosH
                        PLA
                        STA ZP.TokenizerPosL
                        LDA #CaptureMode.Begin
                        Statement.SetCaptureMode();
                        break; // incomplete BEGIN found
                    }
                    
                    CMP #Token.END
                    if (Z)
                    {
                        // Restore position
                        PLA
                        STA ZP.TokenizerPosH
                        PLA
                        STA ZP.TokenizerPosL
                        break; // Found END - complete BEGIN
                    }
                } // loop
            }
        }
        else
        {
            // FUNC case - scan for ENDFUNC
            loop
            {
                Tokenizer.NextToken();
                LDA ZP.CurrentToken
                CMP #Token.EOL
                if (Z) 
                { 
                    // Restore position
                    PLA
                    STA ZP.TokenizerPosH
                    PLA
                    STA ZP.TokenizerPosL
                    LDA #CaptureMode.Func
                    Statement.SetCaptureMode();
                    break; // incomplete function found
                }
                
                CMP #Token.ENDFUNC
                if (Z)
                {
                    // Restore position
                    PLA
                    STA ZP.TokenizerPosH
                    PLA
                    STA ZP.TokenizerPosL
                    break; // Found ENDFUNC - complete function
                }
            } // loop
        }
        
        PLY
        PLX
        PLA
    }

    // Function capture mode line processing
    const string processFuncCaptureTrace = "ProcFunc";
    processLineFunctionCapture()
    {
    #ifdef TRACECONSOLE
        LDA #(processFuncCaptureTrace % 256) STA ZP.TraceMessageL LDA #(processFuncCaptureTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        loop // Single exit pattern
        {
            // Check for tokenization errors
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Check if this line contains ENDFUNC (completing the function)
            checkForCompletionToken();
            if (C)
            {
                // Found ENDFUNC or END - exit capture mode and complete the function
                LDA #CaptureMode.Off
                Statement.SetCaptureMode();
                
                // Complete the captured function
                FunctionDeclaration.CompletePartialFunction();
                Error.CheckError();
                if (NC) 
                { 
                    States.SetFailure(); 
                }
                else
                {
                    Messages.PrintOK();  // Print OK after successful function definition
                    States.SetSuccess();
                }
                break;
            }
            
            // Still capturing - just accumulate tokens
            States.SetSuccess();
            break;
        }
        
    #ifdef TRACECONSOLE
        LDA #(processFuncCaptureTrace % 256) STA ZP.TraceMessageL LDA #(processFuncCaptureTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }

    // Check if current token buffer contains completion token (ENDFUNC or END)
    checkForCompletionToken()
    {
        PHA
        PHX
        PHY
        
        // Save current position
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        
        // Scan for completion token from start of buffer
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        Statement.GetCaptureMode();
        CMP #CaptureMode.Func
        if (Z)
        {
            // Look for ENDFUNC
            loop
            {
                Tokenizer.CompareTokenizerPosToLength();
                if (C) { CLC break; } // Reached end of buffer - not found
                
                Tokenizer.NextToken();
                LDA ZP.CurrentToken
                CMP #Token.ENDFUNC
                if (Z) { SEC break; } // Found completion
            }
        }
        else
        {
            // Look for END (BEGIN mode)
            loop
            {
                Tokenizer.CompareTokenizerPosToLength();
                if (C) { CLC break; } // Reached end of buffer - not found
                
                Tokenizer.NextToken();
                LDA ZP.CurrentToken
                CMP #Token.END
                if (Z) { SEC break; } // Found completion
            }
        }
        
        // Restore position
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
        
        PLY
        PLX
        PLA
    }    
    
    // Exit function capture mode and return to normal (called from Console or Tokenizer on Ctrl+C)
    const string exitFuncModeTrace = "ExitFunc";
    ExitFunctionCaptureMode()
    {
#ifdef TRACECONSOLE
        LDA #(exitFuncModeTrace % 256) STA ZP.TraceMessageL LDA #(exitFuncModeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        LDA #CaptureMode.Off
        Statement.SetCaptureMode();
        
        // Additional cleanup when exiting function capture mode:
        // Reset tokenizer buffer to clear partial function data
        STZ ZP.TokenBufferContentSizeL
        STZ ZP.TokenBufferContentSizeH
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        // Clear any error state that might have been set during capture
        Error.ClearError();
        States.SetSuccess();
        
#ifdef TRACECONSOLE
        LDA #(exitFuncModeTrace % 256) STA ZP.TraceMessageL LDA #(exitFuncModeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Process the tokens in BasicTokenizerBuffer  
    processTokens()
    {
        PHA
        PHX
        
        RMB1 ZP.FLAGS  // Clear bit 1 (exit flag) at start
        
        loop  // Main statement loop for colon-separated statements
        {
            if (BBS1, ZP.FLAGS) { break; } // Exit if bit 1 is set
            
            // Get current token
            Tokenizer.NextToken();  // Returns token in A, updates ZP.CurrentToken
            LDA ZP.CurrentToken
            
            // Check for end of line first
            CMP #Token.EOL
            if (Z) { break; }  // End of all statements
            
            CMP #Token.EOF
            if (Z) { break; }  // End of all statements
            
            // Execute the current statement/command
            switch (A)
            {
                // ========== CONSOLE COMMANDS - Delegate to Commands unit ==========
                case Token.NEW:
                {
                    parseNew();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.CLEAR:
                {
                    parseClear();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.FORGET:
                {
                    parseForget();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.VARS:
                {
                    parseVars();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.LIST:
                {
                    LDX #0 // LIST
                    parseList();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.DASM:
                {
#ifdef DEBUG
                    LDX #1 // DASM
                    parseList();
#else
                    parseDasm();
#endif
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.FUNCS:
                {
                    parseFuncs();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.MEM:
                {
                    parseMem();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.BYE:
                {
                    parseBye();
                    SMB1 ZP.FLAGS  // Always exit after BYE
                }
                
                case Token.HEAP:
                {
                    parseHeap();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.BUFFERS:
                {
                    parseBuffers();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.DUMP:
                {
                    parseDump();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.TRON:
                {
                    parseTron();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.TROFF:
                {
                    parseTroff();
                    Error.CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                
                // File operations - TODO
                case Token.SAVE:
                case Token.LOAD:
                case Token.DIR:
                case Token.DEL:
                {
                    TODO(); BIT ZP.EmulatorPCL
                }
                
                // ========== BASIC LANGUAGE EXECUTION ==========
                case Token.RUN:
                {
                    cmdRun();
                    Error.CheckError();
                    // RUN was a '$MAIN' function call (buffer is munted so even if there was a ':', no point)
                    // We can remove this when function compiling doesn't use shared token and buffers
                    if (NC)
                    {
                        States.IsExiting();
                        if (C)
                        {
                            States.SetFailure(); // don't "BYE" if we have a failure (probably syntax error from Executor)
                        }
                    }
                    else
                    {
                        States.IsExiting();
                        if (C)
                        {
                            States.SetSuccess(); // don't "BYE" if we are just Exiting REPL
                        }
                    }
                    SMB1 ZP.FLAGS  // Always exit after RUN
                }
                
                // COMMENTS - Skip over them
                case Token.REM:
                case Token.COMMENT:
                {
                    // Comments at top level are just ignored
                    // Skip to end of line or next colon
                    loop
                    {
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Token.EOL
                        if (Z) { break; }
                        CMP #Token.COLON
                        if (Z) { break; }
                    }
                }
                
                default:
                {
                    // DEFAULT CASE - All BASIC statements and declarations
                    // This includes: PRINT, assignments, function calls, variable declarations,
                    // FUNC/ENDFUNC, BEGIN/END, IF/THEN, WHILE/WEND, etc.
                    
                    // save the token before rollback -> X
                    TAX 
                    
                    // Roll back one token so Statement can process it
                    Tokenizer.Rollback();
                    
                    // Load the token at the new position
                    Tokenizer.NextToken(); // preserves X
                    Error.CheckError();    // preserves X
                    if (NC)
                    { SMB1 ZP.FLAGS } // Set exit flag on error
                    else
                    {
                        PHX
                        
                        // Let Statement handle it
                        Statement.Execute();  // Handles the current statement
                        
                        PLX
                        
                        Error.CheckError();
                        if (NC) 
                        { SMB1 ZP.FLAGS } // Set exit flag on error
                        else
                        {
                            // Check if we should print OK (for declarations in immediate mode)
                            Statement.IsCaptureModeOn();
                            if (NC)  // Only in immediate mode, not during function capture
                            {
                                LDA ZP.CurrentToken
                                CMP #Token.EOL
                                if (Z)  // At end of statement
                                {
                                    // Print OK for successful declarations
                                    // X = token saved before rollback
                                    
                                    // Input: X is token
                                    // Output: C set if token is a type keyword, NC if not a type keyword, A = BASICType
                                    BASICTypes.FromToken(); 
                                    if (C)
                                    {
                                        Messages.PrintOK();
                                    }
                                }
                            }
                        }
                    }
                }
            } // switch
            
            // Check for statement separator (only if not exiting)
            if (BBR1, ZP.FLAGS)  // Only check if bit 1 is clear (continuing)
            {
                LDA ZP.CurrentToken
                CMP #Token.COLON
                if (NZ) { break; }  // No more statements on this line
            }
            
        } // loop through statements
        
        RMB1 ZP.FLAGS  // Clean up: clear the exit flag before returning
        
        PLX
        PLA
    }
    
    // ========================================================================
    // Console Command Parsing Helpers
    // These parse arguments and call the Commands unit implementations
    // ========================================================================
    
    // Parse and execute NEW command
    parseNew()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'NEW'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdNew(); }
        }
    }
    
    // Parse and execute CLEAR command
    parseClear()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'CLEAR'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdClear(); }
        }
    }
    
    // Parse and execute FORGET command with identifier argument
    parseForget()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            loop // Single exit
            {
                Tokenizer.NextToken(); // consume 'FORGET'
                Error.CheckError();
                if (NC) { break; }
                
                // Expect identifier name
                LDA ZP.CurrentToken
                CMP #Token.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                LDA ZP.TOPL
                STA ZP.STRL
                LDA ZP.TOPH
                STA ZP.STRH
                
                Tokenizer.NextToken(); // consume identifier
                validateEndOfCommand();
                Error.CheckError();
                if (C) { Commands.CmdForget(); } // Uses ZP.STR
                break;
            }
        }
    }
    
    // Parse and execute VARS command
    parseVars()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'VARS'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdVars(); }
        }
    }
    
    // Parse and execute LIST command with optional identifier
    parseList()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'LIST'
            parseOptionalIdentifier(); // Sets ZP.STR or null
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdList(); } // Uses ZP.STR (null = all)
        }
    }
    
    // Parse and execute FUNCS command with optional identifier
    parseFuncs()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'FUNCS'
            parseOptionalIdentifier(); // Sets ZP.STR or null
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdFuncs(); } // Uses ZP.STR (null = all)
        }
    }
    
    // Parse and execute MEM command
    parseMem()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'MEM'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdMem(); }
        }
    }
    
    // Parse and execute BYE command
    parseBye()
    {
        Tokenizer.NextToken(); // consume 'BYE'
        validateEndOfCommand();
        Error.CheckError();
        if (C) { Commands.CmdBye(); }
    }
    
#ifdef DEBUG
    // Parse and execute HEAP command
    parseHeap()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'HEAP'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdHeap(); }
        }
    }
    
    // Parse and execute BUFFERS command
    parseBuffers()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'BUFFERS'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdBuffers(); }
        }
    }
    
    // Parse and execute DUMP command with optional page number
    parseDump()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            loop // Single exit
            {
                Tokenizer.NextToken(); // consume 'DUMP'
                
                // Check for optional page number
                LDA ZP.CurrentToken
                CMP #Token.NUMBER
                if (Z)
                {
                    Tokenizer.GetTokenNumber(); // Result in ZP.ACC
                    Tokenizer.NextToken(); // consume number
                }
                else
                {
                    // No number provided - default to page 0
                    STZ ZP.TOPL
                    STZ ZP.TOPH
                }
                
                validateEndOfCommand();
                Error.CheckError();
                if (C)
                { 
                    Commands.CmdDump(); // Uses ZP.TOP
                } 
                break;
            }
        }
    }
#else
    // Stubs for non-debug builds
    parseHeap()    { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
    parseBuffers() { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
    parseDump()    { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
    parseDasm()    { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
#endif

#if defined(TRACE) || defined(TRACEEXE)
    // Parse and execute TRON command
    parseTron()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'TRON'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdTron(); }
        }
    }
    
    // Parse and execute TROFF command
    parseTroff()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'TROFF'
            validateEndOfCommand();
            Error.CheckError();
            if (C) { Commands.CmdTroff(); }
        }
    }
#else
    // Stubs for non-trace builds
    parseTron()  { Error.OnlyInTrace(); BIT ZP.EmulatorPCL }
    parseTroff() { Error.OnlyInTrace(); BIT ZP.EmulatorPCL }
#endif
    
    // Parse optional identifier argument
    // Output: ZP.STR = pointer to identifier or null (0x0000) if none
    parseOptionalIdentifier()
    {
        PHA
        
        LDA ZP.CurrentToken
        CMP #Token.IDENTIFIER
        if (Z)
        {
            Tokenizer.GetTokenString(); // Result in ZP.TOP
            LDA ZP.TOPL
            STA ZP.STRL
            LDA ZP.TOPH
            STA ZP.STRH
            
            Tokenizer.NextToken(); // consume identifier
        }
        else
        {
            // No identifier - set ZP.STR to null
            STZ ZP.STRL
            STZ ZP.STRH
        }
        
        PLA
    }
    
    // Validate that current token is end of command (EOL or COLON)
    validateEndOfCommand()
    {
        PHA
        
        LDA ZP.CurrentToken
        CMP #Token.EOL
        if (Z) { SEC }  // Valid end
        else
        {
            CMP #Token.COLON
            if (Z) { SEC }  // Valid end (more statements follow)
            else
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                CLC
            }
        }
        
        PLA
    }
    
        // Initialize all global variables with their declared values
    // Called before executing $MAIN to ensure consistent state
    // Input: None
    // Output: All variables initialized to their declared values
    //         C set if successful, NC if error during initialization
    // Munts: Token buffer, all execution-related ZP variables
    const string initGlobsTrace = "initGlobs";
    initializeGlobals()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(initGlobsTrace % 256) STA ZP.TraceMessageL LDA #(initGlobsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Start iteration over all variables
        Variables.IterateVariables();
        // Returns: ZP.IDX = first variable node, C = found, NC = none
        loop // Variable iteration loop
        {
            if (NC) 
            {
                SEC break; // No more variables - success
            }
            
            
            // Clear tokenizer state
            Tokenizer.Initialize();
            
            // Get variable name for assignment
            Variables.GetName(); // -> ZP.STR
            Variables.GetTokens(); // Returns tokens pointer in ZP.NEXT
            
            Variables.GetType(); // -> ZP.ACCT
            

            LDA ZP.ACCT
            AND # BASICType.ARRAY
            if (NZ)
            {
                
                Variables.GetValue(); // Variable = IDX: BASICArray -> TOP, Tokens -> NEXT
                
                // Save the current variable node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Copy array index expression tokens to buffer
                // Note: Variable declarations are single-line, so EOL is a good terminator
                loop
                { 
                    LDA [ZP.NEXT]
                    CMP #Token.EOL // TODO : switch to EOF
                    if (Z)
                    {
                        LDA #Token.EOF
                        Tokenizer.appendToTokenBuffer();
                        break; // Found EOL terminator
                    }
                    CMP #Token.EOF
                    if (Z)
                    {
                        Tokenizer.appendToTokenBuffer();
                        break; // Found EOF terminator
                    }
                    Tokenizer.appendToTokenBuffer();
                    IncNEXT();
                }
                
                STZ ZP.TokenizerPosL
                STZ ZP.TokenizerPosH
                
                // Execute the initialization statement
                Tokenizer.NextToken(); // Get first token
                
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                Statement.EvaluateExpression();
                Error.CheckError();
                if (NC) 
                { 
                    // Error during initialization
                    PLA PLA CLC
                    break;
                }
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                // reload for TOP (overwrites NEXT too!)
                Variables.GetValue(); // Variable = IDX: BASICArray -> TOP, Tokens -> NEXT
                
                Stacks.PopNext(); // newly calculated array dimension
                
//Debug.NL(); XOut(); TOut(); NOut();

                // Input:  BASICArray = TOP, desired number of elements = NEXT
                // Output: BASICArray = TOP (may be the same, may be new - always zeroed out, manages cleanup of previous array)
                BASICArray.Redimension();

                if (NC)
                {
                    break;
                }
//Debug.NL(); XOut(); TOut();
                // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new ARRAY
                Variables.SetValue(); // does not free old
            }
            else
            {
                // Save the current variable node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Check if variable has initialization tokens
                LDA ZP.NEXTL
                ORA ZP.NEXTH
                if (Z) 
                {
                    
                    LDA ZP.ACCT
                    AND #BASICType.MASK
                    switch (A)
                    {
                        case BASICType.STRING:
                        {
                            LDA # Token.STRINGLIT
                            Tokenizer.appendToTokenBuffer();
                            // '\0' (null terminator for string literal)
                            LDA #0x00
                            Tokenizer.appendToTokenBuffer();
                        }
                        case BASICType.BIT:
                        {
                            LDA #Token.FALSE
                            Tokenizer.appendToTokenBuffer();
                        }
                        default:
                        {
                            LDA ZP.ACCT
                            AND #BASICType.ARRAY
                            if (NZ)
                            {
                                // should never get here (see case above)
                                Error.TODO(); BIT ZP.EmulatorPCL
                                States.SetFailure();
                                break;
                            }
                            else
                            {
                                // No initialization tokens - use default value 0
                                LDA #Token.NUMBER
                                Tokenizer.appendToTokenBuffer();
                                
                                // '0'
                                LDA #'0'
                                Tokenizer.appendToTokenBuffer();
                                
                                // '\0' (null terminator for number string)
                                LDA #0x00
                                Tokenizer.appendToTokenBuffer();
                            }
                        }
                    }
                    // Add EOL token
                    LDA #Token.EOF
                    Tokenizer.appendToTokenBuffer();
                }
                else
                {
                    // Copy variable initialization tokens to buffer
                    // Note: Variable declarations are single-line, so EOL is a good terminator
                    loop
                    { 
                        LDA [ZP.NEXT]
                        CMP #Token.EOL // TODO : switch to EOF
                        if (Z)
                        {
                            LDA #Token.EOF
                            Tokenizer.appendToTokenBuffer();
                            break; // Found EOL terminator
                        }
                        Tokenizer.appendToTokenBuffer();
                        IncNEXT();
                    }
                }
                STZ ZP.TokenizerPosL
                STZ ZP.TokenizerPosH
                
                // Execute the initialization statement
                Tokenizer.NextToken(); // Get first token
                
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                // These are simple assignments (FOO = 42, rather than INT FOO = 42)
                Statement.EvaluateExpression();
                Error.CheckError();
                if (NC) 
                { 
                    // Error during initialization
                    PLA PLA
                    CLC
                    break;
                }
                // Restore variable node before continuing iteration
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                Stacks.PopTop();
                Variables.SetValue();
            } // not ARRAY
            
            // Continue to next variable
            Variables.IterateNext();
        } // iterate variables loop
        
#ifdef TRACE
        LDA #(initGlobsTrace % 256) STA ZP.TraceMessageL LDA #(initGlobsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        PLY
        PLX
        PLA
    }
    
    // ========================================================================
    // RUN Command - Stays in Console because it's BASIC execution
    // ========================================================================
    
    // Execute RUN command - run the main program
    const string runTrace = "RUN";
    cmdRun()
    {
#ifdef TRACE
        LDA #(runTrace % 256) STA ZP.TraceMessageL LDA #(runTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            Tokenizer.NextToken(); // consume 'RUN'
            
            initializeGlobals();
                                    
            loop // Single exit block
            {
                // Find the $MAIN function
                LDA #(Messages.BeginFunctionName % 256)
                STA ZP.TOPL
                LDA #(Messages.BeginFunctionName / 256)
                STA ZP.TOPH
                
                Functions.Find(); // Input: ZP.TOP = "$MAIN", Output: ZP.IDX if found
                if (NC)
                {
                    // No main program defined
                    LDA #(Messages.NoMainProgram % 256)
                    STA ZP.ACCL
                    LDA #(Messages.NoMainProgram / 256)
                    STA ZP.ACCH
                    Tools.PrintStringACC();
                    Tools.NL();
                    break;
                }
                // Clear tokenizer state
                Tokenizer.Initialize();
                
                // Construct token buffer with function call to $MAIN()
                // Token buffer will contain: IDENTIFIER "$MAIN" LPAREN RPAREN EOL
                Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
                
                // IDENTIFIER
                LDA #Token.IDENTIFIER
                Tokenizer.appendToTokenBuffer(); // munts IDX!
                
                // Inline the name
                LDX #0
                loop
                {
                    LDA Messages.BeginFunctionName, X
                    if (Z)
                    {
                        Tokenizer.appendToTokenBuffer();
                        break; 
                    }
                    Tokenizer.appendToTokenBuffer();
                    INX
                }
                
                LDA #Token.LPAREN
                Tokenizer.appendToTokenBuffer();
                
                LDA #Token.RPAREN
                Tokenizer.appendToTokenBuffer();
                
                LDA #Token.EOF
                Tokenizer.appendToTokenBuffer();
                
                // Execute the function call
                Tokenizer.NextToken();
                
                
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                Statement.EvaluateExpression(); // executes 'identifier()' as function call
              
                break;
            }
        }
        
#ifdef TRACE
        LDA #(runTrace % 256) STA ZP.TraceMessageL LDA #(runTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Generate an error for commands only allowed at console
    FunctionModeError()
    {
        Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
    }
}
