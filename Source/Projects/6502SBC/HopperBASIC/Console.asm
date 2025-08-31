unit Console // Console.asm
{
    uses "Tokenizer"
    uses "Statement"
    uses "Commands"
    
    validateNextToken()
    {
        Tokenizer.NextToken();
        validateEndOfCommand();
        CheckError();
    }
    
    // Initialize console system
    Initialize()
    {
        BufferManager.InitializeForTokenGeneration();
        //BufferManager.ResetInputBuffer();
        
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
            LDA #1 // Append mode = 1
            STA ZP.OpCodeTemp   
            TokenizeLineWithMode();
        }
        else
        {
            // Normal mode - replace buffer
            STZ ZP.OpCodeTemp  // Replace mode = 0
            TokenizeLineWithMode();
        }
        
        CheckError();
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
            CheckErrorAndSetFailure();
            if (NC) { break; }
            
            // Check for empty line (just EOL token)
            LDA ZP.TokenBufferContentLengthL
            CMP #1
            if (NZ) 
            { 
                // More than one token, process normally
                processTokensAndCheckFunction();
                break;
            }
            
            LDA ZP.TokenBufferContentLengthH
            if (NZ)
            {
                // Definitely more than one token
                processTokensAndCheckFunction();
                break;
            }
            
            // Exactly one token - check if it's EOL by getting first token
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            Tokenizer.NextToken(); // no
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
            CheckError();
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
        Tokenizer.NextToken(); // no
        
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
                    Tokenizer.NextToken(); // no
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
                Tokenizer.NextToken(); // no
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
            CheckErrorAndSetFailure();
            if (NC) { break; }
            
            // Check if this line contains ENDFUNC (completing the function)
            checkForCompletionToken();
            if (C)
            {
                // Found ENDFUNC or END - exit capture mode and complete the function
                LDA #CaptureMode.Off
                Statement.SetCaptureMode();
                
                // Complete the captured function
                FunctionDeclaration.CompletePartialFunction();
                CheckErrorAndSetFailure();
                if (NC) { break; }
                States.SetSuccess();
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
                
                Tokenizer.NextToken(); // no
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
                
                Tokenizer.NextToken(); // no
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
        STZ ZP.TokenBufferContentLengthL
        STZ ZP.TokenBufferContentLengthH
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
            Tokenizer.NextToken();  // no, Returns token in A, updates ZP.CurrentToken
            
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
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.CLEAR:
                {
                    parseClear();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.FORGET:
                {
                    parseForget();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.VARS:
                {
                    parseVars();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.LIST:
                {
                    LDX #0 // LIST
                    parseList();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.DASM:
                {
#ifdef DEBUG
                    LDX #1 // DASM
                    parseList();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else
                    Commands.NotAvailable();
#endif
                    
                }
                case Token.FUNCS:
                {
                    parseFuncs();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.MEM:
                {
                    parseMem();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.BYE:
                {
                    parseBye();
                    SMB1 ZP.FLAGS  // Always exit after BYE
                }
                
                case Token.HEAP:
                {
#ifdef DEBUG                    
                    parseHeap();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else                    
                    Commands.NotAvailable();
#endif
                }
                case Token.BUFFERS:
                {
#ifdef DEBUG
                    parseBuffers();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else
                    Commands.NotAvailable();
#endif
                }
                case Token.DUMP:
                {
#ifdef DEBUG
                    parseDump();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else
                    Commands.NotAvailable();
#endif
                }
                case Token.TRON:
                {
#if defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
                    parseTron();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else
                    Commands.NotAvailable();
#endif
                }
                case Token.TROFF:
                {
#if defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
                    parseTroff();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
#else
                    Commands.NotAvailable();
#endif
                }
                
                case Token.SAVE:
                {
                    parseSave();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.LOAD:
                {
                    parseLoad();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.DIR:
                {
                    parseDir();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.DEL:
                {
                    parseDel();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                case Token.FORMAT:
                {
                    parseFormat();
                    CheckError();
                    if (NC) { SMB1 ZP.FLAGS } // Set exit flag on error
                }
                
                // ========== BASIC LANGUAGE RUN ==========
                case Token.RUN:
                {
                    Tokenizer.NextToken(); // no, consume 'RUN'
                    CmdRun();
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
                        Tokenizer.NextToken(); // no
                        CMP #Token.EOL
                        if (Z) { break; }
                        CMP #Token.COLON
                        if (Z) { break; }
                    }
                }
                
                default:
                {
                    // DEFAULT CASE - All BASIC statements and declarations
                    // This includes: CLS, PRINT, assignments, function calls, variable declarations,
                    // FUNC/ENDFUNC, BEGIN/END, IF/THEN, WHILE/WEND, etc.
                    
                    // save the token before rollback -> X
                    TAX 
#ifdef HASEEPROM
                    CMP #Token.IDENTIFIER
                    if (Z)
                    {   
                        PHX // preserve X for default processing
                                 
                        Tokenizer.GetTokenStringSTR();
                        LDA # DirWalkAction.FindExecutable  // only interested in executables
                        File.Exists(); // Input: ZP.STR, Output: C if exists
                        if (C)
                        {
                            Tokenizer.PeekToken(); // just the identifier?  HEXDUMP vs HEXDUMP(1) or HEX = 10
                            CMP # Token.EOL
                            if (Z)
                            {
                                Tokenizer.NextTokenCheck(); // consume filename - point of no return in terms of default processing below
                                if (C)
                                {
                                    validateEndOfCommand();
                                    if (C)
                                    {
                                        LDA #1 // NEW first
                                        Storage.LoadProgram(); // Execute Identifier: Input: ZP.STR
                                        CheckError();
                                        if (C) 
                                        {
                                            CmdRun();                            
                                        }
                                    }
                                }
                                SMB1 ZP.FLAGS  // Always exit after auto load/run
                            }
                        }
                        
                        PLX
                    }
#endif
                    if (BBR1, ZP.FLAGS)
                    {
                        // Roll back one token so Statement can process it
                        Tokenizer.Rollback();
                        
                        // Load the token at the new position
                        Tokenizer.NextTokenCheck(); // preserves X
                        if (NC)
                        { 
                            SMB1 ZP.FLAGS // Set exit flag on error
                        }
                        else
                        {
                            Statement.Execute();  // Handles the current statement
                            CheckError();
                            if (NC) 
                            {
                                SMB1 ZP.FLAGS // Set exit flag on error
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
    
    checkFilename()
    {
        loop
        {
            LDA ZP.CurrentToken
            CMP # Token.IDENTIFIER
            if (NZ)
            {
                Error.IllegalFilename(); // default
                LDA ZP.CurrentToken
                CMP # Token.STRINGLIT
                if (Z)
                {
                    Error.IdentifierExpected();
                }
                else
                {
                    CMP # Token.EOL
                    if (Z)
                    {
                        Error.FilenameExpected();
                    }
                }
                CLC
                break;
            }
            SEC
            break;
        } // single exit
    }
    
    // Parse and execute SAVE command with string filename
    parseSave()
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
                Tokenizer.NextTokenCheck(); // consume 'SAVE'
                if (NC) { break; }
                
                // Expect string filename identifier
                checkFilename();
                if (NC) { BIT ZP.EmulatorPCL break; }
                
                Tokenizer.GetTokenStringSTR();
                
                validateNextToken();// consume string
                if (C) { Commands.CmdSave(); } // Uses ZP.STR
                break;
            }
        }
    }    
    
    // Parse and execute LOAD command with string filename
    parseLoad()
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
                Tokenizer.NextTokenCheck(); // consume 'LOAD'
                if (NC) { break; }
                
                // Expect string filename identifier
                checkFilename();
                if (NC) { BIT ZP.EmulatorPCL break; }
                
                Tokenizer.GetTokenStringSTR(); // Result in ZP.STR
                
                validateNextToken(); // consume string
                if (C) { Commands.CmdLoad(); } // Uses ZP.STR
                break;
            }
        }
    }   
    
    // Parse and execute DIR command (no arguments)
    parseDir()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            validateNextToken(); // consume 'DIR'
            if (C) { Commands.CmdDir(); }
        }
    }
    
    // Parse and execute DEL command with string filename
    parseDel()
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
                Tokenizer.NextTokenCheck(); // consume 'DEL'
                if (NC) { break; }
                
                // Expect string filename identifier
                checkFilename();
                if (NC) { BIT ZP.EmulatorPCL break; }
                
                Tokenizer.GetTokenStringSTR();
                
                validateNextToken(); // consume string
                if (C) { Commands.CmdDel(); } // Uses ZP.STR
                break;
            }
        }
    }  
    
    // Parse and execute FORMAT command (no arguments)
    parseFormat()
    {
        Statement.IsCaptureModeOn();
        if (C)
        {
            Console.FunctionModeError();
        }
        else
        {
            validateNextToken(); // consume 'FORMAT'
            if (C) { Commands.CmdFormat(); }
        }
    }
    
    
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
            validateNextToken(); // consume 'NEW'
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
            validateNextToken(); // consume 'CLEAR'
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
                Tokenizer.NextTokenCheck(); // consume 'FORGET'
                if (NC) { break; }
                
                // Expect identifier name
                LDA ZP.CurrentToken
                CMP #Token.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                Tokenizer.GetTokenStringSTR();
                
                validateNextToken(); // consume identifier
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
            validateNextToken(); // consume 'VARS'
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
            Tokenizer.NextToken(); // no, consume 'LIST'
            parseOptionalIdentifier(); // Sets ZP.STR or null
            validateEndOfCommand();
            CheckError();
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
            Tokenizer.NextToken(); // no, consume 'FUNCS'
            parseOptionalIdentifier(); // Sets ZP.STR or null
            validateEndOfCommand();
            CheckError();
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
            validateNextToken(); // consume 'MEM'
            if (C) { Commands.CmdMem(); }
        }
    }
    
    // Parse and execute BYE command
    parseBye()
    {
        validateNextToken(); // consume 'BYE'
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
            validateNextToken(); // consume 'HEAP'
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
            validateNextToken(); // consume 'BUFFERS'
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
                Tokenizer.NextToken(); // no, consume 'DUMP'
                
                // Check for optional page number
                CMP #Token.NUMBER
                if (Z)
                {
                    Tokenizer.GetTokenNumber(); // Result in ZP.ACC
                    Tokenizer.NextToken(); // no, consume number
                }
                else
                {
                    // No number provided - default to page 0
                    STZ ZP.TOPL
                    STZ ZP.TOPH
                }
                
                validateEndOfCommand();
                CheckError();
                if (C)
                { 
                    Commands.CmdDump(); // Uses ZP.TOP
                } 
                break;
            }
        }
    }
#endif

#if defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
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
            validateNextToken(); // consume 'TRON'
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
            validateNextToken(); // consume 'TROFF'
            if (C) { Commands.CmdTroff(); }
        }
    }
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
            Tokenizer.GetTokenStringSTR();
            
            Tokenizer.NextToken(); // no, consume identifier
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
    InitializeGlobals()
    {
        PHX
        PHY
        
#ifdef TRACE
        LDA #(initGlobsTrace % 256) STA ZP.TraceMessageL LDA #(initGlobsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Start iteration over all variables
#ifdef HASEEPROM        
        if (BBS3, Storage.LoaderFlags)
        {
            Variables.IterateAll();
        }
        else
        {
            Variables.IterateVariables();
        }
#else
        Variables.IterateVariables();
#endif        
        
        // Returns: ZP.IDX = first variable node, C = found, NC = none
        loop // Variable iteration loop
        {
            if (NC) 
            {
                SEC break; // No more variables - success
            }
            
            
            BufferManager.InitializeForTokenGeneration();
            
            // Get variable name for assignment
            Variables.GetName(); // -> ZP.STR
            
            Variables.GetTokens(); // Returns tokens pointer in ZP.NEXT
            Variables.GetType(); // -> ZP.ACCT
            
            // Save the current variable node
            LDA ZP.IDXL
            STA ZP.GVIL
            LDA ZP.IDXH
            STA ZP.GVIH

            if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
            {
                
                Variables.GetValue(); // Variable = IDX: BASICArray -> TOP, Tokens -> NEXT
                
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
                Tokenizer.NextToken(); // no, Get first token
                
                RMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes) - except for ZP.GVI
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                Statement.EvaluateExpression(); // EXECUTION: re-initialize ARRAY (size expression)- GLOBAL LOAD SAVE (except current variable, GVI)
                CheckError();
                if (NC) 
                { 
                    // Error during initialization
                    CLC
                    break;
                }
                
                // Restore the current variable node
                LDA ZP.GVIL
                STA ZP.IDXL
                LDA ZP.GVIH
                STA ZP.IDXH
                
                // reload for TOP (overwrites NEXT too!)
                Variables.GetValue(); // Variable = IDX: BASICArray -> TOP, Tokens -> NEXT, Element type -> ZP.TOPT
                
                Stacks.PopNext(); // newly calculated array dimension
                
                // Input:  BASICArray = TOP, desired number of elements = NEXT
                // Output: BASICArray = TOP (may be the same, may be new - always zeroed out, manages cleanup of previous array)
                BASICArray.Redimension();

                if (NC)
                {
                    break;
                }
                // Input: ZP.IDX = symbol node address (from Find), ZP.TOP = new ARRAY
                LDA ZP.TOPT
                ORA # BASICType.ARRAY
                STA ZP.TOPT
                Variables.SetValue(); // does not free old ARRAY
            } // ARRAY
            else
            {
                // Check if variable has initialization tokens
                LDA ZP.NEXTL
                ORA ZP.NEXTH
                if (Z) 
                {
                    LDA ZP.ACCT
                    AND # BASICType.MASK
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
                            if (BBR5, ZP.ACCT) // Bit 5 - ARRAY?
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
                            else
                            {
#ifdef DEBUG
                                // should never get here (see case above)
                                Error.TODO(); BIT ZP.EmulatorPCL // DEBUG
                                States.SetFailure();
                                break;
#endif
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
                Tokenizer.NextToken(); // on, Get first token
                
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes) - except for ZP.GVI
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                // These are simple assignments (FOO = 42, rather than INT FOO = 42)
                Statement.EvaluateExpression(); // EXECUTION: re-initialize global variable (RHS expression) - GLOBAL LOAD SAVE (except current variable, GVI)
                CheckError();
                if (NC) 
                { 
                    // Error during initialization
                    CLC
                    break;
                }
                
                // Restore the current variable node
                LDA ZP.GVIL
                STA ZP.IDXL
                LDA ZP.GVIH
                STA ZP.IDXH
                
                Long.PopTop(); // includes ZP.TOPT
                
                Variables.SetValue();
            } // not ARRAY
            
            // Continue to next variable
            Variables.IterateNext();
        } // iterate variables loop
        
        STZ ZP.GVIL
        STZ ZP.GVIH
        
#ifdef TRACE
        LDA #(initGlobsTrace % 256) STA ZP.TraceMessageL LDA #(initGlobsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        PLY
        PLX
    }
    
    // ========================================================================
    // RUN Command - Stays in Console because it's BASIC execution
    // ========================================================================
    
    // Execute RUN command - run the main program
    const string runTrace = "RUN";
    CmdRun()
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
#ifdef HASEEPROM
            STZ Storage.LoaderFlags // clear Bit 3 
#endif
            InitializeGlobals();
                                    
            loop // Single exit block
            {
                // Find the $MAIN function
                Messages.Main(); // point ZP.TOP -> "$MAIN"
                
                Functions.Find(); // Input: ZP.TOP = "$MAIN", Output: ZP.IDX if found
                if (NC)
                {
                    LDA # ErrorID.NoProgram LDX # MessageExtras.None Error.MessageNL();
                    break;
                }
                
                BufferManager.InitializeForTokenGeneration();
                
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
                Tokenizer.NextToken(); // no
                
                
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                Statement.EvaluateExpression(); // executes 'identifier()' as function call- GLOBAL LOAD SAVE
              
                break;
            }
        }
        
        
        CheckError();
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
