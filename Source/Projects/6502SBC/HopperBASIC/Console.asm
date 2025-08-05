unit Console
{
    uses "Messages"
    uses "Error"
    uses "State"
    uses "Tools"
    uses "Tokenizer"
    uses "Statement"
    
    uses "Objects"
    uses "Variables"
    uses "Functions"
    
    uses "Listing"
    
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
        State.SetSuccess();
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
        if (NC) { State.SetFailure(); }  // Return if tokenization failed
        else { State.SetSuccess(); }
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
        State.GetState();
        switch (A)
        {
            case SystemState.Success:   { /* continue */ }
            case SystemState.Failure:   { return; }
            case SystemState.Exiting:   { return; }
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
            if (NC) { State.SetFailure(); break; }
            
            // Check for empty line (just EOL token)
            LDA ZP.TokenBufferLengthL
            CMP #1
            if (NZ) 
            { 
                // More than one token, process normally
                processTokensAndCheckFunction();
                break;
            }
            
            LDA ZP.TokenBufferLengthH
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
            CMP #Tokens.EOL
            if (Z)
            {
                State.SetSuccess(); // Continue (empty line)
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
        State.IsExiting();
        if (NC)
        {
            // not exiting
            Error.CheckError();
            if (NC)
            {
                State.SetFailure();
            }
            else
            {
                // After processing, check if we just processed an incomplete function
                detectIncompleteFunction();
                State.SetSuccess(); // Continue
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
        CMP #Tokens.FUNC
        if (NZ)
        {
            CMP #Tokens.BEGIN
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
                    CMP #Tokens.EOL
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
                    
                    CMP #Tokens.END
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
                CMP #Tokens.EOL
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
                
                CMP #Tokens.ENDFUNC
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
            if (NC) { State.SetFailure(); break; }
            
            // Check if this line contains ENDFUNC (completing the function)
            detectFunctionEnd();
            
            if (C)
            {
                // Complete function captured - process it
                LDA #CaptureMode.Off
                Statement.SetCaptureMode();
                
                FunctionDeclaration.CompletePartialFunction();
                Error.CheckError();
                if (NC) { State.SetFailure(); break; }
            }
            
            State.SetSuccess(); // Continue (either in capture mode or completed)
            break;
        }
        
#ifdef TRACECONSOLE
        LDA #(processFuncCaptureTrace % 256) STA ZP.TraceMessageL LDA #(processFuncCaptureTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Detect if current buffer contains function end token
    detectFunctionEnd()
    {
        PHA
        PHX
        PHY
        
        // Save current position
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        
        loop // Single exit pattern
        {
            // Scan from start of buffer looking for ENDFUNC
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            
            Statement.IsCaptureModeFunc();
            if (C)
            {
                // Look for ENDFUNC
                loop
                {
                    Tokenizer.CompareTokenizerPosToLength();
                    if (C) { CLC break; } // Reached end - not found
                    
                    Tokenizer.NextToken();
                    LDA ZP.CurrentToken
                    CMP #Tokens.ENDFUNC
                    if (Z)
                    {
                        SEC // Found ENDFUNC
                        break;
                    }
                }
            }
            else
            {
                Statement.IsCaptureModeBegin();
                if (C)
                {
                    // Look for END
                    loop
                    {
                        Tokenizer.CompareTokenizerPosToLength();
                        if (C) { CLC break; } // Reached end - not found
                        
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Tokens.END
                        if (Z)
                        {
                            SEC // Found END
                            break;
                        }
                    }
                }
                else
                {
                    // Should not happen - error condition
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    CLC
                }
            }
            break; // Exit outer loop
        }
        
        // Restore position (always executed)
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
        
        PLY
        PLX
        PLA
    }
    
    

    // Execute DUMP command
    const string dumpTrace = "DUMP";
    cmdDump()
    {
#ifdef TRACECONSOLE
        LDA #(dumpTrace % 256) STA ZP.TraceMessageL LDA #(dumpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
#if defined(DEBUG) || defined(TRACE)
            Tokenizer.NextToken(); // consume 'DUMP'
            loop
            {
                // verify that the input is not a syntax error
                LDA ZP.CurrentToken
                switch (A)
                {
                    case Tokens.EOL:
                    {
                        LDA #0 // No argument - default to page 0
                    }
                    case Tokens.NUMBER:
                    {
                        Tokenizer.GetTokenNumber();
                        Tokenizer.NextToken(); // consume the argument
                        LDA ZP.TOPH
                        if (NZ)
                        {
                            CLC // > 255
                            break;
                        }
                        LDA ZP.CurrentToken
                        CMP # Tokens.EOL
                        if (NZ)
                        {
                            CLC // was expecting EOL
                            break;
                        }
                        LDA ZP.TOPL // populated by GetTokenNumber
                    }
                    default:
                    {
                        CLC // unexpected token
                        break;
                    }
                }
                // A contains page number - call DumpPage
                Debug.DumpPage();
                Messages.PrintOK();
                SEC // ok
                break;
            } // single exit
            if (NC)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
            }
#else
            Error.OnlyInDebug(); BIT ZP.EmulatorPCL
#endif
        }
        
#ifdef TRACECONSOLE
        LDA #(dumpTrace % 256) STA ZP.TraceMessageL LDA #(dumpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute HEAP command
    const string heapTrace = "HEAP";
    cmdHeap()
    {
#ifdef TRACECONSOLE
        LDA #(heapTrace % 256) STA ZP.TraceMessageL LDA #(heapTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
#if defined(DEBUG) || defined(TRACE)
            Tokenizer.NextToken(); // consume 'HEAP'
            
            Debug.DumpHeap();
            Messages.PrintOK();
#else
            Error.OnlyInDebug(); BIT ZP.EmulatorPCL
#endif
        }
        
#ifdef TRACECONSOLE
        LDA #(heapTrace % 256) STA ZP.TraceMessageL LDA #(heapTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute BUFFERS command
    const string buffersTrace = "BUFFERS";
    cmdBuffers()
    {
#ifdef TRACECONSOLE
        LDA #(buffersTrace % 256) STA ZP.TraceMessageL LDA #(buffersTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
#if defined(DEBUG) || defined(TRACE)
            Tokenizer.NextToken(); // consume 'BUFFERS'
            
            Debug.DumpBuffers();
            Messages.PrintOK();
#else
            Error.OnlyInDebug(); BIT ZP.EmulatorPCL
#endif
        }
        
#ifdef TRACECONSOLE
        LDA #(buffersTrace % 256) STA ZP.TraceMessageL LDA #(buffersTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute MEM command
    const string memTrace = "MEM";
    CmdMem()
    {
#ifdef TRACECONSOLE
        LDA #(memTrace % 256) STA ZP.TraceMessageL LDA #(memTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            Tokenizer.NextToken(); // consume 'MEM'
            
            LDA #(Messages.MemoryMsg % 256)
            STA ZP.ACCL
            LDA #(Messages.MemoryMsg / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
            
            // Get available memory
            Memory.Available();  // Pushes available memory (UInt) to stack
            Stacks.PopTop();     // Pop into TOP, modifies X
            Tools.PrintDecimalWord();
            
            LDA #(Messages.BytesMsg % 256)
            STA ZP.ACCL
            LDA #(Messages.BytesMsg / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        
#ifdef TRACECONSOLE
        LDA #(memTrace % 256) STA ZP.TraceMessageL LDA #(memTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute BYE command
    const string byeTrace = "BYE";
    cmdBye()
    {
#ifdef TRACECONSOLE
        LDA #(byeTrace % 256) STA ZP.TraceMessageL LDA #(byeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // BYE command works in both modes - allows escape from function capture
        State.SetExiting(); // Set exit status instead of fragile CLC
        
#ifdef TRACECONSOLE
        LDA #(byeTrace % 256) STA ZP.TraceMessageL LDA #(byeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Execute TRON command - turn trace execution on
    const string tronTrace = "TRON";
    cmdTron()
    {
    #ifdef TRACECONSOLE
        LDA #(tronTrace % 256) STA ZP.TraceMessageL LDA #(tronTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
#ifdef TRACE
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            Tokenizer.NextToken(); // consume 'TRON'
            
            // Verify end of line
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (NZ)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
            }
            else
            {
                // Set bit 2 of ZP.FLAGS to enable trace execution
                SMB2 ZP.FLAGS
                Messages.PrintOK();
            }
        }
#else
        Error.OnlyInTrace(); BIT ZP.EmulatorPCL
#endif        
        
    #ifdef TRACECONSOLE
        LDA #(tronTrace % 256) STA ZP.TraceMessageL LDA #(tronTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute TROFF command - turn trace execution off
    const string troffTrace = "TROFF";
    cmdTroff()
    {
    #ifdef TRACECONSOLE
        LDA #(troffTrace % 256) STA ZP.TraceMessageL LDA #(troffTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
#ifdef TRACE
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            Tokenizer.NextToken(); // consume 'TROFF'
            
            // Verify end of line
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (NZ)
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
            }
            else
            {
                // Clear bit 2 of ZP.FLAGS to disable trace execution
                RMB2 ZP.FLAGS
                Messages.PrintOK();
            }
        }
#else
        Error.OnlyInTrace(); BIT ZP.EmulatorPCL
#endif  
        
        
    #ifdef TRACECONSOLE
        LDA #(troffTrace % 256) STA ZP.TraceMessageL LDA #(troffTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    

    // Execute NEW command
#ifdef TRACECONSOLE
    const string newTrace = "NEW";
#endif
    cmdNew()
    {
#ifdef TRACECONSOLE
        LDA #(newTrace % 256) STA ZP.TraceMessageL LDA #(newTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            Tokenizer.NextToken(); // consume 'NEW'
            
            Variables.Clear();
            Functions.Clear();
            Messages.PrintOK();
        }
        
#ifdef TRACECONSOLE
        LDA #(newTrace % 256) STA ZP.TraceMessageL LDA #(newTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute CLEAR command
#ifdef TRACECONSOLE
    const string clearTrace = "CLEAR";
#endif
    cmdClear()
    {
#ifdef TRACECONSOLE
        LDA #(clearTrace % 256) STA ZP.TraceMessageL LDA #(clearTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            Tokenizer.NextToken(); // consume 'CLEAR'
            
            Variables.Clear();
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            Messages.PrintOK();
        }
        
#ifdef TRACECONSOLE
        LDA #(clearTrace % 256) STA ZP.TraceMessageL LDA #(clearTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute FORGET command - remove variable, constant, or function by name
    // Input: ZP.CurrentToken = FORGET token
    // Output: Named symbol removed from appropriate table, or error if not found
    // Usage: FORGET identifier
    // Error: Sets ZP.LastError if function mode, syntax error, or identifier not found
    const string forgetTrace = "FORGET";
    cmdForget()
    {
#ifdef TRACECONSOLE
        LDA #(forgetTrace % 256) STA ZP.TraceMessageL LDA #(forgetTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            loop // Single exit block for clean error handling
            {
                Tokenizer.NextToken(); // consume 'FORGET'
                Error.CheckError();
                if (NC) { break; }
                
                // Expect identifier name
                LDA ZP.CurrentToken
                CMP #Tokens.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get the identifier name
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                Error.CheckError();
                if (NC) { break; }
                
                // Save name pointer for multiple attempts
                LDA ZP.TOPL
                PHA
                LDA ZP.TOPH
                PHA
                
                // Try to remove as variable/constant first
                Variables.Remove(); // Input: ZP.TOP = name pointer
                if (C)
                {
                    // Successfully removed variable/constant
                    // Clean up stack and advance to next token
                    PLA
                    STA ZP.TOPH
                    PLA
                    STA ZP.TOPL
                    
                    Tokenizer.NextToken(); // consume identifier
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Verify end of line
                    LDA ZP.CurrentToken
                    CMP #Tokens.EOL
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
                    Messages.PrintOK();
                    break;
                }
                
                // Not found as variable/constant, try function
                // Restore name pointer
                PLA
                STA ZP.TOPH
                PLA
                STA ZP.TOPL
                
                Functions.Remove(); // Input: ZP.TOP = name pointer
                if (C)
                {
                    // Successfully removed function
                    Tokenizer.NextToken(); // consume identifier
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Verify end of line
                    LDA ZP.CurrentToken
                    CMP #Tokens.EOL
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    Messages.PrintOK();
                    break;
                }
                
                // Not found in either table
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                break;
            } // Single exit block
        }
        
#ifdef TRACECONSOLE
        LDA #(forgetTrace % 256) STA ZP.TraceMessageL LDA #(forgetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute RUN command - execute the main program ($MAIN function)
    // Execute RUN command - execute the main program ($MAIN function)
    const string runTrace = "RUN";
    cmdRun()
    {
    #ifdef TRACECONSOLE
        LDA #(runTrace % 256) STA ZP.TraceMessageL LDA #(runTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
        }
        else
        {
            loop // Single exit block
            {
                Tokenizer.NextToken(); // consume 'RUN'
                Error.CheckError();
                if (NC) { break; }
                
                // Look for $MAIN function
                LDA #(Messages.BeginFunctionName % 256)
                STA ZP.TOPL
                LDA #(Messages.BeginFunctionName / 256)
                STA ZP.TOPH
                
                Functions.Find();
                if (NC)
                {
                    // No main program found
                    LDA #(Messages.NoMainProgram % 256)
                    STA ZP.ACCL
                    LDA #(Messages.NoMainProgram / 256)
                    STA ZP.ACCH
                    Tools.PrintStringACC();
                    break;
                }
                // Create tokens for: IDENTIFIER "$MAIN" LPAREN RPAREN EOL
                LDX #0
                
                // Token 1: IDENTIFIER
                LDA #Tokens.IDENTIFIER
                STA Address.BasicTokenizerBuffer, X
                INX
                LDY #0
                loop
                {
                    LDA [ZP.TOP], Y
                    STA Address.BasicTokenizerBuffer, X
                    if (Z) { break; } // that was '\0'
                    INX
                    INY
                }
                INX

                
                // Token 2: LPAREN
                LDA #Tokens.LPAREN
                STA Address.BasicTokenizerBuffer, X
                INX
                
                // Token 3: RPAREN
                LDA #Tokens.RPAREN
                STA Address.BasicTokenizerBuffer, X
                INX
                
                // Token 4: EOL
                LDA #Tokens.EOL
                STA Address.BasicTokenizerBuffer, X
                INX
                
                // Clear tokenizer state
                Tokenizer.Initialize();
                INX
                STX ZP.TokenBufferLengthL
                
                Tokenizer.NextToken();
                Statement.EvaluateExpression(); // executes 'indentifier()' as function call
                
                break;
            }
        }
        
    #ifdef TRACECONSOLE
        LDA #(runTrace % 256) STA ZP.TraceMessageL LDA #(runTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
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
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        // Clear any error state that might have been set during capture
        Error.ClearError();
        State.SetSuccess();
        
#ifdef TRACECONSOLE
        LDA #(exitFuncModeTrace % 256) STA ZP.TraceMessageL LDA #(exitFuncModeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    /*
    // Process the tokens in BasicTokenizerBuffer  
    processTokens()
    {
        loop  // Main statement loop for colon-separated statements
        {
            // Get current token
            Tokenizer.NextToken();  // Returns token in A, updates ZP.CurrentToken
            LDA ZP.CurrentToken
            
            // Check for end of line first
            CMP #Tokens.EOL
            if (Z) { break; }  // End of all statements
            
            CMP #Tokens.EOF
            if (Z) { break; }  // End of all statements
            
            // Execute the current statement
            switch (A)
            {
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // Comments at top level are just ignored
                    // Skip to end of line or next colon
                    loop
                    {
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Tokens.EOL
                        if (Z) { break; }
                        CMP #Tokens.COLON
                        if (Z) { break; }
                    }
                }
                case Tokens.TRON:
                {
                    cmdTron();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.TROFF:
                {
                    cmdTroff();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.NEW:
                {
                    cmdNew();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.FORGET:
                {
                    cmdForget();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.CLEAR:
                {
                    cmdClear();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.VARS:
                {
                    Listing.CmdVars();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.LIST:
                {
                    Listing.CmdList();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.FUNCS:
                {
                    Listing.CmdFuncs();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.MEM:
                {
                    CmdMem();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.HEAP:
                {
                    cmdHeap();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BUFFERS:
                {
                    cmdBuffers();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.DUMP:
                {
                    cmdDump();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BYE:
                {
                    cmdBye();
                    return; // BYE sets SystemState.Exiting
                }
                case Tokens.SAVE:
                case Tokens.LOAD:
                case Tokens.DIR:
                case Tokens.DEL:
                {
                    Error.NotImplemented(); BIT ZP.EmulatorPCL
                }
                case Tokens.RUN:
                {
                    cmdRun();
                    Error.CheckError();
                    // RUN was a '$MAIN' function call (buffer is munted so even if there was a ':', no point)
                    // We can remove this when function compiling doesn't use shared token and buffers
                    if (NC)
                    {
                        State.IsExiting();
                        if (C)
                        {
                            State.SetFailure(); // don't "BYE" if we have a failure (probably syntax error from Executor)
                        }
                        CLC // Error branch
                    }
                    else
                    {
                        State.IsExiting();
                        if (C)
                        {
                            State.SetSuccess(); // don't "BYE" if we are just Exiting REPL
                        }
                        SEC // No error branch
                    }
                    return;
                }
                default:
                {
                    // Not a console command, try to execute as a statement
                    Statement.Execute();
                    Error.CheckError();
                    if (NC)
                    {
                        State.IsExiting();
                        if (C)
                        {
                            State.SetFailure(); // don't "BYE" if we have a failure (probably syntax error from Executor)
                        }
                        CLC // Error branch
                        return; 
                    }
                    else
                    {
                        State.IsReturn();
                        if (C)
                        {
                            // REPL was a function call (buffer is munted so even if there was a ':', no point)
                            // We can remove this when function compiling doesn't use shared token and buffers
                            return; 
                        }
                        State.IsExiting();
                        if (C)
                        {
                            State.SetSuccess(); // don't "BYE"
                            return; // Exiting implies end of stream (REPL function call?)
                        }
                        SEC // No error branch
                    }
                }
            }
            
            // After executing statement, check what comes next
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                break; // End of line
            }  
            
            CMP #Tokens.COLON
            if (Z) 
            { 
                continue;  // Found colon, continue with next statement
            }

            // If we get here, unexpected token after statement
            Error.SyntaxError(); BIT ZP.EmulatorPCL
            return;
        }
        
        // Success - all statements processed
    }
    */
    
    // In Console.asm - Enhanced processTokens() with hybrid approach:


    // Process the tokens in BasicTokenizerBuffer  
    processTokens()
    {
        loop  // Main statement loop for colon-separated statements
        {
            // Get current token
            Tokenizer.NextToken();  // Returns token in A, updates ZP.CurrentToken
            LDA ZP.CurrentToken
            
            // Check for end of line first
            CMP #Tokens.EOL
            if (Z) { break; }  // End of all statements
            
            CMP #Tokens.EOF
            if (Z) { break; }  // End of all statements
            
            // Execute the current statement/command
            switch (A)
            {
                // CONSOLE COMMANDS - Execute directly (existing pattern)
                case Tokens.TRON:
                {
                    cmdTron();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.TROFF:
                {
                    cmdTroff();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.NEW:
                {
                    cmdNew();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.CLEAR:
                {
                    cmdClear();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.FORGET:
                {
                    cmdForget();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.VARS:
                {
                    Listing.CmdVars();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.LIST:
                {
                    Listing.CmdList();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.FUNCS:
                {
                    Listing.CmdFuncs();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.MEM:
                {
                    CmdMem();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.HEAP:
                {
                    cmdHeap();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BUFFERS:
                {
                    cmdBuffers();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.DUMP:
                {
                    cmdDump();
                    Error.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BYE:
                {
                    cmdBye();
                    return; // BYE sets SystemState.Exiting
                }
                case Tokens.SAVE:
                case Tokens.LOAD:
                case Tokens.DIR:
                case Tokens.DEL:
                {
                    Error.NotImplemented(); BIT ZP.EmulatorPCL
                }
                case Tokens.RUN:
                {
                    cmdRun();
                    Error.CheckError();
                    // RUN was a '$MAIN' function call (buffer is munted so even if there was a ':', no point)
                    // We can remove this when function compiling doesn't use shared token and buffers
                    if (NC)
                    {
                        State.IsExiting();
                        if (C)
                        {
                            State.SetFailure(); // don't "BYE" if we have a failure (probably syntax error from Executor)
                        }
                        CLC // Error branch
                    }
                    else
                    {
                        State.IsExiting();
                        if (C)
                        {
                            State.SetSuccess(); // don't "BYE" if we are just Exiting REPL
                        }
                        SEC // No error branch
                    }
                    return;
                }
                
                // COMMENTS - Skip over them
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // Comments at top level are just ignored
                    // Skip to end of line or next colon
                    loop
                    {
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Tokens.EOL
                        if (Z) { break; }
                        CMP #Tokens.COLON
                        if (Z) { break; }
                    }
                }
                
                default:
                {
                    // DEFAULT CASE - This is where the magic happens
                    // All non-console-command statements go here
                    // This includes: PRINT, assignments, function calls, variable declarations, etc.
                    
                    // Check if this might cause buffer stomping (contains function calls)
                    mightCallFunctions();
                    if (C)  // Need to implement this helper
                    {
                        // Use $REPL function approach for safety
                        executeViaReplFunction();
                        Error.CheckError();
                        if (NC)
                        {
                            State.IsExiting();
                            if (C)
                            {
                                State.SetFailure(); // don't "BYE" if we have a failure
                            }
                            CLC // Error branch
                            return; 
                        }
                        else
                        {
                            State.IsReturn();
                            if (C)
                            {
                                // REPL was a function call (buffer is munted so even if there was a ':', no point)
                                return; 
                            }
                            State.IsExiting();
                            if (C)
                            {
                                State.SetSuccess(); // don't "BYE"
                                return; // Exiting implies end of stream (REPL function call?)
                            }
                            SEC // No error branch
                        }
                    }
                    else
                    {
                        // Safe to execute directly (original path)
                        Statement.Execute();
                        Error.CheckError();
                        if (NC)
                        {
                            State.IsExiting();
                            if (C)
                            {
                                State.SetFailure(); // don't "BYE" if we have a failure
                            }
                            CLC // Error branch
                            return; 
                        }
                        else
                        {
                            State.IsReturn();
                            if (C)
                            {
                                // REPL was a function call (buffer is munted so even if there was a ':', no point)
                                return; 
                            }
                            State.IsExiting();
                            if (C)
                            {
                                State.SetSuccess(); // don't "BYE"
                                return; // Exiting implies end of stream (REPL function call?)
                            }
                            SEC // No error branch
                        }
                    }
                }
            } // switch

            // After executing statement, check what comes next
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                break; // End of line
            }  
            
            CMP #Tokens.COLON
            if (Z) 
            { 
                continue;  // Found colon, continue with next statement
            }

            // If we get here, unexpected token after statement
            Error.SyntaxError(); BIT ZP.EmulatorPCL
            return;
        }
        
        // Success - all statements processed
    }

    // Helper function to detect if current statement sequence might call functions
    // This is a simple heuristic to avoid the $REPL overhead for simple cases
    // Input: ZP.CurrentToken = first token of statement
    // Output: C set if might call functions, NC if safe for direct execution
    const string mightCallFunctionsTrace = "MightCall";
    mightCallFunctions()
    {
    #ifdef TRACE
        LDA #(mightCallFunctionsTrace % 256) STA ZP.TraceMessageL 
        LDA #(mightCallFunctionsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        // Save current tokenizer position
        LDA ZP.TokenizerPosL
        PHA
        LDA ZP.TokenizerPosH
        PHA
        
        // Simple heuristics:
        // 1. If line contains IDENTIFIER followed by LPAREN → might be function call
        // 2. If line contains multiple statements (COLON) → might be complex
        // 3. For now, be conservative and assume most cases need $REPL
        
        LDA ZP.CurrentToken
        switch (A)
        {
            // These are definitely safe for direct execution
            case Tokens.INT:
            case Tokens.WORD:  
            case Tokens.BIT:
            case Tokens.BYTE:
            case Tokens.STRING:
            case Tokens.CONST:
            case Tokens.FUNC:
            case Tokens.BEGIN:
            {
                // Restore tokenizer position
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
                CLC // Safe for direct execution
            }
            
            default:
            {
                // Everything else (PRINT, assignments, function calls) → use $REPL
                // Restore tokenizer position
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
                SEC // Might call functions, use $REPL
            }
        }

    #ifdef TRACE
        LDA #(mightCallFunctionsTrace % 256) STA ZP.TraceMessageL 
        LDA #(mightCallFunctionsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }


    // Create a temporary $REPL function with current line content
    // Input: BasicTokenizerBuffer contains the tokenized REPL line
    // Output: $REPL function created with line content as body
    // Modifies: Functions table, memory allocation
    const string createReplFunctionTrace = "CreateREPL";
    createReplFunction()
    {
    #ifdef TRACE
        LDA #(createReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(createReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        loop // Single exit block
        {
            // Check if $REPL function already exists and remove it
            LDA #(Messages.ReplFunctionName % 256)  // Need to add this to Messages
            STA ZP.TOPL
            LDA #(Messages.ReplFunctionName / 256)
            STA ZP.TOPH
            
            Functions.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // $REPL function exists - remove it (reuse pattern from ExecuteBeginDeclaration)
                Functions.Remove();
                Error.CheckError();
                if (NC) { break; }
                
                // Restore the name pointer for declaration
                LDA #(Messages.ReplFunctionName % 256)
                STA ZP.TOPL
                LDA #(Messages.ReplFunctionName / 256)
                STA ZP.TOPH
            }
            
            LDA # Tokens.EOF
            Tokenizer.appendToTokenBuffer();
            Error.CheckError();
            if (NC) { break; }
            
            // Calculate length of current tokenizer buffer content
            LDA ZP.TokenBufferLengthH
            STA ZP.ACCH
            LDA ZP.TokenBufferLengthL
            STA ZP.ACCL
            
            // Allocate memory for function body tokens (same as captureBeginBody)
            Memory.Allocate(); // Input: ZP.ACC, Output: ZP.IDX = allocated memory
            Error.CheckError();
            if (NC) { break; }
            
            // Copy tokens from BasicTokenizerBuffer to function body
            LDA ZP.TokenBufferLengthH
            STA ZP.FLENGTHH      
            LDA ZP.TokenBufferLengthL
            STA ZP.FLENGTHL      
            
            LDA #(Address.BasicTokenizerBuffer / 256)
            STA ZP.FSOURCEADDRESSH            
            LDA #(Address.BasicTokenizerBuffer % 256)
            STA ZP.FSOURCEADDRESSL

            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
                                    
            Tools.CopyBytes(); // Copy tokens to function body memory

            // Initialize empty arguments list and prepare body tokens
            STZ ZP.NEXTL // Arguments list head = null (no arguments)
            STZ ZP.NEXTH
            
            LDA ZP.IDXL     // Token stream pointer (from allocation)
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Declare the $REPL function (same pattern as ExecuteBeginDeclaration)
            Functions.Declare(); // Input: ZP.TOP = name, ZP.NEXT = args head, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            // Save function node address for later use
            LDA ZP.IDXL
            STA (Statement.replFunctionPtr + 0)  // Need to add this storage
            LDA ZP.IDXH
            STA (Statement.replFunctionPtr + 1)
            
            SEC // Success
            break;
        }

    #ifdef TRACE
        LDA #(createReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(createReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Execute the temporary $REPL function
    // Input: $REPL function created and ready in functions table
    // Output: Function executed with its own buffer space
    // Uses same pattern as cmdRun() but for $REPL instead of $MAIN
    const string executeReplFunctionTrace = "ExecREPL";
    executeReplFunction()
    {
    #ifdef TRACE
        LDA #(executeReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif
    
        loop // Single exit block
        {
            // Create tokens for: IDENTIFIER "$REPL" LPAREN RPAREN EOL
            // (Same pattern as cmdRun() but with "$REPL" instead of "$MAIN")
            LDX #0
            
            // Token 1: IDENTIFIER
            LDA #Tokens.IDENTIFIER
            STA Address.BasicTokenizerBuffer, X
            INX
            
            // Copy "$REPL" string to buffer
            LDY #0
            loop
            {
                LDA Messages.ReplFunctionName, Y
                STA Address.BasicTokenizerBuffer, X
                if (Z) { break; } // that was '\0'
                INX
                INY
            }
            INX // Skip past null terminator
            
            // Token 2: LPAREN
            LDA #Tokens.LPAREN
            STA Address.BasicTokenizerBuffer, X
            INX
            
            // Token 3: RPAREN
            LDA #Tokens.RPAREN
            STA Address.BasicTokenizerBuffer, X
            INX
            
            // Token 4: EOL
            LDA #Tokens.EOL
            STA Address.BasicTokenizerBuffer, X
            INX
            
            // Clear tokenizer state
            Tokenizer.Initialize();
            INX
            STX ZP.TokenBufferLengthL
            
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Execute the function call (same as cmdRun() and Statement.Execute())
            Statement.Execute(); // This will handle the function call via JIT
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
    
    #ifdef TRACE
        LDA #(executeReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Clean up the temporary $REPL function
    // Input: $REPL function exists
    // Output: Function removed from functions table
    const string destroyReplFunctionTrace = "DestroyREPL";  
    destroyReplFunction()
    {
    #ifdef TRACE
        LDA #(destroyReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(destroyReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        loop // Single exit block
        {
            // Find and remove $REPL function
            LDA #(Messages.ReplFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.ReplFunctionName / 256)
            STA ZP.TOPH
            
            Functions.Remove(); // Automatically frees associated memory
            // Don't check error - function might not exist in error cases
            break;
        }

    #ifdef TRACE
        LDA #(destroyReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(destroyReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Execute complex statements via temporary $REPL function
    // This solves the buffer stomping problem for statements that might call functions
    // Input: ZP.CurrentToken points to start of statement(s), tokenizer positioned
    // Output: Statement(s) executed safely with isolated buffers
    const string executeViaReplFunctionTrace = "ExecViaREPL";
    executeViaReplFunction()
    {
    #ifdef TRACE
        LDA #(executeViaReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeViaReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        loop // Single exit block
        {
            // Save current tokenizer position (start of statement)
            LDA ZP.TokenizerPosL
            PHA
            LDA ZP.TokenizerPosH
            PHA
            
            // Scan to end of all statements on this line to get total length
            scanToEndOfStatements();
            Error.CheckError();
            if (NC) 
            {
                // Restore tokenizer position on error
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
                break; 
            }
            
            // Calculate length of statement sequence (current pos - saved pos)
            PLA // Get saved position LSB
            STA ZP.FSOURCEADDRESSL
            PLA // Get saved position MSB  
            STA ZP.FSOURCEADDRESSH
            
            // Length = current position - saved position
            LDA ZP.TokenizerPosL
            SEC
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.TokenizerPosH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
            // Reset tokenizer to start of statements
            LDA ZP.FSOURCEADDRESSL
            STA ZP.TokenizerPosL
            LDA ZP.FSOURCEADDRESSH
            STA ZP.TokenizerPosH
            
            // Create $REPL function with the statement sequence
            createReplFunction();
            Error.CheckError();
            if (NC) { break; }
            
            // Execute the $REPL function 
            executeReplFunction();
            Error.CheckError();
            
            // Clean up (always do this, even on error)
            destroyReplFunction();
            if (NC) 
            {
                break;  // Propagate execution error
            }
            SEC // Success
            State.IsFailure(); // not Success|Return|Exiting
            if (C)
            {
                CLC
            }
            break;
        }

    #ifdef TRACE
        LDA #(executeViaReplFunctionTrace % 256) STA ZP.TraceMessageL 
        LDA #(executeViaReplFunctionTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }

    // Scan forward to find the end of all statements on current line
    // Input: ZP.CurrentToken positioned at start of statements
    // Output: ZP.TokenizerPos positioned after last statement, ZP.CurrentToken = EOL/EOF
    // Modifies: ZP.TokenizerPos, ZP.CurrentToken
    const string scanToEndOfStatementsTrace = "ScanStmts";
    scanToEndOfStatements()
    {
    #ifdef TRACE
        LDA #(scanToEndOfStatementsTrace % 256) STA ZP.TraceMessageL 
        LDA #(scanToEndOfStatementsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        loop // Scan until end of line
        {
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z) { SEC break; } // Found end
            
            CMP #Tokens.EOF
            if (Z) { SEC break; } // Found end
            
            // Advance to next token
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
        }

    #ifdef TRACE
        LDA #(scanToEndOfStatementsTrace % 256) STA ZP.TraceMessageL 
        LDA #(scanToEndOfStatementsTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
    
    
}
