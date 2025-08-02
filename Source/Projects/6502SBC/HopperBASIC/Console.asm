unit Console
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/MemoryMap"
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
        if (NC) { State.SetFailure(); return; }  // Return if tokenization failed
        
        State.SetSuccess();
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
    processLineNormal()
    {
        // Check for tokenization errors
        Error.CheckError();
        if (NC) { State.SetFailure(); return; }
        
        // Check for empty line (just EOL token)
        LDA ZP.TokenBufferLengthL
        CMP #1
        if (NZ) 
        { 
            // More than one token, process normally
            processTokensAndCheckFunction();
            return;
        }
        
        LDA ZP.TokenBufferLengthH
        if (NZ)
        {
            // Definitely more than one token
            processTokensAndCheckFunction();
            return;
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
            return;
        }
        
        // Single non-EOL token, reset position and process it
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        processTokensAndCheckFunction();
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
                // Restore position
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
                return; // Not a function
            }
            
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
    }

    // Function capture mode line processing
    processLineFunctionCapture()
    {
        // Check for tokenization errors
        Error.CheckError();
        if (NC) { State.SetFailure(); 
        return; }
        
        // Check if this line contains ENDFUNC (completing the function)
        detectFunctionEnd();
        
        if (C)
        {
            // Complete function captured - process it
            LDA #CaptureMode.Off
            Statement.SetCaptureMode();
            
            FunctionDeclaration.CompletePartialFunction();
            Error.CheckError();
            if (NC) { State.SetFailure(); 
            return; }
        }
        
        State.SetSuccess(); // Continue (either in capture mode or completed)
    }

    detectFunctionEnd()
    {
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
                }
            }
            break; // Exit outer loop
        }
        
        // Restore position (always executed)
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
    }
    
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
                case Tokens.RUN:
                {
                    cmdRun();
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
                default:
                {
                    // Not a console command, try to execute as a statement
                    Statement.Execute();
                    Error.CheckError();
                    if (NC) { return; }
                }
            }
            
            // After executing statement, check what comes next
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z) { break; }  // End of line
            
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

#ifdef TRACE
    const string cmdDumpTrace = "DUMP";
#endif
    // Execute DUMP command
    cmdDump()
    {
#ifdef TRACE
LDA #(cmdDumpTrace % 256) STA ZP.TraceMessageL LDA #(cmdDumpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdDumpTrace % 256) STA ZP.TraceMessageL LDA #(cmdDumpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
#ifdef DEBUG
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
#ifdef TRACE
LDA #(cmdDumpTrace % 256) STA ZP.TraceMessageL LDA #(cmdDumpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdHeapTrace = "HEAP";
#endif
    // Execute HEAP command
    cmdHeap()
    {
#ifdef TRACE
LDA #(cmdHeapTrace % 256) STA ZP.TraceMessageL LDA #(cmdHeapTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdHeapTrace % 256) STA ZP.TraceMessageL LDA #(cmdHeapTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
#ifdef DEBUG
        Tokenizer.NextToken(); // consume 'HEAP'
        
        Debug.DumpHeap();
        Messages.PrintOK();
#else
        Error.OnlyInDebug(); BIT ZP.EmulatorPCL
#endif
#ifdef TRACE
LDA #(cmdHeapTrace % 256) STA ZP.TraceMessageL LDA #(cmdHeapTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdBuffersTrace = "BUFFERS";
#endif
    // Execute BUFFERS command
    cmdBuffers()
    {
#ifdef TRACE
LDA #(cmdBuffersTrace % 256) STA ZP.TraceMessageL LDA #(cmdBuffersTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdBuffersTrace % 256) STA ZP.TraceMessageL LDA #(cmdBuffersTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
#ifdef DEBUG
        Tokenizer.NextToken(); // consume 'BUFFERS'
        
        Debug.DumpBasicBuffers();
        Messages.PrintOK();
#else
        Error.OnlyInDebug(); BIT ZP.EmulatorPCL
#endif
#ifdef TRACE
LDA #(cmdBuffersTrace % 256) STA ZP.TraceMessageL LDA #(cmdBuffersTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdMemTrace = "MEM";
#endif
    // Execute MEM command
    CmdMem()
    {
#ifdef TRACE
LDA #(cmdMemTrace % 256) STA ZP.TraceMessageL LDA #(cmdMemTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdMemTrace % 256) STA ZP.TraceMessageL LDA #(cmdMemTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
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
#ifdef TRACE
LDA #(cmdMemTrace % 256) STA ZP.TraceMessageL LDA #(cmdMemTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdByeTrace = "BYE";
#endif
    // Execute BYE command
    cmdBye()
    {
#ifdef TRACE
LDA #(cmdByeTrace % 256) STA ZP.TraceMessageL LDA #(cmdByeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // BYE command works in both modes - allows escape from function capture
        State.SetExiting(); // Set exit status instead of fragile CLC
#ifdef TRACE
LDA #(cmdByeTrace % 256) STA ZP.TraceMessageL LDA #(cmdByeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdNewTrace = "NEW";
#endif
    // Execute NEW command
    cmdNew()
    {
#ifdef TRACE
LDA #(cmdNewTrace % 256) STA ZP.TraceMessageL LDA #(cmdNewTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdNewTrace % 256) STA ZP.TraceMessageL LDA #(cmdNewTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
        Tokenizer.NextToken(); // consume 'NEW'
        
        Variables.Clear();
        Functions.Clear();
        Messages.PrintOK();
#ifdef TRACE
LDA #(cmdNewTrace % 256) STA ZP.TraceMessageL LDA #(cmdNewTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdClearTrace = "CLEAR";
#endif
    // Execute CLEAR command
    cmdClear()
    {
#ifdef TRACE
LDA #(cmdClearTrace % 256) STA ZP.TraceMessageL LDA #(cmdClearTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdClearTrace % 256) STA ZP.TraceMessageL LDA #(cmdClearTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
        Tokenizer.NextToken(); // consume 'CLEAR'
        
        Variables.Clear();
        Messages.PrintOK();
#ifdef TRACE
LDA #(cmdClearTrace % 256) STA ZP.TraceMessageL LDA #(cmdClearTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdForgetTrace = "FORGET";
#endif
    // Execute FORGET command - remove variable, constant, or function by name
    // Input: ZP.CurrentToken = FORGET token
    // Output: Named symbol removed from appropriate table, or error if not found
    // Usage: FORGET identifier
    // Error: Sets ZP.LastError if function mode, syntax error, or identifier not found
    cmdForget()
    {
#ifdef TRACE
LDA #(cmdForgetTrace % 256) STA ZP.TraceMessageL LDA #(cmdForgetTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdForgetTrace % 256) STA ZP.TraceMessageL LDA #(cmdForgetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
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
                
                Messages.PrintOK();
                SEC // Success
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
                SEC // Success
                break;
            }
            
            // Not found in either table
            Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
            CLC
            break;
        } // Single exit block
#ifdef TRACE
LDA #(cmdForgetTrace % 256) STA ZP.TraceMessageL LDA #(cmdForgetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef TRACE
    const string cmdRunTrace = "RUN";
#endif
    // Execute RUN command
    cmdRun()
    {
#ifdef TRACE
LDA #(cmdRunTrace % 256) STA ZP.TraceMessageL LDA #(cmdRunTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        Statement.IsCaptureModeOn();
        if (C)
        {
            Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdRunTrace % 256) STA ZP.TraceMessageL LDA #(cmdRunTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        
        // TODO: Run program
        Error.NotImplemented(); BIT ZP.EmulatorPCL
#ifdef TRACE
LDA #(cmdRunTrace % 256) STA ZP.TraceMessageL LDA #(cmdRunTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Exit function capture mode and return to normal (called from Console or Tokenizer on Ctrl+C)
    ExitFunctionCaptureMode()
    {
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
    }
}
