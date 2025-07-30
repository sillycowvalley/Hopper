unit Console
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Messages"
    uses "Tools"
    uses "Tokenizer"
    uses "Statement"
    
    uses "Objects"
    uses "Variables"
    uses "Functions"
    
    // Function capture mode storage - using Statement.asm BasicProcessBuffer2
    const uint funcCaptureMode = Address.BasicProcessBuffer2 + 17; // 0x09D1: 1 byte - console mode
    
    // String constants for VARS command
    const string noVariablesMsg = "NO VARIABLES\n";
    
    // Error handler for commands in function mode
    functionModeError()
    {
        LDA #(Messages.SyntaxError % 256)
        STA ZP.LastErrorL
        LDA #(Messages.SyntaxError / 256)
        STA ZP.LastErrorH
        Messages.StorePC();
        CLC
    }
    
    Initialize()
    {
        // Initialize tokenizer
        Tokenizer.Initialize();
        
        // Initialize symbol tables
        Objects.Initialize();
        
        // Initialize function capture mode
        Statement.SetNormalMode();
    }
    
    // Read a line of input and tokenize it
    ReadLine()
    {
        Tokenizer.ReadLine();    // Read into BasicInputBuffer, sets ZP.BasicInputLength
        
        // Tokenize based on current mode
        IsCaptureMode();
        if (C)
        {
            // Function capture mode - append to existing buffer
            Tokenizer.TokenizeAndAppendLine();
        }
        else
        {
            // Normal mode - replace buffer
            Tokenizer.TokenizeLine();
        }
        
        Messages.CheckError();
        if (NC) { return; }  // Return if tokenization failed
    }
    
    // Enhanced ProcessLine() to handle function capture mode
    ProcessLine()
    {
        IsCaptureMode();
        if (C)
        {
            // Function capture mode processing
            processLineFunctionCapture();
        }
        else
        {
            // Normal mode processing
            processLineNormal();
        }
    }
    
    // Normal mode processing
    processLineNormal()
    {
        // Check for tokenization errors
        Messages.CheckError();
        if (NC) { return; }  // Error during tokenization
        
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
            SEC  // Continue (empty line)
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
        // Check if this line starts an incomplete function
        detectIncompleteFunction();
        if (C)
        {
            SetCaptureMode();
            SEC // Continue in capture mode
            return;
        }
        
        // Process normally
        processTokens();
    }
     
    // Detect if current line starts FUNC but doesn't end with ENDFUNC
    detectIncompleteFunction()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'I'
        Tools.COut();
    #endif

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
            // Restore position
            PLA
            STA ZP.TokenizerPosH
            PLA
            STA ZP.TokenizerPosL
            CLC // Not a function
            return;
        }
        
        // Scan through tokens looking for ENDFUNC
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
                SEC // incomplete function found !!
                return; 
            }
            
            CMP #Tokens.ENDFUNC
            if (Z)
            {
                // Restore position
                PLA
                STA ZP.TokenizerPosH
                PLA
                STA ZP.TokenizerPosL
                CLC // Found ENDFUNC - complete function
                return;
            }
        } // loop
        
    #ifdef DEBUG
        LDA #'D'
        Tools.COut();
        LDA #'I'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
      
    // Process complete function buffer
    processCompleteFunctionBuffer()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        
        // Show buffer state before processing
        LDA #'B'
        Tools.COut();
        LDA ZP.TokenBufferLengthL
        Tools.HOut();
        LDA ZP.TokenBufferLengthH
        Tools.HOut();
        
        LDA #'P'
        Tools.COut();
        LDA ZP.TokenizerPosL
        Tools.HOut();
        LDA ZP.TokenizerPosH
        Tools.HOut();
    #endif

        // Reset to start of function and process normally
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
    #ifdef DEBUG
        LDA #'R' // Reset complete
        Tools.COut();
    #endif
        
        processTokens();
        
    #ifdef DEBUG
        LDA #'P'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }

    // Function capture mode line processing
    processLineFunctionCapture()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'C'
        Tools.COut();
    #endif

        // Check for tokenization errors
        Messages.CheckError();
        if (NC) { return; }
        
        // Check if this line contains ENDFUNC (completing the function)
        detectFunctionEnd();
        
    #ifdef DEBUG
        PHA // Save A
        PHP // Save flags
        if (C)
        {
            LDA #'E' // ENDFUNC detected
            Tools.COut();
        }
        else
        {
            LDA #'N' // No ENDFUNC
            Tools.COut();
        }
        PLP // Restore flags
        PLA // Restore A
    #endif
        
        if (C)
        {
            // Complete function captured - process it
    #ifdef DEBUG
            LDA #'S' // Setting normal mode
            Tools.COut();
    #endif
            SetNormalMode();
            
    #ifdef DEBUG
            LDA #'C' // Calling processCompleteFunctionBuffer
            Tools.COut();
    #endif
            processCompleteFunctionBuffer();
        }
        
    #ifdef DEBUG
        LDA #'L'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
        
        SEC // Continue (either in capture mode or completed)
    }

    detectFunctionEnd()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'E'
        Tools.COut();
    #endif

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
            break; // Exit outer loop
        }
        
        // Restore position (always executed)
        PLA
        STA ZP.TokenizerPosH
        PLA
        STA ZP.TokenizerPosL
        
    #ifdef DEBUG
        LDA #'D'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'E'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
    // Process the tokens in BasicTokenizerBuffer  
    // Returns C to continue, NC to exit
    processTokens()
    {
        SEC  // Default: continue REPL
        
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
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.CLEAR:
                {
                    cmdClear();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.VARS:
                {
                    cmdVars();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.LIST:
                {
                    cmdList();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.RUN:
                {
                    cmdRun();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.FUNCS:
                {
                    cmdFuncs();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.MEM:
                {
                    CmdMem();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.HEAP:
                {
                    cmdHeap();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BUFFERS:
                {
                    cmdBuffers();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.DUMP:
                {
                    cmdDump();
                    Messages.CheckError();
                    if (NC) { return; }
                }
                case Tokens.BYE:
                {
                    cmdBye();
                    CLC  // Exit interpreter
                    return;
                }
                case Tokens.FORGET:
                case Tokens.SAVE:
                case Tokens.LOAD:
                case Tokens.DIR:
                case Tokens.DEL:
                {
                    LDA #(Messages.NotImplemented % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.NotImplemented / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                }
                default:
                {
                    // Not a console command, try to execute as a statement
                    Statement.Execute();
                    Messages.CheckError();
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
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            Messages.StorePC(); // 6502 PC -> IDY
            CLC
            return;
        }
        
        SEC  // Success - continue REPL
    }
    
    // Execute DUMP command
    cmdDump()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
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
            Tools.DumpPage();
            Messages.PrintOK();
            SEC // ok
            break;
        } // single exit
        if (NC)
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            Messages.StorePC();
        }
#else
        LDA #(Messages.OnlyInDebug % 256)
        STA ZP.LastErrorL
        LDA #(Messages.OnlyInDebug / 256)
        STA ZP.LastErrorH
        Messages.StorePC();
#endif    
    }

    // Execute HEAP command
    cmdHeap()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
#ifdef DEBUG
        Tokenizer.NextToken(); // consume 'HEAP'
        
        Tools.DumpHeap();
        Messages.PrintOK();
#else
        LDA #(Messages.OnlyInDebug % 256)
        STA ZP.LastErrorL
        LDA #(Messages.OnlyInDebug / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
#endif
    }
    
    // Execute BUFFERS command
    cmdBuffers()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
#ifdef DEBUG
        Tokenizer.NextToken(); // consume 'BUFFERS'
        
        Tools.DumpBasicBuffers();
        Messages.PrintOK();
#else
        LDA #(Messages.OnlyInDebug % 256)
        STA ZP.LastErrorL
        LDA #(Messages.OnlyInDebug / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
#endif
    }

    // Execute MEM command
    CmdMem()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
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
    }
    
    // Execute BYE command
    cmdBye()
    {
        // BYE command works in both modes - allows escape from function capture
        // NOP - just return NC to exit interpreter
    }
    
    // Execute NEW command
    cmdNew()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'NEW'
        
        Variables.Clear();
        Functions.Clear();
        Messages.PrintOK();
    }
    
    // Execute CLEAR command
    cmdClear()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'CLEAR'
        
        Variables.Clear();
        Messages.PrintOK();
    }
    
    // Execute LIST command
    cmdList()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        // TODO: List program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
    }
    
    // Execute RUN command
    cmdRun()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        // TODO: Run program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
    }
    
    // Execute FUNCS command
    cmdFuncs()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        // TODO: Show functions
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
    }
    
    // Execute VARS command - display all variables and constants
    cmdVars()
    {
        IsCaptureMode();
        if (C)
        {
            functionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'VARS'
        
        // Track if we found any variables or constants
        LDX #0  // Counter for total symbols found
        
        // iterate through variables and constants
        Variables.IterateAll();
        loop
        {
            if (NC) { break; }  // No more variables
            
            // Get symbol type and data type
            Variables.GetType();
            
            // Print symbol type (VAR or CONST)
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            CMP # (SymbolType.CONSTANT << 4)
            if (Z)
            {
                LDA #Tokens.CONST
                Tokenizer.PrintKeyword();   
                LDA #' '
                Serial.WriteChar();
            }
            
            // Get packed type back and extract data type
            LDA ZP.ACCT
            AND # 0x0F
            Tools.PrintType();
                        
            // Print space
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the variable name
            Variables.GetName();
            Tools.PrintStringACC();
            
            // Print " = "
            LDA #' '
            Serial.WriteChar();
            LDA #'='
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the value
            Variables.GetValue();
            Tools.PrintDecimalWord();
            
            // Print newline
            Tools.NL();
            
            INX  // Increment count
            Variables.IterateNext();
        }
              
        // Special case: if no variables or constants found
        CPX #0
        if (Z)
        {
            LDA #(noVariablesMsg % 256)
            STA ZP.ACCL
            LDA #(noVariablesMsg / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        SEC // ok
    }
    
    // Exit function capture mode and return to normal (called from Console or Tokenizer on Ctrl+C)
    ExitFunctionCaptureMode()
    {
        SetNormalMode();
        
        // Additional cleanup when exiting function capture mode:
        // Reset tokenizer buffer to clear partial function data
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        // Clear any error state that might have been set during capture
        Messages.ClearError();
        SEC
    }
}
