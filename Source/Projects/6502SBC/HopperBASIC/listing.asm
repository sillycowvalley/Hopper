unit Listing
{
    uses "Messages"
    uses "Error"
    uses "State"
    uses "Tools"
    uses "Tokenizer"
    uses "TokenIterator"
    
    uses "Objects"
    uses "Variables"
    uses "Functions"
    uses "Arguments"
    uses "Statement"  // For IsCaptureModeOn()
    uses "Console"   // For FunctionModeError()
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Commands consume their tokens and display output to serial
    
    // Check if function name starts with '$' (hidden function)
    IsVisibleFunctionTOP()
    {
        PHA
        LDA [ZP.TOP]  // Get first character
        CMP #'$'
        if (Z) { CLC } else { SEC }
        PLA
    }
    // Check if function name starts with '$' (hidden function)
    IsVisibleFunctionSTR()
    {
        PHA
        LDA [ZP.STR]  // Get first character
        CMP #'$'
        if (Z) { CLC } else { SEC }
        PLA
    }

    // Display a complete function with body
    // Input: ZP.IDX = function node address
    // Output: Complete function printed to serial (signature + body + ENDFUNC/END)
    // Preserves: ZP.IDX (function node address)
    // Modifies: ZP.IDY (argument iteration), ZP.TOP (name pointers), serial output
    const string displayFunctionTrace = "DispFunc";
    displayFunction()
    {
        PHA
        PHX
        PHY
#ifdef TRACECONSOLE
        LDA #(displayFunctionTrace % 256) STA ZP.TraceMessageL LDA #(displayFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Get the function name FIRST (before any other operations that might corrupt ZP.IDX)
        Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        
        // Check if this is the special "BEGIN" function (main program)
        checkForBeginFunctionSTR(); // Input: ZP.STR, Returns C if this is the BEGIN function
        if (C)
        {
            // Display as BEGIN...END block (no FUNC keyword, no parameters)
            displayBeginFunction(); // Input: ZP.IDX = function node
        }
        else
        {
            // Display as regular FUNC...ENDFUNC
            displayRegularFunctionSTR(); // Input: ZP.IDX = function node, ZP.STR = name
        }
#ifdef TRACECONSOLE
        LDA #(displayFunctionTrace % 256) STA ZP.TraceMessageL LDA #(displayFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }

    // Check if function name is "BEGIN"
    // Input: ZP.STR = function name pointer
    // Output: C set if name is "BEGIN", NC if regular function
    // Preserves: ZP.TOP, all other registers
    const string checkBeginFuncTrace = "ChkBegin";
    checkForBeginFunctionSTR()
    {
        PHA
        PHY
        
        // Save ZP.NEXT since StringCompare uses it
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
#ifdef TRACECONSOLE
        LDA #(checkBeginFuncTrace % 256) STA ZP.TraceMessageL LDA #(checkBeginFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        
        
        // Set up comparison with "BEGIN" string
        LDA #(Messages.BeginFunctionName % 256)
        STA ZP.STR2L
        LDA #(Messages.BeginFunctionName / 256)
        STA ZP.STR2H
        
        // Compare function name with "BEGIN"
        Tools.StringCompareSTR(); // Input: ZP.STR vs ZP.STR2, Output: C set if match

#ifdef TRACECONSOLE
        LDA #(checkBeginFuncTrace % 256) STA ZP.TraceMessageL LDA #(checkBeginFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        // Restore ZP.NEXT
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        
        PLY
        PLA
    }

    // Display BEGIN...END function (main program)
    // Input: ZP.IDX = function node, ZP.TOP = name pointer
    // Output: BEGIN block printed to serial
    const string displayBeginFuncTrace = "DispBegin";
    displayBeginFunction()
    {
        PHA
        PHX
        PHY
#ifdef TRACECONSOLE
        LDA #(displayBeginFuncTrace % 256) STA ZP.TraceMessageL LDA #(displayBeginFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Save function node address for body display
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Print "BEGIN"
        LDA #Tokens.BEGIN
        Tokenizer.PrintKeyword();
        
        // Restore function node for body display
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Display function body (token stream)
        displayFunctionBody(); // Input: ZP.IDX = function node
        
        Tools.NL(); // Extra blank line after main program
#ifdef TRACECONSOLE
        LDA #(displayBeginFuncTrace % 256) STA ZP.TraceMessageL LDA #(displayBeginFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }

    // Display regular FUNC...ENDFUNC function
    // Input: ZP.IDX = function node, ZP.TOP = name pointer (from Functions.GetName)
    // Output: Complete function printed to serial
    const string displayRegFuncTrace = "DispReg";
    displayRegularFunctionSTR()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACECONSOLE
        LDA #(displayRegFuncTrace % 256) STA ZP.TraceMessageL LDA #(displayRegFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        
        // Save function node address since we'll be making calls that corrupt ZP.IDX
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Print "FUNC "
        LDA #Tokens.FUNC
        Tokenizer.PrintKeyword();
        LDA #' '
        Serial.WriteChar();
        
        // Print the function name (already in ZP.STR from caller)
        Tools.PrintStringSTR();
        
        // Restore function node for Arguments operations
        PLA
        STA ZP.IDXH
        PLA  
        STA ZP.IDXL
        
        // Save it again for body display
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Print parameter list - get arguments
        Functions.GetArguments(); // Input: ZP.IDX, Output: ZP.IDY = arguments list head, C set if has arguments
        
        // Print opening parenthesis
        LDA #'('
        Serial.WriteChar();
        
        if (C)
        {
            // Has arguments - iterate through them
            Arguments.IterateStart(); // Input: ZP.IDX = function node, Output: ZP.IDY = first argument
            loop
            {
                if (NC) { break; } // No more arguments
                
                // Get and print argument name
                Arguments.GetName(); // Input: ZP.IDY = argument node, Output: ZP.STR = name pointer, always succeeds
                Tools.PrintStringSTR();
                
                // Check if there's another argument
                Arguments.IterateNext(); // Input: ZP.IDY = current arg, Output: ZP.IDY = next arg
                if (C)
                {
                    // More arguments - print comma separator
                    LDA #','
                    Serial.WriteChar();
                    LDA #' '
                    Serial.WriteChar();
                }
            }
        }
        
        // Print closing parenthesis and newline
        LDA #')'
        Serial.WriteChar();
        Tools.NL();
        
        // Restore function node for body display
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Display function body (token stream)
        displayFunctionBody(); // Input: ZP.IDX = function node
        
        Tools.NL();
        Tools.NL(); // Extra blank line after function
#ifdef TRACECONSOLE
        LDA #(displayRegFuncTrace % 256) STA ZP.TraceMessageL LDA #(displayRegFuncTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }

    // Display function body from token stream as readable BASIC code
    // Input: ZP.IDX = function node address
    // Output: Function body printed to serial as formatted BASIC statements
    const string displayFuncBodyTrace = "DispBody";
    displayFunctionBody()
    {
        PHA
        PHX
        PHY
#ifdef TRACECONSOLE
        LDA #(displayFuncBodyTrace % 256) STA ZP.TraceMessageL LDA #(displayFuncBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Get function body tokens
        Functions.GetBody(); // Input: ZP.IDX, Output: ZP.IDY = tokens pointer
        
        // Check if function has a body
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            // Use TokenIterator to render the function body
            TokenIterator.RenderTokenStream(); // Input: ZP.IDY = tokens pointer
        }
#ifdef TRACECONSOLE
        LDA #(displayFuncBodyTrace % 256) STA ZP.TraceMessageL LDA #(displayFuncBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }

    // Display all functions in the system
    // Input: None
    // Output: All function signatures printed to serial, or "NO FUNCTIONS" message
    // Preserves: All caller state
    // Modifies: ZP.IDX (function iteration), serial output
    const string displayAllFuncsTrace = "DispAll";
    displayAllFunctions()
    {
        PHA
        PHX
        PHY
#ifdef TRACECONSOLE
        LDA #(displayAllFuncsTrace % 256) STA ZP.TraceMessageL LDA #(displayAllFuncsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Iterate through functions
        Functions.IterateFunctions(); // Output: ZP.IDX = first function node, C set if found
        loop
        {
            if (NC) { break; }  // No more functions
            
            Functions.GetName(); // -> ZP.STR = name pointer
            IsVisibleFunctionSTR();
            if (C) 
            { 
                displayFunction(); // Input: ZP.IDX = function node
            }
            Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next function
        }
#ifdef TRACECONSOLE
        LDA #(displayAllFuncsTrace % 256) STA ZP.TraceMessageL LDA #(displayAllFuncsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }
    
    displaySpecificFunction()
    {
        loop // Single exit block for error handling
        {
            // Get the function name
            Tokenizer.GetTokenString(); // Output: ZP.TOP = name pointer
            Error.CheckError();
            if (NC) { break; }
            
            // Find the function
            Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node
            if (C)
            {
                IsVisibleFunctionTOP();
            }
            if (NC) 
            { 
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                break; 
            }
            
            // SAVE ZP.IDX before tokenizer call
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH  
            PHA
            
            Tokenizer.NextToken(); // This corrupts ZP.IDX
            
            // RESTORE ZP.IDX immediately
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
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
            
            // Display the found function
            displayFunction(); // Input: ZP.IDX = function node
            SEC
            break;
        }
    }

    // Execute FUNCS command - display all functions or specific function
    // Input: ZP.CurrentToken = FUNCS token (consumed by this method)
    // Output: Function(s) displayed to serial
    // Usage: FUNCS (all functions) or FUNCS functionName (specific function)
    // Error: Sets ZP.LastError if function mode, syntax error, or function not found
    const string cmdFuncsTrace = "FUNCS";
    CmdFuncs()
    {
#ifdef TRACECONSOLE
        LDA #(cmdFuncsTrace % 256) STA ZP.TraceMessageL LDA #(cmdFuncsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            Statement.IsCaptureModeOn();
            if (C)
            {
                Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
                State.SetFailure();
                break;
            }
            
            Tokenizer.NextToken(); // consume 'FUNCS'
            Error.CheckError();
            if (NC) 
            { 
                State.SetFailure(); 
                break; 
            }
            
            // Check if there's a function name argument
            LDA ZP.CurrentToken
            CMP #Tokens.IDENTIFIER
            if (Z)
            {
                // Display specific function
                displaySpecificFunction();
                Error.CheckError();
                if (NC) 
                { 
                    State.SetFailure(); 
                    break;
                }
                State.SetSuccess();
                break;
            }
            
            CMP #Tokens.EOL
            if (NZ)
            {
                // Invalid argument
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                State.SetFailure();
                break;
            }
            
            // Display all functions
            displayAllFunctions();
            State.SetSuccess();
            break;
        } // loop exit
#ifdef TRACECONSOLE
        LDA #(cmdFuncsTrace % 256) STA ZP.TraceMessageL LDA #(cmdFuncsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute LIST command - display complete program listing
    // Input: ZP.CurrentToken = LIST token (NOT consumed - delegated to CmdVars)
    // Output: Complete program listing (constants, variables, functions) displayed to serial
    // Shows constants first, then variables, then functions in creation order per spec
    // Error: Sets ZP.LastError if function mode or other command errors
    const string cmdListTrace = "LIST";
    CmdList()
    {
#ifdef TRACECONSOLE
        LDA #(cmdListTrace % 256) STA ZP.TraceMessageL LDA #(cmdListTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            Statement.IsCaptureModeOn();
            if (C)
            {
                Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
                State.SetFailure();
                break;
            }
            
            // Note: Don't consume 'LIST' token - let CmdVars() consume it
            // This avoids token consumption conflicts
            
            // Display variables and constants (VARS output)
            CmdVars();
            Error.CheckError();
            if (NC) 
            {
                State.SetFailure(); 
                break; 
            }
            
            // Display all functions
            displayAllFunctions();
            Error.CheckError();
            if (NC)
            {
                State.SetFailure(); 
                break;
            }
            
            // Also display $MAIN function if it exists (after regular functions)
            LDA #(Messages.BeginFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.BeginFunctionName / 256)
            STA ZP.TOPH
            
            Functions.Find(); // Input: ZP.TOP = "$MAIN" name, Output: ZP.IDX = node
            if (C)
            {
                // Found $MAIN function - display it
                displayFunction(); // Input: ZP.IDX = function node
            }
            
            State.SetSuccess();
            break;
        } // loop exit
#ifdef TRACECONSOLE
        LDA #(cmdListTrace % 256) STA ZP.TraceMessageL LDA #(cmdListTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Execute VARS command - display all variables and constants
    // Input: ZP.CurrentToken = VARS (or LIST) token (consumed by this method)
    // Output: Constants first, then variables, with blank lines separating sections
    // Error: Sets ZP.LastError if function mode or iteration errors
    const string cmdVarsTrace = "VARS";
    CmdVars()
    {
#ifdef TRACECONSOLE
        LDA #(cmdVarsTrace % 256) STA ZP.TraceMessageL LDA #(cmdVarsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            Statement.IsCaptureModeOn();
            if (C)
            {
                Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
                State.SetFailure();
                break;
            }
            
            Tokenizer.NextToken(); // consume 'VARS' (or 'LIST' when called from CmdList)
            Error.CheckError();
            if (NC) 
            { 
                State.SetFailure(); 
                break;
            }
            
            // PASS 1: Display all constants
            LDX #0  // Counter for constants found
            Variables.IterateConstants(); // Output: ZP.IDX = first constant, C set if found
            loop
            {
                if (NC) { break; }  // No more constants
                
                // Print "CONST "
                LDA #Tokens.CONST
                Tokenizer.PrintKeyword();   
                LDA #' '
                Serial.WriteChar();
                
                // Get symbol type and data type
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                
                // Get packed type and extract data type
                LDA ZP.ACCT
                AND #0x0F
                STA ZP.ACCT
                Tools.PrintType(); // Input: A = dataType
                
                // Print space
                LDA #' '
                Serial.WriteChar();
                
                // Get and print the constant name
                Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
                Tools.PrintStringSTR();
                
                // Print " = "
                LDA #' '
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                
                LDA ZP.ACCT
                AND #0x0F
                CMP #BasicType.STRING
                if (Z)
                {
                    LDA #'"'
                    Serial.WriteChar();
                }
                
                // Get and print the value
                Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
                Tools.PrintVariableValue(); // Input: ZP.TOP = value, ZP.TOPT = type
                
                LDA ZP.ACCT
                AND #0x0F
                CMP #BasicType.STRING
                if (Z)
                {
                    LDA #'"'
                    Serial.WriteChar();
                }
                
                // Print newline
                Tools.NL();
                
                INX  // Increment constant count
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            // If we found constants, add a blank line
            CPX #0
            if (NZ)
            {
                Tools.NL();
            }
            
            // PASS 2: Display all variables
            LDY #0  // Counter for variables found
            Variables.IterateVariables(); // Output: ZP.IDX = first variable, C set if found
            loop
            {
                if (NC) { break; }  // No more variables
                
                // Get symbol type and data type
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                
                // Get packed type and extract data type
                LDA ZP.ACCT
                AND #0x0F
                Tools.PrintType(); // Input: A = dataType
                
                // Print space
                LDA #' '
                Serial.WriteChar();
                
                // Get and print the variable name
                Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
                Tools.PrintStringSTR();
                
                // Print " = "
                LDA #' '
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                
                LDA ZP.ACCT
                AND #0x0F
                CMP #BasicType.STRING
                if (Z)
                {
                    LDA #'"'
                    Serial.WriteChar();
                }
                
                // Get and print the value
                Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
                Tools.PrintVariableValue(); // Input: ZP.TOP = value, ZP.TOPT = type
                
                LDA ZP.ACCT
                AND #0x0F
                CMP #BasicType.STRING
                if (Z)
                {
                    LDA #'"'
                    Serial.WriteChar();
                }
                
                // Print newline
                Tools.NL();
                
                INY  // Increment variable count
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            // If we found variables, add a blank line
            CPY #0
            if (NZ)
            {
                Tools.NL();
            }
            
            State.SetSuccess();
            break;
        } // exit loop
#ifdef TRACECONSOLE
        LDA #(cmdVarsTrace % 256) STA ZP.TraceMessageL LDA #(cmdVarsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
}
