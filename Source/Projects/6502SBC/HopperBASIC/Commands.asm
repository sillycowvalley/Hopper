unit Commands
{
    uses "./Utilities/TokenIterator"

    // API Status: Clean
    // All public methods execute console commands using prepared arguments
    // No tokenizer interaction for command parsing
    
    // ============================================================================
    // Simple Console Commands (no arguments)
    // ============================================================================
    
    // Execute NEW command - clear everything
    // Input: None
    // Output: All variables and functions cleared
    CmdNew()
    {
        Variables.Clear();
        Functions.Clear();
        Messages.PrintOK();
    }
    
    // Execute CLEAR command - reset variables to default values
    // Input: None
    // Output: All variables reset to default values
    CmdClear()
    {
        Variables.Clear();
        Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
        Messages.PrintOK();
    }
    
    // Execute BYE command - exit interpreter
    // Input: None
    // Output: Sets State.Exiting
    CmdBye()
    {
        States.SetExiting();
    }
    
    // Execute MEM command - show available memory
    // Input: None
    // Output: Memory information printed to serial
    CmdMem()
    {
        Memory.Available(); // Returns available bytes in ZP.ACC
        
        LDA #(Messages.MemoryMsg % 256)
        STA ZP.STRL
        LDA #(Messages.MemoryMsg / 256)
        STA ZP.STRH
        Tools.PrintStringSTR();
        
        LDA ZP.ACCL
        STA ZP.TOPL
        LDA ZP.ACCH
        STA ZP.TOPH
        Tools.PrintDecimalWord();
        
        LDA #(Messages.BytesMsg % 256)
        STA ZP.STRL
        LDA #(Messages.BytesMsg / 256)
        STA ZP.STRH
        Tools.PrintStringSTR();
    }
    
    // ============================================================================
    // Console Commands with Arguments
    // ============================================================================
    
    // Execute FORGET command - remove variable, constant, or function by name
    // Input: ZP.STR = name to forget
    // Output: Named symbol removed from appropriate table, or error if not found
    CmdForget()
    {
        PHA
        
        // Try to remove as variable/constant first
        LDA ZP.STRL
        STA ZP.TOPL
        LDA ZP.STRH
        STA ZP.TOPH
        
        Variables.Remove(); // Input: ZP.TOP = name pointer
        if (C)
        {
            // Successfully removed variable/constant
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            Messages.PrintOK();
        }
        else
        {
            // Not found as variable/constant, try function
            Functions.Remove(); // Input: ZP.TOP = name pointer
            if (C)
            {
                // Successfully removed function
                Messages.PrintOK();
            }
            else
            {
                // Not found anywhere
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
            }
        }
        
        PLA
    }
    
    // Execute VARS command - display all variables and constants
    // Input: None
    // Output: All variables displayed to serial
    CmdVars()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit pattern
        {
            // Check if we have any variables
            Variables.IterateVariables(); // Output: ZP.IDX = first variable, C set if found
            if (NC)
            {
                Variables.IterateConstants(); // Output: ZP.IDX = first constant, C set if found
                if (NC)
                {
                    break;
                }
            }
            
            // Then display all constants
            Variables.IterateConstants();
            loop
            {
                if (NC) { break; }  // No more constants
                displayConstant(); // Input: ZP.IDX = constant node
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            // Display all variables first
            Variables.IterateVariables();
            loop
            {
                if (NC) { break; }  // No more variables
                
                displayVariable(); // Input: ZP.IDX = variable node
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Execute LIST command - list program or specific function
    // Input: ZP.STR = function name or null (0x0000) for all
    // Output: Function(s) displayed to serial
    CmdList()
    {
        PHA
        
        loop
        {
            Statement.IsCaptureModeOn();
            if (C)
            {
                Error.OnlyAtConsole(); BIT ZP.EmulatorPCL
                States.SetFailure();
                break;
            }
            
            // Check if specific function requested
            LDA ZP.STRL
            ORA ZP.STRH
            if (NZ)
            {
                // Specific function requested - just show that one
                // Need to copy ZP.STR to ZP.TOP for Functions.Find()
                LDA ZP.STRL
                STA ZP.TOPL
                LDA ZP.STRH
                STA ZP.TOPH
                
                Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node
                if (NC)
                {
                    Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                    States.SetFailure();
                    break;
                }
                
                displayFunctionWithBody(); // Input: ZP.IDX = function node
                States.SetSuccess();
                break;
            }
            
            CmdVars(); // constants and variables
            
            // ZP.STR is null - show all functions with bodies
            displayAllFunctionsWithBodies();
            Error.CheckError();
            if (NC)
            {
                States.SetFailure(); 
                break;
            }

            // Also display $MAIN function if it exists (even though it's "hidden")
            LDA #(Messages.BeginFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.BeginFunctionName / 256)
            STA ZP.TOPH

            Functions.Find(); // Input: ZP.TOP = "$MAIN" name, Output: ZP.IDX = node
            if (C)
            {
                // Found $MAIN function - display it as BEGIN/END
                displayFunctionWithBody(); // Input: ZP.IDX = function node
            }
            break;
        }
        
        PLA
    }
    
    // Execute FUNCS command - display all functions or specific function
    // Input: ZP.STR = function name or null (0x0000) for all
    // Output: Function(s) displayed to serial
    CmdFuncs()
    {
        PHA
        
        // Check if specific function requested
        LDA ZP.STRL
        ORA ZP.STRH
        if (Z)
        {
            // ZP.STR is null - show all functions
            displayAllFunctions();
        }
        else
        {
            // ZP.STR contains function name - show specific function
            displaySpecificFunction();
        }
        
        PLA
    }
    
    // ============================================================================
    // Debug Commands (conditional compilation)
    // ============================================================================
    
#ifdef DEBUG
    // Execute HEAP command - show heap dump
    // Input: None
    // Output: Heap dump printed to serial
    CmdHeap()
    {
        Debug.DumpHeap();
    }
    
    // Execute BUFFERS command - show tokenizer and opcode buffer contents
    // Input: None
    // Output: Buffer contents printed to serial
    CmdBuffers()
    {
        Debug.DumpBuffers();
    }
    
    // Execute DUMP command - hex dump of memory page
    // Input: ZP.TOP = page number (0 if not specified)
    // Output: Hex dump of memory page to serial
    CmdDump()
    {
        // Use ZP.ACC as page number (already set by Console)
        LDA ZP.TOPL
        Debug.DumpPage();
    }
#else
    // Stubs for non-debug builds
    CmdHeap()  { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
    CmdBuffers() { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
    CmdDump()  { Error.OnlyInDebug(); BIT ZP.EmulatorPCL }
#endif

#ifdef TRACE
    // Execute TRON command - enable trace
    // Input: None
    // Output: Trace enabled (bit 2 of ZP.FLAGS set)
    CmdTron()
    {
        // Set bit 2 of ZP.FLAGS to enable trace execution
        SMB2 ZP.FLAGS
        Messages.PrintOK();
    }
    
    // Execute TROFF command - disable trace
    // Input: None
    // Output: Trace disabled (bit 2 of ZP.FLAGS cleared)
    CmdTroff()
    {
        // Clear bit 2 of ZP.FLAGS to disable trace execution
        RMB2 ZP.FLAGS
        Messages.PrintOK();
    }
#else
    // Stubs for non-trace builds
    CmdTron()  { Error.OnlyInTrace(); BIT ZP.EmulatorPCL }
    CmdTroff() { Error.OnlyInTrace(); BIT ZP.EmulatorPCL }
#endif
    
    // ============================================================================
    // Helper Functions for Display
    // ============================================================================
    
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
    
    // Display a variable declaration
    // Input: ZP.IDX = variable node
    displayVariable()
    {
        PHA
        PHX
        PHY
        
        // Get variable type
        Variables.GetType(); // Input: ZP.IDX, Output: A = type
        
        LDA ZP.ACCT
        AND #0x0F
        BASICTypes.PrintType(); // Input: A = dataType
        LDA #' ' Serial.WriteChar();
        
        // Print variable name
        Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        Tools.PrintStringSTR();
        
        LDA #' ' Serial.WriteChar();
        LDA #'=' Serial.WriteChar();
        LDA #' ' Serial.WriteChar();
        
        // Get and print current value
        Variables.GetValue(); // ZP.TOP = value, ZP.TOPT = dataType
        
        // Input: ZP.TOP = value, ZP.TOPT = type, C = quote strings
        SEC
        BASICTypes.PrintValue();
        
        Tools.NL();
        
        PLY
        PLX
        PLA
    }
    
    // Display a constant declaration
    // Input: ZP.IDX = constant node
    displayConstant()
    {
        PHA
        PHX
        PHY
        
        // Print CONST keyword
        LDA #Token.CONST
        Tokens.PrintKeyword();
        LDA #' ' Serial.WriteChar();
        
        // Get constant type
        Variables.GetType(); // Input: ZP.IDX, Output: ACCT = type
        
        LDA ZP.ACCT
        AND #0x0F
        BASICTypes.PrintType(); // Input: A = dataType
        LDA #' ' Serial.WriteChar();
        
        // Print constant name
        Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        Tools.PrintStringSTR();
        
        LDA #' ' Serial.WriteChar();
        LDA #'=' Serial.WriteChar();
        LDA #' ' Serial.WriteChar();
        
        // Get and print current value
        Variables.GetValue(); // ZP.TOP = value, ZP.TOPT = dataType
        
        // Input: ZP.TOP = value, ZP.TOPT = type, C = quote strings
        SEC
        BASICTypes.PrintValue();
        
        Tools.NL();
        
        PLY
        PLX
        PLA
    }
    
    // Display all functions (signatures only)
    displayAllFunctions()
    {
        PHA
        PHX
        PHY
        
        // Check if we have any functions
        Functions.IterateFunctions(); // Output: ZP.IDX = first function, C set if found
        if (C)
        {
            // Iterate through functions
            loop
            {
                if (NC) { break; }  // No more functions
                
                Functions.GetName(); // -> ZP.STR = name pointer
                IsVisibleFunctionSTR();
                if (C)
                {
                    displayFunctionSignature(); // Input: ZP.IDX = function node
                }
                Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display specific function (signature only)
    displaySpecificFunction()
    {
        PHA
        PHX
        PHY
        
        // Find the function
        LDA ZP.STRL
        STA ZP.TOPL
        LDA ZP.STRH
        STA ZP.TOPH
        
        Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node
        if (NC)
        {
            Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
        }
        else
        {
            // Check visibility
            Functions.GetName();
            IsVisibleFunctionSTR();
            if (NC)
            {
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
            }
            else
            {
                displayFunctionSignature(); // Input: ZP.IDX = function node
            }
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display all functions with bodies (for LIST command)
    displayAllFunctionsWithBodies()
    {
        PHA
        PHX
        PHY
        
        // Iterate through functions
        Functions.IterateFunctions(); // Output: ZP.IDX = first function, C set if found
        loop
        {
            if (NC) { break; }  // No more functions
            
            Functions.GetName(); // -> ZP.STR = name pointer
            IsVisibleFunctionSTR();
            if (C)
            {
                displayFunctionWithBody(); // Input: ZP.IDX = function node
            }
            Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display specific function with body (for LIST command)
    displaySpecificFunctionWithBody()
    {
        PHA
        PHX
        PHY
        
        // Find the function
        LDA ZP.STRL
        STA ZP.TOPL
        LDA ZP.STRH
        STA ZP.TOPH
        
        Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node
        if (NC)
        {
            Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
        }
        else
        {
            // Check visibility
            Functions.GetName();
            IsVisibleFunctionSTR();
            if (NC)
            {
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
            }
            else
            {
                displayFunctionWithBody(); // Input: ZP.IDX = function node
            }
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display function signature only
    // Input: ZP.IDX = function node
    displayFunctionSignature()
    {
        PHA
        PHX
        PHY
        
        // Get the function name
        Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        
        // Check if this is the special "BEGIN" function (main program)
        checkForBeginFunctionSTR(); // Input: ZP.STR, Returns C if this is the BEGIN function
        if (C)
        {
            // Display as BEGIN
            LDA #Token.BEGIN
            Tokens.PrintKeyword();
        }
        else
        {
            // Display as FUNC name(args)
            LDA #Token.FUNC
            Tokens.PrintKeyword();
            LDA #' ' Serial.WriteChar();
            
            Tools.PrintStringSTR();
            LDA #'(' Serial.WriteChar();
            
            // Display arguments
            Functions.GetArguments(); // Input: ZP.IDX, Output: ZP.IDY = args list or null
            displayArguments(); // Input: ZP.IDY = arguments list
            
            LDA #')' Serial.WriteChar();
        }
        
        Tools.NL();
        
        PLY
        PLX
        PLA
    }
    
    // Display complete function with body
    // Input: ZP.IDX = function node
    displayFunctionWithBody()
    {
        PHA
        PHX
        PHY
        
        // Save function node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Display signature
        displayFunctionSignature();
        
        // Restore function node
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Display body
        Functions.GetBody(); // Input: ZP.IDX, Output: ZP.IDY = tokens pointer
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            // Use TokenIterator to render the function body
            TokenIterator.RenderTokenStream(); // Input: ZP.IDY = tokens pointer
            Tools.NL(); 
        }
        
        // Get function name to check if it's BEGIN
        Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        checkForBeginFunctionSTR();
        if (C)
        {
            // Print END for BEGIN function
            LDA #Token.END
            Tokens.PrintKeyword();
        }
        else
        {
            // Print ENDFUNC for regular function
            LDA #Token.ENDFUNC
            Tokens.PrintKeyword();
        }
        
        Tools.NL();
        Tools.NL(); // Extra blank line after function
        
        PLY
        PLX
        PLA
    }
    
    // Display function arguments
    // Input: ZP.IDY = arguments list head pointer (or null)
    displayArguments()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            // Has arguments - iterate through them
            Arguments.IterateStart(); // Input: ZP.IDX = function node, Output: ZP.IDY = first argument
            loop
            {
                if (NC) { break; } // No more arguments (handles empty list case)
                
                // Get and print argument name
                Arguments.GetName(); // Input: ZP.IDY = argument node, Output: ZP.STR = name pointer
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
        
        PLY
        PLX
        PLA
    }
    
    // Check if function name is "BEGIN" ($MAIN)
    // Input: ZP.STR = function name pointer
    // Output: C set if name is "$MAIN", NC otherwise
    checkForBeginFunctionSTR()
    {
        PHA
        PHX
        PHY
        
        // Get pointer to "$MAIN" constant
        LDA #(Messages.BeginFunctionName % 256)
        STA ZP.TOPL
        LDA #(Messages.BeginFunctionName / 256)
        STA ZP.TOPH
        
        // Compare strings character by character
        LDY #0
        loop
        {
            LDA [ZP.STR], Y   // Get character from name
            STA ZP.ACCL       // Save it
            LDA [ZP.TOP], Y   // Get character from "$MAIN"
            CMP ZP.ACCL       // Compare them
            if (NZ)
            {
                CLC // Strings don't match
                break;
            }
            // Characters match - check if we're at end of string
            LDA ZP.ACCL       // Get the character back
            if (Z)  // Null terminator - strings match completely
            {
                SEC // Is BEGIN function
                break;
            }
            INY
        }
        
        PLY
        PLX
        PLA
    }
}
