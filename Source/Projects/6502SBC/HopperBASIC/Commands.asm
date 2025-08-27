unit Commands
{
    uses "./Utilities/TokenIterator"
    uses "./Utilities/Tools"
    uses "./Debugging/Dasm"

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
    }
    
    // Execute CLEAR command - reset variables to default values
    // Input: None
    // Output: All variables reset to default values
    CmdClear()
    {
        Variables.Clear();
        Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
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
        
        
        LDA # ErrorID.MemoryMessage LDX # (MessageExtras.SuffixColon|MessageExtras.SuffixSpace) Error.Message();
        
        Memory.AvailableACC(); // Returns available bytes in ZP.ACC
        LDA ZP.ACCL
        STA ZP.TOPL
        LDA ZP.ACCH
        STA ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA # ErrorID.BytesMessage LDX # MessageExtras.PrefixSpace Error.MessageNL();
        
        
#ifdef HASEEPROM
        EEPROM.Detect();
        if (C)  // Set C (detected)
        {
            LDA # ErrorID.EEPROMLabel LDX # (MessageExtras.SuffixColon|MessageExtras.SuffixSpace) Error.Message();
            
            EEPROM.GetSize(); // A -> number of K
            STA ZP.TOPL
            STZ ZP.TOPH
            STZ ZP.TOPT
            Print.Decimal();
            LDA #'K' Print.Char();
            LDA #',' Print.Char();
            Print.Space();
            File.GetAvailable(); // TOP -> number of B
            Print.Decimal();
            LDA # ErrorID.BytesMessage LDX # MessageExtras.PrefixSpace Error.MessageNL();
        }
#endif                
        SEC
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
        }
        else
        {
            // Not found as variable/constant, try function
            Functions.Remove(); // Input: ZP.TOP = name pointer
            if (NC)
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
            TXA
            if (Z) // X=0 -> LIST
            {
                CmdVars(); // constants and variables
            }
            
            // ZP.STR is null - show all functions with bodies
            displayAllFunctionsWithBodies();
            CheckErrorAndSetFailure();
            if (NC) { break; }
            
            // Also display $MAIN function if it exists (even though it's "hidden")
            Messages.Main(); // point ZP.TOP -> "$MAIN"

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
    // Input: ZP.STR = function name or null (0x0000) for all, X=0 for LIST, X=1 for DASM
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
    
    NotAvailable()
    {
        // Missing because:
        //
        // - not DEBUG build?
        // - not TRACE build?
        // - HASEEPROM not defined?
        
        Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
    }
    
#ifdef DEBUG
    // Execute HEAP command - show heap dump
    // Input: None
    // Output: Heap dump printed to serial
    CmdHeap()
    {
        Debug.ValidateHeap();
        Debug.DumpHeap();
    }
    
    // Execute BUFFERS command - show tokenizer and opcode buffer contents
    // Input: None
    // Output: Buffer contents printed to serial
    CmdBuffers()
    {
        Debug.DumpBuffers();
        Debug.ValidateHeap();
    }
    
    // Execute DUMP command - hex dump of memory page
    // Input: ZP.TOP = page number (0 if not specified)
    // Output: Hex dump of memory page to serial
    CmdDump()
    {
        // Use ZP.ACC as page number (already set by Console)
        LDA ZP.TOPL
        Debug.DumpPage();
        Debug.ValidateHeap();
    }
#else
    // Stubs for non-debug builds
    CmdHeap()    { Commands.NotAvailable(); } // only in DEBUG
    CmdBuffers() { Commands.NotAvailable(); } // only in DEBUG
    CmdDump()    { Commands.NotAvailable(); } // only in DEBUG
#endif

#if defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
    // Execute TRON command - enable trace
    // Input: None
    // Output: Trace enabled (bit 2 of ZP.FLAGS set)
    CmdTron()
    {
        // Set bit 2 of ZP.FLAGS to enable trace execution
        SMB2 ZP.FLAGS
    }
    
    // Execute TROFF command - disable trace
    // Input: None
    // Output: Trace disabled (bit 2 of ZP.FLAGS cleared)
    CmdTroff()
    {
        // Clear bit 2 of ZP.FLAGS to disable trace execution
        RMB2 ZP.FLAGS
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
    
    // Display an array variable declaration with contents
    // Input: ZP.IDX = variable node, A = full type (with ARRAY flag)
    // Output: Array info and contents printed to serial
    displayArrayVariable()
    {
        PHA
        PHX
        PHY
        
        // Save the variable node pointer - we need it intact for iteration!
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Get array pointer from value field
        Variables.GetValue(); // ZP.TOP = array pointer, ZP.ACCT = type
        LDA ZP.ACCT
        AND # BASICType.TYPEMASK
        BASICTypes.PrintType();
        
        Print.Space();
        
        // Print variable name
        Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        Print.String();
        
        // Save array pointer in IDX for Array APIs
        LDA ZP.TOPL
        STA ZP.IDXL
        LDA ZP.TOPH
        STA ZP.IDXH
        
        // Print dimensions
        LDA #'[' Serial.WriteChar();
        

        // Get element count
        BASICArray.GetCount(); // Returns count in ZP.ACC

        // Print element count as WORD
        LDA ZP.ACCL
        STA ZP.TOPL
        LDA ZP.ACCH
        STA ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA #']' Serial.WriteChar();
        Print.Space();
        LDA #'=' Serial.WriteChar();
        Print.Space();
        
        // Print array contents
        LDA #'<' Serial.WriteChar();
        
        // Get element type for PrintValue
        BASICArray.GetItemType(); // Returns ZP.ACCT, preserves ZP.ACCL and ZP.ACCH
        
        // Determine how many elements to show (min of 10 or count)
        LDX #0  // Element counter
        loop
        {
            // Check if we've shown 10 elements
            CPX # 10
            if (Z) 
            { 
                // Check if there are more elements (count > 10)
                // We check if 10 < count by comparing high bytes first
                LDA #0           // 10 high byte
                LDY # BASICArray.aiCount+1
                CMP [ZP.IDX], Y  // Compare with count high byte
                if (Z)
                {
                    LDA #10      // 10 low byte  
                    DEY
                    CMP [ZP.IDX], Y  // Compare with count low byte
                }
                // C set if 10 >= count, NC if 10 < count
                if (NC)  // 10 < count means more than 10 elements
                {
                    LDA #',' Serial.WriteChar();
                    Print.Space();
                    LDA #'.' Serial.WriteChar();
                    LDA #'.' Serial.WriteChar();
                    LDA #'.' Serial.WriteChar();
                }
                break; 
            }
            
            // Check if we've shown all elements
            CPX ZP.ACCL
            if (Z)
            {
                LDA ZP.ACCH
                if (Z) { break; }  // No more elements
            }
            
            // Print comma separator if not first element
            CPX #0
            if (NZ)
            {
                LDA #',' Serial.WriteChar();
                Print.Space();
            }
            
            // Get element at index X
            STX ZP.IDYL
            STZ ZP.IDYH
            BASICArray.GetItem(); // Returns value and type in ZP.TOP
            
            // Print the value (type is already in ZP.TOPT from GetItem)
            SEC  // quotes for strings and chars
            BASICTypes.PrintValue(); // preserves X
            
            INX
        }
        
        LDA #'>' Serial.WriteChar();
        
        Print.NewLine();
        
        // Restore the variable node pointer for iteration to continue!
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    // Display a variable declaration
    // Input: ZP.IDX = variable node
    // In Commands.asm, displayVariable() method:
    displayVariable()
    {
        PHA
        PHX
        PHY
        
        // Get variable type
        Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = type
        
        // Check if it's an ARRAY
        if (BBS5, ZP.ACCT) // Bit 5 - ARRAY
        {
            LDA ZP.ACCT  // Get full type for helper
            displayArrayVariable();  // A = full type, ZP.IDX = node
        }
        else
        {
            // Check if it has VAR bit set
            if (BBS4, ZP.ACCT) // Bit 4 - VAR
            {
                // Print "VAR" first
                LDA # Token.VAR
                Tokens.PrintKeyword();
                
                // Then print current underlying type in parentheses
                LDA # '(' Serial.WriteChar();
                
                LDA ZP.ACCT
                AND # BASICType.TYPEMASK  // Get underlying type without VAR bit
                BASICTypes.PrintType();
                LDA # ')' Serial.WriteChar();
            }
            else  // Regular typed variable
            {
                LDA ZP.ACCT
                AND # BASICType.MASK
                BASICTypes.PrintType();
            }
            
            Print.Space();
            
            // Print variable name
            Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
            Print.String();
            
            Print.Space();
            LDA #'=' Serial.WriteChar();
            Print.Space();
            
            // Get and print current value (this needs fresh call to get value)
            Variables.GetValue(); // ZP.TOP = value, ZP.TOPT = dataType
            
            // Input: ZP.TOP = value, ZP.TOPT = type, C = quote strings
            SEC
            BASICTypes.PrintValue();
            
            Print.NewLine();
        }
        
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
        LDA #'(' Print.Char();        
        
        // Get constant type
        Variables.GetType(); // Input: ZP.IDX, Output: ACCT = type
        
        LDA ZP.ACCT
        AND # BASICType.MASK
        BASICTypes.PrintType(); // Input: A = dataType
        LDA #')' Print.Char();
        Print.Space();
        
        // Print constant name
        Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
        Print.String();
        
        Print.Space();
        LDA #'=' Serial.WriteChar();
        Print.Space();
        
        // Get and print current value
        Variables.GetValue(); // ZP.TOP = value, ZP.TOPT = dataType
        
        // Input: ZP.TOP = value, ZP.TOPT = type, C = quote strings
        SEC
        BASICTypes.PrintValue();
        Print.NewLine();
        
        PLY
        PLX
        PLA
    }
    
    // Display all functions (signatures only)
    // X=0 for LIST, X=1 for DASM
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
                    DisplayFunctionSignature(); // Input: ZP.IDX = function node
                }
                Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display specific function (signature only)
    // X=0 for LIST, X=1 for DASM
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
                DisplayFunctionSignature(); // Input: ZP.IDX = function node
            }
        }
        
        PLY
        PLX
        PLA
    }
    
    // Display all functions with bodies (for LIST command)
    // X=0 for LIST, X=1 for DASM
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
    // X=0 for LIST, X=1 for DASM
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
    DisplayFunctionSignature()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDYH
        PHA
        LDA ZP.IDYL
        PHA
        
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
            Print.Space();
            
            Print.String();
            LDA #'(' Serial.WriteChar();
            
            // Display arguments
            Functions.GetArguments(); // Input: ZP.IDX, Output: ZP.IDY = args list or null
            displayArguments(); // Input: ZP.IDY = arguments list
            
            LDA #')' Serial.WriteChar();
        }
        
        Print.NewLine();
        
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDYH
        
        PLY
        PLX
        PLA
    }
    
    DisplayFunctionSuffix()
    {
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
        Print.NewLine();
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
        DisplayFunctionSignature();
        
        // Restore function node
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        TXA
        if (Z) // X == 0 for LIST
        {
            // Display body
            Functions.GetBody(); // Input: ZP.IDX, Output: ZP.IDY = tokens pointer
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (NZ)
            {
                // Use TokenIterator to render the function body
                STZ ZP.TOKERRORH
                STZ ZP.TOKERRORL
                TokenIterator.RenderTokenStream(); // Input: ZP.IDY = tokens pointer
            }
        }
        else
        {
            Dasm.DisassembleFunctionOpCodes();
        }
            
        DisplayFunctionSuffix();
        Print.NewLine(); // Extra blank line after function
        
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
        
        LDX #0
        
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            // Has arguments - iterate through them
            Locals.IterateStart(); // Input: ZP.IDX = function node, Output: ZP.IDY = first argument
            loop
            {
                if (NC) { break; } // No more arguments (handles empty list case)
                
                Locals.GetType(); // Input: ZP.IDY, Output: ZP.ACCT = symbolType|dataType
                LDA ZP.ACCT
                AND # SymbolType.MASK
                CMP # SymbolType.ARGUMENT
                if (Z)
                {
                    CPX #0
                    if (NZ)
                    {
                        // we've already seen arguments - print comma separator
                        LDA #','
                        Serial.WriteChar();
                        Print.Space();
                    }
                    // Get and print argument name
                    Locals.GetName(); // Input: ZP.IDY = argument node, Output: ZP.STR = name pointer
                    Print.String();
                    INX
                }
                // Check if there's another argument
                Locals.IterateNext(); // Input: ZP.IDY = current arg, Output: ZP.IDY = next arg
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
        
        Messages.Main(); // point ZP.TOP -> "$MAIN"
        
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
    
    // Execute DIR command - list files in EEPROM
    // Input: None
    // Output: Directory listing printed to serial
    CmdDir()
    {
#ifdef HASEEPROM 
        File.Dir();
#else
        Commands.NotAvailable();
#endif 
    }
    
    // Execute DEL command - delete file from EEPROM
    // Input: ZP.STR = filename pointer
    // Output: File deleted or error message
    CmdDel()
    {
#ifdef HASEEPROM 
        File.Delete(); // Input: ZP.STR
#else
        Commands.NotAvailable();
#endif 
    }
    
    // Execute SAVE command - save current program to EEPROM
    // Input: ZP.STR = filename pointer  
    // Output: Program saved or error message
    CmdSave()
    {
#ifdef HASEEPROM      
        loop
        {
            File.Exists(); // preserves ZP.STR
            if (C)
            {
                // file exits
                LDX #2 // Messages.OverwriteWarning
                confirmDestructiveAction(); // preserves ZP.STR
                if (C)
                {
                    // confirmed
                    File.Delete(); // preserves ZP.STR
                }
                else
                {
                    // cancelled
                    break;
                }
            }
            Storage.SaveProgram(); // Input: ZP.STR
            break;
        }
#else
        Commands.NotAvailable();
#endif        
    }
    
    // Execute LOAD command - load program from EEPROM
    // Input: ZP.STR = filename pointer
    // Output: Program loaded or error message  
    CmdLoad()
    {
#ifdef HASEEPROM        
        Storage.LoadProgram(); // Input: ZP.STR
#else
        Commands.NotAvailable();
#endif                
    }
    
    // Execute FORMAT command - format EEPROM file system with confirmation
    // Input: None  
    // Output: File system formatted or cancelled
    CmdFormat()
    {
#ifdef HASEEPROM 
        LDX #1 // Messages.FormatWarning
        confirmDestructiveAction();
        if (C)
        {
            File.Format();
            CheckAndPrint();
        }
        // If NC, user cancelled - no action needed (no error)
#else
        Commands.NotAvailable();
#endif        
    }
    
    // Prompt user for Y/N confirmation on destructive operations
    // Input: X for warning #
    // Output: C set if user confirms (Y/y), NC if user cancels (N/n)
    // Preserves: X, Y
    // Munts: A
    confirmDestructiveAction()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        loop // Single exit pattern
        {
            // Display warning message
            switch (X)
            {
                case 1: // FORMAT
                {
                    LDA # ErrorID.FormatWarning LDX # (MessageExtras.None|MessageExtras.SuffixPeriod|MessageExtras.SuffixSpace)
                    Error.Message();
                    LDA # ErrorID.ContinueWarning LDX # (MessageExtras.None|MessageExtras.SuffixSpace)
                }
                case 2: // SAVE
                {
                    LDA # ErrorID.OverwriteWarning LDX # (MessageExtras.None|MessageExtras.SuffixSpace)
                }
            }
            Error.Message();
            LDA # ErrorID.YesNo LDX # (MessageExtras.InParens|MessageExtras.SuffixQuest|MessageExtras.SuffixSpace)
            Error.Message();
            
            loop // Input validation loop
            {
                // Read single character response
                Serial.WaitForChar();
                
                PHA
                // Echo the character
                Serial.WriteChar();
                Print.NewLine();
                PLA
                switch (A)
                {
                    case 'Y':
                    case 'y':
                    { SEC break; }
                    case 'N':
                    case 'n':
                    { CLC break; }
                    default:
                    {
                        // Invalid input - ask again
                        LDA # ErrorID.YesNo LDX # (MessageExtras.SuffixQuest|MessageExtras.SuffixSpace)
                        Error.Message();
                    }
                }
            }
            
            break; // Exit outer loop with C set appropriately
        }
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        PLY
        PLX
        PLA
    }
    
}
