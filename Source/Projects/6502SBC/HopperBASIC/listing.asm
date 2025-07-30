unit Listing
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "Messages"
    uses "Tools"
    uses "Tokenizer"
    uses "Objects"
    uses "Variables"
    uses "Functions"
    uses "Arguments"
    uses "Statement"  // For IsCaptureMode()
    uses "Console"   // For FunctionModeError()
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Commands consume their tokens and display output to serial
    
    // Display a single function signature with parameters
    // Input: ZP.IDX = function node address
    // Output: Function signature printed to serial with newline
    // Preserves: ZP.IDX (function node address)
    // Modifies: ZP.IDY (argument iteration), ZP.TOP (name pointers), serial output
    displayFunction()
    {
        PHA
        PHX
        PHY
        
        // Save function node address since iteration may modify ZP.IDX temporarily
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Print "FUNC "
        LDA #Tokens.FUNC
        Tokenizer.PrintKeyword();
        LDA #' '
        Serial.WriteChar();
        
        // Get and print the function name
        Functions.GetName(); // Input: ZP.IDX, Output: ZP.TOP = name pointer
        Tools.PrintStringTOP();
        
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
                Arguments.GetName(); // Input: ZP.IDY = argument node, Output: ZP.TOP = name pointer
                Tools.PrintStringTOP();
                
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
        
        // Restore function node address
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    // Display all functions in the system
    // Input: None
    // Output: All function signatures printed to serial, or "NO FUNCTIONS" message
    // Preserves: All caller state
    // Modifies: ZP.IDX (function iteration), serial output
    displayAllFunctions()
    {
        PHA
        PHX
        PHY
        
        // Track if we found any functions
        LDX #0  // Counter for total functions found
        
        // Iterate through functions
        Functions.IterateFunctions(); // Output: ZP.IDX = first function node, C set if found
        loop
        {
            if (NC) { break; }  // No more functions
            
            displayFunction(); // Input: ZP.IDX = function node
            
            INX  // Increment function count
            Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next function
        }
        
        // Special case: if no functions found
        CPX #0
        if (Z)
        {
            LDA #(Messages.NoFunctionsMsg % 256)
            STA ZP.ACCL
            LDA #(Messages.NoFunctionsMsg / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        
        PLY
        PLX
        PLA
        SEC // Always succeeds
    }
    
    // Display specific function by name
    // Input: ZP.CurrentToken = IDENTIFIER token (function name)
    // Output: Function signature printed to serial
    // Modifies: ZP.CurrentToken (advanced past function name), ZP.TOP (name lookup), ZP.IDX (function node)
    // Error: Sets ZP.LastError if function not found or syntax error
    displaySpecificFunction()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit block for error handling
        {
            // Get the function name
            Tokenizer.GetTokenString(); // Input: ZP.TokenLiteralPos, Output: ZP.TOP = name pointer
            Messages.CheckError();
            if (NC) { break; }
            
            // Find the function
            Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node, C set if found
            if (NC)
            {
                LDA #(Messages.UndefinedIdentifier % 256)
                STA ZP.LastErrorL
                LDA #(Messages.UndefinedIdentifier / 256)
                STA ZP.LastErrorH
                Messages.StorePC();
                break;
            }
            
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH  
            PHA
            
            // Consume the function name token
            Tokenizer.NextToken();
            
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            Messages.CheckError();
            if (NC) { break; }
            
            // Verify end of line
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (NZ)
            {
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                Messages.StorePC();
                break;
            }
            
            // Display the found function
            displayFunction(); // Input: ZP.IDX = function node
            SEC // Success
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Execute FUNCS command - display all functions or specific function
    // Input: ZP.CurrentToken = FUNCS token (consumed by this method)
    // Output: Function(s) displayed to serial
    // Usage: FUNCS (all functions) or FUNCS functionName (specific function)
    // Error: Sets ZP.LastError if function mode, syntax error, or function not found
    CmdFuncs()
    {
        Statement.IsCaptureMode();
        if (C)
        {
            Console.FunctionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'FUNCS'
        Messages.CheckError();
        if (NC) { return; }
        
        // Check if there's a function name argument
        LDA ZP.CurrentToken
        CMP #Tokens.IDENTIFIER
        if (Z)
        {
            // Display specific function
            displaySpecificFunction();
            return;
        }
        
        CMP #Tokens.EOL
        if (NZ)
        {
            // Invalid argument
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            Messages.StorePC();
            return;
        }
        
        // Display all functions
        displayAllFunctions();
    }
    
    // Execute LIST command - display complete program listing
    // Input: ZP.CurrentToken = LIST token (NOT consumed - delegated to CmdVars)
    // Output: Complete program listing (constants, variables, functions) displayed to serial
    // Shows constants first, then variables, then functions in creation order per spec
    // Error: Sets ZP.LastError if function mode or other command errors
    CmdList()
    {
        Statement.IsCaptureMode();
        if (C)
        {
            Console.FunctionModeError();
            return;
        }
        
        // Note: Don't consume 'LIST' token - let CmdVars() consume it
        // This avoids token consumption conflicts
        
        // Display variables and constants (VARS output)
        CmdVars();
        Messages.CheckError();
        if (NC) { return; }
        
        // Display all functions
        displayAllFunctions();
        Messages.CheckError();
        if (NC) { return; }
        
        SEC // Success
    }
    
    // Execute VARS command - display all variables and constants
    // Input: ZP.CurrentToken = VARS (or LIST) token (consumed by this method)
    // Output: Constants first, then variables, with blank lines separating sections
    // Error: Sets ZP.LastError if function mode or iteration errors
    CmdVars()
    {
        Statement.IsCaptureMode();
        if (C)
        {
            Console.FunctionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'VARS' (or 'LIST' when called from CmdList)
        Messages.CheckError();
        if (NC) { return; }
        
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
            Tools.PrintType(); // Input: A = dataType
            
            // Print space
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the constant name
            Variables.GetName(); // Input: ZP.IDX, Output: ZP.ACC = name pointer
            Tools.PrintStringACC();
            
            // Print " = "
            LDA #' '
            Serial.WriteChar();
            LDA #'='
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the value
            Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
            Tools.PrintDecimalWord(); // Input: ZP.TOP = value, ZP.TOPT = type
            
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
            Variables.GetName(); // Input: ZP.IDX, Output: ZP.ACC = name pointer
            Tools.PrintStringACC();
            
            // Print " = "
            LDA #' '
            Serial.WriteChar();
            LDA #'='
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the value
            Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
            Tools.PrintDecimalWord(); // Input: ZP.TOP = value, ZP.TOPT = type
            
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
        
        SEC // Success
    }
}
