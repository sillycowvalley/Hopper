unit Listing
{
    uses "Messages"
    uses "Tools"
    uses "Tokenizer"
    uses "Objects"
    uses "Variables"
    uses "Functions"
    
    // Display specific function by name
    displaySpecificFunction()
    {
        // Get the function name
        Tokenizer.GetTokenString(); // Returns pointer in ZP.TOP
        
        // Find the function
        Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = node, C set if found
        if (NC)
        {
            LDA #(Messages.UndefinedIdentifier % 256)
            STA ZP.LastErrorL
            LDA #(Messages.UndefinedIdentifier / 256)
            STA ZP.LastErrorH
            Messages.StorePC();
            return;
        }
        
        // Consume the function name token
        Tokenizer.NextToken();
        
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
            return;
        }
        
        // Display the found function
        displayFunction();
        SEC // Success
    }
    
    
    
    // Display all functions (extracted from original cmdFuncs)
    displayAllFunctions()
    {
        // Track if we found any functions
        LDX #0  // Counter for total functions found
        
        // Iterate through functions
        Functions.IterateFunctions();
        loop
        {
            if (NC) { break; }  // No more functions
            
            displayFunction();
            
            INX  // Increment function count
            Functions.IterateNext(); // Continue to next function
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
        
        SEC // Success
    }

    
    
    displayFunction()
    {
        // This code is duplicated in both cmdList() and cmdFuncs():
        LDA #Tokens.FUNC
        Tokenizer.PrintKeyword();
        LDA #' '
        Serial.WriteChar();
        
        Functions.GetName();
        Tools.PrintStringTOP();
        
        Functions.GetArguments();
        LDA #'('
        Serial.WriteChar();
        
        if (C)
        {
            Arguments.IterateStart();
            loop
            {
                if (NC) { break; }
                Arguments.GetName();
                Tools.PrintStringTOP();  // Note: cmdFuncs incorrectly uses PrintStringACC()
                Arguments.IterateNext();
                if (C)
                {
                    LDA #','
                    Serial.WriteChar();
                    LDA #' '
                    Serial.WriteChar();
                }
            }
        }
        
        LDA #')'
        Serial.WriteChar();
        Tools.NL();
    }
    
    
    
    
    // Execute LIST command - display complete program listing
    // Shows constants, variables, functions in creation order per spec
    CmdList()
    {
        IsCaptureMode();
        if (C)
        {
            FunctionModeError();
            return;
        }
        
        // Tokenizer.NextToken(); // consume 'LIST' (cmdVars consumes it)
        
        // Display variables and constants (VARS output)
        CmdVars();
        
        displayAllFunctions();
        
        SEC // Success
    }
    
    
    CmdFuncs()
    {
        IsCaptureMode();
        if (C)
        {
            FunctionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'FUNCS'
        
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
        
        // Display all functions (existing logic)
        displayAllFunctions();
    }
    
    
          
    // Execute VARS command - display all variables and constants
    // Constants first, then variables, with blank lines separating sections
    CmdVars()
    {
        IsCaptureMode();
        if (C)
        {
            Console.FunctionModeError();
            return;
        }
        
        Tokenizer.NextToken(); // consume 'VARS'
        
        // PASS 1: Display all constants
        LDX #0  // Counter for constants found
        Variables.IterateConstants();
        loop
        {
            if (NC) { break; }  // No more constants
            
            // Print "CONST "
            LDA #Tokens.CONST
            Tokenizer.PrintKeyword();   
            LDA #' '
            Serial.WriteChar();
            
            // Get symbol type and data type
            Variables.GetType();
            
            // Get packed type and extract data type
            LDA ZP.ACCT
            AND #0x0F
            Tools.PrintType();
                        
            // Print space
            LDA #' '
            Serial.WriteChar();
            
            // Get and print the constant name
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
            
            INX  // Increment constant count
            Variables.IterateNext();
        }
        
        // If we found constants, add a blank line
        CPX #0
        if (NZ)
        {
            Tools.NL();
        }
        
        // PASS 2: Display all variables
        LDY #0  // Counter for variables found
        Variables.IterateVariables();
        loop
        {
            if (NC) { break; }  // No more variables
            
            // Get symbol type and data type
            Variables.GetType();
            
            // Get packed type and extract data type
            LDA ZP.ACCT
            AND #0x0F
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
            
            INY  // Increment variable count
            Variables.IterateNext();
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
