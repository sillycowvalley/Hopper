unit Statement
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Expression"
    uses "Tools"
    
    uses "Variables"
    
    // Execute a statement starting from current token position
    // Assumes ZP.CurrentToken contains the first token of the statement
    // Returns C if successful, NC if error (error stored in ZP.LastError)
    Execute()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'S'
        Serial.WriteChar();
        LDA ZP.CurrentToken
        Serial.HexOut();
#endif
        
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.PRINT:
            {
                executePrint();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.IF:
            {
                executeIf();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.RETURN:
            {
                executeReturn();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.END:
            {
                executeEnd();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // Could be assignment or function call
                executeIdentifier();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            
            case Tokens.INT:
            case Tokens.WORD:
            case Tokens.BIT:
            {
                executeVariableDeclaration();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            
            default:
            {
                // Unexpected token for statement
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                CLC  // Error
                return;
            }
        }
    }
    
    // Execute PRINT statement
    // PRINT <expression>
    executePrint()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
#endif
        
        // Get next token (should be start of expression)
        Tokenizer.NextToken();
        
        Messages.CheckError();
        if (NC) { return; }
        
        // Check for end of line (PRINT with no arguments)
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // Just print a newline
            LDA #'\n'
            Serial.WriteChar();
            
#ifdef DEBUG
            LDA #'P'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
#endif
            
            SEC  // Success
            return;
        }
        
        // Evaluate the expression
        Expression.Evaluate();
        Messages.CheckError();
        if (NC) { return; }
        
        // Top of stack now contains the result
        // For now, assume it's a number and print it
        Stacks.PopTop();  // Pop result into ZP.TOP
        Tools.PrintDecimalWord();
        
        // Print newline
        LDA #'\n'
        Serial.WriteChar();
        
#ifdef DEBUG
        LDA #'P'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
        
        SEC  // Success
    }
    
    // Execute IF statement
    // IF <expression> THEN <statement>
    executeIf()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'I'
        Serial.WriteChar();
#endif
        
        // Get next token (should be start of condition expression)
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Evaluate the condition
        Expression.Evaluate();
        Messages.CheckError();
        if (NC) { return; }
        
        // Check for THEN keyword
        LDA ZP.CurrentToken
        CMP #Tokens.THEN
        if (NZ)
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            CLC  // Error
            return;
        }
        
        // Get the condition result
        Stacks.PopTop();  // Pop condition into ZP.TOP
        
        // Check if condition is true (non-zero)
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            // Condition is false, skip to end of line
#ifdef DEBUG
            LDA #'I'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
#endif
            SEC  // Success (skip)
            return;
        }
        
        // Condition is true, get next token and execute statement
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Recursively execute the statement after THEN
        Execute();
        
#ifdef DEBUG
        LDA #'I'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
    }
    
    // Execute RETURN statement
    // RETURN [<expression>]
    executeReturn()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'R'
        Serial.WriteChar();
#endif
        
        // Get next token
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Check if there's an expression to return
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // No return value, push 0
            LDA #0
            STA ZP.TOPL
            STA ZP.TOPH
            Stacks.PushTop();
        }
        else
        {
            // Evaluate return expression
            Expression.Evaluate();
            Messages.CheckError();
            if (NC) { return; }
        }
        
        // TODO: Actually return from function when we have function support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        
#ifdef DEBUG
        LDA #'R'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
        
        CLC  // Error
        BRK
    }
    
    // Execute END statement
    executeEnd()
    {
        // TODO: End program execution when we have program support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Error
        BRK
    }
    
    // Execute identifier (assignment or function call)
    executeIdentifier()
    {
        // TODO: Handle variable assignment and function calls
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Error
        BRK
    }
    
    
    // Execute variable declaration
    // INT/WORD/BIT identifier [= expression]
    executeVariableDeclaration()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'V'
        Serial.WriteChar();
        LDA #'D'
        Serial.WriteChar();
#endif
        DumpBasicBuffers();
        
        // Save the data type
        LDA ZP.CurrentToken
        PHA  // Save type token
        
        // Get next token (should be identifier)
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) 
        { 
            PLA  // Clean up stack
            return; 
        }
        
        // Check that we have an identifier
        LDA ZP.CurrentToken
        CMP #Tokens.IDENTIFIER
        if (NZ)
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            PLA  // Clean up stack
            CLC  // Error
            return;
        }
        
        // Get the identifier string
        Tokenizer.GetTokenString();  // Returns pointer in ZP.TOP
        
        // Save name pointer
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
        
        // Get next token
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) 
        { 
            PLA  // Clean up stack
            PLA
            PLA
            return; 
        }
        
        // Check for optional initialization
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (Z)
        {
            // Has initialization - save current tokenizer position as start of expression
            LDA ZP.TokenizerPosL
            PHA
            LDA ZP.TokenizerPosH
            PHA
            
            // Get next token (start of expression)
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) 
            { 
                PLA  // Clean up stack
                PLA
                PLA
                PLA
                PLA
                return; 
            }
            
            // Evaluate the initialization expression
            Expression.Evaluate();
            Messages.CheckError();
            if (NC) 
            { 
                PLA  // Clean up stack
                PLA
                PLA
                PLA
                PLA
                return; 
            }
            
            // Pop the result into TOP
            Stacks.PopTop();  // Result in ZP.TOP, type in ZP.TOPT
            
            // Calculate tokens pointer (saved position + BasicTokenizerBuffer base)
            PLA  // Saved TokenizerPosH
            STA ZP.IDYH
            PLA  // Saved TokenizerPosL
            STA ZP.IDYL
            
            CLC
            LDA #(Address.BasicTokenizerBuffer & 0xFF)
            ADC ZP.IDYL
            STA ZP.IDYL
            LDA #(Address.BasicTokenizerBuffer >> 8)
            ADC ZP.IDYH
            STA ZP.IDYH
        }
        else
        {
            // No initialization - set default value 0 and null tokens pointer
            STZ ZP.TOPL
            STZ ZP.TOPH
            STZ ZP.IDYL
            STZ ZP.IDYH
        }
        
        // Now we have:
        // - Name pointer on stack (2 bytes)
        // - Type token on stack (1 byte)
        // - Value in ZP.TOP
        // - Tokens pointer in ZP.IDY
        
        // Restore name pointer
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        // Build packed symbolType|dataType byte
        PLA  // Get type token
        
        // Convert token to BasicType
        LDX #BasicType.INT  // Default
        CMP #Tokens.INT
        if (Z)
        {
            LDX #BasicType.INT
        }
        else
        {
            CMP #Tokens.WORD
            if (Z)
            {
                LDX #BasicType.WORD
            }
            else
            {
                CMP #Tokens.BIT
                if (Z)
                {
                    LDX #BasicType.BIT
                }
            }
        }
        
        // Pack symbolType|dataType: VARIABLE(1) in high nibble, dataType in low nibble
        TXA  // dataType in A
        ORA #(SymbolType.VARIABLE << 4)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        // Move value from TOP to NEXT (Variables.Declare expects it there)
        LDA ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.TOPH
        STA ZP.NEXTH
        
        // Call Variables.Declare
        // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
        //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
        Variables.Declare();
        Messages.CheckError();
        
#ifdef DEBUG
        LDA #'V'
        Serial.WriteChar();
        LDA #'D'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
    }
}
