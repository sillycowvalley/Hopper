unit Statement
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Expression"
    uses "Tools"
    
    // Execute a statement starting from current token position
    // Assumes ZP.CurrentToken contains the first token of the statement
    // Returns Z if successful, NZ if error (error stored in ZP.LastError)
    Execute()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'S'
        Serial.WriteChar();
        
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.PRINT:
            {
                executePrint();
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
            case Tokens.IF:
            {
                executeIf();
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
            case Tokens.RETURN:
            {
                executeReturn();
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
            case Tokens.END:
            {
                executeEnd();
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // Could be assignment or function call
                executeIdentifier();
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
            default:
            {
                // Unexpected token for statement
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                // DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                return;
            }
        }
    }
    
    // Execute PRINT statement
    // PRINT <expression>
    executePrint()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
        
        // Get next token (should be start of expression)
        Tokenizer.NextToken();
        
        // DEBUG: Show token after PRINT
        LDA #'['
        Serial.WriteChar();
        LDA ZP.CurrentToken
        Serial.HexOut();
        LDA #']'
        Serial.WriteChar();
        
        Messages.CheckError();
        if (NZ) { return; }
        
        // Check for end of line (PRINT with no arguments)
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // Just print a newline
            LDA #'\n'
            Serial.WriteChar();
            
            // DEBUG
            LDA #'P'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
            
            return;
        }
        
        // Evaluate the expression
        Expression.Evaluate();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Top of stack now contains the result
        // For now, assume it's a number and print it
        Stacks.PopTop();  // Pop result into ZP.TOP
        Tools.PrintDecimalWord();
        
        // Print newline
        LDA #'\n'
        Serial.WriteChar();
        
        // DEBUG
        LDA #'P'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Execute IF statement
    // IF <expression> THEN <statement>
    executeIf()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'I'
        Serial.WriteChar();
        
        // Get next token (should be start of condition expression)
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Evaluate the condition
        Expression.Evaluate();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Check for THEN keyword
        LDA ZP.CurrentToken
        CMP #Tokens.THEN
        if (NZ)
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
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
            // DEBUG
            LDA #'I'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
            return;
        }
        
        // Condition is true, get next token and execute statement
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Recursively execute the statement after THEN
        Execute();
        
        // DEBUG
        LDA #'I'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Execute RETURN statement
    // RETURN [<expression>]
    executeReturn()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'R'
        Serial.WriteChar();
        
        // Get next token
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NZ) { return; }
        
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
            if (NZ) { return; }
        }
        
        // TODO: Actually return from function when we have function support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        
        // DEBUG
        LDA #'R'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
        
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
        LDA #1  // Set NZ
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
        LDA #1  // Set NZ
        BRK
    }
}
