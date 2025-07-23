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
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.PRINT:
            {
                executePrint();
                return;
            }
            case Tokens.IF:
            {
                executeIf();
                return;
            }
            case Tokens.RETURN:
            {
                executeReturn();
                return;
            }
            case Tokens.END:
            {
                executeEnd();
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // Could be assignment or function call
                executeIdentifier();
                return;
            }
            default:
            {
                // Unexpected token for statement
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                LDA #1  // Set NZ to indicate error
                return;
            }
        }
    }
    
    // Execute PRINT statement
    // PRINT <expression>
    executePrint()
    {
        // Get next token (should be start of expression)
        Tokenizer.NextToken();
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
            LDA #0  // Set Z for success
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
        
        LDA #0  // Set Z for success
    }
    
    // Execute IF statement
    // IF <expression> THEN <statement>
    executeIf()
    {
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
            LDA #1  // Set NZ
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
            LDA #0  // Set Z for success (but don't execute)
            return;
        }
        
        // Condition is true, get next token and execute statement
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Recursively execute the statement after THEN
        Execute();
    }
    
    // Execute RETURN statement
    // RETURN [<expression>]
    executeReturn()
    {
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
        LDA #1  // Set NZ
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
    }
}