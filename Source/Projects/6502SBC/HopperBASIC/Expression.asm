unit Expression
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Instructions"
    
    // Evaluate an expression starting from current token
    // Pushes result onto value stack
    // Returns Z if successful, NZ if error (error stored in ZP.LastError)
    // Current implementation: simple precedence climbing for +, -, =, <>
    Evaluate()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'E'
        Serial.WriteChar();
        
        // Start with lowest precedence level
        parseComparison();
        
        // DEBUG
        LDA #'E'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse comparison operators (=, <>)
    // Precedence level 1 (lowest)
    parseComparison()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'C'
        Serial.WriteChar();
        
        // Parse left operand
        parseAddition();
        Messages.CheckError();
        if (NZ) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.EQUALS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseAddition();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform equality comparison
                Instructions.Equals();
                continue;
            }
            
            CMP #Tokens.NOTEQUAL
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseAddition();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform not-equal comparison
                Instructions.NotEqual();
                continue;
            }
            
            break; // No more comparison operators
        }
        
        // DEBUG
        LDA #'C'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse addition and subtraction operators (+, -)
    // Precedence level 2
    parseAddition()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'A'
        Serial.WriteChar();
        
        // Parse left operand
        parseUnary();
        Messages.CheckError();
        if (NZ) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.PLUS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseUnary();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform addition
                Instructions.Addition();
                continue;
            }
            
            CMP #Tokens.MINUS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseUnary();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform subtraction
                Instructions.Subtraction();
                continue;
            }
            
            break; // No more addition/subtraction operators
        }
        
        // DEBUG
        LDA #'A'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse unary operators and primary expressions
    // Precedence level 3 (highest)
    parseUnary()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'U'
        Serial.WriteChar();
        
        LDA ZP.CurrentToken
        CMP #Tokens.MINUS
        if (Z)
        {
            // Unary minus
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Push zero onto value stack
            STZ ZP.TOPL
            STZ ZP.TOPH
            LDA #BasicType.INT
            STA ZP.TOPT
            Stacks.PushTop();
            
            // Parse the operand
            parsePrimary();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Negate the result
            Instructions.Subtraction();
            
            // DEBUG
            LDA #'U'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
            
            return;
        }
        
        // Not unary, parse primary
        parsePrimary();
        
        // DEBUG
        LDA #'U'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse primary expressions (numbers, identifiers, parentheses)
    parsePrimary()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Convert token to number and push to stack
                Tokenizer.GetTokenNumber();  // Result in ZP.TOP, type in ZP.TOPT
                
                // DEBUG: Show parsed number
                LDA ZP.TOPT
                Tools.PrintType();
                LDA #':'
                Serial.WriteChar();
                PrintDecimalWord();
                
                LDA ZP.TOPT // type
                Stacks.PushTop();
                
                // Get next token
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // DEBUG
                LDA #'T'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // TODO: Variable lookup when we have variables
                LDA #(Messages.UndefinedIdentifier % 256)
                STA ZP.LastErrorL
                LDA #(Messages.UndefinedIdentifier / 256)
                STA ZP.LastErrorH
                
                // DEBUG
                LDA #'T'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                
                BRK
                return;
            }
            case Tokens.LPAREN:
            {
                // Get next token (start of sub-expression)
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse the sub-expression
                parseComparison();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Expect closing parenthesis
                LDA ZP.CurrentToken
                CMP #Tokens.RPAREN
                if (NZ)
                {
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    return;
                }
                
                // Get next token
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // DEBUG
                LDA #'T'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                
                return;
            }
            default:
            {
                // Unexpected token
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                
                // DEBUG
                LDA #'T'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                
                return;
            }
        }
    }
}
