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
    // Updated precedence: parseComparison() → parseLogical() → parseAddition() → parseMultiplicative() → parseUnary()
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
    
    // Parse comparison operators (=, <>, <, >, <=, >=)
    // Precedence level 1 (lowest)
    parseComparison()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'C'
        Serial.WriteChar();
        
        // Parse left operand
        parseLogical();
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
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform equality comparison
                Instructions.Equal();
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
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform not-equal comparison
                Instructions.NotEqual();
                continue;
            }
            
            CMP #Tokens.LT
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform less-than comparison
                Instructions.LessThan();
                continue;
            }
            
            CMP #Tokens.GT
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform greater-than comparison
                Instructions.GreaterThan();
                continue;
            }
            
            CMP #Tokens.LE
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform less-equal comparison
                Instructions.LessEqual();
                continue;
            }
            
            CMP #Tokens.GE
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseLogical();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform greater-equal comparison
                Instructions.GreaterEqual();
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
    
    // Parse logical operators (AND, OR)
    // Precedence level 2 (AND binds tighter than OR)
    parseLogical()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        
        // Parse left operand (OR has lower precedence, so parse AND first)
        parseLogicalAnd();
        Messages.CheckError();
        if (NZ) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.OR
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Parse right operand
                parseLogicalAnd();
                Messages.CheckError();
                if (NZ) { return; }
                
                // Perform logical OR
                Instructions.Or();
                continue;
            }
            
            break; // No more OR operators
        }
        
        // DEBUG
        LDA #'L'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse logical AND operators (higher precedence than OR)
    parseLogicalAnd()
    {
        // Parse left operand
        parseAddition();
        Messages.CheckError();
        if (NZ) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.AND
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
                
                // Perform logical AND
                Instructions.And();
                continue;
            }
            
            break; // No more AND operators
        }
    }
    
    // Parse addition and subtraction operators (+, -)
    // Precedence level 3
    parseAddition()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'A'
        Serial.WriteChar();
        
        // Parse left operand
        parseMultiplicative();
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
                parseMultiplicative();
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
                parseMultiplicative();
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
    
    // Parse multiplicative operators (*, /, MOD)
    // Precedence level 4 (higher than addition)
    parseMultiplicative()
    {
        // DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'M'
        Serial.WriteChar();
        
        // Parse left operand
        parseUnary();
        Messages.CheckError();
        if (NZ) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.MULTIPLY
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
                
                // Perform multiplication
                Instructions.Multiply();
                continue;
            }
            
            CMP #Tokens.DIVIDE
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
                
                // Perform division
                Instructions.Divide();
                continue;
            }
            
            CMP #Tokens.MOD
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
                
                // Perform modulo
                Instructions.Modulo();
                continue;
            }
            
            break; // No more multiplicative operators
        }
        
        // DEBUG
        LDA #'M'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
    }
    
    // Parse unary operators and primary expressions
    // Precedence level 5 (highest)
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
            
            // Negate the result (0 - operand)
            Instructions.Subtraction();
            
            // DEBUG
            LDA #'U'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
            
            return;
        }
        
        CMP #Tokens.NOT
        if (Z)
        {
            // Logical NOT (unary)
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Parse the operand
            parsePrimary();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Perform logical NOT
            Instructions.LogicalNot();
            
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
