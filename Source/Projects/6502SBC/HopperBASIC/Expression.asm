unit Expression
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    
    // Evaluate an expression starting from current token
    // Pushes result onto value stack
    // Returns Z if successful, NZ if error (error stored in ZP.LastError)
    // Current implementation: simple precedence climbing for +, -, =, <>
    Evaluate()
    {
        // Start with lowest precedence level
        parseComparison();
    }
    
    // Parse comparison operators (=, <>)
    // Precedence level 1 (lowest)
    parseComparison()
    {
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
                doEquals();
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
                doNotEqual();
                continue;
            }
            
            break; // No more comparison operators
        }
        
        LDA #0  // Set Z for success
    }
    
    // Parse addition and subtraction operators (+, -)
    // Precedence level 2
    parseAddition()
    {
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
                doAddition();
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
                doSubtraction();
                continue;
            }
            
            break; // No more addition/subtraction operators
        }
        
        LDA #0  // Set Z for success
    }
    
    // Parse unary operators and primary expressions
    // Precedence level 3 (highest)
    parseUnary()
    {
        LDA ZP.CurrentToken
        CMP #Tokens.MINUS
        if (Z)
        {
            // Unary minus
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Parse the operand
            parsePrimary();
            Messages.CheckError();
            if (NZ) { return; }
            
            // Negate the result
            doUnaryMinus();
            return;
        }
        
        // Not unary, parse primary
        parsePrimary();
    }
    
    // Parse primary expressions (numbers, identifiers, parentheses)
    parsePrimary()
    {
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Convert token to number and push to stack
                Tokenizer.GetTokenNumber();  // Result in ZP.TOP
                Stacks.PushTop();
                
                // Get next token
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                LDA #0  // Set Z for success
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // TODO: Variable lookup when we have variables
                LDA #(Messages.UndefinedIdentifier % 256)
                STA ZP.LastErrorL
                LDA #(Messages.UndefinedIdentifier / 256)
                STA ZP.LastErrorH
                LDA #1  // Set NZ
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
                    LDA #1  // Set NZ
                    return;
                }
                
                // Get next token
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NZ) { return; }
                
                LDA #0  // Set Z for success
                return;
            }
            default:
            {
                // Unexpected token
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                LDA #1  // Set NZ
                return;
            }
        }
    }
    
    // Arithmetic operations
    // All operations work on the top two values on the stack
    
    doAddition()
    {
        // Pop two operands
        Stacks.PopNext();  // Right operand in ZP.NEXT
        Stacks.PopTop();   // Left operand in ZP.TOP
        
        // Add: TOP = TOP + NEXT
        CLC
        LDA ZP.TOPL
        ADC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        ADC ZP.NEXTH
        STA ZP.TOPH
        
        // Push result back
        Stacks.PushTop();
    }
    
    doSubtraction()
    {
        // Pop two operands
        Stacks.PopNext();  // Right operand in ZP.NEXT
        Stacks.PopTop();   // Left operand in ZP.TOP
        
        // Subtract: TOP = TOP - NEXT
        SEC
        LDA ZP.TOPL
        SBC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        SBC ZP.NEXTH
        STA ZP.TOPH
        
        // Push result back
        Stacks.PushTop();
    }
    
    doUnaryMinus()
    {
        // Pop operand
        Stacks.PopTop();
        
        // Negate: TOP = 0 - TOP
        SEC
        LDA #0
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA #0
        SBC ZP.TOPH
        STA ZP.TOPH
        
        // Push result back
        Stacks.PushTop();
    }
    
    doEquals()
    {
        // Pop two operands
        Stacks.PopNext();  // Right operand in ZP.NEXT
        Stacks.PopTop();   // Left operand in ZP.TOP
        
        // Compare: result = (TOP == NEXT) ? 1 : 0
        LDA ZP.TOPL
        CMP ZP.NEXTL
        if (NZ)
        {
            // Low bytes differ, result is false
            STZ ZP.TOPL
            STZ ZP.TOPH
        }
        else
        {
            LDA ZP.TOPH
            CMP ZP.NEXTH
            if (NZ)
            {
                // High bytes differ, result is false
                STZ ZP.TOPL
                STZ ZP.TOPH
            }
            else
            {
                // Equal, result is true
                LDA #1
                STA ZP.TOPL
                STZ ZP.TOPH
            }
        }
        
        // Push result back
        Stacks.PushTop();
    }
    
    doNotEqual()
    {
        // Pop two operands
        Stacks.PopNext();  // Right operand in ZP.NEXT
        Stacks.PopTop();   // Left operand in ZP.TOP
        
        // Compare: result = (TOP != NEXT) ? 1 : 0
        LDA ZP.TOPL
        CMP ZP.NEXTL
        if (NZ)
        {
            // Low bytes differ, result is true
            LDA #1
            STA ZP.TOPL
            STZ ZP.TOPH
        }
        else
        {
            LDA ZP.TOPH
            CMP ZP.NEXTH
            if (NZ)
            {
                // High bytes differ, result is true
                LDA #1
                STA ZP.TOPL
                STZ ZP.TOPH
            }
            else
            {
                // Equal, result is false
                STZ ZP.TOPL
                STZ ZP.TOPH
            }
        }
        
        // Push result back
        Stacks.PushTop();
    }
}