unit Expression
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Instructions"
    
    // Evaluate an expression starting from current token
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression result pushed onto value stack
    //         ZP.CurrentToken = token after expression
    // Munts: Stack, ZP.CurrentToken, all ZP variables used by parsing
    // Error: Sets ZP.LastError if syntax error or type mismatch
    Evaluate()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'E'
        Tools.COut();
#endif
        
        // Start with lowest precedence level
        parseLogical();
        
#ifdef DEBUG
        LDA #'E'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Parse comparison operators (=, <>, <, >, <=, >=)
    // Input: ZP.CurrentToken = current token
    // Output: Comparison result (BIT type) pushed to stack
    //         ZP.CurrentToken = token after comparison expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error
    parseComparison()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
#endif
        
        // Parse left operand
        parseBitwiseOr();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.EQUALS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseOr();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform greater-equal comparison
                Instructions.GreaterEqual();
                continue;
            }
            
            break; // No more comparison operators
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC  // Success
    }
    
    // Parse logical OR operators (BIT operands only)
    // Input: ZP.CurrentToken = current token
    // Output: Logical result (BIT type) pushed to stack
    //         ZP.CurrentToken = token after logical expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error or type mismatch
    parseLogical()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'L'
        Tools.COut();
#endif
        
        // Parse left operand (OR has lower precedence, so parse AND first)
        parseLogicalAnd();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.OR
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseLogicalAnd();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform logical OR (BIT operands only)
                Instructions.LogicalOr();
                continue;
            }
            
            break; // No more OR operators
        }
        
#ifdef DEBUG
        LDA #'L'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC  // Success
    }
    
    // Parse logical AND operators (BIT operands only, higher precedence than OR)
    // Input: ZP.CurrentToken = current token
    // Output: Logical result (BIT type) pushed to stack
    //         ZP.CurrentToken = token after AND expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error or type mismatch
    parseLogicalAnd()
    {
        // Parse left operand (higher precedence - parse comparison operations first)
        parseComparison();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.AND
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseComparison();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform logical AND (BIT operands only)
                Instructions.LogicalAnd();
                continue;
            }
            
            break; // No more AND operators
        }
        
        SEC  // Success
    }    
    // Parse bitwise OR operators (| - numeric operands only)
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise result (promoted numeric type) pushed to stack
    //         ZP.CurrentToken = token after bitwise expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error or type mismatch
    parseBitwiseOr()
    {
        // Parse left operand (OR has lower precedence, so parse AND first)
        parseBitwiseAnd();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.BITWISE_OR
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseBitwiseAnd();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform bitwise OR (numeric operands only)
                Instructions.BitwiseOr();
                continue;
            }
            
            break; // No more bitwise OR operators
        }
        
        SEC  // Success
    }
    
    // Parse bitwise AND operators (& - numeric operands only, higher precedence than |)
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise result (promoted numeric type) pushed to stack
    //         ZP.CurrentToken = token after bitwise expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error or type mismatch
    parseBitwiseAnd()
    {
        // Parse left operand (higher precedence - parse arithmetic operations first)
        parseAddition();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.BITWISE_AND
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseAddition();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform bitwise AND (numeric operands only)
                Instructions.BitwiseAnd();
                continue;
            }
            
            break; // No more bitwise AND operators
        }
        
        SEC  // Success
    }
    
    // Parse addition and subtraction operators (+, -)
    // Input: ZP.CurrentToken = current token
    // Output: Arithmetic result pushed to stack
    //         ZP.CurrentToken = token after additive expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error
    parseAddition()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'A'
        Tools.COut();
#endif
        
        // Parse left operand
        parseMultiplicative();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.PLUS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseMultiplicative();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseMultiplicative();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform subtraction
                Instructions.Subtraction();
                continue;
            }
            
            break; // No more addition/subtraction operators
        }
        
#ifdef DEBUG
        LDA #'A'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC  // Success
    }
    
    // Parse multiplicative operators (*, /, MOD)
    // Input: ZP.CurrentToken = current token
    // Output: Arithmetic result pushed to stack
    //         ZP.CurrentToken = token after multiplicative expression
    // Munts: Stack, ZP.CurrentToken, parsing variables
    // Error: Sets ZP.LastError if syntax error
    parseMultiplicative()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'M'
        Tools.COut();
#endif
        
        // Parse left operand
        parseUnary();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.MULTIPLY
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Parse right operand
                parseUnary();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseUnary();
                Messages.CheckError();
                if (NC) { return; }
                
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
                if (NC) { return; }
                
                // Parse right operand
                parseUnary();
                Messages.CheckError();
                if (NC) { return; }
                
                // Perform modulo
                Instructions.Modulo();
                continue;
            }
            
            break; // No more multiplicative operators
        }
        
#ifdef DEBUG
        LDA #'M'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC  // Success
    }
  
    // Parse unary operators and delegate to primary expressions
    // Input: ZP.CurrentToken = current token (-, NOT, or start of primary expression)
    // Output: Expression result pushed to stack
    //         ZP.CurrentToken = token after unary expression
    // Modifies: Stack, ZP.CurrentToken, ZP.TOP, ZP.TOPT, parsing variables
    // Error: Sets ZP.LastError if syntax error
    parseUnary()
    {
#ifdef DEBUG
        LDA #'<' Tools.COut(); LDA #'U' Tools.COut();
#endif    
        
        LDA ZP.CurrentToken
        CMP #Tokens.MINUS
        if (Z)
        {
            // Unary minus
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { return; }
            
            // Parse the operand first
            parsePrimary();
            Messages.CheckError();
            if (NC) { return; }
            
            // Apply unary minus with proper type handling
            Instructions.UnaryMinus();
            
#ifdef DEBUG
            LDA #'U' Tools.COut();LDA #'>' Tools.COut();
#endif
            
            SEC  // Success
            return;
        }
        
        CMP #Tokens.NOT
        if (Z)
        {
            // Logical NOT (unary)
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { return; }
            
            // Parse the operand
            parsePrimary();
            Messages.CheckError();
            if (NC) { return; }
            
            // Perform logical NOT
            Instructions.LogicalNot();
            
#ifdef DEBUG
            LDA #'U' Tools.COut();LDA #'>' Tools.COut();
#endif        
            SEC  // Success
            return;
        }
        
        // Not unary, parse primary
        parsePrimary();
        
#ifdef DEBUG
        LDA #'U' Tools.COut();LDA #'>' Tools.COut();
#endif
    }      
    
    
        

    
    // Parse primary expressions (numbers, identifiers, parentheses)
    // Input: ZP.CurrentToken = current token (NUMBER, IDENTIFIER, LPAREN, etc.)
    // Output: Primary value pushed to stack
    //         ZP.CurrentToken = token after primary expression
    // Munts: Stack, ZP.CurrentToken, ZP.TOP, ZP.TOPT, parsing variables
    // Error: Sets ZP.LastError if syntax error or undefined identifier
    parsePrimary()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'T'
        Tools.COut();
    #endif
        
        // Assume success initially
        SEC
        
        loop // single exit
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.NUMBER:
                {
                    // Convert token to number and push to stack
                    Tokenizer.GetTokenNumber();  // Result in ZP.TOP, type in ZP.TOPT
                    
        #ifdef DEBUG
                    // Show parsed number
                    LDA #' ' Tools.COut();
                    LDA ZP.TOPT Tools.PrintType();
                    LDA #':' Tools.COut();
                    Tools.PrintDecimalWord();
        #endif
                    
                    LDA ZP.TOPT // type
                    Stacks.PushTop(); // Push value to stack, modifies Y
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                case Tokens.IDENTIFIER:
                {
                    parseIdentifier();
                    break;
                }
                case Tokens.LPAREN:
                {
                    // Get next token (start of sub-expression)
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        // Error from NextToken - flag already set
                        break; 
                    }
                    
                    // Parse the sub-expression
                    parseComparison();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        // Error from parseComparison - flag already set
                        break; 
                    }
                    
                    // Expect closing parenthesis
                    LDA ZP.CurrentToken
                    CMP #Tokens.RPAREN
                    if (NZ)
                    {
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
                        CLC  // Error
                        break;
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                default:
                {
                    // Unexpected token
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC  // Error
                    break;
                }
            }

            
            CLC  // Error(we should never get here)
            break;
        } // Single exit point
    #ifdef DEBUG
        LDA #'T'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
    
        
    // Input: A ZP.CurrentToken == Tokens.IDENTIFIER
    // Output: symbol or function in IDX, A = IndentifierType
    
    ResolveIdentifier()
    {
        PHX
        PHY
        
        loop // Single exit block for clean error handling
        {
            // Get the identifier name for lookup
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            Messages.CheckError();
            if (NC) { break; }
            
            Tokenizer.IsKeyword();
            if (C)
            {
                LDA #'K' Tools.COut();
                
                LDA # IdentifierType.Keyword
                break; // success
            }
            
            STZ ZP.SymbolIteratorFilter  // Accept any symbol type (variable or constant)
            Variables.Find(); // ZP.IDX = symbol node address
            if (C) // Symbol found
            {
                LDA ZP.ACCT
                AND #0xF0  // Extract symbol type (high nibble)
                CMP # (SymbolType.VARIABLE << 4)
                if (Z)
                { 
                    LDA # 'V' Tools.COut();
                    
                    LDA # IdentifierType.Global
                    break; // success
                }
                CMP # (SymbolType.CONSTANT << 4)
                if (Z)
                { 
                    LDA # 'C' Tools.COut();
                    
                    LDA # IdentifierType.Constant
                    break; // success
                }
                // what's this?
                LDA #(Messages.InternalError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InternalError / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                CLC
            }
            Messages.CheckError();
            if (NC) { break; }
            
            // function by same name exists? name pointer in TOP
            LDX #ZP.FunctionsList
            Objects.Find();
            if (C)  
            {
                LDA #'F' Tools.COut();
                
                LDA # IdentifierType.Function
                break; // success
            }
            Messages.CheckError();
            if (NC) { break; }
            
            LDA #'U' Tools.COut();
            
            LDA #(Messages.UndefinedIdentifier % 256)
            STA ZP.LastErrorL
            LDA #(Messages.UndefinedIdentifier / 256)
            STA ZP.LastErrorH
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            LDA # IdentifierType.Undefined
            CLC  // undefined identifier
            break;
        } // end of single exit block
        
        PLY
        PLX
    }
    
    // Helper: Parse identifier (variable or constant lookup)
    // Input: ZP.CurrentToken = Tokens.IDENTIFIER
    // Output: Identifier value pushed to stack, ZP.CurrentToken advanced
    // Modifies: Stack, ZP.CurrentToken, ZP.TOP, ZP.TOPT, parsing variables
    // Error: Sets ZP.LastError if identifier not found or wrong type
    parseIdentifier()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit block for clean error handling
        {
            ResolveIdentifier();
            Messages.CheckError();
            if (NC)
            {
                Messages.StorePC(); // 6502 PC -> IDY
                break; 
            }
            
            CMP # IdentifierType.Global
            if (NZ)
            {
                CMP # IdentifierType.Constant
                if (NZ)
                {
                    // function, local, argument?
                    LDA #(Messages.NotImplemented % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.NotImplemented / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC  // Set NC
                    break;
                }
            }
            
            // Get the value and type from found symbol
            Variables.GetValue();  // Returns value in ZP.TOP, type in ZP.TOPT
            if (NC)
            {
                CLC  // Set NC - error already set by GetValue
                break;
            }
            
            LDA ZP.TOPT  // type
            Stacks.PushTop();
            
            // Get next token
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            // no longer a const expression
            LDA #0
            SetIsConstant();
            
            SEC  // Set C - success
            break;
        } // end of single exit block
        
        PLY
        PLX
        PLA
    }
}
