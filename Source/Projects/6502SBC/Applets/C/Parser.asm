unit Parser
{
    uses "../System/Definitions"
    uses "Errors"
    uses "Tokens"
    uses "Lexer"
    uses "AST"
    
    // Parser zero page
    const byte parserSlots = 0x90;
    
    const byte currentToken = parserSlots+0;   // Current token type
    const uint currentNode  = parserSlots+1;   // Node being built
    const byte currentNodeL = parserSlots+1;
    const byte currentNodeH = parserSlots+2;
    
    // Error messages
    const string msgExpected = "Expected ";
    const string expectVoid  = "void";
    const string expectIdent = "identifier";
    
    // Helper: print "Expected X" error
    // Input: A = token that was expected
    printExpectedError()
    {
        PHA
        LDA #(msgExpected % 256)
        STA ZP.STRL
        LDA #(msgExpected / 256)
        STA ZP.STRH
        Print.String();
        PLA
        
        // A contains the token that was expected
        loop
        {
            switch (A)
            {
                case Token.Void:
                {
                    LDA #(expectVoid % 256)
                    STA ZP.STRL
                    LDA #(expectVoid / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.Identifier:
                {
                    LDA #(expectIdent % 256)
                    STA ZP.STRL
                    LDA #(expectIdent / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.LeftParen:
                {
                    LDA #'('
                    break;
                }
                case Token.RightParen:
                {
                    LDA #')'
                    break;
                }
                case Token.LeftBrace:
                {
                    LDA #'{'
                    break;
                }
                case Token.RightBrace:
                {
                    LDA #'}'
                    break;
                }
                case Token.Semicolon:
                {
                    LDA #';'
                    break;
                }
                default:
                {
                    LDA #'?'
                    break;
                }
            }
            Print.Char();
            break;
        } // loop
        Print.NewLine();
    }
    
    // Helper: consume current token and get next
    consume()
    {
        Lexer.NextToken();
        if (NC) { return; }
        STA currentToken
        SEC
    }
    
    // Helper: expect specific token
    // Input: A = expected token type
    expect()
    {
        CMP currentToken
        if (NZ)
        {
            // Error - unexpected token
            printExpectedError();
            CLC
            return;
        }
        consume();  // Get next token
    }
    
    // Parse: void main() { ... }
    parseFunction() // -> IDY
    {
        // Expect 'void'
        LDA #Token.Void
        expect();
        if (NC) { return; }
        
        // Create function node
        LDA # AST.NodeType.Function
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        // Save function node
        LDA ZP.IDXL
        STA currentNodeL
        LDA ZP.IDXH
        STA currentNodeH
        
        // Expect identifier (function name)
        LDA # Token.Identifier
        CMP currentToken
        if (NZ)
        {
            printExpectedError();
            CLC
            return;
        }
        
        // Create identifier node for function name
        LDA # AST.NodeType.Identifier
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        // Copy function name from lexer's token buffer
        LDA Lexer.TokenBufferL
        STA ZP.ACCL
        LDA Lexer.TokenBufferH
        STA ZP.ACCH
        AST.SetData(); // IDX[iData] = ACC
        
        // Move identifier to IDY
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        // Get function node back in IDX
        LDA currentNodeL
        STA ZP.IDXL
        LDA currentNodeH
        STA ZP.IDXH
        
        AST.AddChild(); // IDX = function, IDY = identifier
        
        consume();  // Move past identifier
        if (NC) { return; }
        
        // Expect '('
        LDA # Token.LeftParen
        expect();
        if (NC) { return; }
        
        // Expect ')'
        LDA # Token.RightParen
        expect();
        if (NC) { return; }
        
        // Parse compound statement (function body)
        parseCompoundStatement(); // -> IDY
        if (NC) { return; }
        
        // Add compound as another child of function
        LDA currentNodeL
        STA ZP.IDXL
        LDA currentNodeH
        STA ZP.IDXH
        
        AST.AddChild(); // IDX = function, IDY = compound
        
        // Return function node in IDY
        LDA currentNodeL
        STA ZP.IDYL
        LDA currentNodeH
        STA ZP.IDYH
        
        SEC
    }
    
    // Parse: { ... }
    parseCompoundStatement() // -> IDY
    {
        // Expect '{'
        LDA # Token.LeftBrace
        expect();
        if (NC) { return; }
        
        // Create compound statement node
        LDA # AST.NodeType.CompoundStmt
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        // Save compound node
        LDA ZP.IDXL
        STA currentNodeL
        LDA ZP.IDXH
        STA currentNodeH
        
        // Parse statements until '}'
        loop
        {
            LDA currentToken
            CMP # Token.RightBrace
            if (Z) { break; }
            
            CMP # Token.EndOfFile
            if (Z) 
            {
                LDA #Token.RightBrace
                printExpectedError();
                CLC
                return;
            }
            
            // Parse expression statement
            parseExpressionStatement(); // -> IDY
            if (NC) { return; }
            
            // Add statement as child of compound
            LDA currentNodeL
            STA ZP.IDXL
            LDA currentNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = compound, IDY = statement
            
            // TODO: continue loop for more statements
        }
        
        // Consume '}'
        consume();
        if (NC) { return; }
        
        // Return compound node in IDY
        LDA currentNodeL
        STA ZP.IDYL
        LDA currentNodeH
        STA ZP.IDYH
        
        SEC
    }
    
    // Parse: expression ;
    parseExpressionStatement() // -> IDY
    {
        // Create expression statement node
        LDA #AST.NodeType.ExprStmt
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        // Save expr statement node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Parse function call
        parseCallExpression(); // -> IDY
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Get expr statement back in IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        AST.AddChild(); // IDX = expr statement, IDY = call
        
        // Move expr statement to IDY for return
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        // Expect ';'
        LDA # Token.Semicolon
        expect();
        if (NC) { return; }
        
        SEC
    }
    
    // Parse: identifier ( arguments )
    parseCallExpression() // -> IDY
    {
        // Expect identifier
        LDA #Token.Identifier
        CMP currentToken
        if (NZ)
        {
            CLC
            return;
        }
        
        // Create call node
        LDA #AST.NodeType.CallExpr
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        // Save call node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Create identifier node for function name
        LDA #AST.NodeType.Identifier
        AST.CreateNode(); // -> IDX
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Copy function name
        LDA Lexer.TokenBufferL
        STA ZP.ACCL
        LDA Lexer.TokenBufferH
        STA ZP.ACCH
        AST.SetData(); // IDX[iData] = ACC
        
        // Move identifier to IDY
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        // Get call node back in IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        AST.AddChild(); // IDX = call, IDY = identifier
        
        // Save call node again
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        consume();  // Move past identifier
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Expect '('
        LDA #Token.LeftParen
        expect();
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Parse arguments
        LDA currentToken
        CMP # Token.StringLiteral
        if (Z)
        {
            // Create string literal node
            LDA # AST.NodeType.StringLit
            AST.CreateNode(); // -> IDX
            if (NC) 
            {
                PLA
                PLA
                return; 
            }
            
            // Copy string data
            LDA Lexer.TokenBufferL
            STA ZP.ACCL
            LDA Lexer.TokenBufferH
            STA ZP.ACCH
            AST.SetData(); // IDX[iData] = ACC
            
            // Move string to IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Get call node back in IDX
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            AST.AddChild(); // IDX = call, IDY = string arg
            
            // Save call node again for return
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            consume();  // Move past string
            if (NC) 
            {
                PLA
                PLA
                return; 
            }
        }
        else
        {
            // No arguments - just restore call node for return
        }
        
        // Expect ')'
        LDA #Token.RightParen
        expect();
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Return call node in IDY
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        SEC
    }
    
    // Main parse function
    Parse()
    {
        // Get first token
        consume();
        if (NC) { return; }
        
        //TODO: for now, just parse one function
        parseFunction(); // -> IDY
        if (NC) { return; }
        
        // Add function to AST root
        AST.GetRoot(); // -> IDX
        AST.AddChild(); // IDX = root, IDY = function
        
        SEC
    }
}
