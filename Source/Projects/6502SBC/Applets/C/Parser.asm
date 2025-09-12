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
    // Input: ZP.STR = what was expected
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
                }
                case Token.RightParen:
                {
                    LDA #')'
                }
                case Token.LeftBrace:
                {
                    LDA #'{'
                }
                case Token.RightBrace:
                {
                    LDA #'}'
                }
                case Token.Semicolon:
                {
                    LDA #';'
                }
                default:
                {
                    LDA #'?'
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
        AST.CreateNode();
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
        AST.CreateNode();
        if (NC) { return; }
        
        // Copy function name from lexer's token buffer
        LDA Lexer.TokenBufferL
        STA ZP.ACCL
        LDA Lexer.TokenBufferH
        STA ZP.ACCH
        AST.SetData();  // Store name pointer in identifier node
        
        // Set identifier as child of function
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        LDA currentNodeL
        STA ZP.IDXL
        LDA currentNodeH
        STA ZP.IDXH
        AST.SetFirstChild();
        
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
        
        // Set compound statement as sibling of function name
        LDA currentNodeL
        STA ZP.IDXL
        LDA currentNodeH
        STA ZP.IDXH
        
        // Get the identifier node (first child)
        LDY # AST.iChild
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH  // IDX = identifier
        
        // IDY already contains compound statement from parseCompoundStatement()
        AST.SetNextSibling(); // Set compound as sibling of identifier
        
        // Restore function node in IDY
        LDA currentNodeL
        STA ZP.IDYL
        LDA currentNodeH
        STA ZP.IDYH
        
        SEC
    }
    
    // Parse: { ... }
    parseCompoundStatement()
    {
        // Expect '{'
        LDA # Token.LeftBrace
        expect();
        if (NC) { return; }
        
        // Create compound statement node
        LDA # AST.NodeType.CompoundStmt
        AST.CreateNode();
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
            
            // TODO: for now, just parse expression statements
            parseExpressionStatement();
            if (NC) { return; }
            
            // Add statement as child of compound
            // TODO: simplified for now - just one statement
            LDA currentNodeL
            STA ZP.IDYL
            LDA currentNodeH
            STA ZP.IDYH
            
            LDA ZP.IDXL
            STA currentNodeL
            LDA ZP.IDXH
            STA currentNodeH
            
            LDA ZP.IDYL
            STA ZP.IDXL
            LDA ZP.IDYH
            STA ZP.IDXH
            AST.SetFirstChild();
        }
        
        // Consume '}'
        consume();
        if (NC) { return; }
        
        // Restore compound node
        LDA currentNodeL
        STA ZP.IDYL
        LDA currentNodeH
        STA ZP.IDYH
        
        SEC
    }
    
    // Parse: expression ;
    parseExpressionStatement()
    {
        // Create expression statement node
        LDA #AST.NodeType.ExprStmt
        AST.CreateNode();
        if (NC) { return; }
        
        // Save it
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // TODO: for now, just parse function calls
        parseCallExpression();
        if (NC) 
        {
            PLA
            PLA
            return; 
        }
        
        // Set call as child of expr statement
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        AST.SetFirstChild();
        
        // Expect ';'
        LDA # Token.Semicolon
        expect();
        if (NC) { return; }
        
        SEC
    }
    
    // Parse: identifier ( arguments )
    parseCallExpression()
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
        AST.CreateNode();
        if (NC) { return; }
        
        // Save call node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Create identifier node for function name
        LDA #AST.NodeType.Identifier
        AST.CreateNode();
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
        AST.SetData();
        
        // Set identifier as first child of call
        LDA ZP.IDXL
        STA ZP.IDYL
        LDA ZP.IDXH
        STA ZP.IDYH
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PHA
        LDA ZP.IDXH
        PHA
        
        AST.SetFirstChild();
        
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
        
        // Parse arguments  TODO: for now, just string literal
        LDA currentToken
        CMP # Token.StringLiteral
        if (Z)
        {
            // Create string literal node
            LDA # AST.NodeType.StringLit
            AST.CreateNode();
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
            AST.SetData();
            
            // Set string as sibling of function name
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Get call node back
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Get identifier (first child)
            LDY # AST.iChild
            LDA [ZP.IDX], Y
            TAX
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            STX ZP.IDXL
            
            AST.SetNextSibling();
            
            consume();  // Move past string
            if (NC) { return; }
        }
        else
        {
            PLA
            PLA
        }
        
        // Expect ')'
        LDA #Token.RightParen
        expect();
        if (NC) { return; }
        
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
        
        // Add function (IDY) to AST root (IDX)
        AST.GetRoot(); // -> IDX
        AST.SetFirstChild();
        
        SEC
    }
}
