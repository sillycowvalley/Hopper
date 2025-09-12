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
    
    // Separate node storage for each parse function
    const uint functionNode  = parserSlots+1;   // parseFunction's node
    const byte functionNodeL = parserSlots+1;
    const byte functionNodeH = parserSlots+2;
    
    const uint compoundNode  = parserSlots+3;   // parseCompoundStatement's node
    const byte compoundNodeL = parserSlots+3;
    const byte compoundNodeH = parserSlots+4;
    
    const uint exprStmtNode  = parserSlots+5;   // parseExpressionStatement's node
    const byte exprStmtNodeL = parserSlots+5;
    const byte exprStmtNodeH = parserSlots+6;
    
    const uint callNode      = parserSlots+7;   // parseCallExpression's node
    const byte callNodeL     = parserSlots+7;
    const byte callNodeH     = parserSlots+8;
    
    // Error messages
    const string msgExpected = "Expected ";
    const string msgError = "Error: 0x";
    const string expectVoid  = "void";
    const string expectIdent = "identifier";
    const string msgLine     = "Line ";
    const string msgColon    = ": ";
    
    printErrorLine()
    {
        LDA #(msgLine % 256)
        STA ZP.STRL
        LDA #(msgLine / 256)
        STA ZP.STRH
        Print.String();
        
        Lexer.GetLineNumber(); // -> ACC
        Shared.MoveAccToTop();
        Long.Print();
        
        LDA #(msgColon % 256)
        STA ZP.STRL
        LDA #(msgColon / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Helper: print "Expected X" error
    // Input: A = token that was expected
    printExpectedError()
    {
        PHA
        
        printErrorLine();
               
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
        CLC
    }
    
    printError()
    {
        PHA
        
        printErrorLine();
        
        LDA #(msgError % 256)
        STA ZP.STRL
        LDA #(msgError / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        Print.Hex(); // error #
        Print.NewLine();
        CLC
    }
    OutOfMemoryError()
    {
        LDA # Error.OutOfMemory
        printError();
        CLC
    }
    
    // Helper: consume current token and get next
    consume()
    {
        Lexer.NextToken();
        if (NC) 
        {
            LDA # Error.UnexpectedFailure
            printError();
            return;
        }
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
            return;
        }
        consume();  // Get next token
    }
    
    // Helper: allocate and copy string from TokenBuffer
    // Output: ZP.STR = pointer to allocated string, C set on success
    copyTokenString() // -> STR
    {
        PHY
        
        loop
        {
            // Get string length
            LDY #0
            loop
            {
                LDA [Lexer.TokenBuffer], Y
                if (Z) { break; }
                INY
            }
            INY  // Include null terminator
            
            // Allocate memory
            TYA
            STA ZP.ACCL
            STZ ZP.ACCH
            Memory.Allocate(); // -> IDX
            if (NC) 
            { 
                Parser.OutOfMemoryError();
                break; 
            }
            
            // Copy string to allocated memory
            LDY #0
            loop
            {
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                if (Z) { break; }
                INY
            }
            
            // Return pointer in STR
            LDA ZP.IDXL
            STA ZP.STRL
            LDA ZP.IDXH
            STA ZP.STRH
            
            
            SEC
            break;
        } // single exit
        PLY
    }
    
    // Parse: void main() { ... }
    parseFunction() // -> IDY
    {
        // Expect 'void'
        LDA #Token.Void
        expect();
        if (NC) { return; }
        
        loop
        {
            // Create function node
            LDA # AST.NodeType.Function
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save function node
            LDA ZP.IDXL
            STA functionNodeL
            LDA ZP.IDXH
            STA functionNodeH
            
            // Expect identifier (function name)
            LDA # Token.Identifier
            CMP currentToken
            if (NZ)
            {
                printExpectedError();
                break;
            }
            
            // Create identifier node for function name
            LDA # AST.NodeType.Identifier
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save identifier node
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Copy function name from token buffer
            copyTokenString(); // -> STR
            if (NC) 
            {
                PLA
                PLA
                break; 
            }
            
            // Move STR to ACC for SetData
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            
            // Restore identifier node and set data
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            AST.SetData(); // IDX[iData] = ACC
            
            // Move identifier to IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Get function node back in IDX
            LDA functionNodeL
            STA ZP.IDXL
            LDA functionNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = function, IDY = identifier
            
            consume();  // Move past identifier
            if (NC) { break; }
            
            // Expect '('
            LDA # Token.LeftParen
            expect();
            if (NC) { break; }
            
            // Expect ')'
            LDA # Token.RightParen
            expect();
            if (NC) { break; }
            
            // Parse compound statement (function body)
            parseCompoundStatement(); // -> IDY
            if (NC) { break; }
            
            // Add compound as another child of function
            LDA functionNodeL
            STA ZP.IDXL
            LDA functionNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = function, IDY = compound
            
            // Return function node in IDY
            LDA functionNodeL
            STA ZP.IDYL
            LDA functionNodeH
            STA ZP.IDYH
            
            SEC
            break;
        } // single exit
    }
    
    // Parse: { ... }
    parseCompoundStatement() // -> IDY
    {
        // Expect '{'
        LDA # Token.LeftBrace
        expect();
        if (NC) { return; }
        
        LDA compoundNodeH
        PHA
        LDA compoundNodeL
        PHA
        
        loop
        {
            // Create compound statement node
            LDA # AST.NodeType.CompoundStmt
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save compound node
            LDA ZP.IDXL
            STA compoundNodeL
            LDA ZP.IDXH
            STA compoundNodeH
            
            // Parse statements until '}'
            loop
            {
                LDA currentToken
                CMP # Token.RightBrace
                if (Z) { SEC break; }
                
                CMP # Token.EndOfFile
                if (Z) 
                {
                    LDA #Token.RightBrace
                    printExpectedError();
                    break;
                }
                
                // Parse expression statement
                parseExpressionStatement(); // -> IDY
                if (NC) { break; }
                
                // Add statement as child of compound
                LDA compoundNodeL
                STA ZP.IDXL
                LDA compoundNodeH
                STA ZP.IDXH
                
                AST.AddChild(); // IDX = compound, IDY = statement
                
                // TODO: continue loop for more statements
            } // inner loop
            if (NC) { break; }
            
            // Consume '}'
            consume();
            if (NC) { break; }
            
            // Return compound node in IDY
            LDA compoundNodeL
            STA ZP.IDYL
            LDA compoundNodeH
            STA ZP.IDYH
            
            SEC
            break;
        } // single exit
        
        PLA
        STA compoundNodeL
        PLA
        STA compoundNodeH
    }
    
    // Parse: expression ;
    parseExpressionStatement() // -> IDY
    {
        // Create expression statement node
        LDA #AST.NodeType.ExprStmt
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        LDA exprStmtNodeH
        PHA
        LDA exprStmtNodeL
        PHA
        
        loop
        {
            // Save expr statement node
            LDA ZP.IDXL
            STA exprStmtNodeL
            LDA ZP.IDXH
            STA exprStmtNodeH
            
            // Parse function call
            parseCallExpression(); // -> IDY
            if (NC) { break; }
            
            // Get expr statement back in IDX
            LDA exprStmtNodeL
            STA ZP.IDXL
            LDA exprStmtNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = expr statement, IDY = call
            
            // Move expr statement to IDY for return
            LDA exprStmtNodeL
            STA ZP.IDYL
            LDA exprStmtNodeH
            STA ZP.IDYH
            
            // Expect ';'
            LDA # Token.Semicolon
            expect();
            if (NC) { break; }
            
            SEC
            break;
        } // single exit
        
        PLA
        STA exprStmtNodeL
        PLA
        STA exprStmtNodeH
    }
    
    // Parse: identifier ( arguments )
    parseCallExpression() // -> IDY
    {
        // Expect identifier
        LDA #Token.Identifier
        CMP currentToken
        if (NZ)
        {
            printExpectedError(); 
            return;
        }
        
        LDA callNodeH
        PHA
        LDA callNodeL
        PHA
        
        loop
        {
            // Create call node
            LDA #AST.NodeType.CallExpr
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save call node
            LDA ZP.IDXL
            STA callNodeL
            LDA ZP.IDXH
            STA callNodeH
            
            // Create identifier node for function name
            LDA #AST.NodeType.Identifier
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save identifier node
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Copy function name
            copyTokenString(); // -> STR
            if (NC) 
            {
                PLA
                PLA
                break; 
            }
            
            // Move STR to ACC for SetData
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            
            // Restore identifier node and set data
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            AST.SetData(); // IDX[iData] = ACC
            
            // Move identifier to IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Get call node back in IDX
            LDA callNodeL
            STA ZP.IDXL
            LDA callNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = call, IDY = identifier
            
            consume();  // Move past identifier
            if (NC) { break; }
            
            // Expect '('
            LDA #Token.LeftParen
            expect();
            if (NC) { break; }
            
            // Parse arguments
            LDA currentToken
            CMP # Token.StringLiteral
            if (Z)
            {
                // Create string literal node
                LDA # AST.NodeType.StringLit
                AST.CreateNode(); // -> IDX
                if (NC) { break; }
                
                // Save string node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Copy string data
                copyTokenString(); // -> STR
                if (NC) 
                {
                    PLA
                    PLA
                    break; 
                }
                
                // Move STR to ACC for SetData
                LDA ZP.STRL
                STA ZP.ACCL
                LDA ZP.STRH
                STA ZP.ACCH
                
                // Restore string node and set data
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                AST.SetData(); // IDX[iData] = ACC
                
                // Move string to IDY
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                // Get call node back in IDX
                LDA callNodeL
                STA ZP.IDXL
                LDA callNodeH
                STA ZP.IDXH
                
                AST.AddChild(); // IDX = call, IDY = string arg
                
                consume();  // Move past string
                if (NC) { break; }
            }
            
            // Expect ')'
            LDA #Token.RightParen
            expect();
            if (NC) { break; }
            
            // Return call node in IDY
            LDA callNodeL
            STA ZP.IDYL
            LDA callNodeH
            STA ZP.IDYH
            
            SEC
            break;
        } // single exit
        
        PLA
        STA callNodeL
        PLA
        STA callNodeH
    }
    
    // Main parse function
    Parse()
    {
        loop
        {
            // Get first token
            consume();
            if (NC) 
            { 
                LDA # Error.SyntaxError
                printError();
                break; 
            }
            
            //TODO: for now, just parse one function
            parseFunction(); // -> IDY
            if (NC) { break; }
            
            // Check if there's more after the function
            LDA currentToken
            CMP #Token.EndOfFile
            if (NZ)
            {
                LDA # Error.SyntaxError
                printError();
                break;
            }
            
            // Add function to AST root
            AST.GetRoot(); // -> IDX
            AST.AddChild(); // IDX = root, IDY = function
            
            SEC
            break;
        } // single exit
    }
}
