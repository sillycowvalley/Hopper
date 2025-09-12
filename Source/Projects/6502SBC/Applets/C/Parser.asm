unit Parser
{
    uses "../System/Definitions"
    uses "Errors"
    uses "Tokens"
    uses "Lexer"
    uses "AST"
    
    // Parser zero page
    const byte parserSlots = 0xB0;
    
    const byte currentToken = parserSlots+0;   // Current token type
    
    // Separate node storage for each parse function
    const uint functionNode  = parserSlots+1;   // parseFunction's node
    const byte functionNodeL = parserSlots+1;
    const byte functionNodeH = parserSlots+2;
    
    const uint compoundNode  = parserSlots+3;   // parseCompoundStatement's node
    const byte compoundNodeL = parserSlots+3;
    const byte compoundNodeH = parserSlots+4;
    
    const uint stmtNode      = parserSlots+5;   // parseExpressionStatement's and parseVariableDeclaration's node
    const byte stmtNodeL     = parserSlots+5;
    const byte stmtNodeH     = parserSlots+6;
    
    const uint callNode      = parserSlots+7;   // parseCallExpression's node
    const byte callNodeL     = parserSlots+7;
    const byte callNodeH     = parserSlots+8;
    
       
    
    // Helper: consume current token and get next
    consume()
    {
        Lexer.NextToken();
        if (NC) 
        {
            LDA # Error.UnexpectedFailure
            Errors.Show();
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
            Errors.Expected();
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
                Errors.OutOfMemory();
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
            STZ functionNodeL
            STZ functionNodeH
            
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
                Errors.Expected();
                break;
            }
            
            // Create identifier node for function name
            LDA # AST.NodeType.Identifier
            AST.CreateNode(); // -> IDX
            if (NC)
            {
                break;
            }
            
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
            if (NC)
            {
                break;
            }
            
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
            
            STZ functionNodeL
            STZ functionNodeH
            
            SEC
            break;
        } // single exit
        
        LDA functionNodeL
        ORA functionNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA functionNodeL
            STA ZP.IDXL
            LDA functionNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
    }
    
    
    // Parse variable declaration: long s;
    parseVariableDeclaration() // -> IDY = VarDecl node
    {
        // currentToken already has the type (Long/Int/Char)
        LDA currentToken
        STA ZP.TEMP  // Save type in zero page instead of stack
        
        consume();  // Move past type token
        if (NC) { return; }
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        STZ stmtNodeH
        STZ stmtNodeL
        
        loop
        {
            // Create VarDecl node
            LDA #AST.NodeType.VarDecl
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save VarDecl node
            LDA ZP.IDXL
            STA stmtNodeL
            LDA ZP.IDXH
            STA stmtNodeH
            
            // Set variable type from TEMP
            LDY #AST.iVarType
            LDA ZP.TEMP
            STA [ZP.IDX], Y
            
            // Expect identifier
            LDA #Token.Identifier
            CMP currentToken
            if (NZ)
            {
                Errors.Expected();
                break;
            }
            
            // Create identifier node
            LDA #AST.NodeType.Identifier
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save identifier node
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Copy variable name
            copyTokenString(); // -> STR
            if (NC)
            {
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                AST.FreeNode();  // Free identifier
                break;
            }
            
            // Set identifier data
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            
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
            
            // Get VarDecl node back
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            
            // Add identifier as child of VarDecl
            AST.AddChild(); // IDX = VarDecl, IDY = identifier
            
            consume();  // Move past identifier
            if (NC) { break; }
            
            // TODO: Check for '=' for initialization
            // For now, just expect ';'
            
            LDA #Token.Semicolon
            expect();
            if (NC) { break; }
            
            // Return VarDecl in IDY
            LDA stmtNodeL
            STA ZP.IDYL
            LDA stmtNodeH
            STA ZP.IDYH
            
            STZ stmtNodeL
            STZ stmtNodeH
            
            SEC
            break;
        } // single exit
        
        LDA stmtNodeL
        ORA stmtNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
        PLA
        STA stmtNodeL
        PLA
        STA stmtNodeH
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
            STZ compoundNodeL
            STZ compoundNodeH
            
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
                    Errors.Expected();
                    break;
                }
                switch(A)
                {
                    case Token.Long:
                    case Token.Int:
                    case Token.Char:
                    {
                        parseVariableDeclaration(); // -> IDY
                        if (NC) { break; }
                    }
                    default:
                    {
                        // Parse expression statement
                        parseExpressionStatement(); // -> IDY
                        if (NC) { break; }
                    }
                }
                
                // Add statement as child of compound
                LDA compoundNodeL
                STA ZP.IDXL
                LDA compoundNodeH
                STA ZP.IDXH
                
                AST.AddChild(); // IDX = compound, IDY = statement
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
            
            STZ compoundNodeL
            STZ compoundNodeH
            
            SEC
            break;
        } // single exit
        
        LDA compoundNodeL
        ORA compoundNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA compoundNodeL
            STA ZP.IDXL
            LDA compoundNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
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
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        STZ stmtNodeH
        STZ stmtNodeL
        
        loop
        {
            // Save expr statement node
            LDA ZP.IDXL
            STA stmtNodeL
            LDA ZP.IDXH
            STA stmtNodeH
            
            // Parse function call
            parseCallExpression(); // -> IDY
            if (NC) { break; }
            
            // Get expr statement back in IDX
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = expr statement, IDY = call
            
            // Move expr statement to IDY for return
            LDA stmtNodeL
            STA ZP.IDYL
            LDA stmtNodeH
            STA ZP.IDYH
            
            // Expect ';'
            LDA # Token.Semicolon
            expect();
            if (NC) { break; }
            
            STZ stmtNodeH
            STZ stmtNodeL
            
            SEC
            break;
        } // single exit
        
        LDA stmtNodeL
        ORA stmtNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
        PLA
        STA stmtNodeL
        PLA
        STA stmtNodeH
    }
    
    // Parse: identifier ( arguments )
    parseCallExpression() // -> IDY
    {
        // Expect identifier
        LDA #Token.Identifier
        CMP currentToken
        if (NZ)
        {
            Errors.Expected(); 
            return;
        }
        
        LDA callNodeH
        PHA
        LDA callNodeL
        PHA
        
        STZ callNodeH
        STZ callNodeL
        
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
            
            STZ callNodeL
            STZ callNodeH
            
            SEC
            break;
        } // single exit
        
        LDA callNodeL
        ORA callNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA callNodeL
            STA ZP.IDXL
            LDA callNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
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
                Errors.Show();
                break; 
            }
            
            LDA currentToken
            switch (A)
            {
                case Token.Void:
                {
                    parseFunction(); // -> IDY
                    if (NC) { break; }   
                    
                    // Add function to AST root
                    AST.GetRoot(); // -> IDX
                    AST.AddChild(); // IDX = root, IDY = function   
                    
                    // Check if there's more after the function
                    LDA currentToken
                    CMP # Token.EndOfFile
                    if (NZ)
                    {
                        LDA # Error.SyntaxError
                        Errors.Show();
                        break;
                    }  
                }
                case Token.Long:
                case Token.Int:
                case Token.Char:
                {
                    // Could be variable declaration or function
                    // TODO: for now, assume variable (since functions must be void)
                    parseVariableDeclaration(); // A = type, -> IDY
                    if (NC) { break; }
                    
                    // Add variable to AST root
                    AST.GetRoot(); // -> IDX
                    AST.AddChild(); // IDX = root, IDY = VarDecl
                }
            }
                        
            SEC
            break;
        } // single exit
    }
}
