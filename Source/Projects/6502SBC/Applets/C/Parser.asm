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
    
    const uint exprNode      = parserSlots+7;   // parseCallExpression's node
    const byte exprNodeL     = parserSlots+7;
    const byte exprNodeH     = parserSlots+8;
    
    const uint rhsExprNode   = parserSlots+9;   // Right expression node
    const byte rhsExprNodeL  = parserSlots+9;
    const byte rhsExprNodeH  = parserSlots+10;
       
    
    // Helper: consume current token and get next
    consume()
    {
        Lexer.NextToken();
        if (NC) 
        {
            LDA # Error.UnexpectedFailure
            Errors.ShowLine();
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
    
    duplicateString() // STR -> STR
    {
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {
            // Get string length
            LDY #0
            loop
            {
                LDA [STR], Y
                if (Z) { break; }
                INY
            }
            INY  // Include null terminator
            
            STY ZP.ACCL
            STZ ZP.ACCH
            Memory.Allocate();
            if (NC) { Errors.OutOfMemory(); break; }
            
            // copy string
            LDY #0
            loop
            {
                LDA [STR], Y
                STA [IDX], Y
                if (Z) { break; }
                INY
            }
            
            LDA ZP.IDXL
            STA ZP.STRL
            LDA ZP.IDXH
            STA ZP.STRH
        
            SEC
            break;    
        } // single exit
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
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
            
            
            // Check for '=' for initialization
            LDA currentToken
            CMP #Token.Assign
            if (Z)  // We have an initializer
            {
                consume();  // Move past '='
                if (NC) { break; }
                
                // Parse the initializer expression (e.g., "0" in "s = 0")
                parseExpression();  // Returns expression in IDY
                if (NC) { break; }
                
                // Save the initializer expression for later
                LDA ZP.IDYL
                PHA
                LDA ZP.IDYH
                PHA
                
                // Create an ASSIGN node for the initialization
                LDA #AST.NodeType.Assign
                AST.CreateNode();  // Returns new node in IDX
                if (NC) { break; }
                
                // Save the ASSIGN node while we build its children
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Get the original identifier from the VarDecl (to duplicate it)
                LDA stmtNodeL
                STA ZP.IDXL
                LDA stmtNodeH
                STA ZP.IDXH
                AST.GetFirstChild();  // Get identifier in IDY
                
                // Extract the string pointer from the original identifier
                LDY #AST.iData  
                LDA [ZP.IDY], Y
                STA ZP.STRL
                INY
                LDA [ZP.IDY], Y
                STA ZP.STRH
                
                // Duplicate the string (to avoid double-free)
                if (C)
                {
                    duplicateString(); // STR -> STR
                }
                
                // Create new identifier node for the ASSIGN
                LDA #AST.NodeType.Identifier
                if (C)
                {
                    AST.CreateNode(); // -> IDX
                }
                
                // Point the new identifier to the duplicated string
                LDA ZP.STRL
                STA ZP.ACCL
                LDA ZP.STRH
                STA ZP.ACCH
                if (C)
                {
                    AST.SetData(); // IDX[iData] = ACC (new string copy)
                }
                
                // Move new identifier to IDY for adding to ASSIGN
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                // Restore ASSIGN node to IDX
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                // Add identifier as first child of ASSIGN (left side)
                if (C)
                {
                    AST.AddChild();  // IDX = ASSIGN, IDY = identifier
                }
                
                // Restore initializer expression to IDY
                PLA
                STA ZP.IDYH
                PLA
                STA ZP.IDYL
                
                // Add initializer as second child of ASSIGN (right side)
                AST.AddChild();  // IDX = ASSIGN, IDY = initializer
                if (NC) { break; }
                
                // ASSIGN is now complete with both children
                // Move it to IDY to wrap in ExprStmt
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                // Create ExprStmt wrapper (assignments as statements need this)
                LDA #AST.NodeType.ExprStmt
                AST.CreateNode();  // Returns new node in IDX
                if (NC) { break; }
                
                // Add complete ASSIGN as child of ExprStmt
                AST.AddChild();  // IDX = ExprStmt, IDY = ASSIGN
                if (NC) { break; }
                
                // Move ExprStmt to IDY for adding as sibling
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                // Restore VarDecl to IDX for adding sibling
                LDA stmtNodeL
                STA ZP.IDXL
                LDA stmtNodeH
                STA ZP.IDXH
                
                // Add ExprStmt as sibling to VarDecl
                // (Both will be added to block by parseCompoundStatement)
                AST.AddSibling();  // IDX = VarDecl, IDY = ExprStmt
                if (NC) { break; }
            }
            
                        
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
        
        LDA exprNodeH
        PHA
        LDA exprNodeL
        PHA
        
        STZ exprNodeH
        STZ exprNodeL
        
        loop
        {
            // Save expr statement node
            LDA ZP.IDXL
            STA exprNodeL
            LDA ZP.IDXH
            STA exprNodeH
            
            // Parse the expression
            parseExpression(); // -> IDY
            if (NC) { break; }
            

            
            // Get expr statement back in IDX
            LDA exprNodeL
            STA ZP.IDXL
            LDA exprNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = expr statement, IDY = expression
            
            // Move expr statement to IDY for return
            LDA exprNodeL
            STA ZP.IDYL
            LDA exprNodeH
            STA ZP.IDYH
            
            // Expect ';'
            LDA # Token.Semicolon
            expect();
            if (NC) { break; }
            
            STZ exprNodeL
            STZ exprNodeH
            
            SEC
            break;
        } // single exit
        
        LDA exprNodeL
        ORA exprNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA exprNodeL
            STA ZP.IDXL
            LDA exprNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
        PLA
        STA exprNodeL
        PLA
        STA exprNodeH
    }
    
    // Top-level expression parser
    parseExpression() // -> IDY
    {
        parseAssignment(); // -> IDY
    }
    
    // Parse assignment expressions (right-associative)
    parseAssignment() // -> IDY
    {
        // Parse left side
        parseRelational(); // -> IDY
        if (NC) { return; }
        
        // Check for assignment operator
        LDA currentToken
        CMP #Token.Assign
        if (NZ)
        {
            // No assignment - return what we have
            SEC
            return;
        }
        
        // Save current exprNode state
        LDA exprNodeH
        PHA
        LDA exprNodeL
        PHA
        LDA rhsExprNodeH
        PHA
        LDA rhsExprNodeL
        PHA
        
        STZ exprNodeL
        STZ exprNodeH
        STZ rhsExprNodeL
        STZ rhsExprNodeH
        
        loop
        {
            // Save left side
            LDA ZP.IDYL
            STA exprNodeL
            LDA ZP.IDYH
            STA exprNodeH
            
            consume();  // Skip '='
            if (NC) { break; }
            
            // Parse right side (recursive for right-associativity)
            parseAssignment(); // -> IDY
            if (NC) { break; }
            
            // Save right side
            LDA ZP.IDYL
            STA rhsExprNodeL
            LDA ZP.IDYH
            STA rhsExprNodeH
            
            // Create Assign node
            LDA #AST.NodeType.Assign
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Add left side as first child
            LDA exprNodeL
            STA ZP.IDYL
            LDA exprNodeH
            STA ZP.IDYH
            
            AST.AddChild(); // IDX = Assign, IDY = left side
            
            // Add right side as second child
            LDA rhsExprNodeL
            STA ZP.IDYL
            LDA rhsExprNodeH
            STA ZP.IDYH
            
            AST.AddChild(); // IDX = Assign, IDY = right side
            
            // Return Assign node in IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Clear our work nodes
            STZ exprNodeL
            STZ exprNodeH
            STZ rhsExprNodeL
            STZ rhsExprNodeH
            
            SEC
            break;
        } // single exit
        
        // Clean up on error
        LDA exprNodeL
        ORA exprNodeH
        if (NZ)
        {
            LDA exprNodeL
            STA ZP.IDXL
            LDA exprNodeH
            STA ZP.IDXH
            AST.FreeNode();
        }
        
        LDA rhsExprNodeL
        ORA rhsExprNodeH
        if (NZ)
        {
            LDA rhsExprNodeL
            STA ZP.IDXL
            LDA rhsExprNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
        // Restore state
        PLA
        STA rhsExprNodeL
        PLA
        STA rhsExprNodeH
        PLA
        STA exprNodeL
        PLA
        STA exprNodeH
    }
       
    // Parse relational operators (<, >, <=, >=)
    parseRelational() // -> IDY
    {
        // For now, just pass through
        parseAdditive(); // -> IDY
        
        // TODO: Handle relational operators
        // loop
        // {
        //     LDA currentToken
        //     CMP #Token.Less / Greater / LessEqual / GreaterEqual
        //     ...
        // }
    }
    
    // Parse additive operators (+, -)
    parseAdditive() // -> IDY
    {
        // For now, just pass through
        parseMultiplicative(); // -> IDY
        
        // TODO: Handle + and -
        // loop
        // {
        //     LDA currentToken
        //     CMP #Token.Plus / Minus
        //     ...
        // }
    }
    
    // Parse multiplicative operators (*, /, %)
    parseMultiplicative() // -> IDY
    {
        // For now, just pass through
        parsePostfix(); // -> IDY
        
        // TODO: Handle *, /, %
        // loop
        // {
        //     LDA currentToken
        //     CMP #Token.Star / Slash / Percent
        //     ...
        // }
    }
    
    // Parse postfix expressions (function calls, array indexing, etc.)
    parsePostfix() // -> IDY
    {
        // Start with a primary
        parsePrimary(); // -> IDY
        if (NC) { return; }
        
        // Check for postfix operators
        loop
        {
            LDA currentToken
            CMP #Token.LeftParen
            if (Z)
            {
                // Convert to function call
                parseCallExpression(); // -> IDY (uses IDY as the identifier)
                // Continue checking for more postfix ops
                continue;
            }
            
            // TODO: Add array indexing with '['
            
            break;  // No more postfix operators
        }
        SEC
    }
    
    // Parse primary expressions (literals, identifiers, parenthesized)
    parsePrimary() // -> IDY
    {
        LDA currentToken
        switch (A)
        {
            case Token.CharLiteral:
            {
                // Create CharLit node
                LDA #AST.NodeType.CharLit
                AST.CreateNode(); // -> IDX
                if (NC) { return; }
                
                // Save the node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Allocate 1 byte for the character value
                LDA #1
                STA ZP.ACCL
                STZ ZP.ACCH
                Memory.Allocate(); // -> IDX
                if (NC)
                {
                    PLA
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    AST.FreeNode();
                    Errors.OutOfMemory();
                    return;
                }
                
                // Copy the character from TokenBuffer to allocated memory
                LDY #0
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                
                // Store pointer in ACC for SetData
                LDA ZP.IDXL
                STA ZP.ACCL
                LDA ZP.IDXH
                STA ZP.ACCH
                
                // Restore node and set data pointer
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                AST.SetData(); // IDX[iData] = ACC (pointer to 1-byte value)
                
                // Move to IDY for return
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                consume();  // Move past the literal
                SEC
            }
            case Token.IntegerLiteral:
            {
                // Create IntLit node
                LDA #AST.NodeType.IntLit
                AST.CreateNode(); // -> IDX
                if (NC) { return; }
                
                // Save the node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Allocate 4 bytes for the 32-bit value
                LDA # 4
                STA ZP.ACCL
                STZ ZP.ACCH
                Memory.Allocate(); // -> IDX
                if (NC)
                {
                    PLA
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    AST.FreeNode();
                    Errors.OutOfMemory();
                    return;
                }
                
                // Copy the 32-bit value from TokenBuffer to allocated memory
                LDY #0
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                INY
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                INY
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                INY
                LDA [Lexer.TokenBuffer], Y
                STA [ZP.IDX], Y
                
                // Store pointer in ACC for SetData
                LDA ZP.IDXL
                STA ZP.ACCL
                LDA ZP.IDXH
                STA ZP.ACCH
                
                // Restore node and set data pointer
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                AST.SetData(); // IDX[iData] = ACC (pointer to 4-byte value)
                
                // Move to IDY for return
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                consume();  // Move past the literal
                SEC
            }
            
            case Token.StringLiteral:
            {
                // Create StringLit node
                LDA #AST.NodeType.StringLit
                AST.CreateNode(); // -> IDX
                if (NC) { return; }
                
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
                    return;
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
                
                // Move to IDY for return
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                consume();  // Move past the literal
                SEC
            }
            
            case Token.Identifier:
            {
                // Create Identifier node
                LDA #AST.NodeType.Identifier
                AST.CreateNode(); // -> IDX
                if (NC) { return; }
                
                // Save identifier node
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Copy identifier name
                copyTokenString(); // -> STR
                if (NC)
                {
                    PLA
                    PLA
                    return;
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
                
                // Move to IDY for return
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                consume();  // Move past identifier
                SEC
            }
            
            case Token.LeftParen:
            {
                consume();  // Skip '('
                if (NC) { return; }
                
                parseExpression(); // -> IDY (recursive)
                if (NC) { return; }
                
                // Expect ')'
                LDA #Token.RightParen
                expect();
            }
            
            default:
            {
                // Unexpected token
                LDA #Error.SyntaxError
                Errors.ShowLine();
                CLC
            }
        }
    }
    
    // Parse function call (when we've seen identifier followed by '(')
    // Input: IDY = identifier node
    // Output: IDY = CallExpr node
    parseCallExpression() // -> IDY
    {
        // At this point, currentToken should be '(' and IDY has the identifier
        LDA currentToken
        CMP #Token.LeftParen
        if (NZ)
        {
            // Not a function call, just return the identifier
            SEC
            return;
        }
        
        consume();  // Skip '('
        if (NC) { return; }
        
        LDA exprNodeH
        PHA
        LDA exprNodeL
        PHA
        
        STZ exprNodeH
        STZ exprNodeL
        
        loop
        {
            // Create CallExpr node
            LDA #AST.NodeType.CallExpr
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save call node
            LDA ZP.IDXL
            STA exprNodeL
            LDA ZP.IDXH
            STA exprNodeH
        
            // Add identifier as first child of CallExpr
            AST.AddChild(); // IDX = CallExpr, IDY = identifier
        
            // Parse arguments
            LDA currentToken
            CMP #Token.RightParen
            if (NZ)  // Has arguments
            {
                loop
                {
                    parseExpression(); // -> IDY
                    if (NC)
                    {
                        break;
                    }
                    
                    // Restore CallExpr to IDX
                    LDA exprNodeH
                    STA ZP.IDXH
                    LDA exprNodeL
                    STA ZP.IDXL
                
                    // Add argument as child
                    AST.AddChild(); // IDX = CallExpr, IDY = argument
                
                    
                    // Check for comma
                    LDA currentToken
                    CMP #Token.Comma
                    if (Z)
                    {
                        consume();
                        if (NC) { break; }
                        continue;
                    }
                    break;
                } // argument loop
                if (NC) { break; }
            } // arguments
            
            // Expect ')'
            LDA #Token.RightParen
            expect();
            if (NC) { break; }
        
            // Return CallExpr in IDY
            LDA exprNodeL
            STA ZP.IDYL
            LDA exprNodeH
            STA ZP.IDYH
            
            STZ exprNodeL
            STZ exprNodeH
            
            SEC
            break;
        } // single exit
        
        LDA exprNodeL
        ORA exprNodeH
        if (NZ)
        {
            // not an ideal exit
            LDA exprNodeL
            STA ZP.IDXL
            LDA exprNodeH
            STA ZP.IDXH
            AST.FreeNode();
            CLC
        }
        
        PLA
        STA exprNodeL
        PLA
        STA exprNodeH
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
                Errors.ShowLine();
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
                        Errors.ShowLine();
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
