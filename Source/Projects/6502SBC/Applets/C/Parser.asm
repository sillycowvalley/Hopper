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
    
    const uint constantNode  = functionNode;    // only global so share parseFunction's node
    const byte constantNodeL = functionNodeL;
    const byte constantNodeH = functionNodeH;
    
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
    
    const uint indexExprNode   = rhsExprNode; 
    const byte indexExprNodeL  = rhsExprNodeL;
    const byte indexExprNodeH  = rhsExprNodeH;
    
    
    const uint binNode   = parserSlots+11;
    const byte binNodeL  = parserSlots+11;
    const byte binNodeH  = parserSlots+12;
    
    const byte binOp      = parserSlots+13;
    const byte simpleOp   = binOp;
    
    const byte bpOffset   = parserSlots+14;
    
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
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
    }
    
        
    // Parse a type token (char, int, long) and handle char*
    // Output: A = type token (Token.Char/Int/Long/CharPtr), C set on success
    parseType()
    {
        loop
        {
            LDA currentToken
            switch (A)
            {
                case Token.Char:
                {
                    consume();
                    if (NC) { break; }
                    
                    // Check for char*
                    LDA currentToken
                    CMP #Token.Star
                    if (Z)
                    {
                        consume();
                        LDA #Token.CharPtr
                    }
                    else
                    {
                        LDA #Token.Char
                    }
                }
                case Token.Int:
                {
                    consume();
                    LDA #Token.Int
                }
                case Token.Long:
                {
                    consume();
                    LDA #Token.Long
                }
                default:
                {
                    // Not a type
                    LDA # Error.SyntaxError
                    Errors.ShowLine();
                    break;
                }
            }
            SEC
            break;
        } // single exit
    }
    
    // TODO: const char array
    // - IntegerLiteral needs to support hex (0xXX)
    // - growString
    // - test range checking (ByteExpected)
    
    
    // Parse array initializer and convert to string: { 'H', 'e', 32, 0x00 } -> StringLit
    // Output: IDY = StringLit node, C set on success
    parseArrayInitializer()
    {
        loop
        {
            // Expect '{'
            LDA #Token.LeftBrace
            expect();
            if (NC) { break; }
            
            // Allocate initial buffer (say, 256 bytes max for const string)
            LDA #0  // 256 bytes
            STA ZP.ACCL
            LDA #1
            STA ZP.ACCH
            Memory.Allocate(); // -> IDX
            if (NC) 
            { 
                Errors.OutOfMemory();
                break; 
            }
            
            // Save string buffer pointer
            LDA ZP.IDXL
            STA ZP.STRL
            LDA ZP.IDXH
            STA ZP.STRH
            
            STZ ZP.IDYL
            STZ ZP.IDYH
            loop
            {
                // Check for closing brace
                LDA currentToken
                CMP #Token.RightBrace
                if (Z) { SEC break; }  // Done
                
                // Parse and store value
                LDA currentToken
                CMP #Token.CharLiteral
                if (Z)
                {
                    // Character literal - get its value from TokenBuffer
                    LDA [Lexer.TokenBuffer]  // The actual character
                    STA [ZP.IDX]
                }
                else
                {
                    CMP #Token.IntegerLiteral
                    if (Z)
                    {
                        // Integer literal - get its value from TokenBuffer
                        LDA [Lexer.TokenBuffer] // LSB of IntegerLiteral
                        STA [ZP.IDX]
                        LDY #1
                        LDA [Lexer.TokenBuffer], Y
                        if (NZ)
                        {
                            LDA # Error.ByteExpected
                            Errors.ShowLine();
                            break;
                        }
                        INY
                        LDA [Lexer.TokenBuffer], Y
                        if (NZ)
                        {
                            LDA # Error.ByteExpected
                            Errors.ShowLine();
                            break;
                        }
                        INY
                        LDA [Lexer.TokenBuffer], Y
                        if (NZ)
                        {
                            LDA # Error.ByteExpected
                            Errors.ShowLine();
                            break;
                        }
                    }
                    else
                    {
                        LDA # Error.SyntaxError
                        Errors.ShowLine();
                        break;
                    }
                }
                
                Shared.IncIDX();
                Shared.IncIDY();
                LDA ZP.IDYH
                
                // Check buffer overflow
                if (NZ) 
                { 
                    LDA # Error.StringTooLong
                    Errors.ShowLine();
                    break; 
                }
                consume();
                if (NC) { break; }
                
                // Check for comma
                LDA currentToken
                CMP #Token.Comma
                if (NZ) { SEC break; } // Done?
                consume(); // ,
                if (NC) { break; }
            } // char loop
            
            if (NC) { break; }
            
            // Expect '}'
            LDA #Token.RightBrace
            expect();
            if (NC) { break; }
            
            // Create StringLit node
            LDA #AST.NodeType.StringLit
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            LDY # AST.iStrFlags
            LDA # StringType.Hex
            STA [ZP.IDX], Y
            
            // Set string pointer as data
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            AST.SetData();
            
            // Return StringLit in IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            SEC
            break;
        } // single exit
    }
    
    parseConstDeclaration() // Input: A = type, -> IDY
    {
        PHA  // Save type

        // Create ConstDecl node
        LDA #AST.NodeType.ConstDecl
        AST.CreateNode(); // -> IDX
        PLA
        if (NC) { return; }
        
        // Store type
        LDY #AST.iConstType
        STA [ZP.IDX], Y
        
        // Save ConstDecl for return
        LDA ZP.IDXL
        STA constantNodeL
        LDA ZP.IDXH
        STA constantNodeH
        
        loop
        {
            // Reuse parseVariableDeclaration's pattern for identifier
            LDA #Token.Identifier
            expect();
            if (NC) { break; }
            
            LDA #AST.NodeType.Identifier
            AST.CreateNode();  // -> IDX
            copyTokenString(); // preserves IDX
            if (NC) { break; }
            
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            AST.SetData();
            
            // Add to ConstDecl
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            LDA constantNodeH
            STA ZP.IDXH
            LDA constantNodeL
            STA ZP.IDXL
            AST.AddChild();
            
            // Parse '= expression;'
            LDA #Token.Assign
            expect();
            if (NC) { break; }
            
            LDA currentToken
            CMP #Token.LeftBrace
            if (Z)
            {
                // Parse array initializer
                parseArrayInitializer(); // -> IDY
            }
            else
            {
                // Parse regular expression (existing code)
                parseExpression(); // -> IDY, munts IDX
            }
            if (NC) { break; }
            
            LDA constantNodeH
            STA ZP.IDXH
            LDA constantNodeL
            STA ZP.IDXL
            AST.AddChild(); // Input: ZP.IDX = node, ZP.IDY = new child
            
            LDA #Token.Semicolon
            expect();
            if (NC) { break; }
            
            // Return ConstDecl in IDY
            LDA constantNodeL
            STA ZP.IDYL
            LDA constantNodeH
            STA ZP.IDYH
            SEC
            break;
        } // single exit
    }
    
    parseFunction() // -> IDY
    {
        PHA // store type
        consume();  // Move past type token
        if (NC) { return; }
        
        // Check if it was char and next is *
        PLA  // Get type back
        PHA  // Save it again
        CMP #Token.Char
        if (Z)
        {
            LDA currentToken
            CMP #Token.Star
            if (Z)
            {
                consume();  // Move past *
                PLA  // Remove Token.Char
                LDA #Token.CharPtr
                PHA  // Save Token.CharPtr instead
            }
        }
        
        // TODO: we need to be able to peek next token (not next char, which could be whitespace)        
        Lexer.CurrentChar();
        CMP #'('
        if (NZ)
        {
            PLA
            parseVariableDeclaration(); // A = type, -> IDY
            
            if (NC) { return; }
        
            // Set scope to Global since we're at program level
            LDY #AST.iVarScope
            LDA #VarScope.Global
            STA [ZP.IDY], Y
            
            return;
        }
        
        loop
        {
            STZ functionNodeL
            STZ functionNodeH
            
            // Create function node
            LDA # AST.NodeType.Function
            AST.CreateNode(); // -> IDX
            PLA
            if (NC) { break; }
            
            // Save function node
            LDX ZP.IDXL
            STX functionNodeL
            LDX ZP.IDXH
            STX functionNodeH
            
            // Store return type in Function node
            LDY #AST.iReturnType  // You'll need to add this field to Function node
            STA [functionNode], Y
            
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
            
            // Copy function name from token buffer
            copyTokenString(); // -> STR
            if (NC) 
            {
                break; 
            }
            
            // Move STR to ACC for SetData
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            
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
            
            CMP #Token.RightParen
            if (NZ)  // Has parameters
            {
                loop
                {
                    // Parse parameter type (int/char/long)
                    LDA currentToken
                    switch (A)
                    {
                        case Token.Char:
                        {
                            consume();  // Move past 'char'
                            if (NC) { break; }
                            // currentToken now has whatever follows
                            LDA currentToken
                            CMP #Token.Star
                            if (Z)  // It's char*
                            {
                                consume();  // Move past '*'
                                if (NC) { break; }
                                LDA #Token.CharPtr
                                STA ZP.TEMP  // Store type as CharPtr
                            }
                            else
                            {
                                LDA #Token.Char
                                STA ZP.TEMP  // Just char
                            }
                        }
                        
                        case Token.FilePtr:
                        case Token.Int:
                        case Token.Long:
                        {
                            STA ZP.TEMP
                            consume();
                            if (NC) { break; }
                        }
                        default:
                        {
                            LDA # Token.Int  // For now, only support int
                            Errors.Expected();
                            break;
                        }
                    }
                    
                    // Create VarDecl node for parameter
                    LDA #AST.NodeType.VarDecl
                    AST.CreateNode(); // -> IDX
                    if (NC) { break; }
                    
                    // parameter as child -> IDY
                    LDA ZP.IDXL
                    STA ZP.IDYL
                    LDA ZP.IDXH
                    STA ZP.IDYH
                    
                    // Store type in iVarType field
                    LDY #AST.iVarType
                    LDA ZP.TEMP
                    STA [ZP.IDY], Y
                    
                    // Parse parameter name
                    LDA currentToken
                    CMP #Token.Identifier
                    if (NZ)
                    {
                        LDA # Token.Identifier
                        Errors.Expected();
                        break;
                    }
                    
                    // Save VarDecl node on stack
                    LDA ZP.IDYL
                    PHA
                    LDA ZP.IDYH
                    PHA
                    
                    // Create Identifier node for parameter name
                    LDA #AST.NodeType.Identifier
                    AST.CreateNode(); // -> IDX
                    if (NC) 
                    { 
                        PLA
                        PLA
                        break; 
                    }
                    
                    // Copy parameter name to Identifier
                    copyTokenString(); // -> STR
                    if (NC)
                    {
                        AST.FreeNode(); // Free the Identifier
                        PLA
                        PLA
                        break;
                    }
                    
                    LDA ZP.STRL
                    STA ZP.ACCL
                    LDA ZP.STRH
                    STA ZP.ACCH
                    AST.SetData(); // Store name in Identifier's iData
                    
                    consume();  // Move past identifier
                    if (NC) { break; }
                    
                    // Add Identifier as child of VarDecl
                    LDA ZP.IDXL
                    STA ZP.IDYL
                    LDA ZP.IDXH
                    STA ZP.IDYH
                    PLA
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    AST.AddChild(); // VarDecl gets Identifier child
                    
                    // Move VarDecl back to IDY for adding to function
                    LDA ZP.IDXL
                    STA ZP.IDYL
                    LDA ZP.IDXH
                    STA ZP.IDYH
                    
                    LDA functionNodeL
                    STA ZP.IDXL
                    LDA functionNodeH
                    STA ZP.IDXH
                    AST.AddChild(); // Function gets parameter as child
                    
                    // Check for comma (more parameters)
                    LDA currentToken
                    CMP #Token.Comma
                    if (Z)
                    {
                        consume();
                        if (NC) { break; }
                        continue;
                    }
                    break;
                } // loop
                
                // Second pass: assign offsets
            
                // Number the parameters:
                LDA functionNodeL
                STA AST.astNodeL
                LDA functionNodeH
                STA AST.astNodeH
            
                AST.CountFunctionParameters();  // [AST.astNode] -> A = count
                CLC
                ADC #3
                STA bpOffset
                
                // Get back to first parameter
                LDY #AST.iChild
                LDA [functionNode], Y
                STA ZP.IDYL
                INY
                LDA [functionNode], Y
                STA ZP.IDYH
                
                LDY #AST.iNext
                LDA [ZP.IDY], Y
                STA ZP.IDXL
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDXH
                
                // Assign offsets to each parameter
                loop
                {
                    LDA ZP.IDXL
                    ORA ZP.IDXH
                    if (Z) { break; }  // No more parameters
                
                    // Store this parameter's BP offset
                    LDY #AST.iOffset
                    LDA bpOffset  // Start at 3 + count, decrement each time
                    STA [ZP.IDX], Y
                    DEC bpOffset
                    
                    // Move to next parameter
                    LDY #AST.iNext
                    LDA [ZP.IDX], Y
                    TAX
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDXH
                    STX ZP.IDXL
                }
            }
        
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
    parseVariableDeclaration() // -> IDY = VarDecl node, A = type
    {
        // currentToken already has the type (Long/Int/Char)
        STA ZP.TEMP  // Save type in zero page instead of stack
        
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
            
            // Set scope to Local by default
            LDY #AST.iVarScope
            LDA #VarScope.Local
            STA [ZP.IDY], Y
            
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
            
            // Copy variable name
            copyTokenString(); // -> STR
            if (NC)
            {
                AST.FreeNode();  // Free identifier
                break;
            }
            
            // Set identifier data
            LDA ZP.STRL
            STA ZP.ACCL
            LDA ZP.STRH
            STA ZP.ACCH
            
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
                    Utilities.DuplicateString(); // STR -> STR
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
    
    // Parse: for (init; condition; update) body
    parseForStatement() // -> IDY
    {
        consume();  // Move past 'for'
        if (NC) { return; }
                        
        // Expect '('
        LDA #Token.LeftParen
        expect();
        if (NC) { return; }
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        STZ stmtNodeH
        STZ stmtNodeL
        
        loop
        {
            // Create For node
            LDA #AST.NodeType.For
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save For node
            LDA ZP.IDXL
            STA stmtNodeL
            LDA ZP.IDXH
            STA stmtNodeH
            
            // Parse init expression (optional)
            LDA currentToken
            CMP # Token.Semicolon
            if (NZ)  // Not empty init
            {
                parseExpression(); // -> IDY
                if (NC)
                {
                    break;
                }

                // Store init expression in For node
                LDY #AST.iForInit
                LDA ZP.IDYL
                STA [stmtNode], Y
                INY
                LDA ZP.IDYH
                STA [stmtNode], Y
            }
            
            // Expect ';'
            LDA #Token.Semicolon
            expect();
            if (NC) { break; }

            // Parse condition expression (optional)
            LDA currentToken
            CMP #Token.Semicolon
            if (NZ)  // Not empty condition
            {
                parseExpression(); // -> IDY
                if (NC) { break; }
                
                // Store condition expression in For node
                LDY #AST.iForExit
                LDA ZP.IDYL
                STA [stmtNode], Y
                INY
                LDA ZP.IDYH
                STA [stmtNode], Y
            }
            
            // Expect ';'
            LDA #Token.Semicolon
            expect();
            if (NC) { break; }
            
            // Parse update expression (optional)
            LDA currentToken
            CMP #Token.RightParen
            if (NZ)  // Not empty update
            {
                parseExpression(); // -> IDY
                if (NC) { break; }
                
                // Store update expression in For node
                LDY #AST.iForNext
                LDA ZP.IDYL
                STA [stmtNode], Y
                INY
                LDA ZP.IDYH
                STA [stmtNode], Y
            }
            
            // Expect ')'
            LDA #Token.RightParen
            expect();
            if (NC) { break; }
            
            // Parse body statement
            parseStatement(); // -> IDY
            if (NC) { break; }
            
            // Add body as child of For node
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = For, IDY = body
            
            // Return For node in IDY
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
    
    // Parse: while (condition) body
    parseWhileStatement() // -> IDY
    {
        consume();  // Move past 'while'
        if (NC) { return; }
        
        // Expect '('
        LDA #Token.LeftParen
        expect();
        if (NC) { return; }
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        STZ stmtNodeH
        STZ stmtNodeL
        
        loop
        {
            // Create While node
            LDA # AST.NodeType.While
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save While node
            LDA ZP.IDXL
            STA stmtNodeL
            LDA ZP.IDXH
            STA stmtNodeH
            
            // Parse condition expression
            parseExpression(); // -> IDY
            if (NC) { break; }
            
            // Add condition as first child
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = While, IDY = condition
            
            // Expect ')'
            LDA #Token.RightParen
            expect();
            if (NC) { break; }
            
            // Parse body statement
            parseStatement(); // -> IDY
            if (NC) { break; }
            
            // Add body as second child
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            
            AST.AddChild(); // IDX = While, IDY = body
            
            // Return While node in IDY
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
    
    // Parse: if (condition) then-stmt [else else-stmt]
    parseIfStatement() // -> IDY
    {
        consume();  // Move past 'if'
        if (NC) { return; }
        
        // Expect '('
        LDA #Token.LeftParen
        expect();
        if (NC) { return; }
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        STZ stmtNodeH
        STZ stmtNodeL
        
        loop
        {
            // Create If node
            LDA #AST.NodeType.If
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Save If node
            LDA ZP.IDXL
            STA stmtNodeL
            LDA ZP.IDXH
            STA stmtNodeH
            
            // Parse condition expression
            parseExpression(); // -> IDY
            if (NC) { break; }
            
            // Add condition as first child
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            AST.AddChild(); // IDX = If, IDY = condition
            
            // Expect ')'
            LDA #Token.RightParen
            expect();
            if (NC) { break; }
            
            // Parse then statement
            parseStatement(); // -> IDY
            if (NC) { break; }
            
            // Add then statement as second child (condition's sibling)
            LDA stmtNodeL
            STA ZP.IDXL
            LDA stmtNodeH
            STA ZP.IDXH
            AST.AddChild(); // IDX = If, IDY = then-stmt
            
            // Check for else clause
            LDA currentToken
            CMP #Token.Else
            if (Z)
            {
                consume(); // Skip 'else'
                if (NC) { break; }
                
                // Parse else statement
                parseStatement(); // -> IDY
                if (NC) { break; }
                
                // Add else statement as third child (then's sibling)
                LDA stmtNodeL
                STA ZP.IDXL
                LDA stmtNodeH
                STA ZP.IDXH
                AST.AddChild(); // IDX = If, IDY = else-stmt
            }
            
            // Return If node in IDY
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
    
    parseReturnStatement() // -> IDY = Return node
    {
        consume();  // Consume 'return'
        if (NC) { return; }
        
        // Create Return node
        LDA # AST.NodeType.Return
        AST.CreateNode(); // -> IDX
        if (NC) { return; }
        
        LDA stmtNodeH
        PHA
        LDA stmtNodeL
        PHA
        
        LDA ZP.IDXL
        STA stmtNodeL
        LDA ZP.IDXH
        STA stmtNodeH
        loop
        {
            // Check for expression or semicolon
            LDA currentToken
            CMP #Token.Semicolon
            if (NZ)  // Has return value
            {
                parseExpression(); // -> IDY
                if (NC) 
                { 
                    break; 
                }
                
                // Restore Return node
                LDA stmtNodeH
                STA ZP.IDXH
                LDA stmtNodeL
                STA ZP.IDXL
                AST.AddChild(); // Return gets expression
            }
            
            // Expect semicolon
            LDA # Token.Semicolon
            expect();
            if (NC) { break; }
            
            // Return Return node in IDY
            LDA stmtNodeL
            STA ZP.IDYL
            LDA stmtNodeH
            STA ZP.IDYH
            
            STZ stmtNodeL
            STZ stmtNodeH
            
            SEC
            
            break;
        }
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
    
    // A = NodeType
    parseSimpleStatement() // -> IDY = new node
    {
        LDX simpleOp
        PHX
        STA simpleOp // NodeType
        
        loop
        {
            consume();
            if (NC) { break; }
            
            // Create simple node
            LDA simpleOp
            AST.CreateNode(); // -> IDX
            if (NC) { break; }
            
            // Return new node in IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH  
            
            LDA simpleOp
            CMP #NodeType.Empty
            if (NZ)
            {
                // Expect semicolon
                LDA #Token.Semicolon
                consume(); // Sets carry appropriately      
            }
            SEC
            break;
        } // single exit
        PLA
        STA simpleOp
    }
    
    parseStatement() // -> IDY
    {
        LDA currentToken
        switch(A)
        {
            case Token.LeftBrace:
            {
                parseCompoundStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.For:
            {
                parseForStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.While:
            {
                parseWhileStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.If:
            {
                parseIfStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.Return:
            {
                parseReturnStatement(); // -> IDY
                if (NC) { return; }
            }
                       
            case Token.Char:
            {
               consume();  // Move past 'char'
               LDA currentToken
               CMP #Token.Star
               if (Z)  // It's char*
               {
                   consume();
                   LDA #Token.CharPtr
               }
               else
               {
                   LDA #Token.Char
               }
               parseVariableDeclaration(); // A = type
            }
            case Token.Long:
            case Token.FilePtr:
            case Token.Int:
            {
                PHA
                consume();
                PLA
                parseVariableDeclaration(); // -> IDY, A = type
                if (NC) { return; }
            }
            case Token.Break:
            {
                LDA #NodeType.Break
                parseSimpleStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.Continue:
            {
                LDA #NodeType.Continue
                parseSimpleStatement(); // -> IDY
                if (NC) { return; }
            }
            case Token.Semicolon:
            {
                LDA # NodeType.Empty
                parseSimpleStatement(); // ->IDY
                if (NC) { return; }
            }
            default:
            {
                parseExpressionStatement(); // -> IDY
                if (NC) { return; }
            }
        }
        SEC
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
                parseStatement(); // -> IDY
                if (NC) { break; }
                
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
        parseAssignment(); // ->IDY
    }
    
    parseAssignmentOperator() // binOp = BinOpType.None | BinOpType.Add | BinOpType.Sub
    {
        loop
        {
            consume();  // Skip '=', '+=', '-='
            if (NC) { break; }
            
            // Parse right side (recursive for right-associativity)
            parseAssignment(); // -> IDY
            if (NC) { break; }
            
            // Save right side
            LDA ZP.IDYL
            STA rhsExprNodeL
            LDA ZP.IDYH
            STA rhsExprNodeH
            
            // Check if compound assignment (binOp != None)
            LDA binOp
            if (NZ)  // Compound assignment - need to create BinOp
            {
                // Create BinOp node
                LDA #AST.NodeType.BinOp
                AST.CreateNode(); // -> IDX
                if (NC) { break; }
                
                // Set operator type
                LDY # AST.iBinOp
                LDA binOp
                STA [ZP.IDX], Y
                
                // Save BinOp node (CloneNode will change IDX!)
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Add left operand (duplicate of original left)
                LDA exprNodeL
                STA ZP.IDYL
                LDA exprNodeH
                STA ZP.IDYH
                AST.CloneNode(); // IDY = node to clone, returns IDY = cloned node
                if (NC) 
                { 
                    PLA
                    PLA
                    break; 
                }
                // Restore BinOp node to IDX
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
                
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Add right operand
                LDA rhsExprNodeL
                STA ZP.IDYL
                LDA rhsExprNodeH
                STA ZP.IDYH
                AST.AddChild(); // IDX = BinOp, IDY = right
                
                // BinOp becomes the new right side
                LDA ZP.IDXL
                STA rhsExprNodeL
                LDA ZP.IDXH
                STA rhsExprNodeH
            }
            
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
            
            // Add right side (or BinOp) as second child
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
            
            // Clear our work nodes (so they don't get dispose)
            STZ rhsExprNodeL
            STZ rhsExprNodeH
            
            SEC
            break;
        } // single exit
    }
    
    // Parse logical OR expressions (||)
    parseLogicalOr() // -> IDY
    {
        parseLogicalAnd(); // Parse left side -> IDY
        if (NC) { return; }
        
        loop
        {
            LDA currentToken
            CMP #Token.LogicalOr
            if (NZ) 
            { 
                SEC
                break;  // Not ||, return what we have
            }
            
            LDX binOp
            PHX
            
            LDA # BinOpType.LogicalOr
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume ||
                if (NC) { break; }
                
                // Create BinOp node
                LDA #AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseLogicalAnd();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    parseBitwiseOr()  // -> IDY
    {
        parseBitwiseAnd();  // Parse left side -> IDY
        if (NC) { return; }
        loop
        {
            LDA currentToken
            CMP # Token.BitwiseOr    // Assuming you have Token.BitwiseOr for '|'
            if (NZ)
            {
                SEC
                break;  // Not |, return what we have
            }
            
            LDX binOp
            PHX
            
            LDA #BinOpType.BitwiseOr
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseBitwiseAnd();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    parseBitwiseAnd()  // -> IDY
    {
        parseEquality();  // Parse left side -> IDY
        if (NC) { return; }
        loop
        {
            LDA currentToken
            CMP # Token.BitwiseAnd    // Single & for bitwise AND
            if (NZ)
            {
                SEC
                break;  // Not &, return what we have
            }
            
            LDX binOp
            PHX
            
            LDA #BinOpType.BitwiseAnd
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseEquality();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    // Parse logical AND expressions (&&)
    parseLogicalAnd() // -> IDY
    {
        parseBitwiseOr(); // Parse left side -> IDY
        if (NC) { return; }
        
        loop
        {
            LDA currentToken
            CMP #Token.LogicalAnd
            if (NZ) 
            { 
                SEC
                break;  // Not &&, return what we have
            }
            
            LDX binOp
            PHX
            
            LDA #BinOpType.LogicalAnd
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume &&
                if (NC) { break; }
                
                // Create BinOp node
                LDA #AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseBitwiseOr();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    // Parse assignment expressions (right-associative)
    parseAssignment() // -> IDY
    {
        // Parse left side
        parseLogicalOr(); // -> IDY
        if (NC) { return; }
        
        // Save current exprNode state
        LDA exprNodeH
        PHA
        LDA exprNodeL
        PHA
        LDA rhsExprNodeH
        PHA
        LDA rhsExprNodeL
        PHA
        LDA binOp
        PHA
        
        STZ rhsExprNodeL
        STZ rhsExprNodeH
        
        // Save left side
        LDA ZP.IDYL
        STA exprNodeL
        LDA ZP.IDYH
        STA exprNodeH
       
        // Check for assignment operator
        LDA currentToken
        switch (A)
        {
            case Token.Assign:
            {
                LDA # BinOpType.None
                STA binOp
                parseAssignmentOperator();    
            }
            case Token.PlusAssign:
            {
                LDA # BinOpType.Add
                STA binOp
                parseAssignmentOperator();    
            }
            case Token.MinusAssign:
            {
                LDA # BinOpType.Sub
                STA binOp
                parseAssignmentOperator();    
            }
            default:
            {
                // No assignment - return what we have
                SEC
            }
        }
        
        // Clean up on error
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
        STA binOp
        PLA
        STA rhsExprNodeL
        PLA
        STA rhsExprNodeH
        PLA
        STA exprNodeL
        PLA
        STA exprNodeH
    }
    
    
    // Parse equality operators (==, !=)
    parseEquality() // -> IDY
    {
        parseRelational(); // Parse left side -> IDY
        if (NC) { return; }
        
        loop
        {
            LDA currentToken
            switch (A)
            {
                case Token.Equal:    // ==
                {
                    LDA #BinOpType.EQ
                }
                case Token.NotEqual: // !=
                {
                    LDA #BinOpType.NE
                }
                default:
                {
                    SEC
                    break;  // Not ==, !=, return what we have
                }
            }
            
            LDX binOp
            PHX
            
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseRelational();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
       
    // Parse relational operators (<, >, <=, >=)
    // Parse relational operators (<, >, <=, >=)
    parseRelational() // -> IDY
    {
        parseAdditive(); // Parse left side -> IDY
        if (NC) { return; }
        
        loop
        {
            LDA currentToken
            switch (A)
            {
                case Token.Less:         // 
                {
                    LDA #BinOpType.LT
                }
                case Token.Greater:      // >
                {
                    LDA #BinOpType.GT
                }
                case Token.LessEqual:    // <=
                {
                    LDA #BinOpType.LE
                }
                case Token.GreaterEqual: // >=
                {
                    LDA #BinOpType.GE
                }
                default:
                {
                    SEC
                    break;  // Not a relational operator, return what we have
                }
            }
            
            LDX binOp
            PHX
            
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseAdditive();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    parseAdditive()  // -> IDY
    {
        parseMultiplicative();  // Parse left side -> IDY
        if (NC) { return; }
        loop
        {
            LDA currentToken
            switch (A)
            {
                case Token.Plus:
                {
                    LDA #BinOpType.Add
                }
                case Token.Minus:
                {
                    LDA #BinOpType.Sub
                }
                default:
                {
                    SEC
                    break;  // Not +/-, return what we have
                }
            }
            LDX binOp
            PHX
            
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseMultiplicative();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    // Parse multiplicative operators (*, /, %)
    parseMultiplicative() // -> IDY
    {
        parseUnary();  // Parse left side -> IDY
        if (NC) { return; }
        loop
        {
            LDA currentToken
            switch (A)
            {
                case Token.Star:
                {
                    LDA # BinOpType.Mul
                }
                case Token.Slash:
                {
                    LDA # BinOpType.Div
                }
                case Token.Percent:
                {
                    LDA # BinOpType.Mod
                }
                default:
                {
                    SEC
                    break;  // Not * / %, return what we have
                }
            }
            LDX binOp
            PHX
            
            STA binOp
            
            LDA binNodeL
            PHA
            LDA binNodeH
            PHA
            
            loop
            {
                consume();  // Consume the operator
                if (NC) { break; }
                
                // Create BinOp node
                LDA # AST.NodeType.BinOp
                AST.CreateNode();  // -> IDX
                if (NC) { break; }
                LDA ZP.IDXH
                STA binNodeH
                LDA ZP.IDXL
                STA binNodeL
                
                // Set operator type
                LDA binOp
                LDY #AST.iBinOp
                STA [ZP.IDX], Y
                
                // Add left operand as first child
                AST.AddChild();  // IDX = BinOp, IDY = left
                
                // Parse right operand
                parseUnary();  // -> IDY
                if (NC) { break; }
                
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                
                // Add right operand as second child
                AST.AddChild();  // IDX = BinOp, IDY = right
                
                // Move BinOp to IDY for next iteration (left-associative)
                LDA ZP.IDXL
                STA ZP.IDYL
                LDA ZP.IDXH
                STA ZP.IDYH
                
                STZ binNodeH
                STZ binNodeL
                
                SEC
                break;
                
            } // single exit
            
            LDA binNodeH
            ORA binNodeL
            if (NZ)
            {
                LDA binNodeH
                STA ZP.IDXH
                LDA binNodeL
                STA ZP.IDXL
                AST.FreeNode();
            }
            
            PLA
            STA binNodeH
            PLA
            STA binNodeL
            PLA
            STA binOp
            if (NC) { break; }
        } // loop
    }
    
    
    // Parse unary expression: ("-" | "+")* <postfix>
    // Output: IDY = expression node, C = success
    parseUnary() // -> IDY
    {
        // Check for unary operators
        LDA currentToken
        switch (A)
        {
            case Token.Minus:
            {
                LDA # AST.UnaryOpType.Minus
                PHA
                consume(); // Move past minus
                
                // Recursively parse the operand
                parseUnary();
                if (NC) 
                { 
                    PLA  // Clean up stack
                    return; 
                }
            }
            case Token.Plus:
            {
                consume(); // Move past plus
                // Just parse the operand - unary plus is a no-op
                parseUnary();
                return;
            }
            case Token.Star:  // Dereference operator
            {
                LDA # AST.UnaryOpType.Dereference
                PHA
                consume(); // Move past *
                
                // Recursively parse the operand (allows *(*ptr) etc.)
                parseUnary();
                if (NC) 
                { 
                    PLA  // Clean up stack
                    return; 
                }
            }
            default:
            {
                // Not a unary operator, parse postfix
                parsePostfix();
                return;
            }
        }
        
        // We have a unary minus - create the node
        // Operand already in IDY, operator type on stack
        
        // Save the operand expression
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        loop
        {
            // Create UnaryOp node
            LDA #AST.NodeType.UnaryOp
            AST.CreateNode(); // -> IDX
            if (NC) 
            { 
                PLA
                PLA
                PLA
                break; 
            }
            
            // Restore operand to IDY
            PLA
            STA ZP.IDYH
            PLA
            STA ZP.IDYL
            
            // Set the operator type
            PLA
            LDY #AST.iUnaryOp
            STA [ZP.IDX], Y
            
            // Add operand as child
            AST.AddChild(); // IDX = UnaryOp, IDY = operand
            if (NC) { break; }
            
            // Move UnaryOp to IDY for return
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            SEC
            break;
        }
    }
    
    // Input: A = PostfixOpType, IDY = expression to apply postfix to
    // Output: IDY = PostfixOp node
    parsePostfixOperator() // -> IDY
    {
        // Save operator type
        PHA
        
        // Save the expression
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        loop
        {
            // Create PostfixOp node
            LDA #AST.NodeType.PostfixOp
            AST.CreateNode(); // -> IDX
            if (NC) 
            { 
                PLA
                PLA
                PLA
                break; 
            }
            
            // Restore expression as IDY
            PLA
            STA ZP.IDYH
            PLA
            STA ZP.IDYL
            
            // Set the operator type
            PLA  // Get operator type
            LDY #AST.iPostfixOp
            STA [ZP.IDX], Y
            
            // Add expression as child of PostfixOp
            AST.AddChild(); // IDX = PostfixOp, IDY = expression
            
            // Move PostfixOp to IDY for return
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            consume(); // Move past ++ or --
            SEC
            break;
        }
    }
    
    // Parse array subscript: buffer[index] -> *(buffer + index)
    // Input: IDY = array/pointer expression
    // Output: IDY = dereferenced subscript expression, C set on success
    parseArraySubscript() // -> IDY
    {
        LDA exprNodeL
        PHA
        LDA exprNodeH
        PHA
        LDA indexExprNodeL
        PHA
        LDA indexExprNodeH
        PHA
        
        // Save array/pointer expression
        LDA ZP.IDYL
        STA exprNodeL
        LDA ZP.IDYH
        STA exprNodeH
        
        loop
        {
            consume();  // Skip '['
            if (NC) { break; }
            
            // Parse index expression
            parseExpression(); // -> IDY (index)
            if (NC) { break; }
            
            // Save index expression
            LDA ZP.IDYL
            STA indexExprNodeL
            LDA ZP.IDYH
            STA indexExprNodeH  // Fixed typo - was indexExprNodeL
            
            // Expect ']'
            LDA #Token.RightBracket
            expect();
            if (NC) 
            { 
                break; 
            }
            
            // Create BinOp(Add) for (buffer + index)
            LDA #AST.NodeType.BinOp
            AST.CreateNode(); // -> IDX
            if (NC) 
            { 
                break; 
            }
            
            // Set operator to Add
            LDY #AST.iBinOp
            LDA #AST.BinOpType.Add
            STA [ZP.IDX], Y
            
            // Add array/pointer as left operand
            LDA exprNodeL  // Use saved value instead of stack-relative
            STA ZP.IDYL
            LDA exprNodeH
            STA ZP.IDYH
            AST.AddChild(); // IDX = BinOp, IDY = buffer
            
            // Add index as right operand
            LDA indexExprNodeL
            STA ZP.IDYL
            LDA indexExprNodeH
            STA ZP.IDYH
            AST.AddChild(); // IDX = BinOp, IDY = index
            
            // Save BinOp
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            // Create UnaryOp(Dereference)
            LDA #AST.NodeType.UnaryOp
            AST.CreateNode(); // -> IDX
            if (NC) 
            { 
                break; 
            }
            
            // Set operator to Dereference
            LDY #AST.iUnaryOp
            LDA #UnaryOpType.Dereference
            STA [ZP.IDX], Y
            
            // Add BinOp as child
            AST.AddChild(); // IDX = UnaryOp, IDY = BinOp
            
            // Move result to IDY
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            
            SEC
            break;
        } // single exit
        
        PLA
        STA indexExprNodeH
        PLA
        STA indexExprNodeL
        PLA
        STA exprNodeH
        PLA
        STA exprNodeL
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
            switch (A)
            {
                case Token.LeftParen:
                {
                    // Convert to function call
                    parseCallExpression(); // -> IDY (uses IDY as the identifier)
                    if (NC) { break; }
                    // Continue checking for more postfix ops
                    continue;
                }
                case Token.Increment: // ++
                {
                    LDA #PostfixOpType.Increment
                    parsePostfixOperator(); // -> IDY
                    if (NC) { break; }
                }
                case Token.Decrement: // --
                {
                    LDA #PostfixOpType.Decrement
                    parsePostfixOperator(); // -> IDY
                    if (NC) { break; }
                }
                case Token.LeftBracket:  // Array subscript
                {
                    parseArraySubscript(); // IDY = array -> IDY = *(array + index)
                    if (NC) { return; }
                }
            }
            SEC
            break;  // No more postfix operators
        }
    }
    
    // Parse primary expressions (literals, identifiers, parenthesized)
    parsePrimary() // -> IDY
    {
        LDA currentToken
        switch (A)
        {
            case Token.Null:
            {
                // Create IntLit node with value 0
                LDA #AST.NodeType.IntLit
                AST.CreateNode(); // -> IDX
                
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
                LDA #0
                STA [ZP.IDX], Y
                INY
                STA [ZP.IDX], Y
                INY
                STA [ZP.IDX], Y
                INY
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
                
                consume();
                SEC
            }
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
                
                LDY # AST.iStrFlags
                LDA # StringType.Normal
                STA [ZP.IDX], Y
                
                // Copy string data
                copyTokenString(); // -> STR
                if (NC)
                {
                    return;
                }
                
                // Move STR to ACC for SetData
                LDA ZP.STRL
                STA ZP.ACCL
                LDA ZP.STRH
                STA ZP.ACCH
                
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
                
                // Copy identifier name
                copyTokenString(); // -> STR
                if (NC)
                {
                    return;
                }
                
                // Move STR to ACC for SetData
                LDA ZP.STRL
                STA ZP.ACCL
                LDA ZP.STRH
                STA ZP.ACCH
                
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
#ifdef DEBUG
LDA #'p' Print.Char();
#endif            
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
        // Get first token
        consume();
        if (NC) 
        { 
#ifdef DEBUG
LDA #'x' Print.Char();
#endif
            LDA # Error.SyntaxError
            Errors.ShowLine();
            return; 
        }
        
        loop
        {
            // Check for end of file
            LDA currentToken
            CMP #Token.EndOfFile
            if (Z) 
            { 
                SEC  // Success - parsed everything
                break; 
            }
            
            LDA currentToken
            switch (A)
            {
                case Token.Const:
                {
                    consume();  // Move past 'const'
                    if (NC) { break; }
                    parseType();  // Get type in A
                    if (NC) { break; } 
                    parseConstDeclaration();  // A = type, -> IDY
                    if (NC) { break; } 
                    
                    // Add constant to AST root
                    AST.GetRoot(); // -> IDX
                    AST.AddChild(); // IDX = root, IDY = constant
                }
                case Token.Char:
                case Token.Long:
                case Token.FilePtr:
                case Token.Int:
                case Token.Void:
                {
                    parseFunction(); // -> IDY
                    if (NC) { break; }   
                    
                    // Add function (or variable) to AST root
                    AST.GetRoot(); // -> IDX
                    AST.AddChild(); // IDX = root, IDY = function/variable  
                }
                default:
                {
                    LDA currentToken
                    Errors.Unexpected();
                    break;
                }
            }
            if (NC) { break; }  // Error occurred
                        
            // Continue looping to parse next function
        } // single exit
    }
}
