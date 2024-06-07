unit Parser
{
    uses "AST"
    uses "TinyToken"
    uses "Lexer"
    
    record ParserError
    {
        ParserErrorType Type;
        uint Line;
    }
    
    enum ParserErrorType
    {
        NONE,
        UNEXPECTED_TOKEN,
        EXPECTED_EXPRESSION,
        EXPECTED_CLOSING_PAREN
    }
    
    string ToString(ParserError error)
    {
        string result = "Undefined error";
        switch (error.Type)
        {
            case ParserErrorType.NONE: { result = "No error"; }
            case ParserErrorType.UNEXPECTED_TOKEN: { result = "Unexpected token"; }
            case ParserErrorType.EXPECTED_EXPRESSION: { result = "Expected expression"; }
            case ParserErrorType.EXPECTED_CLOSING_PAREN: { result = "Expected closing parenthesis"; }
        }
        return result + " at line " + (error.Line).ToString();
    }
    
    record Parser
    {
        <Token> Tokens;
        uint Current;
    }
    
    Parser Parser(<Token> tokens)
    {
        Parser result;
        result.Tokens = tokens;
        result.Current = 0;
        return result;
    }
    
    bool isAtEnd(Parser parser)
    {
        Token current = (parser.Tokens).GetItem(parser.Current);
        return current.Type == TokenType.EOF;
    }
    
    Token advance(ref Parser parser)
    {
        if (!isAtEnd(parser)) { parser.Current++; }
        return previous(parser);
    }
    
    Token peek(Parser parser)
    {
        Token result = (parser.Tokens).GetItem(parser.Current);
        return result;
    }
    
    Token previous(Parser parser)
    {
        Token result = (parser.Tokens).GetItem(parser.Current - 1);
        return result;
    }

    Token peekNext(Parser parser)
    {
        Token result;
        if (parser.Current + 1 >= (parser.Tokens).Count)
        {
            result = (parser.Tokens).GetItem(parser.Current);
            return result;
        }
        result = (parser.Tokens).GetItem(parser.Current + 1);
        return result;
    }
    
    bool match(ref Parser parser, TokenType tokenType)
    {
        if (isAtEnd(parser)) { return false; }
        if ((peek(parser)).Type != tokenType) { return false; }
        _ = advance(ref parser);
        return true;
    }
    
    ParserError parseExpression(ref Parser parser, ref Expr expr)
    {
        expr = parseEquality(ref parser);
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = expr.Line;
        return success;
    }
    
    Expr parseEquality(ref Parser parser)
    {
        Expr expr = parseComparison(ref parser);
        while (match(ref parser, TokenType.SYM_EQEQ) || match(ref parser, TokenType.SYM_NEQ))
        {
            string operator = (previous(parser)).Lexeme;
            Expr right = parseComparison(ref parser);
            expr = AST.ExprBinary(expr.Line, expr, operator, right);
        }
        return expr;
    }
    
    Expr parseComparison(ref Parser parser)
    {
        Expr expr = parseAddition(ref parser);
        while (match(ref parser, TokenType.SYM_LT) || match(ref parser, TokenType.SYM_LTE) ||
               match(ref parser, TokenType.SYM_GT) || match(ref parser, TokenType.SYM_GTE))
        {
            string operator = (previous(parser)).Lexeme;
            Expr right = parseAddition(ref parser);
            expr = AST.ExprBinary(expr.Line, expr, operator, right);
        }
        return expr;
    }
    
    Expr parseAddition(ref Parser parser)
    {
        Expr expr = parseMultiplication(ref parser);
        while (match(ref parser, TokenType.SYM_PLUS) || match(ref parser, TokenType.SYM_MINUS))
        {
            string operator = (previous(parser)).Lexeme;
            Expr right = parseMultiplication(ref parser);
            expr = AST.ExprBinary(expr.Line, expr, operator, right);
        }
        return expr;
    }
    
    Expr parseMultiplication(ref Parser parser)
    {
        Expr expr = parseUnary(ref parser);
        while (match(ref parser, TokenType.SYM_STAR) || match(ref parser, TokenType.SYM_SLASH))
        {
            string operator = (previous(parser)).Lexeme;
            Expr right = parseUnary(ref parser);
            expr = AST.ExprBinary(expr.Line, expr, operator, right);
        }
        return expr;
    }
    
    Expr parseUnary(ref Parser parser)
    {
        if (match(ref parser, TokenType.SYM_BANG) || match(ref parser, TokenType.SYM_MINUS))
        {
            string operator = (previous(parser)).Lexeme;
            Expr right = parseUnary(ref parser);
            return AST.ExprUnary(right.Line, operator, right);
        }
        return parsePrimary(ref parser);
    }
    
    Expr parsePrimary(ref Parser parser)
    {
        if (match(ref parser, TokenType.LIT_NUMBER))
        {
            return AST.ExprLiteral((previous(parser)).Line, (previous(parser)).Lexeme);
        }
        if (match(ref parser, TokenType.LIT_CHAR))
        {
            return AST.ExprLiteral((previous(parser)).Line, (previous(parser)).Lexeme);
        }
        if (match(ref parser, TokenType.LIT_STRING))
        {
            return AST.ExprLiteral((previous(parser)).Line, (previous(parser)).Lexeme);
        }
        if (match(ref parser, TokenType.IDENTIFIER))
        {
            Expr varExpr = AST.ExprVariable((previous(parser)).Line, (previous(parser)).Lexeme);
            if ((peek(parser)).Type == TokenType.SYM_LPAREN)
            {
                _ = advance(ref parser); // Consume the '('
                <Expr> arguments;
                if (!match(ref parser, TokenType.SYM_RPAREN))
                {
                    loop
                    {
                        Expr arg;
                        ParserError error = parseExpression(ref parser, ref arg);
                        if (error.Type != ParserErrorType.NONE)
                        {
                            return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
                        }
                        arguments.Append(arg);
                        if (!match(ref parser, TokenType.SYM_COMMA))
                        {
                            if (!match(ref parser, TokenType.SYM_RPAREN))
                            {
                                return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
                            }
                            break;
                        }
                    }
                }
                return AST.ExprCall(varExpr.Line, varExpr, arguments);
            }
            return varExpr;
        }
        if (match(ref parser, TokenType.SYM_LPAREN))
        {
            Expr expr;
            ParserError error = parseExpression(ref parser, ref expr);
            if (error.Type != ParserErrorType.NONE)
            {
                return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
            }
            if (!match(ref parser, TokenType.SYM_RPAREN))
            {
                return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
            }
            return expr;
        }
        return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
    }
    
    ParserError parseDeclaration(ref Parser parser, ref Decl decl)
    {
        if (match(ref parser, TokenType.KW_FUNC))
        {
            return parseFunctionDeclaration(ref parser, ref decl);
        }
        else if (match(ref parser, TokenType.KW_CONST))
        {
            return parseConstantDeclaration(ref parser, ref decl);
        }
        else
        {
            return parseVariableDeclaration(ref parser, ref decl);
        }
    }
    
    ParserError parseVariableDeclaration(ref Parser parser, ref Decl decl)
    {
        StmtVar varStmt;
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = (peek(parser)).Line;
            return error;
        }
        varStmt.Name = (previous(parser)).Lexeme;
        uint line = (previous(parser)).Line;
    
        Expr initializer;
        if (match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error = parseExpression(ref parser, ref initializer);
            if (error.Type != ParserErrorType.NONE) 
            { 
                return error; 
            }
        }
        else
        {
            initializer = AST.ExprLiteral(line, "0"); // Using "0" as the default value
        }
        varStmt.Initializer = initializer;
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Decl result;
        result.Line = line;
        result.Type = DeclType.VAR_DECL;
        result.VarDecl = varStmt;
        decl = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseConstantDeclaration(ref Parser parser, ref Decl decl)
    {
        StmtVar constStmt;
        uint line = (peek(parser)).Line;
    
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        constStmt.Name = (previous(parser)).Lexeme;
        line = (previous(parser)).Line;
    
        if (!match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Expr initializer;
        ParserError initError = parseExpression(ref parser, ref initializer);
        if (initError.Type != ParserErrorType.NONE)
        {
            return initError;
        }
        constStmt.Initializer = initializer;
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Decl result;
        result.Line = line;
        result.Type = DeclType.VAR_DECL; // Assuming const and var share the same struct, if not create a new type
        result.VarDecl = constStmt;
        decl = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
        
    ParserError parseFunctionDeclaration(ref Parser parser, ref Decl decl)
    {
        DeclFunc funcDecl;
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = (peek(parser)).Line;
            return error;
        }
        funcDecl.Name = (previous(parser)).Lexeme;
        uint line = (previous(parser)).Line;
    
        if (!match(ref parser, TokenType.SYM_LPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = (peek(parser)).Line;
            return error;
        }
    
        <string> parameters;
        if (!match(ref parser, TokenType.SYM_RPAREN))
        {
            loop
            {
                if (!match(ref parser, TokenType.IDENTIFIER))
                {
                    ParserError error;
                    error.Type = ParserErrorType.UNEXPECTED_TOKEN;
                    error.Line = (peek(parser)).Line;
                    return error;
                }
                parameters.Append((previous(parser)).Lexeme);
                if (!match(ref parser, TokenType.SYM_COMMA))
                {
                    if (!match(ref parser, TokenType.SYM_RPAREN))
                    {
                        ParserError error;
                        error.Type = ParserErrorType.UNEXPECTED_TOKEN;
                        error.Line = (peek(parser)).Line;
                        return error;
                    }
                    break;
                }
            }
        }
    
        funcDecl.Params = parameters;
    
        <Stmt> body;
        if (!match(ref parser, TokenType.SYM_LBRACE))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = (peek(parser)).Line;
            return error;
        }
        while (!match(ref parser, TokenType.SYM_RBRACE) && !isAtEnd(parser))
        {
            Stmt statement;
            ParserError error = parseStatement(ref parser, ref statement);
            if (error.Type != ParserErrorType.NONE) 
            { 
                return error; 
            }
            body.Append(statement);
        }
    
        funcDecl.Body = body;
        Decl result;
        result.Line = line;
        result.Type = DeclType.FUNC_DECL;
        result.FuncDecl = funcDecl;
        decl = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError ParseProgram(ref Parser parser, ref Program prog)
    {
        <Decl> declarations;
        ParserError error;
        loop
        {
            Decl decl;
            error = parseDeclaration(ref parser, ref decl);
            if (error.Type != ParserErrorType.NONE) 
            { 
                return error; 
            }
            declarations.Append(decl);
            if (isAtEnd(parser)) 
            { 
                break; 
            }
        }
        prog.Declarations = declarations;
    
        ParserError result;
        result.Type = ParserErrorType.NONE;
        result.Line = 0;
        return result;
    }
    
    ParserError parseStatement(ref Parser parser, ref Stmt stmt)
    {
        if (match(ref parser, TokenType.KW_IF)) 
        {
            return parseIfStatement(ref parser, ref stmt);
        } 
        else if (match(ref parser, TokenType.KW_WHILE)) 
        {
            return parseWhileStatement(ref parser, ref stmt);
        } 
        else if (match(ref parser, TokenType.KW_FOR)) 
        {
            return parseForStatement(ref parser, ref stmt);
        } 
        else if (match(ref parser, TokenType.KW_RETURN)) 
        {
            return parseReturnStatement(ref parser, ref stmt);
        } 
        else if (match(ref parser, TokenType.SYM_LBRACE)) 
        {
            return parseBlockStatement(ref parser, ref stmt);
        }
        else 
        {
            Token token = peek(parser);
            if (token.Type == TokenType.KW_CONST)
            {
                return parseLocalConstDeclaration(ref parser, ref stmt);
            }
            else if ((token.Type == TokenType.KW_BYTE) || (token.Type == TokenType.KW_WORD) ||
                     (token.Type == TokenType.KW_INT) || (token.Type == TokenType.KW_UINT) ||
                     (token.Type == TokenType.KW_BOOL) || (token.Type == TokenType.KW_CHAR))
            {
                return parseLocalVariableDeclaration(ref parser, ref stmt);
            }
            else if ((token.Type == TokenType.IDENTIFIER) && ((peekNext(parser)).Type == TokenType.SYM_EQ))
            {
                return parseAssignmentStatement(ref parser, ref stmt);
            }
            else
            {
                return parseExpressionStatement(ref parser, ref stmt);
            }
        }
    }

    ParserError parseAssignmentStatement(ref Parser parser, ref Stmt stmt)
    {
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = (peek(parser)).Line;
            return error;
        }

        uint line = (previous(parser)).Line;
        string varName = (previous(parser)).Lexeme;

        if (!match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }

        Expr value;
        ParserError valueError = parseExpression(ref parser, ref value);
        if (valueError.Type != ParserErrorType.NONE)
        {
            return valueError;
        }

        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }

        StmtExpr exprStmt;
        exprStmt.Expression = AST.ExprBinary(line, AST.ExprVariable(line, varName), "=", value);

        Stmt resultStmt;
        resultStmt.Line = line;
        resultStmt.Type = StmtType.EXPR_STMT;
        resultStmt.ExprStmt = exprStmt;
        stmt = resultStmt;

        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseWhileStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
        if (!match(ref parser, TokenType.SYM_LPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        Expr condition;
        ParserError conditionError = parseExpression(ref parser, ref condition);
        if (conditionError.Type != ParserErrorType.NONE)
        {
            return conditionError;
        }
        if (!match(ref parser, TokenType.SYM_RPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.EXPECTED_CLOSING_PAREN;
            error.Line = line;
            return error;
        }
        Stmt body;
        ParserError bodyError = parseStatement(ref parser, ref body);
        if (bodyError.Type != ParserErrorType.NONE)
        {
            return bodyError;
        }
        StmtWhile whileStmt;
        whileStmt.Condition = condition;
        whileStmt.Body = body;
        Stmt result;
        result.Line = line;
        result.Type = StmtType.WHILE_STMT;
        result.WhileStmt = whileStmt;
        stmt = result;
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseForStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
        if (!match(ref parser, TokenType.SYM_LPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Stmt initializer;
        if (match(ref parser, TokenType.SYM_SEMICOLON))
        {
            initializer.Line = line;
            initializer.Type = StmtType.NO_OP_STMT;
        }
        else
        {
            ParserError initError = parseAssignmentStatement(ref parser, ref initializer);
            if (initError.Type != ParserErrorType.NONE)
            {
                return initError;
            }
        }
    
        Expr condition;
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError conditionError = parseExpression(ref parser, ref condition);
            if (conditionError.Type != ParserErrorType.NONE)
            {
                return conditionError;
            }
            if (!match(ref parser, TokenType.SYM_SEMICOLON))
            {
                ParserError error;
                error.Type = ParserErrorType.UNEXPECTED_TOKEN;
                error.Line = line;
                return error;
            }
        }
        else
        {
            condition = AST.ExprLiteral(line, "1");
        }
    
        Expr increment;
        if (!match(ref parser, TokenType.SYM_RPAREN))
        {
            ParserError incrementError = parseExpression(ref parser, ref increment);
            if (incrementError.Type != ParserErrorType.NONE)
            {
                return incrementError;
            }
            if (!match(ref parser, TokenType.SYM_RPAREN))
            {
                ParserError error;
                error.Type = ParserErrorType.EXPECTED_CLOSING_PAREN;
                error.Line = line;
                return error;
            }
        }
        else
        {
            increment = AST.ExprLiteral(line, "1");
        }
    
        Stmt body;
        ParserError bodyError = parseStatement(ref parser, ref body);
        if (bodyError.Type != ParserErrorType.NONE)
        {
            return bodyError;
        }
    
        StmtFor forStmt;
        forStmt.Initializer = initializer;
        forStmt.Condition = condition;
        forStmt.Increment = increment;
        forStmt.Body = body;
    
        Stmt result;
        result.Line = line;
        result.Type = StmtType.FOR_STMT;
        result.ForStmt = forStmt;
        stmt = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseReturnStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
    
        StmtReturn returnStmt;
        returnStmt.Expression = AST.ExprLiteral(line, ""); // Placeholder for return expression
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            Expr expr;
            ParserError error = parseExpression(ref parser, ref expr);
            if (error.Type != ParserErrorType.NONE)
            {
                return error;
            }
            returnStmt.Expression = expr;
    
            if (!match(ref parser, TokenType.SYM_SEMICOLON))
            {
                ParserError result;
                result.Type = ParserErrorType.UNEXPECTED_TOKEN;
                result.Line = line;
                return result;
            }
        }
    
        Stmt resultStmt;
        resultStmt.Line = line;
        resultStmt.Type = StmtType.RETURN_STMT;
        resultStmt.ReturnStmt = returnStmt;
        stmt = resultStmt;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseBlockStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
        <Stmt> statements;
    
        while (!match(ref parser, TokenType.SYM_RBRACE) && !isAtEnd(parser))
        {
            Stmt statement;
            ParserError error = parseStatement(ref parser, ref statement);
            if (error.Type != ParserErrorType.NONE) 
            {
                return error;
            }
            statements.Append(statement);
        }
    
        StmtBlock blockStmt;
        blockStmt.Statements = statements;
    
        Stmt resultStmt;
        resultStmt.Line = line;
        resultStmt.Type = StmtType.BLOCK_STMT;
        resultStmt.BlockStmt = blockStmt;
        stmt = resultStmt;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseExpressionStatement(ref Parser parser, ref Stmt stmt)
    {
        Expr expression;
        ParserError error = parseExpression(ref parser, ref expression);
        if (error.Type != ParserErrorType.NONE) 
        {
            return error;
        }
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON)) 
        {
            ParserError result;
            result.Type = ParserErrorType.UNEXPECTED_TOKEN;
            result.Line = expression.Line;
            return result;
        }
    
        StmtExpr exprStmt;
        exprStmt.Expression = expression;
    
        Stmt resultStmt;
        resultStmt.Line = expression.Line;
        resultStmt.Type = StmtType.EXPR_STMT;
        resultStmt.ExprStmt = exprStmt;
        stmt = resultStmt;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = expression.Line;
        return success;
    }
    
    ParserError parseLocalVariableDeclaration(ref Parser parser, ref Stmt stmt)
    {
        uint line = (peek(parser)).Line;
    
        if (!match(ref parser, TokenType.KW_BYTE) && !match(ref parser, TokenType.KW_WORD) &&
            !match(ref parser, TokenType.KW_INT) && !match(ref parser, TokenType.KW_UINT) &&
            !match(ref parser, TokenType.KW_BOOL) && !match(ref parser, TokenType.KW_CHAR))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        
        // Assuming we need to store the type as well, extending the StmtVar to have Type field
        string varType = (previous(parser)).Lexeme;
        
        StmtVar varStmt;
        varStmt.Type = varType;
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        varStmt.Name = (previous(parser)).Lexeme;
        line = (previous(parser)).Line;
    
        Expr initializer;
        if (match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error = parseExpression(ref parser, ref initializer);
            if (error.Type != ParserErrorType.NONE) 
            { 
                return error; 
            }
        }
        else
        {
            initializer = AST.ExprLiteral(line, "0"); // Using "0" as the default value
        }
        varStmt.Initializer = initializer;
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Stmt result;
        result.Line = line;
        result.Type = StmtType.VAR_DECL; // Assuming const and var share the same struct, if not create a new type
        result.VarDecl = varStmt;
        stmt = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseLocalConstDeclaration(ref Parser parser, ref Stmt stmt)
    {
        uint line = (peek(parser)).Line;
    
        if (!match(ref parser, TokenType.KW_CONST))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        if (!match(ref parser, TokenType.KW_BYTE) && !match(ref parser, TokenType.KW_WORD) &&
            !match(ref parser, TokenType.KW_INT) && !match(ref parser, TokenType.KW_UINT) &&
            !match(ref parser, TokenType.KW_BOOL) && !match(ref parser, TokenType.KW_CHAR))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        
        // Assuming we need to store the type as well, extending the StmtVar to have Type field
        string varType = (previous(parser)).Lexeme;
        
        StmtVar constStmt;
        constStmt.Type = varType;
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        constStmt.Name = (previous(parser)).Lexeme;
        line = (previous(parser)).Line;
    
        if (!match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Expr initializer;
        ParserError initError = parseExpression(ref parser, ref initializer);
        if (initError.Type != ParserErrorType.NONE)
        {
            return initError;
        }
        constStmt.Initializer = initializer;
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
    
        Stmt result;
        result.Line = line;
        result.Type = StmtType.VAR_DECL; // Assuming const and var share the same struct, if not create a new type
        result.VarDecl = constStmt;
        stmt = result;
    
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
    
    ParserError parseIfStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
        if (!match(ref parser, TokenType.SYM_LPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.UNEXPECTED_TOKEN;
            error.Line = line;
            return error;
        }
        Expr condition;
        ParserError conditionError = parseExpression(ref parser, ref condition);
        if (conditionError.Type != ParserErrorType.NONE)
        {
            return conditionError;
        }
        if (!match(ref parser, TokenType.SYM_RPAREN))
        {
            ParserError error;
            error.Type = ParserErrorType.EXPECTED_CLOSING_PAREN;
            error.Line = line;
            return error;
        }
        Stmt thenBranch;
        ParserError thenError = parseStatement(ref parser, ref thenBranch);
        if (thenError.Type != ParserErrorType.NONE)
        {
            return thenError;
        }
        Stmt elseBranch;
        if (match(ref parser, TokenType.KW_ELSE))
        {
            ParserError elseError = parseStatement(ref parser, ref elseBranch);
            if (elseError.Type != ParserErrorType.NONE)
            {
                return elseError;
            }
        }
        else
        {
            // Create a minimal no-op statement for the else branch
            Stmt resultStmt;
            resultStmt.Line = line;
            resultStmt.Type = StmtType.EXPR_STMT;
            StmtExpr exprStmt;
            exprStmt.Expression = AST.ExprLiteral(line, "0"); // Using "0" as a no-op placeholder expression
            resultStmt.ExprStmt = exprStmt;
            elseBranch = resultStmt;
        }
        StmtIf ifStmt;
        ifStmt.Condition = condition;
        ifStmt.ThenBranch = thenBranch;
        ifStmt.ElseBranch = elseBranch;
        Stmt result;
        result.Line = line;
        result.Type = StmtType.IF_STMT;
        result.IfStmt = ifStmt;
        stmt = result;
        ParserError success;
        success.Type = ParserErrorType.NONE;
        success.Line = line;
        return success;
    }
}

