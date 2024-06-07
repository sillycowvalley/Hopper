unit Parser
{
    uses "AST"
    uses "TinyToken"
    uses "Lexer"

    enum ParserError
    {
        NONE,
        UNEXPECTED_TOKEN,
        EXPECTED_EXPRESSION,
        EXPECTED_CLOSING_PAREN
    }

    string ToString(ParserError error)
    {
        switch (error)
        {
            case ParserError.NONE: { return "No error"; }
            case ParserError.UNEXPECTED_TOKEN: { return "Unexpected token"; }
            case ParserError.EXPECTED_EXPRESSION: { return "Expected expression"; }
            case ParserError.EXPECTED_CLOSING_PAREN: { return "Expected closing parenthesis"; }
        }
        return "Unknown error";
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
        Token result = (parser.Tokens).GetItem(parser.Current-1);
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
        return ParserError.NONE;
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
        if (match(ref parser, TokenType.IDENTIFIER))
        {
            return AST.ExprVariable((previous(parser)).Line, (previous(parser)).Lexeme);
        }
        if (match(ref parser, TokenType.SYM_LPAREN))
        {
            Expr expr;
            ParserError error = parseExpression(ref parser, ref expr);
            if (error != ParserError.NONE) 
            {
                IO.WriteLn("Expected expression");
                Diagnostics.Die(1);
            }
            if (!match(ref parser, TokenType.SYM_RPAREN))
            {
                IO.WriteLn("Expected ')'");
                Diagnostics.Die(1);
            }
            return expr;
        }
        IO.WriteLn("Expected expression");
        Diagnostics.Die(1);
        return AST.ExprLiteral(0, "0");  // Using "0" as the default placeholder value
    }
    

    ParserError parseDeclaration(ref Parser parser, ref Decl decl)
    {
        if (match(ref parser, TokenType.KW_FUNC))
        {
            return parseFunctionDeclaration(ref parser, ref decl);
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
            return ParserError.UNEXPECTED_TOKEN;
        }
        varStmt.Name = (previous(parser)).Lexeme;
        uint line = (previous(parser)).Line;
    
        Expr initializer;
        if (match(ref parser, TokenType.SYM_EQ))
        {
            ParserError error = parseExpression(ref parser, ref initializer);
            if (error != ParserError.NONE) { return error; }
        }
        else
        {
            initializer = AST.ExprLiteral(line, "0"); // Using "0" as the default value
        }
        varStmt.Initializer = initializer;
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON))
        {
            return ParserError.UNEXPECTED_TOKEN;
        }
    
        Decl result;
        result.Line = line;
        result.Type = DeclType.VAR_DECL;
        result.VarDecl = varStmt;
        decl = result;
        return ParserError.NONE;
    }
    
    
    ParserError parseFunctionDeclaration(ref Parser parser, ref Decl decl)
    {
        DeclFunc funcDecl;
        if (!match(ref parser, TokenType.IDENTIFIER))
        {
            return ParserError.UNEXPECTED_TOKEN;
        }
        funcDecl.Name = (previous(parser)).Lexeme;
        uint line = (previous(parser)).Line;
    
        if (!match(ref parser, TokenType.SYM_LPAREN))
        {
            return ParserError.UNEXPECTED_TOKEN;
        }
    
        <string> parameters;
        if (!match(ref parser, TokenType.SYM_RPAREN))
        {
            loop
            {
                if (!match(ref parser, TokenType.IDENTIFIER))
                {
                    return ParserError.UNEXPECTED_TOKEN;
                }
                parameters.Append((previous(parser)).Lexeme);
                if (!match(ref parser, TokenType.SYM_COMMA))
                {
                    if (!match(ref parser, TokenType.SYM_RPAREN))
                    {
                        return ParserError.UNEXPECTED_TOKEN;
                    }
                    break;
                }
            }
        }
    
        funcDecl.Params = parameters;
    
        <Stmt> body;
        if (!match(ref parser, TokenType.SYM_LBRACE))
        {
            return ParserError.UNEXPECTED_TOKEN;
        }
        while (!match(ref parser, TokenType.SYM_RBRACE) && !isAtEnd(parser))
        {
            Stmt statement;
            ParserError error = parseStatement(ref parser, ref statement);
            if (error != ParserError.NONE) { return error; }
            body.Append(statement);
        }
    
        funcDecl.Body = body;
        Decl result;
        result.Line = line;
        result.Type = DeclType.FUNC_DECL;
        result.FuncDecl = funcDecl;
        decl = result;
        return ParserError.NONE;
    }

    ParserError ParseProgram(ref Parser parser, ref Program prog)
    {
        <Decl> declarations;
        ParserError error;
        loop
        {
            Decl decl;
            error = parseDeclaration(ref parser, ref decl);
            if (error != ParserError.NONE) { return error; }
            declarations.Append(decl);
            if (isAtEnd(parser)) { break; }
        }
        prog.Declarations = declarations;
        return ParserError.NONE;
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
            return parseExpressionStatement(ref parser, ref stmt);
        }
    }
    
    ParserError parseIfStatement(ref Parser parser, ref Stmt stmt)
    {
        // Implement this function as needed
        Diagnostics.Die(0x0A);
        return ParserError.NONE;
    }
    
    ParserError parseWhileStatement(ref Parser parser, ref Stmt stmt)
    {
        // Implement this function as needed
        Diagnostics.Die(0x0A);
        return ParserError.NONE;
    }
    
    ParserError parseForStatement(ref Parser parser, ref Stmt stmt)
    {
        // Implement this function as needed
        Diagnostics.Die(0x0A);
        return ParserError.NONE;
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
            if (error != ParserError.NONE)
            {
                return error;
            }
            returnStmt.Expression = expr;
    
            if (!match(ref parser, TokenType.SYM_SEMICOLON))
            {
                return ParserError.UNEXPECTED_TOKEN;
            }
        }
    
        Stmt result;
        result.Line = line;
        result.Type = StmtType.RETURN_STMT;
        result.ReturnStmt = returnStmt;
        stmt = result;
        return ParserError.NONE;
    }
    
    ParserError parseBlockStatement(ref Parser parser, ref Stmt stmt)
    {
        uint line = (previous(parser)).Line;
        <Stmt> statements;
    
        while (!match(ref parser, TokenType.SYM_RBRACE) && !isAtEnd(parser))
        {
            Stmt statement;
            ParserError error = parseStatement(ref parser, ref statement);
            if (error != ParserError.NONE) 
            {
                return error;
            }
            statements.Append(statement);
        }
    
        StmtBlock blockStmt;
        blockStmt.Statements = statements;
    
        Stmt result;
        result.Line = line;
        result.Type = StmtType.BLOCK_STMT;
        result.BlockStmt = blockStmt;
        stmt = result;
        return ParserError.NONE;
    }
    
    ParserError parseExpressionStatement(ref Parser parser, ref Stmt stmt)
    {
        Expr expression;
        ParserError error = parseExpression(ref parser, ref expression);
        if (error != ParserError.NONE) 
        {
            return error;
        }
    
        if (!match(ref parser, TokenType.SYM_SEMICOLON)) 
        {
            return ParserError.UNEXPECTED_TOKEN;
        }
    
        StmtExpr exprStmt;
        exprStmt.Expression = expression;
    
        Stmt result;
        result.Line = expression.Line;
        result.Type = StmtType.EXPR_STMT;
        result.ExprStmt = exprStmt;
        stmt = result;
        return ParserError.NONE;
    }
        
    
}

