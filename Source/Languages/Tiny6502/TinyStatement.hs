unit TinyStatement
{
    friend TinyCompile, TinyExpression;
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyExpression"
    
    bool parseIfStatement()
    {
        TinyScanner.Advance(); // Skip 'if'
        Token token = TinyScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'if'");
            return false;
        }
        
        TinyScanner.Advance(); // Skip '('
        string booleanType;
        if (!TinyExpression.parseExpression(ref booleanType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        if (!parseBlock())
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type == TokenType.KW_ELSE)
        {
            TinyScanner.Advance(); // Skip 'else'
            if (!parseBlock())
            {
                return false;
            }
        }
        
        return true;
    }
    
    bool parseWhileStatement()
    {
        TinyScanner.Advance(); // Skip 'while'
        Token token = TinyScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'while'");
            return false;
        }
        
        TinyScanner.Advance(); // Skip '('
        string booleanType;
        if (!TinyExpression.parseExpression(ref booleanType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        if (!parseBlock())
        {
            return false;
        }
        return true;
    }
    
    bool parseForStatement()
    {
        TinyScanner.Advance(); // Skip 'for'
        Token token = TinyScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'for'");
            return false;
        }
        
        // for initialize clause
        TinyScanner.Advance(); // Skip '('
        token = TinyScanner.Current();
        if (TinyToken.IsTypeKeyword(token.Type))
        {
            if (!parseLocalVarDeclaration())
            {
                return false;
            }
        }
        else if (!parseExpressionStatement())
        {
            return false;
        }
        
        // for condition clause
        string booleanType;
        if (!TinyExpression.parseExpression(ref booleanType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        
        // for increment clause
        string incrementType;
        TinyScanner.Advance(); // Skip ';'
        if (!TinyExpression.parseExpression(ref incrementType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after increment, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        if (!parseBlock())
        {
            return false;
        }
        return true;
    }
    
    bool parseReturnStatement()
    {
        TinyScanner.Advance(); // Skip 'return'
        Token token = TinyScanner.Current();
        
        // Check for empty return statement
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            string actualType;
            if (!TinyExpression.parseExpression(ref actualType))
            {
                return false;
            }
            
            token = TinyScanner.Current();
        }
        
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after return statement, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        return true;
    }
    
    bool parseBreakStatement()
    {
        TinyScanner.Advance(); // Skip 'break'
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after 'break'");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        //TinyCode.EmitBreak(); // TODO: Implement in TinyCode
        return true;
    }
    
    bool parseContinueStatement()
    {
        TinyScanner.Advance(); // Skip 'continue'
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after 'continue'");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        //TinyCode.EmitContinue(); // TODO: Implement in TinyCode
        return true;
    }
    
    
    bool parseLocalVarDeclaration()
    {
        string tp;
        if (!TinyCompile.parseType(ref tp))
        {
            return false;
        }
    
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "expected identifier after type, ('" + token.Lexeme + "')");
            return false;
        }
    
        string name = token.Lexeme;
        TinyScanner.Advance(); // Skip identifier
    
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_EQ)
        {
            TinyScanner.Advance(); // Skip '='
    
            if (tp == "func")
            {
                token = TinyScanner.Current();
                if (token.Type != TokenType.IDENTIFIER)
                {
                    Error(token.SourcePath, token.Line, "expected function name after '=', ('" + token.Lexeme + "')");
                    return false;
                }
    
                string functionName = token.Lexeme;
                TinyScanner.Advance(); // Skip function name
    
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_SEMICOLON)
                {
                    Error(token.SourcePath, token.Line, "expected ';' after function pointer assignment, ('" + token.Lexeme + "')");
                    return false;
                }
    
                //TinyCode.DefineLocalFunctionPointer(tp, name, functionName); // TODO: Implement in TinyCode
            }
            else
            {
                string exprType;
                if (!TinyExpression.parseExpression(ref exprType))
                {
                    return false;
                }
                
                if (!IsAutomaticCast(tp, exprType))
                {
                    // TODO
                    //Error(token.SourcePath, token.Line, "type mismatch: expected '" + tp + "', got '" + exprType + "'");
                    //return false;
                }
    
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_SEMICOLON)
                {
                    Error(token.SourcePath, token.Line, "expected ';' after variable assignment, ('" + token.Lexeme + "')");
                    return false;
                }
               
                //TinyCode.DefineLocalVar(tp, name); // TODO: Implement in TinyCode
            }
        }
        else
        {
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after variable declaration, ('" + token.Lexeme + "')");
                return false;
            }
    
            //TinyCode.DefineLocalVar(tp, name); // TODO: Implement in TinyCode
        }
        
        TinyScanner.Advance(); // Skip ';'
    
        return true;
    }
    
    
    bool parseLocalConstDeclaration()
    {
        TinyScanner.Advance(); // Skip 'const'
        
        string constantType;
        if (!TinyCompile.parseType(ref constantType))
        {
            return false;
        }
    
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "expected identifier after type");
            return false;
        }
    
        string name = token.Lexeme;
        TinyScanner.Advance(); // Skip identifier
    
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_EQ)
        {
            Error(token.SourcePath, token.Line, "expected '=' after identifier");
            return false;
        }
    
        TinyScanner.Advance(); // Skip '='
        
        constantType = "const " + constantType;
    
        string expressionType;
        string value;
        if (!TinyConstant.parseConstantExpression(ref value, ref expressionType))
        {
            return false;
        }
        if (!IsAutomaticCast(constantType, expressionType))
        {
            TypeError(constantType, expressionType);
        }
    
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after constant declaration, ('" + token.Lexeme + "')");
            return false;
        }
    
        TinyScanner.Advance(); // Skip ';'
        return TinyConstant.DefineConst(constantType, name, value);
    }
    
    bool parseAssignmentOrExpression()
    {
        Token token = TinyScanner.Current();
        Token nextToken = TinyScanner.Peek(); // Peek at the next token
        if (nextToken.Type == TokenType.SYM_EQ)
        {
            // It's an assignment
            string name = token.Lexeme;
            TinyScanner.Advance(); // Skip identifier
            
            TinyScanner.Advance(); // Skip '='
            
            if (!TinyExpression.parseExpression())
            {
                return false;
            }
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after assignment, ('" + token.Lexeme + "')");
                return false;
            }
            
            TinyScanner.Advance(); // Skip ';'
            
            TinyCode.DefineAssignment(name);
            return true;
        }
        else
        {
            // It's an expression
            if (!TinyExpression.parseExpression())
            {
                return false;
            }
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after expression, ('" + token.Lexeme + "')");
                return false;
            }
            
            TinyScanner.Advance(); // Skip ';'
            return true;
        }
    }
    
    bool parseExpressionStatement()
    {
        string actualType;
        if (!TinyExpression.parseExpression(ref actualType))
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after expression, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        return true;
    }
    bool parseBlock()
    {
        TinyConstant.EnterBlock();
        
        Token token = TinyScanner.Current();
        
        if (token.Type != TokenType.SYM_LBRACE)
        {
            Error(token.SourcePath, token.Line, "expected '{' to start block, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip '{'
        
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RBRACE)
            {
                break; // exit the loop when reaching '}'
            }
            
            if (!parseStatement())
            {
                return false;
            }
        }
        
        TinyScanner.Advance(); // Skip '}'
        
        TinyConstant.LeaveBlock();
        return true;
    }
    
    bool parseStatement()
    {
        Token token = TinyScanner.Current();
        switch (token.Type)
        {
            case TokenType.KW_IF:
            {
                if (!parseIfStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_WHILE:
            {
                if (!parseWhileStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_FOR:
            {
                if (!parseForStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_RETURN:
            {
                if (!parseReturnStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_BREAK:
            {
                if (!parseBreakStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_CONTINUE:
            {
                if (!parseContinueStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_SWITCH:
            {
                if (!parseSwitchStatement())
                {
                    return false;
                }
            }
            case TokenType.KW_BYTE:
            case TokenType.KW_WORD:
            case TokenType.KW_CHAR:
            case TokenType.KW_BOOL:
            case TokenType.KW_INT:
            case TokenType.KW_UINT:
            case TokenType.KW_FUNC:
            {
                if (!parseLocalVarDeclaration())
                {
                    return false;
                }
            }
            case TokenType.KW_CONST:
            {
                if (!parseLocalConstDeclaration())
                {
                    return false;
                }
            }
            case TokenType.IDENTIFIER:
            {
                if (!parseExpressionStatement())
                {
                    return false;
                }
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unexpected token in statement: " + TinyToken.ToString(token.Type));
                return false;
            }
        }
        return true;
    }

    bool parseSwitchStatement()
    {
        TinyScanner.Advance(); // Skip 'switch'
        Token token = TinyScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'switch', ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip '('
        
        string switchType;
        if (!TinyExpression.parseExpression(ref switchType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after switch expression, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_LBRACE)
        {
            Error(token.SourcePath, token.Line, "expected '{' to start switch block, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip '{'
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RBRACE)
            {
                break; // exit the loop when reaching '}'
            }
            
            if (token.Type == TokenType.KW_CASE)
            {
                if (!parseCaseStatement(switchType))
                {
                    return false;
                }
            }
            else if (token.Type == TokenType.KW_DEFAULT)
            {
                if (!parseDefaultStatement())
                {
                    return false;
                }
            }
            else if (!parseStatement())
            {
                return false;
            }
        }
        
        TinyScanner.Advance(); // Skip '}'
        return true;
    }

    bool parseCaseStatement(string switchType)
    {
        TinyScanner.Advance(); // Skip 'case'
        
        string caseType;
        if (!TinyExpression.parseExpression(ref caseType))
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_COLON)
        {
            Error(token.SourcePath, token.Line, "expected ':' after case value, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ':'
        return true;
    }

    bool parseDefaultStatement()
    {
        TinyScanner.Advance(); // Skip 'default'
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_COLON)
        {
            Error(token.SourcePath, token.Line, "expected ':' after default, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ':'
        return true;
    }
}

