unit TinyStatement
{
    friend TinyCompile, TinyExpression;

    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyExpression"
    
    bool parseStatement()
    {
        Token token = TinyScanner.Current();
        switch (token.Type)
        {
            case TokenType.KW_IF:
            {
                return parseIfStatement();
            }
            case TokenType.KW_WHILE:
            {
                return parseWhileStatement();
            }
            case TokenType.KW_FOR:
            {
                return parseForStatement();
            }
            case TokenType.KW_RETURN:
            {
                return parseReturnStatement();
            }
            case TokenType.KW_BYTE:
            case TokenType.KW_WORD:
            case TokenType.KW_CHAR:
            case TokenType.KW_BOOL:
            case TokenType.KW_INT:
            case TokenType.KW_UINT:
            {
                return parseLocalVarDeclaration();
            }
            case TokenType.IDENTIFIER:
            {
                return parseAssignmentOrExpression();
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unexpected token in statement: " + TinyToken.ToString(token.Type));
                return false;
            }
        }
        Error(token.SourcePath, token.Line, "unexpected token in statement: " + TinyToken.ToString(token.Type));
        return false;
    }

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
        if (!TinyExpression.parseExpression())
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
        if (!TinyExpression.parseExpression())
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
        
        TinyScanner.Advance(); // Skip '('
        if (!parseExpressionStatement())
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after initialization");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        if (!TinyExpression.parseExpression())
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after condition");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        if (!TinyExpression.parseExpression())
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after increment");
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
        if (!TinyExpression.parseExpression())
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after return statement");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
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
        
        if (!TinyExpression.parseExpression())
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after variable declaration");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        TinyCode.DefineLocalVar(tp, name);
        return true;
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
                Error(token.SourcePath, token.Line, "expected ';' after assignment");
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
                Error(token.SourcePath, token.Line, "expected ';' after expression");
                return false;
            }
            
            TinyScanner.Advance(); // Skip ';'
            return true;
        }
    }

    bool parseExpressionStatement()
    {
        if (!TinyExpression.parseExpression())
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after expression");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        return true;
    }
    
    bool parseBlock()
    {
        Token token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_LBRACE)
        {
            Error(token.SourcePath, token.Line, "expected '{' to start block");
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
        return true;
    }

    bool parseFunctionBody()
    {
        Token token;
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RBRACE)
            {
                break; // exit the loop when reaching '}'
            }
            
            // Parse statements
            if (!parseStatement())
            {
                return false;
            }
        }
        return true;
    }
}

