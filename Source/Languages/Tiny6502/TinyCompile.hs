unit TinyCompile
{
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    
    bool Compile()
    {
        bool success;
        success = parseProgram();
        return success;
    }
    
    bool parseProgram()
    {
        bool success = true;
        Token token;
        
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.EOF)
            {
                break; // exit the loop when reaching EOF
            }
            
            switch (token.Type)
            {
                case TokenType.KW_CONST:
                {
                    success = parseConst();
                }
                case TokenType.KW_BYTE:
                case TokenType.KW_WORD:
                case TokenType.KW_CHAR:
                case TokenType.KW_BOOL:
                case TokenType.KW_INT:
                case TokenType.KW_UINT:
                {
                    success = parseGlobalVar();
                }
                case TokenType.KW_FUNC:
                {
                    success = parseFunction();
                }
                default:
                {
                    Error(token.SourcePath, token.Line, "unexpected token: " + TinyToken.ToString(token.Type) + "('" + token.Lexeme + "')");
                    success = false;
                }
            }
            
            if (!success)
            {
                break; // exit the loop on error
            }
        }
        
        return success;
    }
    
    bool parseConst()
    {
        TinyScanner.Advance(); // Skip 'const'
        Token token = TinyScanner.Current();
        
        if ((token.Type != TokenType.KW_BYTE) && (token.Type != TokenType.KW_WORD) && (token.Type != TokenType.KW_CHAR) && (token.Type != TokenType.KW_BOOL) && (token.Type != TokenType.KW_INT) && (token.Type != TokenType.KW_UINT))
        {
            Error(token.SourcePath, token.Line, "expected type after 'const'");
            return false;
        }
        
        string tp = token.Lexeme;
        TinyScanner.Advance(); // Skip type
        
        token = TinyScanner.Current();
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
        
        token = TinyScanner.Current();
        if ((token.Type != TokenType.LIT_NUMBER) && (token.Type != TokenType.LIT_CHAR))
        {
            Error(token.SourcePath, token.Line, "expected literal value after '='");
            return false;
        }
        
        string value = token.Lexeme;
        TinyScanner.Advance(); // Skip value
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after constant declaration");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        //TinyCode.DefineConst(tp, name, value);
        return true;
    }
    
    bool parseGlobalVar()
    {
        Token token = TinyScanner.Current();
        string tp = token.Lexeme;
        TinyScanner.Advance(); // Skip type
        
        token = TinyScanner.Current();
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
        
        token = TinyScanner.Current();
        if ((token.Type != TokenType.LIT_NUMBER) && (token.Type != TokenType.LIT_CHAR))
        {
            Error(token.SourcePath, token.Line, "expected literal value after '='");
            return false;
        }
        
        string value = token.Lexeme;
        TinyScanner.Advance(); // Skip value
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after variable declaration");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        TinyCode.DefineGlobalVar(tp, name, value);
        return true;
    }
    
    bool parseFunction()
    {
        TinyScanner.Advance(); // Skip 'func'
        Token token = TinyScanner.Current();
    
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "expected identifier after 'func'");
            return false;
        }
    
        string name = token.Lexeme;
        TinyScanner.Advance(); // Skip identifier
    
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after function name");
            return false;
        }
    
        TinyScanner.Advance(); // Skip '('
        if (!parseParameterList())
        {
            return false;
        }
    
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after parameter list");
            return false;
        }
    
        TinyScanner.Advance(); // Skip ')'
    
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_SEMICOLON)
        {
            // This is a forward declaration
            TinyScanner.Advance(); // Skip ';'
            //TinyCode.DefineForwardFunction(name);
            return true;
        }
        else if (token.Type == TokenType.SYM_LBRACE)
        {
            // This is an actual function definition
            TinyScanner.Advance(); // Skip '{'
            if (!parseFunctionBody())
            {
                return false;
            }
    
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_RBRACE)
            {
                Error(token.SourcePath, token.Line, "expected '}' to end function body");
                return false;
            }
    
            TinyScanner.Advance(); // Skip '}'
            TinyCode.DefineFunction(name);
            return true;
        }
        else
        {
            Error(token.SourcePath, token.Line, "expected '{' to start function body or ';' for forward declaration");
            return false;
        }
    }
    
    bool parseParameterList()
    {
        Token token;
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RPAREN)
            {
                break; // exit the loop when reaching ')'
            }
            
            bool isConst = false;
            if (token.Type == TokenType.KW_CONST)
            {
                isConst = true;
                TinyScanner.Advance(); // Skip 'const'
                token = TinyScanner.Current();
            }
            
            if ((token.Type != TokenType.KW_BYTE) && (token.Type != TokenType.KW_WORD) && (token.Type != TokenType.KW_CHAR) && (token.Type != TokenType.KW_BOOL) && (token.Type != TokenType.KW_INT) && (token.Type != TokenType.KW_UINT))
            {
                Error(token.SourcePath, token.Line, "expected parameter type ('" + token.Lexeme + "')");
                return false;
            }
            
            string tp = token.Lexeme;
            TinyScanner.Advance(); // Skip type
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected parameter name ('" + token.Lexeme + "')");
                return false;
            }
            
            string name = token.Lexeme;
            TinyScanner.Advance(); // Skip name
            
            //TinyCode.DefineParameter(tp, name, isConst); // TODO
            
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_COMMA)
            {
                TinyScanner.Advance(); // Skip ','
            }
            else if (token.Type != TokenType.SYM_RPAREN)
            {
                Error(token.SourcePath, token.Line, "expected ',' or ')' after parameter");
                return false;
            }
        }
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
            case TokenType.IDENTIFIER:
            {
                return parseExpressionStatement();
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
        if (!parseExpression())
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
        return parseBlock();
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
        if (!parseExpression())
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
        return parseBlock();
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
        if (!parseExpression())
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
        if (!parseExpression())
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
        return parseBlock();
    }
    
    bool parseReturnStatement()
    {
        TinyScanner.Advance(); // Skip 'return'
        if (!parseExpression())
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
    
    bool parseExpressionStatement()
    {
        if (!parseExpression())
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
    
    bool parseExpression()
    {
        // Placeholder for parsing expressions
        TinyScanner.Advance();
        return true;
    }
}

