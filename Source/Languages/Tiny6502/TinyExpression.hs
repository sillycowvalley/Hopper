unit TinyExpression
{
    friend TinyCompile, TinyStatement;
    
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    
    bool parseExpression()
    {
        return parseAssignmentExpression();
    }
    
    bool parseAssignmentExpression()
    {
        if (!parseBinaryExpression())
        {
            return false;
        }
    
        Token token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_EQ)
        {
            TinyScanner.Advance(); // Skip '='
    
            if (!parseExpression())
            {
                return false;
            }
        }
        else if (TinyToken.IsCompoundAssignmentOperator(token.Type))
        {
            TinyScanner.Advance(); // Skip compound assignment operator
    
            if (!parseExpression())
            {
                return false;
            }
        }
    
        return true;
    }
    
    bool parseBinaryExpression()
    {
        if (!parseUnaryExpression())
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        while (TinyToken.IsBinaryOperator(token.Type))
        {
            TinyScanner.Advance(); // Skip operator
            if (!parseUnaryExpression())
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }
    
    bool parseUnaryExpression()
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            TinyScanner.Advance(); // Skip unary operator
            if (!parsePrimaryExpression())
            {
                return false;
            }
        }
        else
        {
            if (!parsePrimaryExpression())
            {
                return false;
            }
        }
        return true;
    }
    
    
    bool parsePrimaryExpression()
    {
        Token token = TinyScanner.Current();
        if (token.Type == TokenType.IDENTIFIER)
        {
            TinyScanner.Advance(); // Skip identifier
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_LPAREN)
            {
                TinyScanner.Advance(); // Skip '('
                if (!parseArgumentList())
                {
                    return false;
                }
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RPAREN)
                {
                    Error(token.SourcePath, token.Line, "expected ')' after argument list, ('" + token.Lexeme + "')");
                    return false;
                }
                TinyScanner.Advance(); // Skip ')'
            }
            else if (token.Type == TokenType.SYM_LBRACKET)
            {
                TinyScanner.Advance(); // Skip '['
                if (!parseExpression())
                {
                    return false;
                }
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after array index");
                    return false;
                }
                TinyScanner.Advance(); // Skip ']'
            }
            else if (token.Type == TokenType.KW_AS)
            {
                TinyScanner.Advance(); // Skip 'as'
                string castType;
                if (!TinyCompile.parseType(ref castType))
                {
                    return false;
                }
            }
            else if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
            {
                TinyScanner.Advance(); // Skip '++' or '--'
            }
        }
        else if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
        {
            TinyScanner.Advance(); // Skip '++' or '--'
            token = TinyScanner.Current();
            if (token.Type == TokenType.IDENTIFIER)
            {
                TinyScanner.Advance(); // Skip identifier
            }
            else
            {
                Error(token.SourcePath, token.Line, "expected identifier after '++' or '--'");
                return false;
            }
        }
        else if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_TRUE) || (token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_NULL))
        {
            TinyScanner.Advance(); // Skip literal or boolean literal
        }
        else if (token.Type == TokenType.SYM_LPAREN)
        {
            TinyScanner.Advance(); // Skip '('
            if (!parseExpression())
            {
                return false;
            }
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_RPAREN)
            {
                Error(token.SourcePath, token.Line, "expected ')' after expression, ('" + token.Lexeme + "')");
                return false;
            }
            TinyScanner.Advance(); // Skip ')'
        }
        else if (token.Type == TokenType.KW_FUNC)
        {
            TinyScanner.Advance(); // Skip 'func'
            token = TinyScanner.Current();
            if (token.Type == TokenType.IDENTIFIER)
            {
                TinyScanner.Advance(); // Skip identifier
            }
            else
            {
                Error(token.SourcePath, token.Line, "expected identifier after 'func'");
                return false;
            }
        }
        else
        {
            Error(token.SourcePath, token.Line, "unexpected token in expression: " + TinyToken.ToString(token.Type));
            return false;
        }
        return true;
    }
    
    
    bool parseArgumentList()
    {
        Token token;
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RPAREN)
            {
                break; // exit the loop when reaching ')'
            }
            if (!parseExpression())
            {
                return false;
            }
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_COMMA)
            {
                TinyScanner.Advance(); // Skip ','
            }
            else if (token.Type != TokenType.SYM_RPAREN)
            {
                Error(token.SourcePath, token.Line, "expected ',' or ')' in argument list");
                return false;
            }
        }
        return true; // success
    }
}
    
