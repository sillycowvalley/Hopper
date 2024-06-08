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
        else if (isCompoundAssignmentOperator(token.Type))
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
        while (isBinaryOperator(token.Type))
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
        else if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_TRUE) || (token.Type == TokenType.KW_FALSE))
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
    
    bool isBinaryOperator(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.SYM_PLUS:
            case TokenType.SYM_MINUS:
            case TokenType.SYM_STAR:
            case TokenType.SYM_SLASH:
            case TokenType.SYM_PERCENT:
            case TokenType.SYM_AMP:
            case TokenType.SYM_PIPE:
            case TokenType.SYM_CARET:
            case TokenType.SYM_EQEQ:
            case TokenType.SYM_NEQ:
            case TokenType.SYM_LT:
            case TokenType.SYM_LTE:
            case TokenType.SYM_GT:
            case TokenType.SYM_GTE:
            {
                return true;
            }
            default:
            {
                return false;
            }
        }
        return false; // unreachable but required by Hopper
    }
    
    bool isCompoundAssignmentOperator(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.SYM_PLUSEQ:
            case TokenType.SYM_MINUSEQ:
            case TokenType.SYM_STAREQ:
            case TokenType.SYM_SLASHEQ:
            {
                return true;
            }
            default:
            {
                return false;
            }
        }
        return false; // unreachable but required by Hopper
    }
}

