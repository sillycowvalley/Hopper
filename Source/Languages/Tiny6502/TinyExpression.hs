unit TinyExpression
{
    friend TinyCompile, TinyStatement;
    
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyConstant"
    
    bool parseExpression(ref string actualType)
    {
        return parseAssignmentExpression(ref actualType);
    }
    
    bool parseAssignmentExpression(ref string actualType)
    {
        if (!parseBinaryExpression(ref actualType))
        {
            return false;
        }
    
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_EQ) || IsCompoundAssignmentOperator(token.Type))
        {
            TinyScanner.Advance(); // Skip '=' or compound assignment operator
    
            string rhsType;
            if (!parseExpression(ref rhsType))
            {
                return false;
            }
            
            if (!IsTypeCompatible(actualType, rhsType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
        }
        return true;
    }
    
    bool parseBinaryExpression(ref string actualType)
    {
        if (!parseUnaryExpression(ref actualType))
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        while (TinyToken.IsBinaryOperator(token.Type))
        {
            TinyScanner.Advance(); // Skip operator
            string rhsType;
            if (!parseUnaryExpression(ref rhsType))
            {
                return false;
            }
            if (!IsTypeCompatible(actualType, rhsType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }
    
    bool parseUnaryExpression(ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            TinyScanner.Advance(); // Skip unary operator
            if (!parsePrimaryExpression(ref actualType))
            {
                return false;
            }
        }
        else
        {
            if (!parsePrimaryExpression(ref actualType))
            {
                return false;
            }
        }
        return true;
    }
    
    
    bool parsePrimaryExpression(ref string actualType)
    {
        Token token = TinyScanner.Current();
        if (token.Type == TokenType.IDENTIFIER)
        {
            string name = token.Lexeme;
            TinyScanner.Advance(); // Skip identifier
            
            string constantValue;
            if (GetConst(name, ref actualType, ref constantValue))
            {
                return true;
            }
            
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
                if (!parseExpression(ref actualType))
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
        else if (token.Type == TokenType.LIT_NUMBER)
        {
            string value;
            if (!TinyConstant.parseConstantPrimary(ref value, ref actualType))
            {
                return false;
            }
        }
        else if ((token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_TRUE) || (token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_NULL))
        {
            TinyScanner.Advance(); // Skip literal or boolean literal
        }
        else if (token.Type == TokenType.SYM_LPAREN)
        {
            TinyScanner.Advance(); // Skip '('
            if (!parseExpression(ref actualType))
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
            string argType;
            if (!parseExpression(ref argType))
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
    
