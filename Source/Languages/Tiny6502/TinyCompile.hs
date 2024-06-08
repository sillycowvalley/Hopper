unit TinyCompile
{
    friend TinyStatement, TinyExpression;
    
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyStatement"
    uses "TinyExpression"
    
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
        
        string tp;
        if (!parseType(ref tp))
        {
            return false;
        }
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
        string tp;
        if (!parseType(ref tp))
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
        if (token.Type == TokenType.SYM_EQ)
        {
            TinyScanner.Advance(); // Skip '='
            
            if (!TinyExpression.parseExpression())
            {
                return false;
            }
        }
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after variable declaration");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        TinyCode.DefineGlobalVar(tp, name, "");
        return true;
    }
    
    bool parseFunction()
    {
        TinyScanner.Advance(); // Skip 'func'
        Token token = TinyScanner.Current();

        string returnType;
        if (!parseType(ref returnType))
        {
            return false;
        }

        token = TinyScanner.Current();
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
            if (!TinyStatement.parseFunctionBody())
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
            
            string tp;
            if (!parseType(ref tp))
            {
                return false;
            }
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected parameter name");
                return false;
            }
            
            string name = token.Lexeme;
            TinyScanner.Advance(); // Skip name
            
            //TinyCode.DefineParameter(tp, name); // TODO
            
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
        return true; // success
    }
    
    bool parseType(ref string tp)
    {
        Token token = TinyScanner.Current();
    
        if (token.Type == TokenType.KW_CONST)
        {
            TinyScanner.Advance(); // Skip 'const': TODO
            token = TinyScanner.Current();
        }
    
        if ((token.Type != TokenType.KW_BYTE) && (token.Type != TokenType.KW_WORD) && (token.Type != TokenType.KW_CHAR) && (token.Type != TokenType.KW_BOOL) && (token.Type != TokenType.KW_INT) && (token.Type != TokenType.KW_UINT))
        {
            Error(token.SourcePath, token.Line, "expected type");
            return false;
        }
    
        tp = token.Lexeme;
        TinyScanner.Advance(); // Skip type
    
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_LBRACKET)
        {
            TinyScanner.Advance(); // Skip '['
            token = TinyScanner.Current();
            if (token.Type == TokenType.LIT_NUMBER)
            {
                tp += "[" + token.Lexeme + "]";
                TinyScanner.Advance(); // Skip number
            }
            else
            {
                tp += "[]";
            }
    
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_RBRACKET)
            {
                Error(token.SourcePath, token.Line, "expected ']'");
                return false;
            }
            TinyScanner.Advance(); // Skip ']'
        }
        return true;
    }
}

