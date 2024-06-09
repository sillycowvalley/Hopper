unit TinyExpression
{
    friend TinyCompile, TinyStatement;
    
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyConstant"
    uses "TinyType"
    uses "TinySymbols"
    
    bool parseExpression(ref string actualType)
    {
        if (!parseAssignmentExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();        
        if (token.Type == TokenType.KW_AS)
        {
            TinyCode.PadOut("// as TODO", 0);
            // cast
            TinyScanner.Advance(); // Skip 'as'
            if (!TinyCompile.parseType(ref actualType))
            {
                return false;
            }
        }
        return true;   
    }

    bool parseAssignmentExpression(ref string actualType)
    {
        if (!parseLogicalOrExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_EQ) || IsCompoundAssignmentOperator(token.Type))
        {
            Token nameToken = TinyScanner.Previous();
            string name = nameToken.Lexeme;
            string op = token.Lexeme;
            TinyCode.PadOut("// " + name + " " + op, 0);
            
        
            TinyScanner.Advance(); // Skip '=' or compound assignment operator

            string rhsType;
            if (!parseExpression(ref rhsType))
            {
                return false;
            }
            if (!IsAutomaticCast(actualType, rhsType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            
            int    offset;
            bool   isGlobal;
            string variableType;
            if (!GetVariable(name, ref variableType, ref offset, ref isGlobal))
            {
                Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
            }
            TinyCode.PopVariable(name, offset, IsByteType(variableType), isGlobal);
        }
        
        
        return true;
    }

    bool parseLogicalOrExpression(ref string actualType)
    {
        if (!parseLogicalAndExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_PIPEPIPE)
        {
            TinyScanner.Advance(); // Skip '||'
            string rhsType;
            if (!parseLogicalAndExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchBoolTypes(rhsType, actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            actualType = "bool";
            token = TinyScanner.Current();
            TinyCode.PadOut("// || TODO", 0);
        }
        return true;
    }

    bool parseLogicalAndExpression(ref string actualType)
    {
        if (!parseBitwiseOrExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_AMPAMP)
        {
            TinyScanner.Advance(); // Skip '&&'
            string rhsType;
            if (!parseBitwiseOrExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchBoolTypes(rhsType, actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            actualType = "bool";
            token = TinyScanner.Current();
            TinyCode.PadOut("// && TODO", 0);
        }
        return true;
    }

    bool parseBitwiseOrExpression(ref string actualType)
    {
        if (!parseBitwiseXorExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_PIPE)
        {
            TinyScanner.Advance(); // Skip '|'
            string rhsType;
            if (!parseBitwiseXorExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// | TODO", 0);
        }
        return true;
    }

    bool parseBitwiseXorExpression(ref string actualType)
    {
        if (!parseBitwiseAndExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_CARET)
        {
            TinyScanner.Advance(); // Skip '^'
            string rhsType;
            if (!parseBitwiseAndExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// ^ TODO", 0);
        }
        return true;
    }

    bool parseBitwiseAndExpression(ref string actualType)
    {
        if (!parseEqualityExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_AMP)
        {
            TinyScanner.Advance(); // Skip '&'
            string rhsType;
            if (!parseEqualityExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// & TODO", 0);
        }
        return true;
    }

    bool parseEqualityExpression(ref string actualType)
    {
        if (!parseRelationalExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_EQEQ) || (token.Type == TokenType.SYM_NEQ))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip '==' or '!='
            string rhsType;
            if (!parseRelationalExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            actualType = "bool";
            token = TinyScanner.Current();
            TinyCode.PadOut("// " + op + " TODO", 0); 
        }
        return true;
    }

    bool parseRelationalExpression(ref string actualType)
    {
        if (!parseShiftExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_LT) || (token.Type == TokenType.SYM_LTE) || (token.Type == TokenType.SYM_GT) || (token.Type == TokenType.SYM_GTE))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip '<', '<=', '>', '>='
            string rhsType;
            if (!parseShiftExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            actualType = "bool";
            token = TinyScanner.Current();
            TinyCode.PadOut("// " + op + " TODO", 0); 
        }
        return true;
    }

    bool parseShiftExpression(ref string actualType)
    {
        if (!parseAdditiveExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_LSHIFT) || (token.Type == TokenType.SYM_RSHIFT))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip '<<' or '>>'
            string rhsType;
            if (!parseAdditiveExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// " + op + " TODO", 0); 
        }
        return true;
    }

    bool parseAdditiveExpression(ref string actualType)
    {
        if (!parseMultiplicativeExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_MINUS))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip '+' or '-'
            string rhsType;
            if (!parseMultiplicativeExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// " + op + " TODO", 0); 
        }
        return true;
    }

    bool parseMultiplicativeExpression(ref string actualType)
    {
        if (!parseUnaryExpression(ref actualType))
        {
            return false;
        }

        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_STAR) || (token.Type == TokenType.SYM_SLASH) || (token.Type == TokenType.SYM_PERCENT))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip '*', '/', '%'
            string rhsType;
            if (!parseUnaryExpression(ref rhsType))
            {
                return false;
            }
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            token = TinyScanner.Current();
            TinyCode.PadOut("// " + op + " TODO", 0); 
        }
        return true;
    }

    bool parseUnaryExpression(ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip unary operator
            if (!parsePrimaryExpression(ref actualType))
            {
                return false;
            }
            TinyCode.PadOut("// " + op + " TODO", 0); 
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
                if (actualType.EndsWith(']'))
                {
                    // const char[] palette = ".,'~=+:;*%&$OXB#@ ";
                    token = TinyScanner.Current();
                    if (token.Type == TokenType.SYM_LBRACKET)
                    {
                        // pick out an array member
                        TinyScanner.Advance(); // Skip '['
                        string indexType;
                        if (!parseExpression(ref indexType))
                        {
                            return false;
                        }
                        if (!IsIndexType(indexType))
                        {
                            Error(token.SourcePath, token.Line, "integral index type expected");
                            return false;
                        }
                        actualType = GetArrayMemberType(actualType);
                        token = TinyScanner.Current();
                        if (token.Type != TokenType.SYM_RBRACKET)
                        {
                            Error(token.SourcePath, token.Line, "expected ']' after array index");
                            return false;
                        }
                        TinyScanner.Advance(); // Skip ']'
                    }
                    else
                    {
                        // return the entire array
                    }
                }
                return true;
            }
            
            
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_LPAREN)
            {
                // function call
                if (!GetFunction(name, ref actualType))
                {
                    int  offset;
                    bool isGlobal;
                    if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                    {
                        Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                    }
                    if (actualType != "func")
                    {
                        Error(token.SourcePath, token.Line, "invalid use of '" + name + "'");
                    }
                    // untyped func pointer, arbitrarily pick "word" as return type
                    actualType = "word";
                }
                
                TinyScanner.Advance(); // Skip '('
                if (!parseArgumentList(name))
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
                // array accessor
                int  offset;
                bool isGlobal;
                if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                {
                    Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                    return false;
                }
                
                TinyScanner.Advance(); // Skip '['
                string indexType;
                if (!parseExpression(ref indexType))
                {
                    return false;
                }
                if (!IsIndexType(indexType))
                {
                    Error(token.SourcePath, token.Line, "integral index type expected");
                    return false;
                }
                actualType = GetArrayMemberType(actualType);
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after array index");
                    return false;
                }
                TinyScanner.Advance(); // Skip ']'
            }
            else
            {
                // variable
                int  offset;
                bool isGlobal;
                if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                {
                    Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                    return false;
                }
                if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
                {
                    // i++ or i-- : value before --/++ is used in the expression
                    TinyCode.Map(token);
                    TinyScanner.Advance(); // Skip '++' or '--'
                    TinyCode.PostIncrement(name, offset, IsByteType(actualType), token.Type == TokenType.SYM_PLUSPLUS, isGlobal);
                }
                else
                {
                    TinyCode.PushVariable(name, offset, IsByteType(actualType), isGlobal);
                }
            }
        }
        else if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
        {
            // ++i or --i  : value after --/++ is used in the expression
            bool inc = token.Type == TokenType.SYM_PLUSPLUS;
            TinyScanner.Advance(); // Skip '++' or '--'
            token = TinyScanner.Current();
            if (token.Type == TokenType.IDENTIFIER)
            {
                string name = token.Lexeme;
                TinyCode.Map(token);
                TinyScanner.Advance(); // Skip identifier
                int  offset;
                bool isGlobal;
                if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                {
                    Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                    return false;
                }
                TinyCode.PreIncrement(name, offset, IsByteType(actualType), inc, isGlobal);
            }
            else
            {
                Error(token.SourcePath, token.Line, "expected identifier after '++' or '--'");
                return false;
            }
        }
        else if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_TRUE))
        {
            string value;
            if (!TinyConstant.parseConstantPrimary(ref value, ref actualType))
            {
                return false;
            }
        }
        /*
        else if (token.Type == TokenType.LIT_CHAR)
        {
            actualType = "char";
            TinyScanner.Advance(); // Skip literal    
        }
        else if ((token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_TRUE))
        {
            actualType = "bool";
            TinyScanner.Advance(); // Skip literal    
        }
        */
        else if ((token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.KW_NULL))
        {
            // TODO : actualType
            TinyScanner.Advance(); // Skip literal
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
    
    bool parseArgumentList(string functionName)
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
                Error(token.SourcePath, token.Line, "expected ',' or ')' in argument list, ('" + token.Lexeme + "')");
                return false;
            }
        }
        return true; // success
    }
}

