unit TinyStatement
{
    friend TinyCompile, TinyExpression;
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyExpression"
    uses "TinyType"
    uses "TinySymbols"
    
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
        if (!IsAutomaticCast("bool", booleanType, false, false))
        {
            PrintLn("HERE");
            TypeError("bool", booleanType);
            return false;    
        }
        
        TinyCode.If("if");
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        if (!parseBlock(true))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type == TokenType.KW_ELSE)
        {
            TinyCode.Else();
            TinyScanner.Advance(); // Skip 'else'
            if (!parseBlock(true))
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
        
        TinyCode.Loop("while");
        
        TinyScanner.Advance(); // Skip '('
        string booleanType;
        if (!TinyExpression.parseExpression(ref booleanType))
        {
            return false;
        }
        if (!IsAutomaticCast("bool", booleanType, false, false))
        {
            TypeError("bool", booleanType);
            return false;    
        }
        
        // conditional exit
        TinyCode.IfExit("while exit", "Z");  // X==0 -> false -> exit
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ')'
        if (!parseBlock(false))
        {
            return false;
        }
        TinyCode.EndLoop("while");
        return true;
    }
    
    bool parseForStatement()
    {
        TinyScanner.Advance(); // Skip 'for'
        Token token = TinyScanner.Current();
        
        TinyCode.Map(token);
        
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
        else if (!parseExpressionStatement(false))
        {
            return false;
        }
        
        TinyCode.Loop("for");
        
        // for condition clause
        string booleanType;
        if (!TinyExpression.parseExpression(ref booleanType))
        {
            return false;
        }
        
        if (!IsAutomaticCast("bool", booleanType, false, false))
        {
            TypeError("bool", booleanType);
            return false;    
        }
        
        // conditional exit
        TinyCode.IfExit("for exit", "Z"); // X==0 -> false -> exit
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        TinyScanner.Advance(); // Skip ';'
        
        TinyCode.Capturing();
        // for increment clause
        if (!parseExpressionStatement(true))
        {
            return false;
        }
        string captured = TinyCode.Captured();
               
        if (!parseBlock(false))
        {
            return false;
        }
        
        TinyCode.EmitCaptured(captured);
        
        TinyCode.EndLoop("for");
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
            // TODO : return value
            
            token = TinyScanner.Current();
        }
        
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after return statement, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        TinyCode.Break("return");
        
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
        TinyCode.Break("");
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
        TinyCode.Continue();
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
        TinyCode.Map(token);
    
        string name = token.Lexeme;
        TinyScanner.Advance(); // Skip identifier
        
        if (!DefineVariable(tp, name, LocalOffset, false))
        {
            return false;
        }
        
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// initialize '" + name + "' (BP+" + (LocalOffset).ToString() +")", 0);
        
        if (TinyType.IsByteType(tp))
        {
            TinyCode.PushByte(0, tp);
        }
        else
        {
            TinyCode.PushWord(0, tp);
        }
        
        LocalOffset = LocalOffset + int(IsByteType(tp) ? 1 : 2);
    
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
                
                if (!IsAutomaticCast(tp, exprType, false, false))
                {
                    Error(token.SourcePath, token.Line, "type mismatch: expected '" + tp + "', was '" + exprType + "'");
                    return false;
                }
    
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_SEMICOLON)
                {
                    Error(token.SourcePath, token.Line, "expected ';' after variable assignment, ('" + token.Lexeme + "')");
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
        }
        else
        {
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after variable declaration, ('" + token.Lexeme + "')");
                return false;
            }
        }
        
        TinyScanner.Advance(); // Skip ';'
    
        return true;
    }
    
    
    bool parseLocalConstDeclaration()
    {
        TinyCode.Generating = false;
        
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
        if (!IsAutomaticCast(constantType, expressionType, false, false))
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
        
        TinyCode.Generating = true;
        
        if (constantType == "const char[]")
        {
            uint index;
            DefineStringConst(value, ref index);
            value = index.ToString();
        }
        return TinyConstant.DefineConst(constantType, name, value);
    }
    bool parseExpressionStatement(bool forIncrement)
    {
        string actualType;
        if (!TinyExpression.parseExpression(ref actualType))
        {
            return false;
        }
        
        Token token = TinyScanner.Current();
        if (token.Type != (forIncrement ? TokenType.SYM_RPAREN : TokenType.SYM_SEMICOLON))
        {
            Error(token.SourcePath, token.Line, "expected '" + (forIncrement ? ')' : ';') + "' after expression, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip terminationToken
               
        // discard unused return value
        if (actualType != "void")
        {
            TinyCode.PopBytes(IsByteType(actualType) ? 1 : 2, "expression statement");
        }
        
        return true;
    }
    bool parseBlock(bool scope)
    {
        if (scope)
        {
            TinyConstant.EnterBlock();
            TinySymbols.EnterBlock(true);
        }
            
        
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
        if (scope)
        {
            TinySymbols.LeaveBlock("", true);
            TinyConstant.LeaveBlock();
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
            case TokenType.KW_MEM:
            {
                if (!parseExpressionStatement(false))
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

