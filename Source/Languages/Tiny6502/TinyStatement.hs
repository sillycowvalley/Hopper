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
        if (!parseBlock(true, "if")) // if scope block
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type == TokenType.KW_ELSE)
        {
            TinyCode.Else();
            TinyScanner.Advance(); // Skip 'else'
            if (!parseBlock(true, "else")) // else scope block
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
        if (!parseBlock(false, "while")) // while scope block
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
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_SEMICOLON)
        {
            // empty expression is ok - no exit condition or exit jump
        }
        else
        {
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
        }
        
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
               
        if (!parseBlock(false, "for")) // for scope block
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
        
        string returnType;
        if (!GetFunction(CurrentFunction, ref returnType))
        {
            Die(0x0B);
        }
        
        // Check for empty return statement
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            if (returnType == "void")
            {
                Error(token.SourcePath, token.Line, "';' expected");
                return false;
            }
            
            string actualType;
            if (!TinyExpression.parseExpression(ref actualType))
            {
                return false;
            }
            // validate return type
            if (!IsAutomaticCast(returnType, actualType, false, false))
            {
                TypeError(returnType, actualType);
                return false;
            }
            
            
            token = TinyScanner.Current();
        }
        else if (returnType != "void")
        {
            Error(token.SourcePath, token.Line, "'" + returnType + "' expression expected");
            return false;
        }
        
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after return statement, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip ';'
        
        if (returnType != "void")
        {
            TinyCode.Ret(IsByteType(returnType));
        }
        
        byte bytes;
        uint level = GetCurrentVariableLevel();
        loop
        {
            FreeAutomaticAllocations(level);
            bytes += GetLevelBytes(level);
            if (level == 1) { break; }
            level--;
        }
        TinyCode.PopBytes(bytes, CurrentFunction + " locals");
        
        TinyCode.Return("exit " + CurrentFunction + " " + VariableComment());
        
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
        uint size;
        if (!TinyCompile.parseType(ref tp, ref size))
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
        
        string memberType;
        if (IsArrayType(tp, ref memberType) && (size != 0))
        {
            if (!IsByteType(memberType))
            {
                size *= 2;
            }
            TinyCode.PushWord(size, "array size");
            TinyCode.PadOut("TinySys.Malloc();", 0);
            TinyCode.PadOut("PLY", 0);
            TinyCode.PadOut("PLY", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 0);
            TinyCode.PadOut("PHA", 0);
            TinyCode.PadOut("LDA ZP.TOPH", 0);
            TinyCode.PadOut("PHA", 0);
            TinyCode.PopVariable(name, LocalOffset, false, false);
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
    
                string toFunctionName = token.Lexeme;
                TinyScanner.Advance(); // Skip function name
                
                // TODO : local func pointer
                // - create local variable of type 'func'
                // - assign it the word value of the constant Resoures.FunctionName (PopVariable(name, LocalOffset, false, false);)
                // - add FunctionName to the list of constants to emit to 'resources.asm'
    
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_SEMICOLON)
                {
                    Error(token.SourcePath, token.Line, "expected ';' after function pointer assignment, ('" + token.Lexeme + "')");
                    return false;
                }
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
        uint size;
        if (!TinyCompile.parseType(ref constantType, ref size))
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
        Token token = TinyScanner.Current();
        if ((forIncrement && (token.Type == TokenType.SYM_RPAREN)) || (!forIncrement && (token.Type == TokenType.SYM_SEMICOLON)))
        {
            // empty statement is ok
            TinyScanner.Advance(); // Skip termination Token
            return true;
        }
        
        string actualType;
        if (!TinyExpression.parseExpression(ref actualType))
        {
            return false;
        }
        
        token = TinyScanner.Current();
        if (token.Type != (forIncrement ? TokenType.SYM_RPAREN : TokenType.SYM_SEMICOLON))
        {
            Error(token.SourcePath, token.Line, "expected '" + (forIncrement ? ')' : ';') + "' after expression, ('" + token.Lexeme + "')");
            return false;
        }
        
        TinyScanner.Advance(); // Skip termination Token
               
        // discard unused return value
        if (actualType != "void")
        {
            TinyCode.PopBytes(IsByteType(actualType) ? 1 : 2, "expression statement"); // after expression
        }
        
        return true;
    }
    bool parseBlock(bool scope, string comment)
    {
        if (scope)
        {
            TinyConstant.EnterBlock();
            TinySymbols.EnterBlock(true, comment);
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
            TinySymbols.LeaveBlock(comment, true); // generic scope block
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
            case TokenType.SYM_SEMICOLON:
            {            
                // empty statement is ok
                TinyScanner.Advance(); // Skip ';'
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

