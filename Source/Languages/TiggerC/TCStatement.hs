unit TCStatement
{
    friend TCCompile, TCExpression;
    uses "TCToken"
    uses "TCScanner"
    uses "TCCode"
    uses "TCGen"
    uses "TCExpression"
    uses "TCType"
    uses "TCSymbols"
    
    bool parseAssembler()
    {
        TCScanner.Advance(); // Skip 'asm'
        
        Token token = TCScanner.Current();
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'if'");
            return false;
        }
        TCScanner.Advance(); // Skip '('
        
        token = TCScanner.Current();
        if (token.Type != TokenType.LIT_STRING)
        {
            Error(token.SourcePath, token.Line, "assembly string expected");
            return false;
        }
        TCCode.PadOut(token.Lexeme, 0);
        
        TCScanner.Advance(); // Skip string
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition");
            return false;
        }
        TCScanner.Advance(); // Skip ')'
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after 'break'");
            return false;
        }
        TCScanner.Advance(); // Skip ';'
        return true;
    }
    
    bool parseIfStatement()
    {
        TCScanner.Advance(); // Skip 'if'
        Token token = TCScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'if'");
            return false;
        }
        
        TCGen.BeginStream(false);
        
        TCScanner.Advance(); // Skip '('
        string booleanType;
        if (!TCExpression.Expression(ref booleanType)) // if statement bool expression
        {
            return false;
        }
        if (!IsAutomaticCast("bool", booleanType, false, false))
        {
            TypeError("bool", booleanType);
            return false;    
        }
        
        TCGen.IF(false); // no {
        
        TCGen.FlushStream();
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition");
            return false;
        }
        
        TCScanner.Advance(); // Skip ')'
        if (!parseBlock(true, "if")) // if scope block
        {
            return false;
        }
        
        token = TCScanner.Current();
        if (token.Type == TokenType.KW_ELSE)
        {
            TCCode.Else();
            TCScanner.Advance(); // Skip 'else'
            token = TCScanner.Current();    
            if (token.Type == TokenType.KW_IF)
            {
                TCCode.PadOut("{ // else if", 0);
                if (!parseIfStatement())
                {
                    return false;
                }
                TCCode.PadOut("} // else if", 0);
            }
            else
            {
                if (!parseBlock(true, "else")) // else scope block
                {
                    return false;
                }
            }
        }
        return true;
    }
    
    bool parseWhileStatement()
    {
        TCScanner.Advance(); // Skip 'while'
        Token token = TCScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'while'");
            return false;
        }
        
        TCCode.Loop("while");
        
        TCGen.BeginStream(false);
        
        TCScanner.Advance(); // Skip '('
        string booleanType;
        if (!TCExpression.Expression(ref booleanType)) // while statement bool expression
        {
            return false;
        }
        if (!IsAutomaticCast("bool", booleanType, false, false))
        {
            TypeError("bool", booleanType);
            return false;    
        }
        
        // conditional exit
        TCGen.LoopExit("while exit");  // X==0 -> false -> exit
        
        TCGen.FlushStream();
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip ')'
        if (!parseBlock(false, "while")) // while scope block
        {
            return false;
        }
        TCCode.EndLoop("while");
        return true;
    }
    
    bool parseForStatement()
    {
        TCScanner.Advance(); // Skip 'for'
        Token token = TCScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'for'");
            return false;
        }
        
        // for initialize clause
        TCScanner.Advance(); // Skip '('
        token = TCScanner.Current();
        if (TCToken.IsTypeKeyword(token.Type))
        {
            TCGen.BeginStream(false);
            if (!parseLocalVarDeclaration())
            {
                return false;
            }
            TCGen.FlushStream();
        }
        else 
        {
            TCGen.BeginStream(false);
            if (!parseExpressionStatement(false)) // 'for' initialize clause
            {
                return false;
            }
            TCGen.FlushStream();
        }
        
        TCCode.Loop("for");
        
        // for condition clause
        token = TCScanner.Current();
        if (token.Type == TokenType.SYM_SEMICOLON)
        {
            // empty expression is ok - no exit condition or exit jump
        }
        else
        {
            TCGen.BeginStream(false);
            
            string booleanType;
            if (!TCExpression.Expression(ref booleanType)) // for statement condition clause expression
            {
                return false;
            }
            
            if (!IsAutomaticCast("bool", booleanType, false, false))
            {
                TypeError("bool", booleanType);
                return false;    
            }
            
            // conditional exit
            TCGen.LoopExit("for exit"); // X==0 -> false -> exit
            
            TCGen.FlushStream();
        }
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after condition, ('" + token.Lexeme + "')");
            return false;
        }
        TCScanner.Advance(); // Skip ';'
        
        TCGen.BeginStream(true);
        
        // for increment clause
        if (!parseExpressionStatement(true)) // 'for' increment clause
        {
            return false;
        }
        <Instruction> captured = TCGen.CaptureStream();
               
        if (!parseBlock(false, "for")) // for scope block
        {
            return false;
        }
        
        TCGen.EmitStream(captured);
        
        TCCode.EndLoop("for");
        return true;
    }
    
    bool parseReturnStatement()
    {
        TCScanner.Advance(); // Skip 'return'
        Token token = TCScanner.Current();
        
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
            if (!TCExpression.Expression(ref actualType)) // return statement expression
            {
                return false;
            }
            // validate return type
            if (!IsAutomaticCast(returnType, actualType, false, false))
            {
                TypeError(returnType, actualType);
                return false;
            }
            token = TCScanner.Current();
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
        
        TCScanner.Advance(); // Skip ';'
        
        if (returnType != "void")
        {
            TCCode.Ret(IsByteType(returnType));
        }
        
        TCCode.Return("exit " + CurrentFunction + " " + VariableComment());
        
        return true;
    }
       
    bool parseBreakStatement()
    {
        TCScanner.Advance(); // Skip 'break'
        
        Token token = TCScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after 'break'");
            return false;
        }
        
        TCScanner.Advance(); // Skip ';'
        TCCode.Break("");
        return true;
    }
    
    bool parseContinueStatement()
    {
        TCScanner.Advance(); // Skip 'continue'
        
        Token token = TCScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after 'continue'");
            return false;
        }
        
        TCScanner.Advance(); // Skip ';'
        TCCode.Continue();
        return true;
    }
    
    
    bool parseLocalVarDeclaration()
    {
        string tp;
        uint size;
        if (!TCCompile.parseType(ref tp, ref size))
        {
            return false;
        }
        
        Token token = TCScanner.Current();
        if (CurrentIsNaked)
        {
            Error(token.SourcePath, token.Line, "'" + token.Lexeme + "': no local variables in naked function");
            return false;
        }
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "expected identifier after type, ('" + token.Lexeme + "')");
            return false;
        }
        string name = token.Lexeme;
        TCScanner.Advance(); // Skip identifier
        
        bool isGlobal;
        int  offset = LocalOffset;
        uint slotSize = IsByteType(tp) ? 1 : 2;
        if ((IsValueType(tp) || IsPointerType(tp)) && (BlockLevel == 2) && RequestStaticLocal(CurrentFunction, name, slotSize)) 
        {
            // top local scope, value type variabes or pointer variables (not automatically allocated)
            isGlobal = true;
            offset = int(GlobalOffset);
        }
        if (!DefineVariable(tp, name, offset, isGlobal)) // parseLocalVarDeclaration
        {
            return false;
        }
        if (isGlobal)
        {
            TCGen.Comment("initialize '" + name + "' (" + (GlobalOffset).ToString() + ")");
            TCGen.ZeroGlobal(TCType.IsByteType(tp), GlobalOffset);
            GlobalOffset = GlobalOffset + slotSize;
        }
        else
        {
            TCGen.Comment("initialize '" + name + "' (BP+" + (LocalOffset).ToString() +")");
            TCGen.PushImmediate(TCType.IsByteType(tp), 0);
            string memberType;
            if (IsArrayType(tp, ref memberType) && (size != 0))
            {
                if (!IsByteType(memberType))
                {
                    size *= 2;
                }
                TCGen.BeginStream(false);
                TCGen.PushImmediate(false, size);
                TCGen.Call("malloc", false, true, 2);
                TCGen.PopVariable(LocalOffset, false, false);
                TCGen.FlushStream();
            }
            LocalOffset = LocalOffset + int(slotSize);
        }
    
        token = TCScanner.Current();
        if (token.Type == TokenType.SYM_EQ)
        {
            TCScanner.Advance(); // Skip '='
    
            string exprType;
            if (!TCExpression.Expression(ref exprType)) // local variable initializer expression
            {
                return false;
            }
            
            if (!IsAutomaticCast(tp, exprType, false, false))
            {
                Error(token.SourcePath, token.Line, "type mismatch: expected '" + tp + "', was '" + exprType + "'");
                return false;
            }

            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after variable assignment, ('" + token.Lexeme + "')");
                return false;
            }
            
            string variableType;
            if (!GetVariable(name, ref variableType, ref offset, ref isGlobal))
            {
                Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
            }
            TCGen.PopVariable(offset, IsByteType(variableType), isGlobal);
        }
        else
        {
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after variable declaration, ('" + token.Lexeme + "')");
                return false;
            }
        }
        
        TCScanner.Advance(); // Skip ';'
    
        return true;
    }
    
    
    bool parseLocalConstDeclaration()
    {
        TCCode.Generating = false;
        
        TCScanner.Advance(); // Skip 'const'
        
        string constantType;
        uint size;
        if (!TCCompile.parseType(ref constantType, ref size))
        {
            return false;
        }
    
        Token token = TCScanner.Current();
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "expected identifier after type");
            return false;
        }
    
        string name = token.Lexeme;
        TCScanner.Advance(); // Skip identifier
    
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_EQ)
        {
            Error(token.SourcePath, token.Line, "expected '=' after identifier");
            return false;
        }
    
        TCScanner.Advance(); // Skip '='
        
        constantType = "const " + constantType;
    
        string expressionType;
        string value;
        if (!TCConstant.parseConstantExpression(ref value, ref expressionType))
        {
            return false;
        }
        if (!IsAutomaticCast(constantType, expressionType, false, false))
        {
            TypeError(constantType, expressionType);
        }
        
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after constant declaration, ('" + token.Lexeme + "')");
            return false;
        }
    
        TCScanner.Advance(); // Skip ';'
        
        TCCode.Generating = true;
        
        if (constantType == "const char[]")
        {
            uint index;
            DefineStringConst(value, ref index);
            value = index.ToString();
        }
        return TCConstant.DefineConst(constantType, name, value);
    }
    bool parseExpressionStatement(bool forIncrement)
    {
        Token token = TCScanner.Current();
        if ((forIncrement && (token.Type == TokenType.SYM_RPAREN)) || (!forIncrement && (token.Type == TokenType.SYM_SEMICOLON)))
        {
            // empty statement is ok
            TCScanner.Advance(); // Skip termination Token
            return true;
        }
        
        string actualType;
        if (!TCExpression.Expression(ref actualType)) // expression statement
        {
            return false;
        }
        
        token = TCScanner.Current();
        if (token.Type != (forIncrement ? TokenType.SYM_RPAREN : TokenType.SYM_SEMICOLON))
        {
            Error(token.SourcePath, token.Line, "expected '" + (forIncrement ? ')' : ';') + "' after expression, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip termination Token
               
        // discard unused return value
        if (actualType != "void")
        {
            if (InStreamMode)
            {
                TCGen.DecSP(IsByteType(actualType) ? 1 : 2);
            }
            else
            {
                TCCode.PopBytes(IsByteType(actualType) ? 1 : 2, "expression statement"); // after expression
            }
        }
        
        return true;
    }
    bool parseBlock(bool scope, string comment)
    {
        if (scope)
        {
            TCConstant.EnterBlock();
            TCSymbols.EnterBlock(true, comment);
        }
            
        
        Token token = TCScanner.Current();
        
        if (token.Type != TokenType.SYM_LBRACE)
        {
            Error(token.SourcePath, token.Line, "expected '{' to start block, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip '{'
        
        loop
        {
            token = TCScanner.Current();
            if (token.Type == TokenType.SYM_RBRACE)
            {
                break; // exit the loop when reaching '}'
            }
            
            if (!parseStatement())
            {
                return false;
            }
        }
        
        TCScanner.Advance(); // Skip '}'
        if (scope)
        {
            TCSymbols.LeaveBlock(comment, true); // generic scope block
            TCConstant.LeaveBlock();
        }
        return true;
    }
    
    bool parseStatement()
    {
        Token token = TCScanner.Current();
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
            {
                TCGen.BeginStream(false);
                if (!parseLocalVarDeclaration())
                {
                    return false;
                }
                TCGen.FlushStream();
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
                TCGen.BeginStream(false);
                loop
                {
                    if (!parseExpressionStatement(false)) // assignment
                    {
                        return false;
                    }
                    token = TCScanner.Current();
                    if ((token.Type != TokenType.IDENTIFIER) && (token.Type != TokenType.KW_MEM))
                    {
                        break;    
                    }
                }
                TCGen.FlushStream();
            }
            case TokenType.SYM_SEMICOLON:
            {            
                // empty statement is ok
                TCScanner.Advance(); // Skip ';'
            }
            case TokenType.KW_ASM:
            {
                if (!parseAssembler())
                {
                    return false;
                }
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unexpected token in statement: " + TCToken.ToString(token.Type));
                return false;
            }
        }
        return true;
    }

    bool parseSwitchStatement()
    {
        TCScanner.Advance(); // Skip 'switch'
        Token token = TCScanner.Current();
        
        if (token.Type != TokenType.SYM_LPAREN)
        {
            Error(token.SourcePath, token.Line, "expected '(' after 'switch', ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip '('
        
        
        string switchType;
        if (!TCExpression.Expression(ref switchType)) // switch statement expression
        {
            return false;
        }
        if (!IsByteType(switchType))
        {
            Error(token.SourcePath, token.Line, "switch expression should be single byte type");
            return false;
        }
        switchType = switchType.Replace("const ", "");
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_RPAREN)
        {
            Error(token.SourcePath, token.Line, "expected ')' after switch expression, ('" + token.Lexeme + "')");
            return false;
        }
        TCCode.PadOut("PLX", 0);
        TCCode.PadOut("switch (X)", 0);
        TCCode.PadOut("{", 0);
        TCScanner.Advance(); // Skip ')'
        token = TCScanner.Current();
        if (token.Type != TokenType.SYM_LBRACE)
        {
            Error(token.SourcePath, token.Line, "expected '{' to start switch block, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip '{'
        bool defaultSeen;
        loop
        {
            token = TCScanner.Current();
            if (token.Type == TokenType.SYM_RBRACE)
            {
                break; // exit the loop when reaching '}'
            }
            
            if (token.Type == TokenType.KW_CASE)
            {
                if (defaultSeen)
                {
                    Error(token.SourcePath, token.Line, "no more 'case's allowed after 'default'");
                    return false;
                }
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
                defaultSeen = true;
            }
            else 
            {
                Error(token.SourcePath, token.Line, "'case' or 'default' expected");
                return false;
            }
        }
        TCCode.PadOut("}", 0);
        TCScanner.Advance(); // Skip '}'
        return true;
    }

    bool parseCaseStatement(string switchType)
    {
        TCScanner.Advance(); // Skip 'case'
        
        string caseType;
        
        string expressionType;
        string value;
        if (!TCConstant.parseConstantExpression(ref value, ref expressionType))
        {
            return false;
        }
        
        Token token = TCScanner.Current();
        if (expressionType != switchType)
        {
            Error(token.SourcePath, token.Line, "'" + switchType + "' constant expected");
            return false;    
        }
        if (token.Type != TokenType.SYM_COLON)
        {
            Error(token.SourcePath, token.Line, "expected ':' after case value, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip ':'
        switch (expressionType)
        {
            case "byte":
            {
                TCCode.PadOut("case " + value + ":", 0);
            }
            case "bool":
            {
                TCCode.PadOut("case " + (value == "false" ? "0" : "1") + ":", 0);
            }
            case "char":
            {
                TCCode.PadOut("case '" + value + "':", 0);    
            }
            default:
            {
                Die(0x0B);
            }
        }
        
        if (!parseBlock(true, "case")) // case scope block
        {
            return false;
        }
        return true;
    }

    bool parseDefaultStatement()
    {
        TCScanner.Advance(); // Skip 'default'
        Token token = TCScanner.Current();
        if (token.Type != TokenType.SYM_COLON)
        {
            Error(token.SourcePath, token.Line, "expected ':' after default, ('" + token.Lexeme + "')");
            return false;
        }
        
        TCScanner.Advance(); // Skip ':'
        
        TCCode.PadOut("default:", 0);
        
        if (!parseBlock(true, "case")) // case scope block
        {
            return false;
        }
        return true;
    }
}

