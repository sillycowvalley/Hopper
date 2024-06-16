unit TinyCompile
{
    friend TinyStatement, TinyExpression;
    
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyGen"
    uses "TinyStatement"
    uses "TinyExpression"
    uses "TinyConstant"
    uses "TinyType"
    uses "TinySymbols"
    
    int localOffset;
    int LocalOffset   { get { return localOffset; } set { localOffset = value; } }
    uint globalOffset;
    uint GlobalOffset { get { return globalOffset; } set { globalOffset = value; } }
    
    < <Instruction> > globalDefinitions;   
    
    bool Compile()
    {
        bool success;
        loop
        {
            // global scope
            TinyConstant.EnterBlock();
            TinySymbols.EnterBlock(true, "program");
            
            if (!parseProgram())
            {
                break;
            }
        
            // global scope
            TinySymbols.LeaveBlock("program", true);
            TinyConstant.LeaveBlock();
            
            Token token = TinyScanner.Current();
            string returnType;
            if (!GetFunction("main", ref returnType))
            {
                Error(token.SourcePath, token.Line, "'main()' entrypoint missing");
                break;
            }
            if (returnType != "void")
            {
                Error(token.SourcePath, token.Line, "'main()' entrypoint should not have return type");
                break;
            }
            success = true;
            break;
        }
        
        return success;
    }
    
    bool parseProgram()
    {
        bool success = true;
        Token token;
        bool usesDone;
        
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
                    if (!usesDone)
                    {
                        TinyCode.Uses();
                        usesDone = true;
                    }
                    success = parseGlobalVar();
                }
                case TokenType.KW_FUNC:
                {
                    if (!usesDone)
                    {
                        TinyCode.Uses();
                        usesDone = true;
                    }
                    success = parseFunction();
                }
                case TokenType.SYM_HASH:
                {
                    success = parseSymbol();
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
        TinyCode.Generating = false;
        
        TinyScanner.Advance(); // Skip 'const'
        Token token = TinyScanner.Current();
        
        string constantType;
        uint size;
        if (!parseType(ref constantType, ref size))
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
    
    bool parseSymbol()
    {
        TinyScanner.Advance(); // Skip #
        
        Token token = TinyScanner.Current();
        if ((token.Type != TokenType.IDENTIFIER) || (token.Lexeme != "define"))
        {
            Error(token.SourcePath, token.Line, "'#define' expected");
            return false;
        }
        
        TinyScanner.Advance(); // Skip 'define'
        
        token = TinyScanner.Current();
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "preprocessor symbol identifier expected");
            return false;
        }
        if ((token.Lexeme == "CPU_6502") || (token.Lexeme == "CPU_65UINO"))
        {
            Error(token.SourcePath, token.Line, "Tiny6502 only supports CPU_65C02S");
            return false;
        }
        if (token.Lexeme == "EXPERIMENTAL")
        {
            IsExperimental = true;
        }
        DefineSymbol(token.Lexeme);
        
        TinyScanner.Advance(); // Skip symbol
        return true;
    }
 
    EmitGlobals()
    {
        TinyCode.StartUp();
        foreach (var global in globalDefinitions)
        {
            <Instruction> captured = global;
            TinyGen.EmitStream(captured);
        }
    }
    
    bool parseGlobalVar()
    {
        TinyGen.BeginStream(true);
        
        string tp;
        uint size;
        if (!parseType(ref tp, ref size))
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
        if (!DefineVariable(tp, name, int(GlobalOffset), true))
        {
            return false;
        }
        
        TinyScanner.Advance(); // Skip identifier

        BlockLevel++;
        TinyGen.Comment("initialize '" + name + "' (" + (GlobalOffset).ToString() + ") A");
        
        // make a slot on the stack
        TinyGen.PushImmediate(TinyType.IsByteType(tp), 0);
        
        string memberType;
        if (IsArrayType(tp, ref memberType) && (size != 0))
        {
            if (!IsByteType(memberType))
            {
                size *= 2;
            }
            /*
            TinyCode.PushWord(size);
            TinyCode.PadOut("TinySys.Malloc();", 0);
            TinyCode.PadOut("PLY", 0);
            TinyCode.PadOut("PLY", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 0);
            TinyCode.PadOut("PHA", 0);
            TinyCode.PadOut("LDA ZP.TOPH", 0);
            TinyCode.PadOut("PHA", 0);
            TinyCode.PopVariable(name, int(GlobalOffset), false, true);
            */
            TinyGen.PushImmediate(false, size);
            TinyGen.Call("malloc", false, true, 2);
            TinyGen.PopVariable(int(GlobalOffset), false, true);
        }
        
        BlockLevel--;
        
        GlobalOffset = GlobalOffset + (IsByteType(tp) ? 1 : 2);
            
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_EQ)
        {
            TinyScanner.Advance(); // Skip '='
            string exprType;
            if (!TinyExpression.Expression(ref exprType)) // global initializer expression
            {
                return false;
            }
            
            if (!IsAutomaticCast(tp, exprType, false, false))
            {
                Error(token.SourcePath, token.Line, "type mismatch: expected '" + tp + "', was '" + exprType + "'");
                return false;
            }
            
            int    offset;
            bool   isGlobal;
            string variableType;
            if (!GetVariable(name, ref variableType, ref offset, ref isGlobal))
            {
                Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
            }
            TinyGen.PopVariable(offset, IsByteType(variableType), isGlobal);
        }
    
        token = TinyScanner.Current();
        if (token.Type != TokenType.SYM_SEMICOLON)
        {
            Error(token.SourcePath, token.Line, "expected ';' after variable declaration, (" + token.Lexeme + "')");
            return false;
        }
    
        TinyScanner.Advance(); // Skip ';'
        
        <Instruction> captured = TinyGen.CaptureStream();
        globalDefinitions.Append(captured);
        
        return true;
    }
    
    
    bool parseFunction()
    {
        bool success;
        
        LocalOffset = 0;
        
        loop
        {
            TinyScanner.Advance(); // Skip 'func'
            Token token = TinyScanner.Current();
            
            // Optional return type
            string returnType = "void";
            uint size;
            if (token.Type != TokenType.IDENTIFIER)
            {
                if (!parseType(ref returnType, ref size))
                {
                    break;
                }
                token = TinyScanner.Current();
            }
        
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected identifier after 'func', (" + token.Lexeme + "')");
                break;
            }
        
            string functionName = token.Lexeme;
            TinyScanner.Advance(); // Skip identifier
            
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_EQ)
            {
                // Handle function pointer assignment
                TinyScanner.Advance(); // Skip '='
                
                token = TinyScanner.Current();
                if (token.Type != TokenType.IDENTIFIER)
                {
                    Error(token.SourcePath, token.Line, "expected function name after '=', ('" + token.Lexeme + "')");
                    return false;
                }
    
                string toFunctionName = token.Lexeme;
                TinyScanner.Advance(); // Skip function name
                
                // TODO : global func pointer
                // - create global variable of type 'func'
                // - assign it the word value of the constant Resoures.FunctionName
                // - add FunctionName to the list of constants to emit to 'resources.asm'
                
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_SEMICOLON)
                {
                    Error(token.SourcePath, token.Line, "expected ';' after function pointer assignment, (" + token.Lexeme + "')");
                    break;
                }
        
                TinyScanner.Advance(); // Skip ';'
                
                
                success = true;
                break;
            }
            
            TinyCode.Function(functionName);
            TinyConstant.EnterBlock();
            TinySymbols.EnterBlock(false, functionName + " arguments"); // for arguments
            
            if (!DefineFunction(returnType, functionName))
            {
                break;
            }
            if (token.Type != TokenType.SYM_LPAREN)
            {
                Error(token.SourcePath, token.Line, "expected '(' after function name, (" + token.Lexeme + "')");
                break;
            }
        
            TinyScanner.Advance(); // Skip '('
            if (!parseParameterList(functionName))
            {
                break;
            }
            TinySymbols.UpdateArgumentOffsets(functionName);
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_RPAREN)
            {
                Error(token.SourcePath, token.Line, "expected ')' after parameter list, (" + token.Lexeme + "')");
                break;
            }
        
            TinyScanner.Advance(); // Skip ')'
        
            token = TinyScanner.Current();
            bool generate;
            if (token.Type == TokenType.SYM_SEMICOLON)
            {
                // This is a forward declaration or a system method
                TinyScanner.Advance(); // Skip ';'
                
                TinyCode.OfferSystemMethod(functionName);
            }
            else if (token.Type == TokenType.SYM_LBRACE)
            {
                generate = true;
                TinyCode.EmitDeferred();
                
                if (functionName == "main")
                {
                    EmitGlobals();
                }
                TinyCode.Enter();
                
                // This is an actual function definition
                if (!TinyStatement.parseBlock(false, "function " + functionName)) // method scope block
                {
                    break;
                }
                
                TinyCode.Leave();
            }
            else
            {
                Error(token.SourcePath, token.Line, "expected '{' to start function body or ';' for forward declaration");
                break;
            }
            
            TinySymbols.LeaveBlock(functionName, generate); // for arguments
            TinyConstant.LeaveBlock();
            
            success = true;
            break;
        } // loop
        
        
        return success;
    }
    
    bool parseParameterList(string functionName)
    {
        Token token;
        int offset;
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RPAREN)
            {
                break; // exit the loop when reaching ')'
            }
            
            string variableType;
            uint size;
            if (!parseType(ref variableType, ref size))
            {
                return false;
            }
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected parameter name");
                return false;
            }
            
            string variableName = token.Lexeme;
            
            if (!DefineVariable(variableType, variableName, offset, false))
            {
                return false;
            }
            if (!DefineArgument(functionName, variableType, variableName, offset))
            {
                return false;
            }
            offset++;
            
            TinyScanner.Advance(); // Skip name
            
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
    
    bool parseType(ref string tp, ref uint size)
    {
        Token token = TinyScanner.Current();
    
        if (token.Type == TokenType.KW_CONST)
        {
            tp = "const ";
            TinyScanner.Advance(); // Skip 'const'
            token = TinyScanner.Current();
        }
        else
        {
            tp = "";
        }
    
        if (!TinyToken.IsTypeKeyword(token.Type))
        {
            Error(token.SourcePath, token.Line, "expected type");
            return false;
        }
    
        tp += token.Lexeme;
        TinyScanner.Advance(); // Skip type
    
        token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_LBRACKET)
        {
            TinyScanner.Advance(); // Skip '['
            token = TinyScanner.Current();
    
            // Check for number or identifier (constant)
            if (token.Type != TokenType.SYM_RBRACKET)
            {
                string value;
                string actualType;
                if (!TinyConstant.parseConstantExpression(ref value, ref actualType))
                {
                    return false;
                }
                if (!UInt.TryParse(value, ref size))
                {
                    Die(0x0B);
                }
                tp += "[" + value + "]";
            }
            else
            {
                tp += "[]";
            }
    
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_RBRACKET)
            {
                Error(token.SourcePath, token.Line, "expected ']', ('" + token.Lexeme + "')");
                return false;
            }
            TinyScanner.Advance(); // Skip ']'
        }
        return true;
    }
}
