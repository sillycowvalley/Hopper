unit TCCompile
{
    friend TCStatement, TCExpression;
    
    uses "TCToken"
    uses "TCScanner"
    uses "TCCode"
    uses "TCGen"
    uses "TCStatement"
    uses "TCExpression"
    uses "TCConstant"
    uses "TCType"
    uses "TCSymbols"
    
    int localOffset;
    int LocalOffset   { get { return localOffset; } set { localOffset = value; } }
    uint globalOffset;
    uint GlobalOffset { get { return globalOffset; } set { globalOffset = value; } }
    
    bool firstPass;
    bool FirstPass { get { return firstPass; } set { firstPass = value; } }
    bool compiling;
    bool Compiling { get { return compiling; } set { compiling = value; } }
    
    < <Instruction> > globalDefinitions;  
    
    Reset() 
    {
        globalDefinitions.Clear();
        GlobalOffset = 0;
    }
    
    bool Compile()
    {
        bool success;
        loop
        {
            // global scope
            TCConstant.EnterBlock();
            TCSymbols.EnterBlock(true, "program");
            
            if (!parseProgram())
            {
                break;
            }
        
            // global scope
            TCSymbols.LeaveBlock("program", true);
            TCConstant.LeaveBlock();
            
            Token token = TCScanner.Current();
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
        
        Parser.ProgressTick("c"); // C compiler
        
        loop
        {
            token = TCScanner.Current();
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
                        TCCode.Uses();
                        usesDone = true;
                    }
                    success = parseGlobalVar();
                }
                case TokenType.KW_FUNC:
                {
                    if (!usesDone)
                    {
                        TCCode.Uses();
                        usesDone = true;
                    }
                    success = parseFunction();
                    Parser.ProgressTick("c"); // C compiler
                }
                case TokenType.SYM_HASH:
                {
                    success = parseSymbol();
                }
                default:
                {
                    Error(token.SourcePath, token.Line, "unexpected token: " + TCToken.ToString(token.Type) + "('" + token.Lexeme + "')");
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
        TCCode.Generating = false;
        
        TCScanner.Advance(); // Skip 'const'
        Token token = TCScanner.Current();
        
        string constantType;
        uint size;
        if (!parseType(ref constantType, ref size))
        {
            return false;
        }
        token = TCScanner.Current();
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
    
    bool parseSymbol()
    {
        TCScanner.Advance(); // Skip #
        
        Token token = TCScanner.Current();
        if ((token.Type != TokenType.IDENTIFIER) || (token.Lexeme != "define"))
        {
            Error(token.SourcePath, token.Line, "'#define' expected");
            return false;
        }
        
        TCScanner.Advance(); // Skip 'define'
        
        token = TCScanner.Current();
        if (token.Type != TokenType.IDENTIFIER)
        {
            Error(token.SourcePath, token.Line, "preprocessor symbol identifier expected");
            return false;
        }
        if ((token.Lexeme == "CPU_6502") || (token.Lexeme == "CPU_65UINO"))
        {
            Error(token.SourcePath, token.Line, "Tigger C only supports CPU_65C02S");
            return false;
        }
        if (token.Lexeme == "EXPERIMENTAL")
        {
            IsExperimental = true;
        }
        if (token.Lexeme == "ZEROPAGEGLOBALS")
        {
            if (globalDefinitions.Count != 0)
            {
                Error(token.SourcePath, token.Line, "Must be defined before the first global variable definition");
                return false;
            }
            ZeroPageGlobals = true;
        }
        DefineSymbol(token.Lexeme);
        
        TCScanner.Advance(); // Skip symbol
        return true;
    }
 
    EmitGlobals()
    {
        TCCode.StartUp();
        foreach (var global in globalDefinitions)
        {
            <Instruction> captured = global;
            TCGen.EmitStream(captured);
        }
    }
    
    bool parseGlobalVar()
    {
        bool success;
        loop
        {
            CurrentFunction = "main"; // malloc and free happen in main()
            
            TCGen.BeginStream(true);
            
            string tp;
            uint size;
            if (!parseType(ref tp, ref size))
            {
                break;
            }
            uint slotSize = (IsByteType(tp) ? 1 : 2);
        
            Token token = TCScanner.Current();
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected identifier after type");
                break;
            }
            string name = token.Lexeme;
            
            if (GlobalOffset + slotSize > GlobalLimit)
            {
                Error(token.SourcePath, token.Line, "global space limit (" + (GlobalLimit).ToString() + " bytes) exceeded");
                break;
            }
            if (!DefineVariable(tp, name, int(GlobalOffset), true)) // parseGlobalVar
            {
                break;
            }
            
            TCScanner.Advance(); // Skip identifier

            BlockLevel++;
            TCGen.Comment("initialize '" + name + "' (" + (GlobalOffset).ToString() + ")");
            TCGen.ZeroGlobal(TCType.IsByteType(tp), GlobalOffset);
            
            string memberType;
            if (IsArrayType(tp, ref memberType) && (size != 0))
            {
                if (!IsByteType(memberType))
                {
                    size *= 2;
                }
                TCGen.PushImmediate(false, size);
                TCGen.Call("malloc", false, true, 2);
                TCGen.PopVariable(int(GlobalOffset), false, true);
            }
            
            BlockLevel--;
            
            GlobalOffset = GlobalOffset + slotSize;
                
            token = TCScanner.Current();
            if (token.Type == TokenType.SYM_EQ)
            {
                TCScanner.Advance(); // Skip '='
                string exprType;
                if (!TCExpression.Expression(ref exprType)) // global initializer expression
                {
                    return false;
                }
                
                if (!IsAutomaticCast(tp, exprType, false, false))
                {
                    Error(token.SourcePath, token.Line, "type mismatch: expected '" + tp + "', was '" + exprType + "'");
                    break;
                }
                
                int    offset;
                bool   isGlobal;
                string variableType;
                if (!GetVariable(name, ref variableType, ref offset, ref isGlobal))
                {
                    Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                    break;
                }
                TCGen.PopVariable(offset, IsByteType(variableType), isGlobal);
            }
        
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_SEMICOLON)
            {
                Error(token.SourcePath, token.Line, "expected ';' after variable declaration, (" + token.Lexeme + "')");
                break;
            }
        
            TCScanner.Advance(); // Skip ';'
            
            <Instruction> captured = TCGen.CaptureStream();
            globalDefinitions.Append(captured);
            
            success = true;
            break;
        } // loop
        
        CurrentFunction = "<none>";
        
        return success;
    }
    
    
    bool parseFunction()
    {
        bool success;
        
        LocalOffset = 0;
        
        loop
        {
            TCScanner.Advance(); // Skip 'func'
            Token token = TCScanner.Current();
            
            // Optional return type
            string returnType = "void";
            uint size;
            if (token.Type != TokenType.IDENTIFIER)
            {
                if (!parseType(ref returnType, ref size))
                {
                    break;
                }
                token = TCScanner.Current();
            }
        
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected identifier after 'func', (" + token.Lexeme + "')");
                break;
            }
        
            string functionName = token.Lexeme;
            
            if (!FirstPass)
            {
                Compiling = MustCompile(functionName);
            }
            
            CurrentIsNaked = functionName.StartsWith("__");
            TCScanner.Advance(); // Skip identifier
            
            if (CurrentIsNaked && (returnType != "void"))
            {
                Error(token.SourcePath, token.Line, "naked functions should have no return type");
                break;        
            }
            
            TCCode.Function(functionName);
            TCConstant.EnterBlock();
            TCSymbols.EnterBlock(false, functionName + (GetRequiresFrame(functionName) ? " arguments" : "")); // for arguments
            
            if (!DefineFunction(returnType, functionName))
            {
                break;
            }
            
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_LPAREN)
            {
                Error(token.SourcePath, token.Line, "expected '(' after function name, (" + token.Lexeme + "')");
                break;
            }
        
            TCScanner.Advance(); // Skip '('
            
            if (CurrentIsNaked)
            {
                token = TCScanner.Current();
                if (token.Type != TokenType.SYM_RPAREN)
                {
                    Error(token.SourcePath, token.Line, "naked functions should have no arguments");
                    break;        
                }
            }
            
            if (!parseParameterList(functionName))
            {
                break;
            }
            TCSymbols.UpdateArgumentOffsets(functionName);
            
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_RPAREN)
            {
                Error(token.SourcePath, token.Line, "expected ')' after parameter list, (" + token.Lexeme + "')");
                break;
            }
        
            TCScanner.Advance(); // Skip ')'
        
            token = TCScanner.Current();
            bool generate;
            if (token.Type == TokenType.SYM_SEMICOLON)
            {
                // This is a forward declaration or a system method or a prototype
                TCScanner.Advance(); // Skip ';'
                
                SetFunctionPrototype(functionName);
            }
            else if (token.Type == TokenType.SYM_LBRACE)
            {
                /*
                if (!FirstPass && Compiling)
                {
                    PrintLn();
                    Print("Function: " + functionName);
                    if (GetRequiresFrame(functionName))
                    {
                        Print(" STACK", Colour.MatrixBlue, Colour.Black);
                    }
                }
                */
                if (FirstPass)
                {
                    TCSymbols.InitializeFunctionCalls(functionName);
                }
                
                generate = true;
                TCCode.EmitDeferred();
                
                if (functionName == "main")
                {
                    EmitGlobals();
                }
                if (!CurrentIsNaked)
                {
                    TCCode.Enter(functionName);
                }
                
                // This is an actual function definition
                if (!TCStatement.parseBlock(false, "function " + functionName)) // method scope block
                {
                    break;
                }
                if (!CurrentIsNaked)
                {
                    TCCode.Leave(functionName);
                }
            }
            else
            {
                Error(token.SourcePath, token.Line, "expected '{' to start function body or ';' for forward declaration");
                break;
            }
            
            TCSymbols.LeaveBlock(functionName, generate); // for arguments
            TCConstant.LeaveBlock();
            
            if (!FirstPass)
            {
                Compiling = true;
            }
            
            CurrentIsNaked = false;
            
            success = true;
            break;
        } // loop
        
        CurrentFunction = "<none";
        
        return success;
    }
    
    bool parseParameterList(string functionName)
    {
        Token token;
        int offset;
        loop
        {
            token = TCScanner.Current();
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
            
            token = TCScanner.Current();
            if (token.Type != TokenType.IDENTIFIER)
            {
                Error(token.SourcePath, token.Line, "expected parameter name");
                return false;
            }
            
            string variableName = token.Lexeme;
            
            if (!DefineVariable(variableType, variableName, offset, false)) // parseParameterList
            {
                return false;
            }
            if (!DefineArgument(functionName, variableType, variableName, offset))
            {
                return false;
            }
            offset++;
            
            TCScanner.Advance(); // Skip name
            
            token = TCScanner.Current();
            if (token.Type == TokenType.SYM_COMMA)
            {
                TCScanner.Advance(); // Skip ','
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
        Token token = TCScanner.Current();
    
        if (token.Type == TokenType.KW_CONST)
        {
            tp = "const ";
            TCScanner.Advance(); // Skip 'const'
            token = TCScanner.Current();
        }
        else
        {
            tp = "";
        }
    
        if (!TCToken.IsTypeKeyword(token.Type))
        {
            if (token.Type == TokenType.SYM_LBRACKET)
            {
                TCScanner.Advance(); // Skip '['
                token = TCScanner.Current();
                if (token.Type == TokenType.SYM_RBRACKET)
                {
                    TCScanner.Advance(); // Skip ']'
                    tp += "[]";
                    return true; // generic pointer type
                }
            }
            Error(token.SourcePath, token.Line, "expected type");
            return false;
        }
    
        tp += token.Lexeme;
        TCScanner.Advance(); // Skip type
    
        token = TCScanner.Current();
        if (token.Type == TokenType.SYM_LBRACKET)
        {
            TCScanner.Advance(); // Skip '['
            token = TCScanner.Current();
    
            // Check for number or identifier (constant)
            if (token.Type != TokenType.SYM_RBRACKET)
            {
                string value;
                string actualType;
                if (!TCConstant.parseConstantExpression(ref value, ref actualType))
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
    
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_RBRACKET)
            {
                Error(token.SourcePath, token.Line, "expected ']', ('" + token.Lexeme + "')");
                return false;
            }
            TCScanner.Advance(); // Skip ']'
        }
        return true;
    }
}
