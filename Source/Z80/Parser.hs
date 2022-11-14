unit Parser
{

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Diagnostics"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Z80/Chunks"
    uses "/Source/Z80/Symbols"
    uses "/Source/Z80/CodeGen"

    <string, string> previousToken;
    <string, string> currentToken;
    < <string, string> > futureTokens;
    
    <uint> methodsToCompile;
    <uint> methodsCompiled;
    <uint> functionsToCompile;
    <uint> functionsCompiled;
    
    <string, <uint> > functionCallPatches;
    <string, <uint> > methodCallPatches;
    
    <string, uint> functionAddresses;
    <string, uint> methodAddresses;
    
    <string, bool> unitsParsed;
    
    bool hadError;
    long embeddedDataSize;
    string embeddedDataPath;
     
    bool automaticUpCast(string actualType, string desiredType)
    {
        bool ok = false;
        loop
        {
            if ((actualType == "byte") && ((desiredType == "int") || (desiredType == "+int") || (desiredType == "-int")))
            {
                ok = true;
            }
            else if ((actualType == "byte") && (desiredType == "uint"))
            {
                ok = true;
            }
            else if ((actualType == "+int") && (desiredType == "uint"))
            {
                ok = true; // not "-int"
            }
            else if ((actualType == "+int") && (desiredType == "int"))
            {
                ok = true;
            }
            else if ((actualType == "-int") && (desiredType == "int"))
            {
                ok = true;
            }
            break;
        }
        return ok;
    }
    
    ProgressTick(string tick)
    {
        Print(tick, Color.MatrixBlue, Color.SlateBlue);
    }
    
    <string> GetSourceList()
    {
        <string> sourceList;
        foreach (var kv in unitsParsed)
        {
            sourceList.Append(kv.key);
        }
        return sourceList;
    }
    
    errorAt(<string, string> token, string message)
    {
        if (!hadError)
        {
            string ln = token["line"];
            string lexeme = token["lexeme"];
            Print("[line " + ln + "]");
            HopperToken ttype = Token.GetType(token);
            if (ttype == HopperToken.EOF)
            {
                Print(" Error at end");
            }
            else if (ttype == HopperToken.Error)
            {
                // nothing
            }
            else if (lexeme.Length > 0)
            {
                Print(" Error at '" + lexeme + "'");
            }
            PrintLn(": " + message);
        }
        hadError = true;
    }
    
    error(string message)
    {
        errorAt(previousToken, message);
    }
    errorAtCurrent(string message)
    {
        errorAt(currentToken, message);
    }
    
    ErrorAtCurrent(string message)
    {
        errorAtCurrent(message);
    }

    reset()
    {
        <string, string> empty;
        previousToken = empty;
        currentToken = empty;
        futureTokens.Clear();
    }
    
    nextToken()
    {
        if (futureTokens.Length == 0)
        {
            currentToken = Scanner.Next();
        }
        else
        {
            currentToken = futureTokens.GetItem(0);
            futureTokens.Remove(0);
        }
    }
    <string, string> peekToken()
    {
        <string, string> pk = Scanner.Next();
        futureTokens.Append(pk);
        return pk;
    }
    advance()
    {
        previousToken = currentToken;
        loop
        {
            nextToken();
            HopperToken ttype = Token.GetType(currentToken);
            if (ttype != HopperToken.Error)
            {
                break;
            }
            errorAtCurrent(currentToken["lexeme"]); // lexeme of Error token is the message
        }
    }
    
    consume(HopperToken consumeType, string message)
    {
        consume(consumeType, "", message);
    }
    consume(HopperToken consumeType, string keyword, string message)
    {
        loop
        {
            HopperToken ttype = Token.GetType(currentToken);
            if (ttype == consumeType)
            {   
                if (ttype == HopperToken.Keyword)
                {
                    keyword = "|" + keyword +"|";
                    string search = "|" + currentToken["lexeme"] +"|";
                    if (keyword.Contains(search))
                    {
                        advance();
                        break;
                    }
                }
                else
                {
                    advance();
                    break;
                }
            }
            errorAtCurrent(message);
            break;
        }
    }
    bool check(HopperToken checkType)
    {
        return check(checkType, "");
    }
    bool check(HopperToken checkType, string keyword)
    {
        bool result = false;
        loop
        {
            HopperToken ttype = Token.GetType(currentToken);
            if (ttype == HopperToken.Keyword)
            {
                result = (checkType == ttype);
                if (result)
                {
                    keyword = "|" + keyword +"|";
                    string search = "|" + currentToken["lexeme"] +"|";
                    result = keyword.Contains(search);
                }
            }
            else if ((ttype == HopperToken.Identifier) && (keyword.Length != 0))
            {
                result = (checkType == ttype);
                if (result)
                {
                    result = keyword == currentToken["lexeme"];
                }
            }
            else
            {
                result = checkType == ttype;
            }
            break;
        }
        return result;
    }
    
    byte makeConstant(string name, int value)
    {
        int constant = Symbols.WriteConstant(name, value);
        if (constant > 255)
        {
            error("Too many 'int' constants.");
            constant = 0;
        }
        return byte(constant);
    }
    byte makeConstant(string name, string value)
    {
        int constant = Symbols.WriteConstant(name, value);
        if (constant > 255)
        {
            error("Too many 'string' constants.");
            constant = 0;
        }
        return byte(constant);
    }
    byte makeConstant(string name, byte value)
    {
        int constant = Symbols.WriteConstant(name, value);
        if (constant > 255)
        {
            error("Too many 'byte' constants.");
            constant = 0;
        }
        return byte(constant);
    }
    < <string> > argumentDeclaration()
    {
        < <string> > arguments;
        <string> argumentNames;
        loop
        {
            if (!check(HopperToken.LParen))
            {
                error("'(' expected");
                break;
            }
            advance();
            loop
            {
                if (check(HopperToken.RParen))
                {
                    advance(); // )
                    break; // done
                }
                if (arguments.Length != 0)
                {
                    if (!check(HopperToken.Comma))
                    {
                        error("',' expected");
                        break;
                    }
                    advance();
                }
                
                if (!check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
                {
                    error("type keyword expected");
                    break;
                }
                advance();
                string ttype = previousToken["lexeme"];
                if (!check(HopperToken.Identifier))
                {
                    error("identifier expected");
                    break;
                }
                advance();
                string identifier = previousToken["lexeme"];
                if (argumentNames.Contains(identifier))
                {
                    error("argument '" + identifier + "' already exists");
                    break;
                }
                argumentNames.Append(identifier);
                <string> argument;
                argument.Append(ttype);
                argument.Append(identifier);
                arguments.Append(argument);
            }
            break;
        }
        return arguments;
    }
    <string> walkBlock()
    {
        <string> blockPos;
        loop
        {
            if (!check(HopperToken.LBrace))
            {
                error("'{' expected");
                break;
            }
            blockPos.Append(currentToken["pos"]);
            blockPos.Append(currentToken["line"]);
            blockPos.Append(currentToken["source"]);
            advance();
            int nested = 1;
            loop
            {
                if (hadError)
                {
                    error("'}' expected");
                    break;
                }
                if (check(HopperToken.RBrace))
                {
                    advance(); // }
                    nested--;
                    if (nested == 0)
                    {
                        break;
                    }
                    continue;
                }
                if (check(HopperToken.LBrace))
                {
                    // nested { }
                    nested++;
                }
                advance(); // anything else
            }
            break;
        }
        return blockPos;
    }
    methodDeclaration()
    {
        loop
        {
            if (!check(HopperToken.Identifier))
            {
                error("identifier expected");
                break;
            }
            advance();
            string identifier = previousToken["lexeme"];
            if (Token.IsReservedWord(identifier))
            {
                error("identifier '" + identifier + "' is a reserved word");
                break;
            }
            < <string> > arguments = argumentDeclaration();
            if (hadError)
            {
                break;
            }
            <string> blockPos = walkBlock();
            if (hadError)
            {
                break;
            }
            uint fIndex = AddMethod(identifier, arguments, blockPos);
            break;
        }
    }
    mainmethodDeclaration()
    {
        loop
        {
            <string> blockPos = walkBlock();
            if (hadError)
            {
                break;
            }
            < <string> > arguments;
            uint fIndex = AddMethod("main", arguments, blockPos);
            break;
        }
    }
    functionDeclaration(string returnType, string identifier)
    {
        loop
        {
            < <string> > arguments = argumentDeclaration();
            if (hadError)
            {
                break;
            }
            <string> blockPos = walkBlock();
            if (hadError)
            {
                break;
            }
            uint fIndex = AddFunction(identifier, arguments, returnType, blockPos);
            break;
        }
    }
    string compileConstantPrimary(string expectedType)
    {
        // primary        → IDENTIFIER | NUMBER | STRING | "true" | "false" 
        // TODO :                 | "(" expression ")" ;
        string value;
        string actualType;
        HopperToken ttype = Token.GetType(currentToken);
        loop
        {
            switch (ttype)
            {
                case HopperToken.Bool:
                {
                    if (currentToken["lexeme"] == "true")
                    {
                        value = "true";
                        advance();
                    }
                    else if (currentToken["lexeme"] == "false")
                    {
                        value = "false";
                        advance();
                    }
                    else
                    {
                        errorAtCurrent("unexpected boolean literal");
                        break;
                    }
                    actualType = "bool";
                }
                case HopperToken.StringConstant:
                {
                    advance();
                    value = currentToken["lexeme"];
                    actualType = "string";
                }
                case HopperToken.Char:
                {
                    advance();
                    value = currentToken["lexeme"];
                    actualType = "char";
                }
                case HopperToken.Integer:
                {
                    long l;
                    if (!Token.TryParseLong(currentToken["lexeme"], ref l))
                    {
                        errorAtCurrent("invalid integer literal");
                        break;
                    }
                    if ((expectedType == "int") || (expectedType == "+int") || (expectedType == "-int"))
                    {
                        if ((l < -32768) || (l > 32767))
                        {
                            errorAtCurrent("'int' constant out of range");
                            break;
                        }
                        if (l < 0)
                        {
                            actualType = "-int"; // -ve int
                        }
                        else
                        {
                            actualType = "+int";
                        }
                    }
                    else if (expectedType == "uint")
                    {
                        if ((l < 0) || (l > 0xFFFF))
                        {
                            errorAtCurrent("'uint' constant out of range");
                            break;
                        }
                        actualType = "uint";
                    }
                    else if (expectedType == "byte")
                    {
                        if ((l < 0) || (l > 255))
                        {
                            errorAtCurrent("'byte' constant out of range");
                            break;
                        }
                        actualType = "byte";
                    }
                    value = currentToken["lexeme"];
                    advance();
                }
                case HopperToken.Identifier:
                {
                    switch (expectedType)
                    {
                        case "byte":
                        {
                            byte bvalue;
                            if (!ResolveConstant(currentToken["lexeme"], ref bvalue))
                            {
                                errorAtCurrent("'byte' constant identifier expected");
                            }
                            value = bvalue.ToString();
                            actualType = "byte";
                            advance();
                        }
                        case "uint":
                        {
                            uint uivalue;
                            if (!ResolveConstant(currentToken["lexeme"], ref uivalue))
                            {
                                errorAtCurrent("'uint' constant identifier expected");
                            }
                            value = uivalue.ToString();
                            actualType = "uint";
                            advance();
                        }
                        default:
                        {
                            if ((expectedType == "int") || (expectedType == "+int") || (expectedType == "-int"))
                            {
                                int ivalue;
                                if (!ResolveConstant(currentToken["lexeme"], ref ivalue))
                                {
                                    errorAtCurrent("'int' constant identifier expected");
                                }
                                value = ivalue.ToString();
                                if (ivalue < 0)
                                {
                                    actualType = "-int";
                                }
                                else
                                {
                                    actualType = "+int";
                                }
                                advance();
                            }
                            else
                            {
                                errorAtCurrent("constant identifier case not implemented");
                                break;
                            }
                        }
                    }
                }
                default:
                {
                    errorAtCurrent("constant expected");
                    break;
                }
            }
            break;
        }
        if (expectedType != actualType)
        {
            errorAtCurrent("expected '" + expectedType + "' constant expression, (was '" + actualType + "')");
        }
        return value;
    }
    string compileConstantExpression(string expectedType)
    {
        return compileConstantPrimary(expectedType);
    }
    constOrGlobalDeclaration(string gtype, string identifier, bool global)
    {
        loop
        {
            string value;
            if (!check(HopperToken.Assign))
            {
                if (!global)
                {
                    error("'=' expected");
                    break;
                }
            }
            else
            {
                // has initial value
                advance(); // =
                switch (gtype)
                {
                    case "bool":
                    {
                        value = compileConstantExpression("bool");
                    }
                    case "string":
                    {
                        value = compileConstantExpression("string");
                    }
                    case "byte":
                    {
                        if (check(HopperToken.Add))
                        {
                            advance();
                        }
                        value = compileConstantExpression("byte");
                    }
                    case "uint":
                    {
                        if (check(HopperToken.Add))
                        {
                            advance();
                        }
                        value = compileConstantExpression("uint");
                    }
                    case "int":
                    {
                        string sign;
                        string itype = "+int";
                        if (check(HopperToken.Add))
                        {
                            advance();
                        }
                        else if (check(HopperToken.Subtract))
                        {
                            advance();
                            sign = "-";
                            itype = "-int";
                        }
                        value = sign + compileConstantExpression(itype);
                    }
                    case "char":
                    {
                        value = compileConstantExpression("string");
                    }
                    
                }
                if (hadError)
                {
                    break;
                }
                
            }
            if (!check(HopperToken.SemiColon))
            {
                error("';' expected");
                break;
            }
            advance();
            if (Symbols.SymbolExists(identifier))
            {
                error("symbol '" + identifier + "' already exists");
                break;
            }
            if (global)
            {
                AddGlobal(identifier, gtype, value);
            }
            else
            {
                uint index;
                switch (gtype)
                {
                    case "bool":
                    {
                        bool bvalue = false;
                        if (value == "true")
                        {
                            bvalue = true;
                        }
                        index = Symbols.WriteConstant(identifier, bvalue);
                    }
                    case "byte":
                    {
                        uint i;
                        if (Token.TryParseUInt(value, ref i))
                        {
                        }
                        byte b = byte(i);
                        index = Symbols.WriteConstant(identifier, b);
                    }
                    case "uint":
                    {
                        uint ui;
                        if (Token.TryParseUInt(value, ref ui))
                        {
                        }
                        index = Symbols.WriteConstant(identifier, ui);
                    }
                    case "string":
                    {
                        index = Symbols.WriteConstant(identifier, value);
                    }
                    case "char":
                    {
                        char c = value[0];
                        index = Symbols.WriteConstant(identifier, c);
                    }
                    default:
                    {
                        if ((gtype == "int") || (gtype == "+int") || (gtype == "-int"))
                        {
                            int i;
                            if (Token.TryParseInt(value, ref i))
                            {
                            }
                            index = Symbols.WriteConstant(identifier, i);
                        }
                    }
                }
                if (index > 255)
                {
                    error("limit of 256 " + gtype + " constants exceeded");
                }
            }
            break;
        }
    }
    constDeclaration()
    {
        loop
        {
            advance(); // "const"
            if (!check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
            {
                error("type keyword expected");
                break;
            }
            advance();
            string ttype = previousToken["lexeme"];
            if (!check(HopperToken.Identifier))
            {
                error("identifier expected");
                break;
            }
            advance();
            string identifier = previousToken["lexeme"];
            if (Token.IsReservedWord(identifier))
            {
                error("identifier '" + identifier + "' is a reserved word");
                break;
            }
            constOrGlobalDeclaration(ttype, identifier, false);
            break;
        }
    }
    globalDeclaration(string gtype, string identifier)
    {
        constOrGlobalDeclaration(gtype, identifier, true);
    }
    
    usesUnit()
    {
        loop
        {
            advance();
            if (!check(HopperToken.StringConstant))
            {
                error("path of unit source expected");
                break;
            }
            advance();
            string hsPath = previousToken["lexeme"];
            string hsPathLower = hsPath.ToLower();
            if (!hsPathLower.EndsWith(".zs"))
            {
                hsPath = hsPath + ".zs";
            }
            if (!File.Exists(hsPath))
            {
                error("'" + hsPath + "' not found");
                break;
            }
            hsPathLower = hsPath.ToLower();
            if (!unitsParsed.Contains(hsPathLower))
            {
                unitsParsed[hsPathLower] = false; // false means we're aware of it but we haven't parsed it yet
            }
            break;
        }
    }
    
    embedData()
    {
        loop
        {
            advance();
            if (!check(HopperToken.StringConstant))
            {
                error("path of .hexe expected");
                break;
            }
            advance();
            string hexePath = previousToken["lexeme"];
            if (!File.Exists(hexePath))
            {
                error("'" + hexePath + "' not found");
                break;
            }
            if (embeddedDataSize != 0)
            {
                error("only a single 'embed' is allowed");
                break;
            }
            embeddedDataPath = hexePath;
            embeddedDataSize = File.GetSize(hexePath);
            break;
        }
    }
    
    functionOrGlobalDeclaration()
    {
        loop
        {
            if (!check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
            {
                error("type keyword expected");
                break;
            }
            advance();
            string ltype = previousToken["lexeme"];
            if (!check(HopperToken.Identifier))
            {
                error("identifier expected");
                break;
            }
            advance();
            string identifier = previousToken["lexeme"];
            if (Token.IsReservedWord(identifier))
            {
                error("identifier '" + identifier + "' is a reserved word");
                break;
            }
            if (check(HopperToken.Assign) || check(HopperToken.SemiColon))
            {
                globalDeclaration(ltype, identifier);
                break;
            }
            else if (check(HopperToken.LParen))
            {
                
                functionDeclaration(ltype, identifier);
                break;
            }
            else
            {
                error("'(', ';' or '=' expected (function or global variable)");
                break;
            }
        }
    }
    
    declaration()
    {
        if (check(HopperToken.LBrace))
        {
            mainmethodDeclaration();
        }
        else if (check(HopperToken.Keyword, "const"))
        {
            constDeclaration();
        }
        else if (check(HopperToken.Keyword, "embed"))
        {
            embedData();
        }
        else if (check(HopperToken.Keyword, "uses"))
        {
            usesUnit();
        }
        else if (check(HopperToken.Identifier))
        {
            methodDeclaration();
        }
        else if (check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
        {
            functionOrGlobalDeclaration();
        }
        else
        {
            error("function, method or global declaration expected.");
        }
    }
    
    // consume(..) : consume the token and error if it is not a match
    // check(..)   : check against the currentToken, true if winner
    // advance(..) : consume the next token (typically after check(..))
    
    bool BuildSymbols(string sourcePath)
    {
        unitsParsed[sourcePath] = true; // make sure program is included in source paths
        
        Scanner.New();
        hadError = false;
        bool firstUnit = true;
        loop
        {
            Scanner.Load(sourcePath);
            long pos = 0;
            Reset(pos, 1, sourcePath);
            
            advance(); // load up the first token
            if (firstUnit)
            {
                consume(HopperToken.Keyword, "program", "'program' expected");
                consume(HopperToken.Identifier, "Program name identifier expected");
            }
            else
            {
                consume(HopperToken.Keyword, "unit", "'unit' expected");
                consume(HopperToken.Identifier, "Unit name identifier expected");
            }
            consume(HopperToken.LBrace, "'{' expected");
            
            firstUnit = false;
            bool endedProperly = false;
            PrintLn();
            loop
            {
                if (check(HopperToken.EOF))
                {
                    break;
                }
                if (check(HopperToken.RBrace))
                {   
                    endedProperly = true;
                    break;
                }
                if (hadError)
                {
                    break;
                }
                declaration();
                Print(".", Color.MatrixBlue, Color.PaleBlue);
            }
            
            if (hadError)
            {
                // already seen an error message
            }
            else if (!endedProperly)
            {
                // can't 'consume' at the end of the file
                error("'}' expected'");
            }
            else
            {
                uint mIndex;
                if (!Symbols.GetMethodIndex("main", ref mIndex))
                {
                    error("program requires entry point");
                }
            }
            // any more units to parse?
            sourcePath = "";
            foreach (var kv in unitsParsed)
            {
                if (!kv.value)
                {
                    // not yet parsed
                    sourcePath = kv.key;
                    unitsParsed[sourcePath] = true;
                    break;
                }
            }
            if (sourcePath == "")
            {
                break;
            }
        }
        return hadError;
    }
    
    compileFunctionCall(<string, string> function)
    {
        loop
        {
            consume(HopperToken.LParen, "'(' expected");
            if (hadError)
            {
                break;
            }
                        
            < <string> > parameters = GetFunctionArguments(function["name"]);
            bool first = true;
            foreach (var parameter in parameters)
            {
                if (!first)
                {
                    consume(HopperToken.Comma, "',' expected");
                    if (hadError)
                    {
                        break;
                    }
                }
                string expectedType = parameter[0];
                string actualType = compileExpression(expectedType);
                if (actualType != expectedType)
                {
                    // bool automaticUpCast(string actualType, string desiredType)
                    if (automaticUpCast(actualType, expectedType))
                    {
                        // ok
                    }
                    else
                    {
                        error("type mismatch in parameter, '" + expectedType + "' expected (was '" + actualType + "')");
                    }
                }
                
                first = false;
            }
            if (hadError)
            {
                break;
            }
            consume(HopperToken.RParen, "')' expected");
            if (hadError)
            {
                break;
            }
            
            string functionName = function["name"];
            
            Chunks.WriteChunk(OpCode.CALL, currentToken);
            uint functionCallPatch = Chunks.GetNextAddress();
            Chunks.WriteChunk(0, currentToken);
            Chunks.WriteChunk(0, currentToken);
            Chunks.UpdatePeepholeBoundary();
            uint fIndex;
            bool found = false;
            if (GetFunctionIndex(functionName, ref fIndex))
            {
                foreach (var i in functionsToCompile)
                {
                    if (i == fIndex)
                    {
                        found = true;
                        break;
                    }
                }
                foreach (var i in functionsCompiled)
                {
                    if (i == fIndex)
                    {
                        found = true;
                        break;
                    }
                }
            }
            if (!found)
            {
                functionsToCompile.Append(fIndex);
            }
            
            if (!functionCallPatches.Contains(functionName))
            {
                <uint> empty;
                functionCallPatches[functionName] = empty;
            }
            <uint> patchList = functionCallPatches[functionName];
            patchList.Append(functionCallPatch);
            functionCallPatches[functionName] = patchList;
            
            // push the return value onto the stack
            Chunks.WriteChunk(OpCode.PUSHHL, currentToken);
            break;
        }
    }
    
    compileMethodCall(<string, string> method)
    {
        string identifier = currentToken["lexeme"];
        loop
        {
            //PrintLn(Token.ToString(previousToken));
            //PrintLn(Token.ToString(currentToken));
            advance();
            consume(HopperToken.LParen, "'(' expected");
            if (hadError)
            {
                break;
            }
                        
            < <string> > parameters = GetMethodArguments(method["name"]);
            bool first = true;
            foreach (var parameter in parameters)
            {
                if (!first)
                {
                    consume(HopperToken.Comma, "',' expected");
                    if (hadError)
                    {
                        break;
                    }
                }
                string expectedType = parameter[0];
                string actualType = compileExpression(expectedType);
                if (actualType != expectedType)
                {
                    if (automaticUpCast(actualType, expectedType))
                    {
                        actualType = expectedType;
                    }
                    else
                    {
                        error("type mismatch for parameter, '" + expectedType + "' expected (was '" + actualType + "')");
                    }
                }
                first = false;
            }
            if (hadError)
            {
                break;
            }
            consume(HopperToken.RParen, "')' expected");
            if (hadError)
            {
                break;
            }
            string methodName = method["name"];
            
            Chunks.WriteChunk(OpCode.CALL, currentToken);
            uint methodCallPatch = Chunks.GetNextAddress();
            Chunks.WriteChunk(0, currentToken);
            Chunks.WriteChunk(0, currentToken);
            Chunks.UpdatePeepholeBoundary();
            
            uint mIndex;
            bool found = false;
            if (GetMethodIndex(methodName, ref mIndex))
            {
                foreach (var i in methodsToCompile)
                {
                    if (i == mIndex)
                    {
                        found = true;
                        break;
                    }
                }
                foreach (var i in methodsCompiled)
                {
                    if (i == mIndex)
                    {
                        found = true;
                        break;
                    }
                }
            }
            if (!found)
            {
                methodsToCompile.Append(mIndex);
            }
            
            if (!methodCallPatches.Contains(methodName))
            {
                <uint> empty;
                methodCallPatches[methodName] = empty;
            }
            <uint> patchList = methodCallPatches[methodName];
            patchList.Append(methodCallPatch);
            methodCallPatches[methodName] = patchList;
            
            consume(HopperToken.SemiColon, "';' expected");
            if (hadError)
            {
                break;
            }
            
            break;
        }
    }
   
    compileAssignMemoryOrPort()
    {
        loop
        {
            string deviceName = currentToken["lexeme"];
            bool isPort = (deviceName == "port");
            bool isWord = (deviceName == "memoryword");
            bool simpleIY = false;
            int delta = 0;
            advance();
            <string,string> sourceToken = currentToken;
            consume(HopperToken.LBracket, "'[' expected");
            if (hadError)
            {
                break;
            }
            
            if (!isPort)
            {
                simpleIY = TryConsumeSimpleIY(ref delta);
            }
            if (!simpleIY)
            {
                string indexType = compileExpression("uint");
                if (hadError)
                {
                    break;
                }
                if (isPort)
                {
                    if (indexType != "byte")
                    {
                        errorAtCurrent("index for port should be 'byte' expression, (was '" + indexType + "')");
                        break;
                    }
                }
                else
                {
                    if ((indexType != "uint") && (indexType != "byte"))
                    {
                        errorAtCurrent("index for memory should be 'uint' expression, (was '" + indexType + "')");
                        break;
                    }
                }
            }
            
            consume(HopperToken.RBracket, "']' expected");
            if (hadError)
            {
                break;
            }
            if (check(HopperToken.Assign))
            {
                advance(); // =
                string expectedType = "byte";
                if (isWord)
                {
                    expectedType = "uint";
                }
                string valueType = compileExpression(expectedType);
                if (expectedType == "byte")
                {
                    if (valueType != "byte")
                    {
                        errorAtCurrent("'byte' expression expected for " + deviceName + ", (was '" + valueType + "')");
                        break;
                    }
                }
                else // word
                {
                    if ((valueType != "byte") && (valueType != "+int") && (valueType != "uint"))
                    {
                        errorAtCurrent("'uint' expression expected for " + deviceName + ", (was '" + valueType + "')");
                        break;
                    }
                }
                if (isPort)
                {
                    CodeGen.WritePort(sourceToken);
                }
                else if (simpleIY)
                {
                    CodeGen.WriteMemoryIY(sourceToken, isWord, delta);
                }
                else
                {
                    CodeGen.WriteMemory(sourceToken, isWord);
                }
            }
            else if (isPort)
            {
                errorAtCurrent("'=' expected");
            }
            else if (check(HopperToken.Increment))
            {
                advance(); // ++
                if (simpleIY)
                {
                    errorAtCurrent("'memory[iy]++' not implemented");
                }
                CodeGen.WriteMemoryIncDec(sourceToken, isWord, true);
            }
            else if (check(HopperToken.Decrement))
            {
                advance(); // --
                if (simpleIY)
                {
                    errorAtCurrent("'memory[iy]--' not implemented");
                }
                CodeGen.WriteMemoryIncDec(sourceToken, isWord, false);
            }
            else
            {
                errorAtCurrent("'=', '++' or '--' expected");
            }
            
            
            consume(HopperToken.SemiColon, "';' expected");
            break;
        }
    }
    string compileSP(string expectedType)
    {
        string actualType = "uint";
        WriteChunk(OpCode.LDIY, currentToken);
        WriteChunk(0, currentToken);
        WriteChunk(0, currentToken);
        WriteChunk(OpCode.ADDIYSP, currentToken);
        WriteChunk(OpCode.PUSHIY, currentToken);
        advance(); // sp
        return actualType;
    }
    string compileIY(string expectedType)
    {
        <string,string> sourceToken = currentToken;
        string actualType = "uint";
        
        // iy -> [top]
        WriteChunk(OpCode.PUSHIY, sourceToken);
        advance(); // iy
        return actualType;
    }
    string compilePC(string expectedType)
    {
        <string,string> sourceToken = currentToken;
        string actualType = "uint";
        
        // bc -> [top]
        WriteChunk(OpCode.PUSHBC, sourceToken);
        advance(); // bc
        return actualType;
    }
    
    compileAssignPC()
    {
        loop
        {
            <string,string> sourceToken = currentToken;
            advance(); // pc
            if (check(HopperToken.Assign))
            {
                advance(); // =
                string expectedType = "uint";
                string valueType = compileExpression(expectedType);
                if (hadError)
                {
                    break;
                }
                if ((valueType != "byte") && (valueType != "+int") && (valueType != "uint"))
                {
                    errorAtCurrent("'uint' expression expected for 'pc', (was '" + valueType + "')");
                    break;
                }
                // [top] -> iy
                WriteChunk(OpCode.POPBC, sourceToken);
            }
            else if (check(HopperToken.Increment))
            {
                advance(); // ++
                WriteChunk(OpCode.INCBC, sourceToken);
            }
            else if (check(HopperToken.Decrement))
            {
                advance(); // --
                WriteChunk(OpCode.DECBC, sourceToken);
            }
            else
            {
                errorAtCurrent("'=', '++' or '--' expected");
            }
            consume(HopperToken.SemiColon, "';' expected");
            break;
        }
    }
    
    compileAssignIY()
    {
        loop
        {
            <string,string> sourceToken = currentToken;
            advance(); // iy
            if (check(HopperToken.Assign))
            {
                advance(); // =
                string expectedType = "uint";
                string valueType = compileExpression(expectedType);
                if (hadError)
                {
                    break;
                }
                if ((valueType != "byte") && (valueType != "+int") && (valueType != "uint"))
                {
                    errorAtCurrent("'uint' expression expected for 'iy', (was '" + valueType + "')");
                    break;
                }
                // [top] -> iy
                WriteChunk(OpCode.POPIY, sourceToken);
            }
            else if (check(HopperToken.Increment))
            {
                advance(); // ++
                WriteChunk(OpCode.INCIY, sourceToken);
            }
            else if (check(HopperToken.Decrement))
            {
                advance(); // --
                WriteChunk(OpCode.DECIY, sourceToken);
            }
            else
            {
                errorAtCurrent("'=', '++' or '--' expected");
            }
            consume(HopperToken.SemiColon, "';' expected");
            break;
        }
    }
    string compileLine(string expectedType)
    {
        string actualType = "uint";
        loop
        {
            byte lsb = 0;
            byte msb = 0;
            if (!Chunks.ConvertOffsetToBytes(currentToken["line"], ref msb, ref lsb))
            {
                errorAtCurrent("invalid integer literal");
                break;
            }
            WriteChunk(OpCode.LDHL, currentToken);
            WriteChunk(lsb, currentToken);
            WriteChunk(msb, currentToken);
            WriteChunk(OpCode.PUSHHL, currentToken);
            advance(); // line
            break;
        }
        return actualType;
    }
    string compileSource(string expectedType)
    {
        string actualType = "string";
        loop
        {
            string sourcePath = currentToken["source"];
            
            // TODO : hard to generate a string constant here
            // - StringNew() followed by lots of calls to StringAppend()?
            WriteChunk(OpCode.LDHL, currentToken);
            WriteChunk(0, currentToken);
            WriteChunk(0, currentToken);
            WriteChunk(OpCode.PUSHHL, currentToken);
            advance(); // source
            break;
        }
        return actualType;
    }
    
    bool TryConsumeSimpleIY(ref int delta)
    {
        bool simpleIY = false;
        if (check(HopperToken.Identifier, "iy"))
        {
            <string, string> sg = peekToken();
            HopperToken sgt = Token.GetType(sg);
            if (sgt == HopperToken.RBracket)
            {
                simpleIY = true;
                delta = 0;
                advance(); // iy
            }
            else if ((sgt == HopperToken.Add) || (sgt == HopperToken.Subtract))
            {
                <string, string> deltaToken = peekToken();
                HopperToken deltaType = Token.GetType(deltaToken);
                if (deltaType == HopperToken.Integer)
                {
                    <string, string> rb = peekToken();
                    HopperToken rbt = Token.GetType(rb);
                    if (rbt == HopperToken.RBracket)
                    {
                        int di;
                        if (Token.TryParseInt(deltaToken["lexeme"], ref di))
                        {
                            if ((di >= 0) && (di <= 128))
                            {
                                simpleIY = true;
                                
                                if (sgt == HopperToken.Subtract)
                                {
                                    delta = -di;
                                }
                                else
                                {
                                    delta = di;
                                }
                                advance(); // iy
                                advance(); // +-
                                advance(); // int
                            }
                        }
                    }
                }
            }
            else
            {
                PrintLn(sg["lexeme"]);
            }
        }
        return simpleIY;
    }
    
    string compileMemoryOrPort(string expectedType)
    {
        string actualType = "byte";
        loop
        {
            bool simpleIY;
            int  delta;
            bool isPort = (currentToken["lexeme"] == "port");
            bool isWord = (currentToken["lexeme"] == "memoryword");
            if (isWord)
            {
                actualType = "uint";
            }
            advance();
            <string,string> sourceToken = currentToken;
            
            consume(HopperToken.LBracket, "'[' expected");
            if (hadError)
            {
                break;
            }
            if (!isPort)
            {
                simpleIY = TryConsumeSimpleIY(ref delta);
            }
            
            if (!simpleIY)
            {
                string indexType = compileExpression("uint");
                if (isPort)
                {
                    if (indexType != "byte")
                    {
                        errorAtCurrent("index for port should be 'byte' expression, (was '" + indexType + "')");
                        break;
                    }
                    CodeGen.ReadPort(sourceToken);
                }
                else
                {
                    if ((indexType != "uint") && (indexType != "byte"))
                    {
                        errorAtCurrent("index for memory should be 'uint' expression, (was '" + indexType + "')");
                        break;
                    }
                    CodeGen.ReadMemory(sourceToken, isWord);
                }
            }
            else
            {
                CodeGen.ReadMemoryIY(sourceToken, isWord, delta);
            }
            consume(HopperToken.RBracket, "']' expected");
            if (hadError)
            {
                break;
            }
            break;
        }
        return actualType;
    }
    
    string compilePrimary(string expectedType)
    {
        // primary        → IDENTIFIER | NUMBER | STRING | "true" | "false" | "(" expression ")" ;
        string actualType;
        HopperToken ttype = Token.GetType(currentToken);
        loop
        {
            switch (ttype)
            {
                case HopperToken.Bool:
                {
                    <string,string> sourceToken = currentToken;
                    bool value = false;
                    if (currentToken["lexeme"] == "true")
                    {
                        value = true;
                    }
                    else if (currentToken["lexeme"] == "false")
                    {
                    }
                    else
                    {
                        errorAtCurrent("unexpected boolean literal");
                        break;
                    }
                    CodeGen.PushBoolean(value, sourceToken);
                    advance();
                    actualType = "bool";
                }
                case HopperToken.Char:
                {
                    <string,string> sourceToken = currentToken;
                    string value = currentToken["lexeme"];
                    char c = value[0];
                    CodeGen.PushByte(byte(c), sourceToken);
                    advance();
                    actualType = "char";
                }
                case HopperToken.Integer:
                {
                    <string,string> sourceToken = currentToken;
                    byte lsb = 0;
                    byte msb = 0;
                    //PrintLn("lexeme=" + currentToken["lexeme"]);
                    if (!Chunks.ConvertOffsetToBytes(currentToken["lexeme"], ref msb, ref lsb))
                    {
                        errorAtCurrent("invalid integer literal");
                        break;
                    }
                    
                    //PrintLn("lsb=" + lsb.ToString());
                    //PrintLn("msb=" + msb.ToString());
                    CodeGen.PushWord(lsb, msb, sourceToken);
                    if (msb == 0)
                    {
                        actualType = "byte";
                    }
                    else 
                    {
                        long l;
                        if (Token.TryParseLong(currentToken["lexeme"], ref l))
                        {
                            if ((l < -32768) || (l > 0xFFFF))
                            {
                                errorAtCurrent("integer literal out of range" );
                                break;
                            }
                        }
                        if (l < 0)
                        {
                            actualType = "-int"; // -ve int
                        }
                        else if (l > 32767)
                        {
                            actualType = "uint";
                        }
                        else if (expectedType == "uint")
                        {
                            actualType = "uint";
                        }
                        else
                        {
                            actualType = "+int";
                        }
                    }
                    advance();
                }
                case HopperToken.StringConstant:
                {
                    // CODEGEN : system call to put new string index on stack?
                    advance();
                    actualType = "string";
                }
                case HopperToken.Identifier:
                {
                    string identifier = currentToken["lexeme"];
                    if (identifier == "memory")
                    {
                        actualType = compileMemoryOrPort(expectedType);
                        break;
                    }
                    else if (identifier == "memoryword")
                    {
                        actualType = compileMemoryOrPort(expectedType);
                        break;
                    }
                    else if (identifier == "port")
                    {
                        actualType = compileMemoryOrPort(expectedType);
                        break;
                    }
                    else if (identifier == "sp")
                    {
                        actualType = compileSP(expectedType);
                        break;
                    }
                    else if (identifier == "iy")
                    {
                        actualType = compileIY(expectedType);
                        break;
                    }
                    else if (identifier == "pc")
                    {
                        actualType = compilePC(expectedType);
                        break;
                    }
                    else if (identifier == "line")
                    {
                        actualType = compileLine(expectedType);
                        break;
                    }
                    else if (identifier == "source")
                    {
                        actualType = compileSource(expectedType);
                        break;
                    }
                    <string,string> variable = Symbols.GetVariable(identifier);
                    if (variable.Count == 0)
                    {
                        <string,string> function = Symbols.GetFunction(identifier);
                        if (function.Count == 0)
                        {
                            bool bbvalue;
                            if (Symbols.ResolveConstant(identifier, ref bbvalue))
                            {
                                CodeGen.PushBoolean(bbvalue, currentToken);
                                actualType = "bool";
                                advance();
                                break;
                            }
                            byte bvalue;
                            if (Symbols.ResolveConstant(identifier, ref bvalue))
                            {
                                CodeGen.PushByte(bvalue, currentToken);
                                actualType = "byte";
                                advance();
                                break;
                            }
                            int ivalue;
                            if (Symbols.ResolveConstant(identifier, ref ivalue))
                            {
                                byte lsb = 0;
                                byte msb = 0;
                                if (!Chunks.ConvertOffsetToBytes(ivalue.ToString(), ref msb, ref lsb))
                                {
                                    errorAtCurrent("invalid 'int' constant");
                                    break;
                                }
                                CodeGen.PushWord(lsb, msb, currentToken);
                                if (ivalue < 0)
                                {
                                    actualType = "-int";
                                }
                                else
                                {
                                    actualType = "+int";
                                }
                                advance();
                                break;
                            }
                            uint uvalue;
                            if (Symbols.ResolveConstant(identifier, ref uvalue))
                            {
                                byte lsb = 0;
                                byte msb = 0;
                                if (!Chunks.ConvertOffsetToBytes(uvalue.ToString(), ref msb, ref lsb))
                                {
                                    errorAtCurrent("invalid 'uint' constant");
                                    break;
                                }
                                CodeGen.PushWord(lsb, msb, currentToken);
                                actualType = "uint";
                                advance();
                                break;
                            }
                            errorAtCurrent("variable or function identifier expected");
                            break;
                        }
                        actualType = function["returntype"];
                        advance();
                        compileFunctionCall(function);
                        break;
                    }
                    advance();
                    actualType = variable["type"];
                    
                    switch (variable["kind"])
                    {
                        case "global":
                        {
                            byte lsb = 0;
                            byte msb = 0;
                            if (Chunks.ConvertOffsetToBytes(variable["offset"], ref msb, ref lsb))
                            {
                            }
                            //PrintLn("PUSH global:" + identifier + " " + lsb.ToString() + " " + msb.ToString());
                            //OutputDebug(variable);
                            WriteChunk(OpCode.LDHLIND, currentToken);
                            WriteChunk(lsb, currentToken);
                            WriteChunk(msb, currentToken);
                            WriteChunk(OpCode.PUSHHL, currentToken);
                        }
                        case "local":
                        {
                            // "local" has -ve offset
                            int lsb = 0;
                            if (Token.TryParseInt(variable["offset"], ref lsb))
                            {
                            }
                            lsb = -lsb;
                            int lsb1 = lsb+1;
                            if (lsb < 0)
                            {
                                lsb = lsb + 256; // -1 -> 255
                            }
                            if (lsb1 < 0)
                            {
                                lsb1 = lsb1 + 256; // -1 -> 255
                            }
                            //PrintLn("PUSH local:" + identifier + " " + lsb.ToString() + " " + lsb1.ToString());
                            //OutputDebug(variable);
                            WriteChunk(OpCode.LDLIXIND, currentToken);
                            WriteChunk(byte(lsb), currentToken);
                            if (actualType != "bool") // boolean doesn't care about MSB
                            {
                                WriteChunk(OpCode.LDHIXIND, currentToken);
                                WriteChunk(byte(lsb1), currentToken);
                            }
                            WriteChunk(OpCode.PUSHHL, currentToken);
                        }
                        case "argument":
                        {
                            // "argument" has +ve offset
                            int lsb = 0;
                            if (Token.TryParseInt(variable["offset"], ref lsb))
                            {
                            }
                            lsb = lsb + 4; // IX and SP
                            int lsb1 = lsb+1;
                            if (lsb < 0)
                            {
                                lsb = lsb + 256; // -1 -> 255
                            }
                            if (lsb1 < 0)
                            {
                                lsb1 = lsb1 + 256; // -1 -> 255
                            }
                            //PrintLn("PUSH argument:" + identifier + " " + variable["offset"] + " " + lsb.ToString() + " " + lsb1.ToString());
                            //OutputDebug(variable);
                            WriteChunk(OpCode.LDLIXIND, currentToken);
                            WriteChunk(byte(lsb), currentToken);
                            if (actualType != "bool") // boolean doesn't care about MSB
                            {
                                WriteChunk(OpCode.LDHIXIND, currentToken);
                                WriteChunk(byte(lsb1), currentToken);
                            }
                            WriteChunk(OpCode.PUSHHL, currentToken);
                        }
                    }
                }
                case HopperToken.Keyword:
                {
                    string castToType = currentToken["lexeme"];
                    if ((castToType == "char") || (castToType == "byte") || 
                    (castToType == "int") ||(castToType == "+int") ||(castToType == "-int") || (castToType == "uint"))
                    {
                        advance();
                        consume(HopperToken.LParen, "'(' expected for cast expression");
                        string castExpressionType = compileExpression(castToType);
                        if (automaticUpCast(castExpressionType, castToType))
                        {
                            // no brainers like int(byte) and uint(byte)
                        }
                        else if ((castToType == "byte") && (castExpressionType == "char"))
                        {
                            // still a NOP for CODEGEN but user should make this cast conciously
                        }
                        else if ((castToType == "char") && (castExpressionType == "byte"))
                        {
                            // still a NOP for CODEGEN but user should make this cast conciously
                        }
                        else if ((castToType == "char") && (castExpressionType == "uint"))
                        {
                            // still a NOP for CODEGEN but user should make this cast conciously
                            // TODO: runtime range check
                        }
                        else if ((castToType == "byte") && (castExpressionType == "uint"))
                        {
                            // still a NOP for CODEGEN but user should make this cast conciously
                            // TODO: runtime range check
                        }
                        else
                        {
                            errorAtCurrent("cast from '" + castExpressionType + "' to '" + castToType + "' is illegal");
                            break;
                        }
                        consume(HopperToken.RParen, "')' expected");
                        actualType = castToType;
                        break;
                    }
                    
                    PrintLn(Token.ToString(previousToken));
                    PrintLn(Token.ToString(currentToken));
                    errorAtCurrent("TODO compilePrimary HopperToken.Keyword not implemented");
                }
                case HopperToken.LParen:
                {
                    advance();
                    actualType = compileExpression(expectedType);
                    if (hadError)
                    {
                        break;
                    }
                    consume(HopperToken.RParen, "')' expected");
                }
                default:
                {
                    PrintLn(Token.ToString(previousToken));
                    PrintLn(Token.ToString(currentToken));
                    
                    errorAtCurrent("TODO compilePrimary not implemented");
                }
            }
            break;
        }
        return actualType;
    }
    
    
    
    string compileUnary(string expectedType)
    {
        // unary          → ( "!" | "-" | "+" | "++" | "--" | "~" ) unary     - right to left
        //                 | primary ;
        string actualType;
        loop
        {
            HopperToken unary = HopperToken.Undefined;
            <string,string> sourceToken = currentToken;
            if (check(HopperToken.Subtract))
            {
                advance();
                unary = HopperToken.Subtract;
            }
            else if (check(HopperToken.BooleanNot))
            {
                advance();
                unary = HopperToken.BooleanNot;
            }
            else if (check(HopperToken.BitNot))
            {
                PrintLn(Token.ToString(previousToken));
                PrintLn(Token.ToString(currentToken));
                
                errorAtCurrent("TODO compileUnary not implemented");
            }
            actualType = compilePrimary(expectedType);
            if (hadError)
            {
                break;
            }
            if (unary == HopperToken.Subtract)
            {
                if ((actualType != "int") && (actualType != "-int") && (actualType != "+int"))
                {
                    if (automaticUpCast(actualType, "int"))
                    {
                        actualType = "int";
                    }
                    else
                    {
                        errorAtCurrent("type mismatch, 'int' expected (was '" + actualType + "') F");
                        break;
                    }
                }
                // integer Subtract
                CodeGen.IntegerUnaryMinus(sourceToken);
            }
            else if (unary == HopperToken.BooleanNot)
            {
                if (actualType != "bool")
                {
                    errorAtCurrent("type mismatch, 'bool' expected");
                    break;
                }
                CodeGen.BooleanNot(sourceToken);
            }
            break;
        }
        return actualType;
    }
    string compileFactor(string expectedType)
    {
        // TODO : factor         → typecast( ( "/" | "*" | "%" ) typecast )* ;
        // Division: 
        //    https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Division#16.2F16_division   signed?
        //    https://map.grauw.nl/articles/mult_div_shifts.php#div
        // Multiplication:
        //    https://map.grauw.nl/articles/mult_div_shifts.php#mult
        
        string actualType;
        loop
        {
            actualType = compileUnary(expectedType);
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.Multiply) || check(HopperToken.Divide)|| check(HopperToken.Modulus))
                {
                    <string,string> sourceToken = currentToken;
                    advance(); // * or / or %
                    HopperToken operation = Token.GetType(previousToken);
                    if (operation == HopperToken.Multiply)
                    {
                        HopperToken contantTokenType = Token.GetType(currentToken);
                        if (contantTokenType == HopperToken.Integer)
                        {
                            string constantInteger = currentToken["lexeme"];
                            if ((constantInteger == "0") 
                             || (constantInteger == "1") 
                             || (constantInteger == "2") 
                             || (constantInteger == "3") 
                             || (constantInteger == "4")
                             || (constantInteger == "5")
                             )
                            {
                                advance();
                                IntegerMultiplyConstant(constantInteger, sourceToken);
                                continue;
                            }
                        }
                    }
                            
                    string rightType = compileUnary(expectedType);
                    if (actualType != rightType)
                    {
                        if (automaticUpCast(rightType, actualType))
                        {
                            rightType = actualType;
                        }
                        else
                        {
                            errorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "') F");
                        }
                    }
                    if (hadError)
                    {
                        break;
                    }
                    switch (operation)
                    {
                        case HopperToken.Multiply:
                        {
                            CodeGen.IntegerMultiply(sourceToken);
                        }
                        case HopperToken.Divide:
                        {
                            CodeGen.IntegerDivideModulus(sourceToken, false);
                        }
                        case HopperToken.Modulus:
                        {
                            CodeGen.IntegerDivideModulus(sourceToken, true);
                        }
                        default:
                        {
                            errorAtCurrent("compileFactor not implemented for this operation");
                        }
                    }
                    continue;
                }
                break;
            }
            break;
        }
        return actualType;
    }
    string compileShift(string expectedType)
    {
        // TODO : shift          → factor( ( "<<" | ">>" ) factor )* ;
        string actualType;
        loop
        {
            actualType = compileFactor(expectedType);
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.ShiftRight) || check(HopperToken.ShiftLeft))
                {
                    advance(); //<< | >>
                    HopperToken operation = Token.GetType(previousToken);
                    <string,string> operationToken = previousToken;
                    
                    string rvalue = compileConstantExpression("byte");
                    
                    switch (operation)
                    {
                        case HopperToken.ShiftRight:
                        {
                            BitwiseShiftRight(rvalue, operationToken);
                        }
                        case HopperToken.ShiftLeft:
                        {
                            BitwiseShiftLeft(rvalue, operationToken);
                        }
                    }
                }
                
                break;
            }
            break;
        }
        return actualType;
    }
    string compileTerm(string expectedType)
    {
        string actualType;
        loop
        {
            actualType = compileShift(expectedType);
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.Subtract) || check(HopperToken.Add))
                {
                    <string,string> sourceToken = currentToken;
                    advance(); // + or -
                    HopperToken operation = Token.GetType(previousToken);
                            
                    string rightType = compileShift(expectedType);
                    if (actualType != rightType)
                    {
                        if (automaticUpCast(rightType, actualType))
                        {
                            rightType = actualType;
                        }
                        else
                        {
                            errorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "') A");
                        }
                    }
                    if (hadError)
                    {
                        break;
                    }
                    switch (operation)
                    {
                        case HopperToken.Add:
                        {
                            CodeGen.IntegerAdd(sourceToken);
                        }
                        case HopperToken.Subtract:
                        {
                            CodeGen.IntegerSubtract(sourceToken);
                        }
                        default:
                        {
                            errorAtCurrent("compileTerm not implemented for this operation");
                        }
                    }
                    continue;
                }
                break;
            }
            break;
        }
        return actualType;
    }
    string compileComparison(string expectedType)
    {
        // comparison     → term( ( ">" | ">=" | "<" | "<=" ) term ) ;
        string actualType;
        loop
        {
            string leftType = compileTerm(expectedType);
            actualType = leftType;
            if (hadError)
            {
                break;
            }
            if (check(HopperToken.LT) || check(HopperToken.LE) || check(HopperToken.GT) || check(HopperToken.GE))
            {
                if ((leftType != "uint") && (leftType != "byte") && (leftType != "int")&& (leftType != "+int")&& (leftType != "-int"))
                {
                    errorAtCurrent("comparison operations only legal for integral types, (not '" + leftType + "')");
                }
                <string,string> comparisonToken = currentToken;
                    
                actualType = "bool";
                    
                advance(); // <, >, <=, >=
                HopperToken operation = Token.GetType(previousToken);
                string rightType = compileTerm(expectedType);
                if (leftType != rightType)
                {
                    //bool automaticUpCast(string actualType, string desiredType)
                    if (automaticUpCast(rightType, leftType))
                    {
                        rightType = leftType;
                    }
                    else if ((leftType == "byte") && ((rightType == "int") || (rightType == "+int") || (rightType == "-int")))
                    {
                        leftType = "int"; // that works byte -> int
                    }
                    else
                    {
                        PrintLn(actualType);
                        PrintLn(Token.ToString(previousToken));
                        PrintLn(Token.ToString(currentToken));

                        errorAtCurrent("type mismatch, '" + leftType + "' expected (was '" + rightType + "') B");
                    }
                }
                if (hadError)
                {
                    break;
                }
                switch (operation)
                {
                    case HopperToken.LT:
                    {
                        CodeGen.LessThan(comparisonToken);
                    }
                    case HopperToken.LE:
                    {
                        LessThanOrEqual(comparisonToken);
                    }
                    case HopperToken.GT:
                    {
                        GreaterThan(comparisonToken);
                    }
                    case HopperToken.GE:
                    {
                        GreaterThanOrEqual(comparisonToken);
                    }
                }
            }
            break;
        } // loop
        
        return actualType;
    }
    string compileBitAnd(string expectedType)
    {
        // bitand         → comparison( ( "&" ) comparison )* ;
        string actualType;
        loop
        {
            string leftType = compileComparison(expectedType);
            actualType = leftType;
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.BitAnd))
                {
                    if ((leftType != "uint") && (leftType != "byte"))
                    {
                        errorAtCurrent("bitwise operations only legal for 'uint' and 'byte', (not '" + leftType + "') B");
                    }
                    <string,string> sourceToken = currentToken;
                    advance(); // &
                            
                    string rightType = compileComparison(expectedType);
                    if (actualType != rightType)
                    {
                        if (automaticUpCast(rightType, actualType))
                        {
                            rightType = actualType;
                        }
                        else if ((leftType == "byte") && (rightType == "uint"))
                        {
                            actualType = "uint"; // that works byte -> uint
                            leftType = "uint";
                        }
                        else
                        {
                            errorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "') C");
                        }
                    }
                    if (hadError)
                    {
                        break;
                    }
                    CodeGen.BitwiseAnd(sourceToken);
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;
    }
    string compileBitXor(string expectedType)
    {
        // TODO : bitxor         → bitand( ( "^" ) bitand )* ;
        string actualType;
        actualType = compileBitAnd(expectedType);
        return actualType;
    }
    string compileBitOr(string expectedType)
    {
        // bitor          → bitxor( ( "|" ) bitxor )* ;
        string actualType;
        loop
        {
            string leftType = compileBitXor(expectedType);
            actualType = leftType;
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.BitOr))
                {
                    if ((leftType != "uint") && (leftType != "byte"))
                    {
                        errorAtCurrent("bitwise operations only legal for 'uint' and 'byte', (not '" + leftType + "') A");
                    }
                    <string,string> sourceToken = currentToken;
                    advance(); // |
                            
                    string rightType = compileBitXor(expectedType);
                    if (actualType != rightType)
                    {
                        if (automaticUpCast(rightType, actualType))
                        {
                            rightType = actualType;
                        }
                        else if ((leftType == "byte") && (rightType == "uint"))
                        {
                            actualType = "uint"; // that works byte -> uint
                            leftType = "uint";
                        }
                        else
                        {
                            errorAtCurrent("type mismatch, '" + actualType + "' expected (was '" + rightType + "') D");
                        }
                    }
                    if (hadError)
                    {
                        break;
                    }
                    CodeGen.BitwiseOr(sourceToken);
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;
    }
    string compileBooleanAnd(string expectedType)
    {
        // booland        → bitor( ( "&&" ) bitor )* ;
        string actualType;
        loop
        {
            string leftType = compileBitOr(expectedType);
            if (hadError)
            {
                break;
            }
            actualType = leftType;
            loop
            {
                if (check(HopperToken.BooleanAnd))
                {
                    if (leftType != "bool")
                    {
                        errorAtCurrent("boolean expression expected");
                        break;
                    }
                    <string,string> sourceToken = currentToken;
                    
                    // short circuit
                    // short circuit
                    Chunks.UpdatePeepholeBoundary();
                    WriteChunk(OpCode.POPHL, sourceToken);
                    Chunks.UpdatePeepholeBoundary();
                    WriteChunk(OpCode.PUSHHL, sourceToken);
                    WriteChunk(OpCode.LDAL, sourceToken);
                    WriteChunk(OpCode.ORAL, sourceToken);
                    WriteChunk(OpCode.JPZ, sourceToken);     // JP Z shortCircuit
                    uint shortCircuit = Chunks.GetNextAddress();
                    WriteChunk(0, sourceToken); // placeholder
                    WriteChunk(0, sourceToken); // placeholder
                    Chunks.UpdatePeepholeBoundary();
                    
                    advance(); // &&
                    string rightType = compileBitOr(expectedType);
                    if (hadError)
                    {
                        break;
                    }
                    if (rightType != "bool")
                    {
                        errorAtCurrent("boolean expression expected");
                        break;
                    }
                    CodeGen.BooleanAnd(sourceToken);
// shortCircuit:
                    Chunks.PatchJumpToHere(shortCircuit);
                    Chunks.UpdatePeepholeBoundary();
                    
                    continue;
                }
                break;
            } // loop
            break;
        } // loop
        return actualType;
    }
    string compileBooleanOr(string expectedType)
    {
        // boolor         → booland( ( "||" ) booland )* ;
        string actualType;
        loop
        {
            string leftType = compileBooleanAnd(expectedType);
            if (hadError)
            {
                break;
            }
            actualType = leftType;
            
            loop
            {
                if (check(HopperToken.BooleanOr))
                {
                    if (leftType != "bool")
                    {
                        errorAtCurrent("boolean expression expected");
                        break;
                    }
                    <string,string> sourceToken = currentToken;
                    
                    // short circuit
                    Chunks.UpdatePeepholeBoundary();
                    WriteChunk(OpCode.POPHL, sourceToken);
                    Chunks.UpdatePeepholeBoundary();
                    WriteChunk(OpCode.PUSHHL, sourceToken);
                    WriteChunk(OpCode.LDAL, sourceToken);
                    WriteChunk(OpCode.ORAL, sourceToken);
                    WriteChunk(OpCode.JPNZ, sourceToken);     // JP NZ shortCircuit
                    uint shortCircuit = Chunks.GetNextAddress();
                    WriteChunk(0, sourceToken); // placeholder
                    WriteChunk(0, sourceToken); // placeholder
                    Chunks.UpdatePeepholeBoundary();
                    
                    advance(); // ||
                    string rightType = compileBooleanAnd(expectedType);
                    if (hadError)
                    {
                        break;
                    }
                    if (rightType != "bool")
                    {
                        errorAtCurrent("boolean expression expected");
                        break;
                    }
                    CodeGen.BooleanOr(sourceToken);
             
// shortCircuit:
                    Chunks.PatchJumpToHere(shortCircuit);
                    Chunks.UpdatePeepholeBoundary();
                    
                    continue;
                }
                break;
            } // loop
            
            
            break;
        } // loop
        return actualType;
    }
    string compileEquality(string expectedType)
    {
        // equality       → boolor ( ( "!=" | "==" ) boolor ) ;
        string actualType;
        loop
        {
            string leftType = compileBooleanOr(expectedType);
            actualType = leftType;
            if (hadError)
            {
                break;
            }
            loop
            {
                if (check(HopperToken.EQ) || check(HopperToken.NE))
                {
                    <string,string> equalityToken = currentToken;
                    
                    actualType = "bool";
                    
                    advance(); // != or ==
                    HopperToken operation = Token.GetType(previousToken);
                    
                    bool zeroCheck = Chunks.RemoveZero();
                    
                    string rightType = compileBooleanOr(leftType);
                    
                    if (leftType != rightType)
                    {
                        if (automaticUpCast(rightType, leftType))
                        {
                            rightType = leftType;
                        }
                        else if ((leftType == "byte") && ((rightType == "int") || (rightType == "+int") || (rightType == "-int")))
                        {
                            leftType = "int"; // that works byte -> int
                        }
                        else if ((leftType == "byte") && (rightType == "uint"))
                        {
                            leftType = "uint"; // that works byte -> uint
                        }
                        else if ((leftType == "+int") && (rightType == "uint"))
                        {
                            leftType = "uint"; // that works "+int" -> "uint" (not "-int" or "int")
                        }
                        else
                        {
                            errorAtCurrent("type mismatch, '" + leftType + "' expected (was '" + rightType + "')");
                        }
                    }
                    if (hadError)
                    {
                        break;
                    }
                    if (zeroCheck)
                    {
                        if (operation == HopperToken.EQ)
                        {
                            CodeGen.ZeroCheck(equalityToken);
                        }
                        else
                        {
                            CodeGen.NotZeroCheck(equalityToken);
                        }
                        break;
                    }
                    
                    switch (operation)
                    {
                        case HopperToken.EQ:
                        {
                            if (leftType == "bool")
                            {
                                CodeGen.CompareEqualBoolean(equalityToken);
                            }
                            else
                            {
                                CodeGen.CompareEqual(equalityToken);
                            }
                        }
                        case HopperToken.NE:
                        {
                            if (leftType == "bool")
                            {
                                CodeGen.CompareNotEqualBoolean(equalityToken);
                            }
                            else
                            {
                                CodeGen.CompareNotEqual(equalityToken);
                            }
                        }
                        default:
                        {
                            errorAtCurrent("compileEquality not implemented for this operation");
                        }
                    }
                }
                break;
            }
            break;
        }
        return actualType;
    }
    string compileExpression(string expectedType)
    {
        string actualType;
        actualType = compileEquality(expectedType);
        return actualType;
    }
    compileSwitch()
    {
        uint tableAddress;
        uint defaultAddress;
        <uint> endJumps;
        <string, uint> caseLocations;
        <string,string> sourceToken;
        loop
        {
            advance(); // switch
            sourceToken = currentToken;
            consume(HopperToken.LParen, "'(' expected");
            if (hadError)
            {
                break;
            }
            string actualType = compileExpression("byte"); // switch (expr)
            if ((actualType != "byte") && (actualType != "char"))
            {
                error("switch requires 'byte' or 'char' type, (not '" + actualType +"')");
            }
            if (hadError)
            {
                break;
            }
            consume(HopperToken.RParen, "')' expected");
            if (hadError)
            {
                break;
            }
            consume(HopperToken.LBrace, "'{' expected");
            if (hadError)
            {
                break;
            }
            WriteChunk(OpCode.POPHL, sourceToken); // since we know we are about to use it
            WriteChunk(OpCode.ADDHLHL, sourceToken); // HL = HL * 2
            WriteChunk(OpCode.LDDE, sourceToken);    // DE = tableAddress
            tableAddress = Chunks.GetNextAddress();
            WriteChunk(0, sourceToken);
            WriteChunk(0, sourceToken);
            WriteChunk(OpCode.ADDHLDE, sourceToken); // HL = HL + DE
            WriteChunk(OpCode.LDAHL, sourceToken);   // LD A, (HL)
            WriteChunk(OpCode.INCHL, sourceToken);   // INC HL
            WriteChunk(OpCode.LDHHL, sourceToken);   // LD H, (HL)
            WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
            WriteChunk(OpCode.JPHL, sourceToken);    // JP (HL)
            
            loop
            {
                if (check(HopperToken.RBrace)) //
                {
                    advance(); // }
                    break;
                }
                if (hadError)
                {
                    break;
                }
                uint lastCase = 0;
                loop
                {
                    if (check(HopperToken.Keyword, "default"))
                    {
                        advance(); // default
                        consume(HopperToken.Colon, "':' expected");
                        if (hadError)
                        {
                            break;
                        }
                        if (defaultAddress != 0)
                        {
                            errorAtCurrent("only one 'default' allowed in switch");
                            break;
                        }
                        defaultAddress = Chunks.GetNextAddress();
                        continue;
                    }
                    if (!check(HopperToken.Keyword, "case"))
                    {
                        break;
                    }
                    advance(); // case
                    string value = compileConstantExpression(actualType);
                    if (hadError)
                    {
                        break;
                    }
                    if (caseLocations.Contains(value))
                    {
                        errorAtCurrent("duplicate case in switch: '" + previousToken["lexeme"] + "'");
                        break;
                    }
                    caseLocations[value] = Chunks.GetNextAddress();
                    uint ucaseValue;
                    if (!TryParseUInt(value, ref ucaseValue))
                    {
                        errorAtCurrent("error in switch case constant");
                        break;
                    }
                    if (ucaseValue > 255)
                    {
                        error("switch requires 'byte' or 'char' type, ( '" + value +"' is out of range)");
                        break;
                    }
                    lastCase = ucaseValue;
                    consume(HopperToken.Colon, "':' expected");
                }
                if (hadError)
                {
                    break;
                }
                if (!check(HopperToken.LBrace, "'{' expected"))
                {
                    break;
                }
                
                Chunks.UpdatePeepholeBoundary();
                compileBlock(false); // case block
                if (hadError)
                {
                    break;
                }
                
                sourceToken = previousToken;
                WriteChunk(OpCode.JP, sourceToken);
                uint endJump = Chunks.GetNextAddress();
                WriteChunk(0, sourceToken); // placeholder
                WriteChunk(0, sourceToken); // placeholder
                Chunks.UpdatePeepholeBoundary();
                endJumps.Append(endJump);
                
                
                
            } // loop case block
            break;
        } // loop switch block
        
        if (!hadError)
        {
            byte upperBound = 0;
            sourceToken = previousToken;
            Chunks.UpdatePeepholeBoundary();
            
            // build the jump table
            uint[256] table;
            if (0 != defaultAddress)
            {
                for (uint i=0; i < 256; i++)
                {
                    table[i] = defaultAddress;
                }
            }
            byte lastCase = 0;
            foreach (var kv in caseLocations)
            {
                uint ucaseValue;
                if (TryParseUInt(kv.key, ref ucaseValue))
                {
                }
                byte caseValue = byte(ucaseValue);
                table[caseValue] = kv.value;
                if (caseValue > lastCase)
                {
                    lastCase = caseValue;
                }
            }
            if (0 != defaultAddress)
            {
                lastCase = 255;
            }
            
            Chunks.PatchJumpToHere(tableAddress);
            Chunks.UpdatePeepholeBoundary();
            byte iTable = 0;
            loop
            {
                if (iTable > lastCase)
                {
                    break;
                }
                uint address = table[iTable];
                uint lsb = (address % 256);
                uint msb = (address / 256);
                Chunks.UpdatePeepholeBoundary();
                WriteChunk(byte(lsb), sourceToken);
                Chunks.UpdatePeepholeBoundary();
                WriteChunk(byte(msb), sourceToken);
                iTable++;
            }
            Chunks.UpdatePeepholeBoundary();
    
            // endLabel:
            foreach (var endJump in endJumps)
            {
                Chunks.PatchJumpToHere(endJump);
            }
        }
    }
    
    compileIf()
    {
        <uint> endJumps;
        loop
        {
            advance(); // if
            <string,string> sourceToken = currentToken;
            consume(HopperToken.LParen, "'(' expected");
            if (hadError)
            {
                break;
            }
            string actualType = compileExpression("bool"); // if (expr)
            if (actualType != "bool")
            {
                error("boolean expression expected");
            }
            if (hadError)
            {
                break;
            }
            WriteChunk(OpCode.POPHL, sourceToken);
            
            // is HL zero? -> Z
            OpCode lastOpCode = GetLastOpCode();
            if (lastOpCode != OpCode.INCL) // INC L sets Z for L
            {
                WriteChunk(OpCode.LDAL, sourceToken);    
                WriteChunk(OpCode.ORAL, sourceToken);
            }
            
            WriteChunk(OpCode.JPZ, sourceToken);
            uint toElseLabel = Chunks.GetNextAddress();
            WriteChunk(0, sourceToken); // placeholder
            WriteChunk(0, sourceToken); // placeholder
            Chunks.UpdatePeepholeBoundary();
            consume(HopperToken.RParen, "')' expected");
            if (hadError)
            {
                break;
            }
            compileBlock(false); // if block
            
            if (check(HopperToken.Keyword, "else"))
            {
                WriteChunk(OpCode.JP, sourceToken);
                endJumps.Append(Chunks.GetNextAddress());
                WriteChunk(0, sourceToken); // placeholder
                WriteChunk(0, sourceToken); // placeholder
            }
            Chunks.UpdatePeepholeBoundary();
            
            // elseLabel:
            Chunks.PatchJumpToHere(toElseLabel);
            
            if (!check(HopperToken.Keyword, "else"))
            {
                break;
            }
            advance(); // else
            // else block
            if (check(HopperToken.Keyword, "if"))
            {
                // else if block..
                continue;
            }
            compileBlock(false); // else block
            
            break;
        } // loop
        if (!hadError)
        {
            // endLabel:
            foreach (var endJump in endJumps)
            {
                Chunks.PatchJumpToHere(endJump);
            }
        }
    }
    compileLoop()
    {
        loop
        {
            
            advance(); // loop
            uint loopAddress = Chunks.GetNextAddress();
            Chunks.UpdatePeepholeBoundary();
             
            compileBlock(true); // loop block

            <string,string> braceToken = previousToken;
            
            // replace the NOP's inserted by break jump patching in compileBlockOpen()
            uint current = Chunks.GetNextAddress();
            uint lsb = (loopAddress % 256);
            uint msb = (loopAddress / 256);
            
            OpCode op0 = Chunks.GetOpCode(current-3);
            OpCode op1 = Chunks.GetOpCode(current-2);
            OpCode op2 = Chunks.GetOpCode(current-1);
            if ((op0 != OpCode.NOP) || (op1 != OpCode.NOP) || (op2 != OpCode.NOP))
            {
                PrintLn("Patch end-of-loop JP:");
                PrintLn("  " + Chunks.OpCodeToString(op0));
                PrintLn("  " + Chunks.OpCodeToString(op1));
                PrintLn("  " + Chunks.OpCodeToString(op2));
                error("something went horribly wrong with the end-of-loop patch");
            }
            PatchChunk(current-3, OpCode.JP, braceToken);
            PatchChunk(current-2, byte(lsb), braceToken);
            PatchChunk(current-1, byte(msb), braceToken);
            break;
        }
    }
    compileContinue()
    {
        loop
        {
            <string,string> continueToken = currentToken;
            advance(); // continue
            consume(HopperToken.SemiColon, "';' expected");
            if (hadError)
            {
                break;
            }
            uint continueAddress;
            if (!Chunks.GetContinueAddress(ref continueAddress))
            {
                error("'continue' not inside loop block");
                break;
            }
            
            uint popSize = 0;
            uint originalCount = Symbols.GetLocalCountOnLoopEntry();
            uint extrasCount = Symbols.GetLocalsCount();
            loop
            {
                if (extrasCount == originalCount)
                {
                    break;
                }
                //string name = Symbols.GetLocalName(extrasCount-1);
                //PrintLn("continue pops '" + name + "' on line " + continueToken);
                popSize++;
                extrasCount--;
            }
            popLocals(popSize, continueToken["source"], continueToken["line"]);
            
            uint lsb = (continueAddress % 256);
            uint msb = (continueAddress / 256);
            WriteChunk(OpCode.JP, continueToken);
            WriteChunk(byte(lsb), continueToken);
            WriteChunk(byte(msb), continueToken);
            break;
        }
    }
    popLocals(uint popSize, string path, string braceLine)
    {
        while (popSize > 0)
        {
            WriteChunk(OpCode.INCSP, path, braceLine);
            WriteChunk(OpCode.INCSP, path, braceLine);
            popSize--;
        }
    }
    
    compileReturn()
    {
        loop
        {
            advance(); // return
            
            string returnType = Symbols.GetCurrentReturnType();
            
            string actualType = compileExpression(returnType);
            if (actualType != returnType)
            {
                error("type mismatch for 'return', '" + returnType + "' expected (was '" + actualType + "')");
            }
            if (hadError)
            {
                break;
            }
            <string,string> semiToken = currentToken;
            consume(HopperToken.SemiColon, "';' expected");
            if (hadError)
            {
                break;
            }
            WriteChunk(OpCode.POPHL, semiToken); // return value in HL
            
            uint localsToPop = Symbols.GetLocalsCount();
            popLocals(localsToPop, semiToken["source"], semiToken["line"]); // on return we pop all locals (for all nested blocks) before we can POP IX
            
            uint argumentsToPop = Symbols.GetArgumentsToPopCount();
            WriteChunk(OpCode.POPIX, semiToken);
            if (argumentsToPop > 0)
            {
                WriteChunk(OpCode.POPDE, semiToken); // the return address
                loop
                {
                    WriteChunk(OpCode.INCSP, semiToken); // pop an argument
                    WriteChunk(OpCode.INCSP, semiToken);
                    argumentsToPop--;
                    if (argumentsToPop == 0)
                    {
                        break;
                    }
                }
                WriteChunk(OpCode.PUSHDE, semiToken); // the return address
            }
            Chunks.WriteChunk(OpCode.RET, semiToken);
            break;
        }
    }
    compileBreak()
    {
        loop
        {
            <string,string> breakToken = currentToken;
            advance(); // break
            consume(HopperToken.SemiColon, "';' expected");
            if (hadError)
            {
                break;
            }
            
            uint popSize = 0;
            uint originalCount = Symbols.GetLocalCountOnLoopEntry();
            uint extrasCount = Symbols.GetLocalsCount();
            loop
            {
                if (extrasCount == originalCount)
                {
                    break;
                }
                //string name = Symbols.GetLocalName(extrasCount-1);
                //PrintLn("break pops '" + name + "' on line " + breakToken);
                popSize++;
                extrasCount--;
            }
            popLocals(popSize, breakToken["source"], breakToken["line"]);
            
            WriteChunk(OpCode.JP, breakToken);
            uint breakPatchAddress = Chunks.GetNextAddress();
            WriteChunk(0, breakToken); // placeholder
            WriteChunk(0, breakToken); // placeholder
            Chunks.UpdatePeepholeBoundary();
            if (!Chunks.PatchBreak(breakPatchAddress))
            {
                error("'break' not inside loop block");
                break;
            }
            break;
        }
    }
    
    compileLocalDeclaration()
    {
        loop
        {
            if (!check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
            {
                error("type keyword expected");
                break;
            }
            advance();
            string ltype = previousToken["lexeme"];
            if (!check(HopperToken.Identifier))
            {
                error("identifier expected");
                break;
            }
            advance();
            string identifier = previousToken["lexeme"];
            if (Symbols.LocalSymbolExists(identifier))
            {
                error("symbol '" + identifier + "' already exists");
                break;
            }
            if (Token.IsReservedWord(identifier))
            {
                error("identifier '" + identifier + "' is a reserved word");
                break;
            }
            uint index = Symbols.AddLocal(identifier, ltype);
        
            // initilize a stack slot for our new local
            WriteChunk(OpCode.LDHL, currentToken);
            WriteChunk(0, currentToken);
            WriteChunk(0, currentToken);
            WriteChunk(OpCode.PUSHHL, currentToken);
            
            if (check(HopperToken.SemiColon))
            {
                advance(); // ;
                break;
            }
            if (!check(HopperToken.Assign))
            {
                error("'=' expected");
                break;
            }
            advance(); // =
            string actualType = compileExpression(ltype);
            if (actualType != ltype)
            {
                if (automaticUpCast(actualType, ltype))
                {
                    ltype = actualType;
                }
                else
                {
                    error("type mismatch in assignment, '" + ltype + "' expected (was '" + actualType + "') - 1");
                }
            }
            if (!check(HopperToken.SemiColon))
            {
                error("';' expected");
                break;
            }
            advance(); // ;
            
            <string,string> variable = Symbols.GetVariable(identifier);
            
            // "local" has -ve offset
            int lsb = 0;
            if (Token.TryParseInt(variable["offset"], ref lsb))
            {
            }
            lsb = -lsb;
            int lsb1 = lsb+1;
            if (lsb < 0)
            {
                lsb = lsb + 256; // -1 -> 255
            }
            if (lsb1 < 0)
            {
                lsb1 = lsb1 + 256; // -1 -> 255
            }
            //PrintLn("POP local: " + identifier + " " + lsb.ToString() + " " + lsb1.ToString() + " (" + previousToken["line"] + ")");
            //OutputDebug(variable);
            WriteChunk(OpCode.POPHL, previousToken);
            WriteChunk(OpCode.LDIXINDL, previousToken);
            WriteChunk(byte(lsb), previousToken);
            WriteChunk(OpCode.LDIXINDH, previousToken);
            WriteChunk(byte(lsb1), previousToken);
            
            break;
        }
    }
    compileIncrementDecrement(string identifier)
    {
        loop
        {
            <string,string> variable = Symbols.GetVariable(identifier);
            advance(); // ++/--
            HopperToken operation = Token.GetType(previousToken);
            <string,string> operationToken = previousToken;
            
            string vtype = variable["type"];
            if ((vtype != "byte") && (vtype != "int") && (vtype != "-int") && (vtype != "+int") && (vtype != "uint"))
            {
                if (operation == HopperToken.Decrement)
                {
                    errorAtCurrent("'--' requires an integral type, (not '" + vtype + "')");
                }
                else
                {
                    errorAtCurrent("'++' requires an integral type, (not '" + vtype + "')");
                }
                break;
            }
            
            if (!check(HopperToken.SemiColon))
            {
                error("';' expected");
                break;
            }
            advance(); // ;
            OpCode op = OpCode.INCHL;
            if (operation == HopperToken.Decrement)
            {
                op = OpCode.DECHL;
            }
            
            switch (variable["kind"])
            {
                case "global":
                {
                    byte lsb = 0;
                    byte msb = 0;
                    if (Chunks.ConvertOffsetToBytes(variable["offset"], ref msb, ref lsb))
                    {
                    }
                    WriteChunk(OpCode.LDHLIND, operationToken);
                    WriteChunk(lsb, operationToken);
                    WriteChunk(msb, operationToken);
                    WriteChunk(op, operationToken);
                    WriteChunk(OpCode.LDINDHL, operationToken);
                    WriteChunk(lsb, operationToken);
                    WriteChunk(msb, operationToken);
                    
                    PeepholeOptimizer(); // after the operands
                }
                case "local":
                {
                    // "local" has -ve offset
                    int lsb = 0;
                    if (Token.TryParseInt(variable["offset"], ref lsb))
                    {
                    }
                    lsb = -lsb;
                    int lsb1 = lsb+1;
                    if (lsb < 0)
                    {
                        lsb = lsb + 256; // -1 -> 255
                    }
                    if (lsb1 < 0)
                    {
                        lsb1 = lsb1 + 256; // -1 -> 255
                    }
                    
                    WriteChunk(OpCode.LDLIXIND, operationToken);
                    WriteChunk(byte(lsb), operationToken);
                    WriteChunk(OpCode.LDHIXIND, operationToken);
                    WriteChunk(byte(lsb1), operationToken);
                    WriteChunk(op, operationToken);
                    WriteChunk(OpCode.LDIXINDL, operationToken);
                    WriteChunk(byte(lsb), operationToken);
                    WriteChunk(OpCode.LDIXINDH, operationToken);
                    WriteChunk(byte(lsb1), operationToken);
                }
                case "argument":
                {
                    // "argument" has -ve offset
                    // POP [BP+offset]
                    int lsb = 0;
                    if (Token.TryParseInt(variable["offset"], ref lsb))
                    {
                    }
                    lsb = lsb + 4; // IX and SP
                    int lsb1 = lsb+1;
                    if (lsb < 0)
                    {
                        lsb = lsb + 256; // -1 -> 255
                    }
                    if (lsb1 < 0)
                    {
                        lsb1 = lsb1 + 256; // -1 -> 255
                    }
                    WriteChunk(OpCode.LDLIXIND, operationToken);
                    WriteChunk(byte(lsb), operationToken);
                    WriteChunk(OpCode.LDHIXIND, operationToken);
                    WriteChunk(byte(lsb1), operationToken);
                    WriteChunk(op, operationToken);
                    WriteChunk(OpCode.LDIXINDL, operationToken);
                    WriteChunk(byte(lsb), operationToken);
                    WriteChunk(OpCode.LDIXINDH, operationToken);
                    WriteChunk(byte(lsb1), operationToken);
                }
            }
            break;
        }
    }
    compileAssignment()
    {
        string identifier = currentToken["lexeme"];
        loop
        {
            if ((currentToken["lexeme"] == "port") || (currentToken["lexeme"] == "memory")|| (currentToken["lexeme"] == "memoryword"))
            {
                compileAssignMemoryOrPort();
                break;
            }
            if (currentToken["lexeme"] == "iy")
            {
                compileAssignIY();
                break;
            }
            if (currentToken["lexeme"] == "pc")
            {
                compileAssignPC();
                break;
            }
            <string,string> variable = Symbols.GetVariable(identifier);
            if (variable.Count == 0)
            {
                errorAtCurrent("variable identifier expected");
                break;
            }
            advance();
            string vtype = variable["type"];
            if (check(HopperToken.Increment) || check(HopperToken.Decrement))
            {
                compileIncrementDecrement(identifier);
                break;
            }
            if (!check(HopperToken.Assign))
            {
                errorAtCurrent("'=' expected");
                break;
            }
            advance(); // =
            string actualType = compileExpression(vtype);
            if (actualType != vtype)
            {
                if (automaticUpCast(actualType, vtype))
                {
                    vtype = actualType;
                }
                else
                {
                    error("type mismatch in assignment, '" + vtype + "' expected (was '" + actualType + "') - 2");
                }
            }
            if (!check(HopperToken.SemiColon))
            {
                error("';' expected");
                break;
            }
            <string,string> assignToken = currentToken;
            advance(); // ;
            
            switch (variable["kind"])
            {
                case "global":
                {
                    byte lsb = 0;
                    byte msb = 0;
                    if (Chunks.ConvertOffsetToBytes(variable["offset"], ref msb, ref lsb))
                    {
                    }
                    //PrintLn("POP global:" + identifier + " " + lsb.ToString() + " " + msb.ToString());
                    //OutputDebug(variable);
                    WriteChunk(OpCode.POPHL, assignToken);
                    WriteChunk(OpCode.LDINDHL, assignToken);
                    WriteChunk(lsb, assignToken);
                    WriteChunk(msb, assignToken);
                }
                case "local":
                {
                    // "local" has -ve offset
                    int lsb = 0;
                    if (Token.TryParseInt(variable["offset"], ref lsb))
                    {
                    }
                    lsb = -lsb;
                    int lsb1 = lsb+1;
                    if (lsb < 0)
                    {
                        lsb = lsb + 256; // -1 -> 255
                    }
                    if (lsb1 < 0)
                    {
                        lsb1 = lsb1 + 256; // -1 -> 255
                    }
                    //PrintLn("POP local:" + identifier + " " + lsb.ToString() + " " + lsb1.ToString());
                    //OutputDebug(variable);
                    WriteChunk(OpCode.POPHL, assignToken);
                    WriteChunk(OpCode.LDIXINDL, assignToken);
                    WriteChunk(byte(lsb), assignToken);
                    if (actualType != "bool") // boolean doesn't care about MSB
                    {
                        WriteChunk(OpCode.LDIXINDH, assignToken);
                        WriteChunk(byte(lsb1), assignToken);
                    }
                }
                case "argument":
                {
                    // "argument" has -ve offset
                    // POP [BP+offset]
                    int lsb = 0;
                    if (Token.TryParseInt(variable["offset"], ref lsb))
                    {
                    }
                    lsb = lsb + 4; // IX and SP
                    int lsb1 = lsb+1;
                    if (lsb < 0)
                    {
                        lsb = lsb + 256; // -1 -> 255
                    }
                    if (lsb1 < 0)
                    {
                        lsb1 = lsb1 + 256; // -1 -> 255
                    }
                    //PrintLn("POP argument:" + identifier + " " + variable["offset"] + " " + lsb.ToString() + " " + lsb1.ToString());
                    //OutputDebug(variable);
                    WriteChunk(OpCode.POPHL, assignToken);
                    WriteChunk(OpCode.LDIXINDL, assignToken);
                    WriteChunk(byte(lsb), assignToken);
                    if (actualType != "bool") // boolean doesn't care about MSB
                    {
                        WriteChunk(OpCode.LDIXINDH, assignToken);
                        WriteChunk(byte(lsb1), assignToken);
                    }
                }
            }
            break;
        }
    }
   
    compileStatement()
    {
        loop
        {
            if (hadError)
            {
                break;
            }
            HopperToken ttype = Token.GetType(currentToken);
            switch (ttype)
            {
                case HopperToken.RBrace:
                {
                    // end of block
                }
                case HopperToken.Keyword:
                {
                    if (check(HopperToken.Keyword, "|bool|byte|uint|int|string|char|"))
                    {
                        compileLocalDeclaration();
                    }
                    else if (check(HopperToken.Keyword, "if"))
                    {
                        compileIf();
                    }
                    else if (check(HopperToken.Keyword, "break"))
                    {
                        compileBreak();
                    }
                    else if (check(HopperToken.Keyword, "continue"))
                    {
                        compileContinue();
                    }
                    else if (check(HopperToken.Keyword, "return"))
                    {
                        compileReturn();
                    }
                    else if (check(HopperToken.Keyword, "loop"))
                    {
                        compileLoop();
                    }
                    else if (check(HopperToken.Keyword, "switch"))
                    {
                        compileSwitch();
                    }
                    else
                    {
                        PrintLn(Token.ToString(previousToken));
                        PrintLn(Token.ToString(currentToken));
                        errorAtCurrent("TODO compileStatement not implemented for this keyword");
                    }
                }
                case HopperToken.Identifier:
                {
                    string identifier = currentToken["lexeme"];
                    uint iMethod;
                    if (GetMethodIndex(identifier, ref iMethod))
                    {
                        <string,string> method = Symbols.GetMethod(identifier);
                        compileMethodCall(method);
                    }
                    else
                    {
                        compileAssignment();
                    }
                }
                default:
                {
                    PrintLn(Token.ToString(previousToken));
                    PrintLn(Token.ToString(currentToken));
                
                    errorAtCurrent("TODO compileStatement not implemented for this token");
                }
            }
            break;
        } // loop
    }
   
    compileBlockOpen(bool isLoopBlock)
    {
        uint originalCount = Symbols.GetLocalsCount();
        <string,string>  braceToken;
        if (isLoopBlock)
        {
            Symbols.SetLocalCountOnLoopEntry(originalCount);
            uint continueAddress = Chunks.GetNextAddress();
            Chunks.UpdatePeepholeBoundary();
            Chunks.PushContinue(continueAddress);
            Chunks.PushBreak();
        }
        loop
        {
            loop
            {
                if (check(HopperToken.EOF))
                {
                    break;
                }
                if (check(HopperToken.RBrace))
                {   
                    braceToken = currentToken;
                    advance(); // }
                    break;
                }
                if (hadError)
                {
                    break;
                }
                compileStatement();
            }
            
            
            OpCode opCode = Chunks.GetLastOpCode();
            if (opCode != OpCode.RET) // needs to be done in compileReturn instead
            {
                uint popSize = 0;
                loop
                {
                    uint extrasCount = Symbols.GetLocalsCount();
                    if (extrasCount == originalCount)
                    {
                        break;
                    }
                    string name = Symbols.GetLocalName(extrasCount-1);
                    string popType = Symbols.GetLocalType(extrasCount-1);
                    popSize++;
                    Symbols.PopLastLocal();
                }
                popLocals(popSize, braceToken["source"], braceToken["line"]);
            }
            if (isLoopBlock)
            {
                WriteChunk(OpCode.NOP, braceToken);
                WriteChunk(OpCode.NOP, braceToken);
                WriteChunk(OpCode.NOP, braceToken);
                uint breakAddress = Chunks.GetNextAddress();
                Chunks.UpdatePeepholeBoundary();
                Chunks.PopContinue();
                Chunks.PopAndPatchBreak(breakAddress);
                
                Symbols.PopLocalCountOnLoopEntry();
            }
            break;
        }
    }
    compileBlock(bool isLoopBlock)
    {
        loop
        {
            if (!check(HopperToken.LBrace))
            {   
                errorAtCurrent("'{' expected");
                break;
            }
            advance(); // {
            compileBlockOpen(isLoopBlock);
            break;
        }
    }
    
    compileMethod(uint mIndex)
    {
        loop
        {
            string methodName = GetMethodName(mIndex);
            long pos  = Symbols.GetMethodPos(methodName);
            uint ln = Symbols.GetMethodLine(methodName);
            string sourcePath = Symbols.GetMethodSourcePath(methodName);
            Scanner.Reset(pos, ln, sourcePath);
            Symbols.ResetLocals();
            reset();
            
            advance();
            uint ib;
            string braceLine = currentToken["line"];
            <string,string> braceToken = currentToken;
            if (Token.TryParseUInt(braceLine, ref ib))
            {
                ib = ib - 1;
                braceLine = ib.ToString();
            }
            
            Symbols.AddMethodArguments(methodName);
            
            methodAddresses[methodName] = Chunks.GetNextAddress();
            Chunks.UpdatePeepholeBoundary();
            WriteChunk(OpCode.PUSHIX, braceToken["source"], braceLine);
            WriteChunk(OpCode.LDIX, braceToken["source"], braceLine);
            WriteChunk(0, braceToken["source"], braceLine);
            WriteChunk(0, braceToken["source"], braceLine);
            WriteChunk(OpCode.ADDIXSP, braceToken["source"], braceLine);

            compileBlockOpen(false);
            if (hadError)
            {
                break;
            }
            
            braceLine = previousToken["line"];
            braceToken = previousToken;
            
            uint argumentsToPop = Symbols.GetArgumentsToPopCount();
            WriteChunk(OpCode.POPIX, braceToken["source"], braceLine);
            if (argumentsToPop > 0)
            {
                WriteChunk(OpCode.POPDE, braceToken["source"], braceLine); // the return address
                loop
                {
                    WriteChunk(OpCode.INCSP, braceToken["source"], braceLine); // pop an argument
                    WriteChunk(OpCode.INCSP, braceToken["source"], braceLine);
                    argumentsToPop--;
                    if (argumentsToPop == 0)
                    {
                        break;
                    }
                }
                WriteChunk(OpCode.PUSHDE, braceToken["source"], braceLine); // the return address
            }
            uint actualIndex;
            bool mFound = Symbols.GetMethodIndex("main", ref actualIndex);
            if (mFound && (mIndex == actualIndex))
            {
                Chunks.WriteChunk(OpCode.HALT, braceToken["source"], braceLine);
            }
            else
            {
                Chunks.WriteChunk(OpCode.RET, braceToken["source"], braceLine);
            }
            break;
        }
    }
    compileFunction(uint fIndex)
    {
        loop
        {
            string functionName = Symbols.GetFunctionName(fIndex);
            long pos  = Symbols.GetFunctionPos(functionName);
            uint ln = Symbols.GetFunctionLine(functionName);
            string sourcePath = Symbols.GetFunctionSourcePath(functionName);
            Scanner.Reset(pos, ln, sourcePath);
            
            Symbols.ResetLocals();
            reset();
            
            advance();
            uint ib;
            string braceLine = currentToken["line"];
            <string,string> braceToken = currentToken;
            if (Token.TryParseUInt(braceLine, ref ib))
            {
                ib = ib - 1;
                braceLine = ib.ToString();
            }
            
            Symbols.AddFunctionArguments(functionName);
            Symbols.SetCurrentReturnType(functionName);
            
            functionAddresses[functionName] = Chunks.GetNextAddress();
            Chunks.UpdatePeepholeBoundary();
            WriteChunk(OpCode.PUSHIX, braceToken["source"], braceLine);
            WriteChunk(OpCode.LDIX, braceToken["source"], braceLine);
            WriteChunk(0, braceToken["source"], braceLine);
            WriteChunk(0, braceToken["source"], braceLine);
            WriteChunk(OpCode.ADDIXSP, braceToken["source"], braceLine);

            compileBlockOpen(false);
            if (hadError)
            {
                break;
            }
            
            OpCode opCode = Chunks.GetLastOpCode();
            if (opCode != OpCode.RET)
            {
                error("function must end on 'return' statement");
            }
            break;
        }
    }
    EmbedData()
    {
        file hexeFile = File.Open(embeddedDataPath);
        loop
        {
            byte db = hexeFile.Read();
            if (!hexeFile.IsValid())
            {
                break;
            }
            Chunks.WriteChunk(db, "", "0");
        }
    }
    long GetEmbeddedDataSize()
    {
        return embeddedDataSize;
    }
    bool Compile(string sourcePath)
    {
        // start with Main
        uint mIndex;
        if (!Symbols.GetMethodIndex("main", ref mIndex))
        {
            error("where is 'main'?");
        }
        methodsToCompile.Clear();
        methodsCompiled.Clear();
        functionsToCompile.Clear();
        functionsCompiled.Clear();

        // all just to get currentToken right for 
        string methodName = GetMethodName(mIndex);
        long pos  = Symbols.GetMethodPos(methodName);
        uint ln = Symbols.GetMethodLine(methodName);
        
        // fake token for first '{' for disassembly listing
        <string,string> startToken;
        startToken["type"] = HopperTokenToString(HopperToken.LBrace);
        // ["lexeme"]  - string
        startToken["line"] = ln.ToString();
        startToken["source"] = sourcePath;
        startToken["pos"]    = pos.ToString();
        // ["literal"] - depends
        
        // jump past any embedded data
        WriteChunk(OpCode.JP, startToken);
        uint jumpPastData = Chunks.GetNextAddress();
        WriteChunk(0x00, startToken);
        WriteChunk(0x00, startToken);
        if (embeddedDataSize > 0)
        {
            EmbedData();
            PrintLn("'" + embeddedDataPath + "' embedded");
        }
        else
        {
            PrintLn();
        }
        Chunks.UpdatePeepholeBoundary();
        Chunks.PatchJumpToHere(jumpPastData);
        
        // initialize the SP (0x0000 so it becomes 0xFFFF after the first push?)
        WriteChunk(OpCode.LDIX, startToken);
        WriteChunk(0x00, startToken);
        WriteChunk(0x00, startToken);
        WriteChunk(OpCode.LDSPIX, startToken);
        
        // generate stack slots for globals (and initialize)
        uint globals = Symbols.GetGlobalsCount();
        for (uint g = 0; g < globals; g++)
        {
            string gtype = Symbols.GetGlobalType(g);
            string value = Symbols.GetGlobalValue(g);
            
            byte lsb = 0;
            byte msb = 0;
            if (value.Length > 0)
            {
                if (Chunks.ConvertOffsetToBytes(value, ref msb, ref lsb))
                {
                }
            }
            WriteChunk(OpCode.LDHL, startToken);
            WriteChunk(lsb, startToken);
            WriteChunk(msb, startToken);
            WriteChunk(OpCode.PUSHHL, startToken);
        }
        compileMethod(mIndex);
        Print(".", Color.MatrixBlue, Color.SlateBlue);
        
        //<uint> methodsToCompile;
        //<uint> functionsToCompile;
        
        loop
        {
            if (hadError)
            {
                break;
            }
            if (functionsToCompile.Length > 0)
            {   
                uint fIndex = functionsToCompile[0];
                functionsToCompile.Remove(0);
                compileFunction(fIndex);
                functionsCompiled.Append(fIndex);
                Print(".", Color.MatrixBlue, Color.SlateBlue);
                continue;
            }
            uint length = methodsToCompile.Length;
            if (methodsToCompile.Length > 0)
            {
                mIndex = methodsToCompile[0];
                methodsToCompile.Remove(0);
                methodsCompiled.Append(mIndex);
                compileMethod(mIndex);
                Print(".", Color.MatrixBlue, Color.SlateBlue);
                continue;
            }
            break;
        }
        if (!hadError)
        {
            foreach (var functionPatch in functionCallPatches)
            {
                string name = functionPatch.key;
                <uint> callFromAddresses = functionPatch.value;
                uint callToAddress = functionAddresses[name];
                foreach (var location in callFromAddresses)
                {
                    Chunks.PatchCall(location, callToAddress);
                }
            }
            foreach (var methodPatch in methodCallPatches)
            {
                string name = methodPatch.key;
                <uint> callFromAddresses = methodPatch.value;
                uint callToAddress = methodAddresses[name];
                foreach (var location in callFromAddresses)
                {
                    Chunks.PatchCall(location, callToAddress);
                }
            }
        }
        return hadError;
    }
}
