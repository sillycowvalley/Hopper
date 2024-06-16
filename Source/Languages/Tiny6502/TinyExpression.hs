unit TinyExpression
{
    uses "TinyToken"
    uses "TinyScanner"
    uses "TinyCode"
    uses "TinyGen"
    uses "TinyOps"
    uses "TinyConstant"
    uses "TinyType"
    uses "TinySymbols"
    
    bool parseExpression(ref string actualType)
    {
        bool success;
        loop
        {
            if (!parseAssignmentExpression(ref actualType))
            {
                break;
            }
    
            Token token = TinyScanner.Current();        
            if (token.Type == TokenType.KW_AS)
            {
                string asType;
                TinyScanner.Advance(); // Skip 'as'
                uint size;
                if (!TinyCompile.parseType(ref asType, ref size))
                {
                    break;
                }
                if (IsAutomaticCast(asType, actualType, false, true))
                {
                    actualType = asType;
                }
                else
                {
                    Error(token.SourcePath, token.Line, "'" + actualType + "' as '" + asType + "' failed");
                    break;
                }
            }
            success = true;
            break;
        }
        return success;   
    }

    bool parseAssignmentExpression(ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.IDENTIFIER) || (token.Type == TokenType.KW_MEM))
        {
            string name = token.Lexeme;
            bool hasIndex;
            bool isByteIndex;
            bool isIncDecIndexed;
            
            Token peek  = TinyScanner.Peek();
            if (peek.Type == TokenType.SYM_LLBRACKET)
            {
                TinyScanner.Advance(); // <name>
                TinyScanner.Advance(); // '[['
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
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after index expression");
                    return false;
                }
                peek  = TinyScanner.Peek();
                hasIndex = true;
                isByteIndex = IsByteType(indexType);
                isIncDecIndexed = (peek.Type == TokenType.SYM_PLUSPLUS) || (peek.Type == TokenType.SYM_MINUSMINUS);
            }
           
            if ((peek.Type == TokenType.SYM_EQ) || IsCompoundAssignmentOperator(peek.Type) || isIncDecIndexed)
            {
                TinyScanner.Advance(); // name or ']'
                    
                token = TinyScanner.Current(); 
                string op = token.Lexeme;
                
                TinyGen.Comment(name + " " + op);
                TinyScanner.Advance(); // Skip '=' or compound assignment operator
                
                
                int    offset;
                bool   isGlobal;
                if (hasIndex && (name == "mem"))
                {
                    actualType = "byte";
                    if (isIncDecIndexed)
                    {
                        TinyGen.Dup(isByteIndex);
                    }
                    // memory address is TOS, twice
                }
                else
                {
                    if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                    {
                        Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                        return false;
                    }
                    if (hasIndex)
                    {
                        actualType = GetArrayMemberType(actualType);
                    }
                }
                bool isByte = IsByteType(actualType);
                
                if (hasIndex && (name != "mem"))
                {
                    // calculate the address we want to access
                    if (isByteIndex)
                    {
                        // for simplicity, cast the index to a word
                        TinyCode.CastPad(false);
                    }
                    if (!isByte)
                    {
                        // index *= 2;
                        TinyGen.Dup(false);
                        TinyGen.Add(false);
                    }
                    TinyGen.PushVariable(offset, false, isGlobal);
                    TinyGen.Add(false);
                    if (isIncDecIndexed)
                    {
                        TinyGen.Dup(false);
                    }
                    // array member address is TOS
                }
                
                if (op != "=")
                {
                    if (hasIndex && (name == "mem"))
                    {
                        TinyGen.Dup(isByteIndex);
                        TinyGen.PushMemory(isByteIndex, isByte); // TinyCode.ReadMemory(isByteIndex, isByte);
                    }
                    else
                    {
                        if (hasIndex)
                        {
                            // push member based on address
                            TinyGen.Comment("array assignment operator '" + op + "' getitem");
                            TinyGen.Dup(false);
                            TinyGen.PushMemory(false, isByte); // TinyCode.ReadMemory(false, isByte);
                        }
                        else
                        {
                            TinyGen.PushVariable(offset, isByte, isGlobal);
                        }
                    }
                }
                if (isIncDecIndexed)
                {
                    TinyGen.PushImmediate(isByte, 1);
                }
                else
                {  
                    string rhsType;
                    if (!parseExpression(ref rhsType))
                    {
                        return false;
                    }
                    
                    if (!IsAutomaticCast(actualType, rhsType, false, false))
                    {
                        TypeError(actualType, rhsType);
                        return false;
                    }
                }
                
                switch (op)
                {
                    case "+=":
                    case "++":
                    {
                        TinyGen.Add(IsByteType(actualType));
                    }
                    case "--":
                    case "-=":
                    {
                        TinyGen.Sub(IsByteType(actualType));
                    }
                    case "/=":
                    {
                        if (IsSignedType(actualType))
                        {
                            TinyGen.DivI();
                        }
                        else
                        {
                            TinyGen.Div(IsByteType(actualType));
                        }
                    }
                    case "*=":
                    {
                        if (IsSignedType(actualType))
                        {
                            TinyGen.MulI();
                        }
                        else
                        {
                            TinyGen.Mul(IsByteType(actualType));
                        }
                    }
                }
                
                if (hasIndex && (name == "mem"))
                {
                    TinyGen.PopMemory(isByteIndex, isByte); // TinyCode.WriteMemory(isByteIndex, isByte);
                    if (isIncDecIndexed)
                    {
                        TinyGen.PushMemory(isByteIndex, isByte); // TinyCode.ReadMemory(isByteIndex, isByte); // for the expression result
                    }
                }
                else
                {
                    if (hasIndex)
                    {
                        // pop member based on address
                        TinyGen.Comment("array assignment operator '" + op + "' setitem");
                        TinyGen.PopMemory(false, isByte); // TinyCode.WriteMemory(false, isByte);
                        if (isIncDecIndexed)
                        {
                            TinyGen.PushMemory(false, isByte); // TinyCode.ReadMemory(false, isByte); // for the expression result
                        }
                    }
                    else
                    {
                        TinyGen.PopVariable(offset, isByte, isGlobal);
                    }
                }
                if (!isIncDecIndexed)
                {
                    actualType = "void"; // no expression result to pop
                }
                return true;
            }
        }
        
        if (!parseConditionalExpression(ref actualType))
        {
            return false;
        }
        return true;
    }
    
    bool parseConditionalExpression(ref string actualType)
    {
        if (!parseLogicalOrExpression(ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        if (token.Type == TokenType.SYM_QUESTION)
        {
            if (actualType != "bool")
            {
                Error(token.SourcePath, token.Line, "boolean expression expected");
                return false;
            }    
            
            //TinyCode.If("if");
            //TinyCode.PadOut("{", 0);
            
            TinyGen.IF();
            
            TinyScanner.Advance(); // Skip '?'
            string trueType;
            if (!parseExpression(ref trueType))
            {
                return false;
            }
            
            //TinyCode.PadOut("}", 0);
            //TinyCode.Else();
            //TinyCode.PadOut("{", 0);
            
            TinyGen.ELSE();
            
            token = TinyScanner.Current();
            if (token.Type != TokenType.SYM_COLON)
            {
                Error(token.SourcePath, token.Line, "expected ':' in conditional expression");
                return false;
            }
            TinyScanner.Advance(); // Skip ':'
            string falseType;
            if (!parseConditionalExpression(ref falseType))
            {
                return false;
            }
            
            //TinyCode.PadOut("}", 0);
            TinyGen.ENDIF();
          
            if (!MatchTypes(trueType, ref falseType))
            {
                TypeError(trueType, falseType);
                return false;
            }
            actualType = falseType;
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
            token = TinyScanner.Current();
            // assuming both arguments are either 0 or 1
            TinyGen.Or(true);
            actualType = "bool";
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
            token = TinyScanner.Current();
            // assuming both arguments are either 0 or 1
            TinyGen.And(true);
            actualType = "bool";
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
            TinyGen.Or(IsByteType(actualType));
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
            TinyGen.Xor(IsByteType(actualType));
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
            TinyGen.And(IsByteType(actualType));
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
            token = TinyScanner.Current();
            
            if (!MatchTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            if (op == "==")
            {
                TinyGen.EQ(IsByteType(actualType));
            }
            else
            {
                // !=
                TinyGen.NE(IsByteType(actualType));
            }
        
            actualType = "bool";
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
            token = TinyScanner.Current(); // we fail if we don't call this?!
            
            
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            switch (op)
            {
                case "<":
                {
                    if (IsSignedType(actualType))
                    {
                        TinyGen.LTI();
                    }
                    else
                    {
                        TinyGen.LT(IsByteType(actualType));
                    }
                }
                case "<=":
                {
                    if (IsSignedType(actualType))
                    {
                        TinyGen.LEI();
                    }
                    else
                    {
                        TinyGen.LE(IsByteType(actualType));
                    }
                }
                case ">":
                {
                    if (IsSignedType(actualType))
                    {
                        TinyGen.GTI();
                    }
                    else
                    {
                        TinyGen.GT(IsByteType(actualType));
                    }
                }
                case ">=":
                {
                    if (IsSignedType(actualType))
                    {
                        TinyGen.GEI();
                    }
                    else
                    {
                        TinyGen.GE(IsByteType(actualType));
                    }
                }
            }
            actualType = "bool";
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
            if (op == "<<")
            {
                TinyGen.Shl(IsByteType(actualType)); // assumes [top] is <= 255
            }
            else
            {
                TinyGen.Shr(IsByteType(actualType)); // assumes [top] is <= 255
            }
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
            token = TinyScanner.Current(); // we fail if we don't call this?!
            
            if (!MatchNumericTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            if (op == "+")
            {
                TinyGen.Add(IsByteType(actualType));
            } 
            else
            {
                TinyGen.Sub(IsByteType(actualType));
            }
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
            if (op == "*")
            {
                if (IsSignedType(actualType))
                {
                    TinyGen.MulI();
                }
                else
                {
                    TinyGen.Mul(IsByteType(actualType));
                }
            }
            else if (op == "/")
            {
                if (IsSignedType(actualType))
                {
                    TinyGen.DivI();
                }
                else
                {
                    TinyGen.Div(IsByteType(actualType));
                }
            }
            else if (op == "%")
            {
                if (IsSignedType(actualType))
                {
                    TinyGen.ModI();
                }
                else
                {
                    TinyGen.Mod(IsByteType(actualType));
                }
            }
            else
            {
                Die(0x0B);
            } 
        }
        return true;
    }

    bool parseUnaryExpression(ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            string op = token.Lexeme;
            if (op == "-")
            {
                TinyGen.Comment("0 -");
                TinyGen.PushImmediate(false, 0);
            }
            TinyScanner.Advance(); // Skip unary operator
            if (!parsePrimaryExpression(ref actualType))
            {
                return false;
            }
            if (op == "-")
            {
                if (!IsNumericType(actualType))
                {
                    Error(token.SourcePath, token.Line, "numeric expression expected");
                    return false;
                }
                if (!IsAutomaticCast("int", actualType, false, true))
                {
                    TypeError("int", actualType);
                    return false;
                }
                actualType = "int";
                TinyGen.Sub(IsByteType(actualType));
            }
            else if (op == "~")
            {
                if (!IsNumericType(actualType))
                {
                    Error(token.SourcePath, token.Line, "numeric expression expected");
                    return false;
                }
                TinyGen.Not(IsByteType(actualType));
            }
            else if (op == "!")
            {
                if (actualType != "bool")
                {
                    Error(token.SourcePath, token.Line, "boolean expression expected");
                    return false;
                }
                TinyGen.Not(true);
            }
            else
            { 
                Die(0x0B);
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
        bool success;
        loop
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
                                break;
                            }
                            if (!IsIndexType(indexType))
                            {
                                Error(token.SourcePath, token.Line, "integral index type expected");
                                break;
                            }
                            if (!actualType.Contains('['))
                            {
                                Error(token.SourcePath, token.Line, "array identifier expected");
                                break;
                            }
                            actualType = GetArrayMemberType(actualType);
                            token = TinyScanner.Current();
                            if (token.Type != TokenType.SYM_RBRACKET)
                            {
                                Error(token.SourcePath, token.Line, "expected ']' after array index");
                                break;
                            }
                            TinyScanner.Advance(); // Skip ']'
                            
                            if (IsByteType(indexType))
                            {
                                CastPad(false);
                            }
                            
                            uint address;
                            _ = UInt.TryParse(constantValue, ref address);
                            TinyGen.PushConst(address);
                            
                            TinyGen.Add(false);  // const [] access in parsePrimaryExpression : add the index to the address (assumes char/byte array)
                            TinyGen.PushMemory(false, true); // TinyCode.ReadMemory(false, true); 
                        }
                        else
                        {
                            // return the entire array
                            uint address;
                            _ = UInt.TryParse(constantValue, ref address);
                            TinyCode.PushConst(address);
                        }
                    } // []
                    else
                    {
                        uint   literalValue;
                        string literalComment = "const " + name;
                        if (actualType == "const bool")
                        {
                            literalValue = (constantValue == "true" ? 1 : 0);
                        }
                        else if (actualType == "const char")
                        {
                            literalValue = byte(constantValue[0]);
                        }
                        else if (actualType == "const int")
                        {
                            int value;
                            if (!Int.TryParse(constantValue, ref value))
                            {
                                Die(0x0B);
                            }
                            literalValue = UInt.FromBytes(value.GetByte(0), value.GetByte(1));
                        }
                        else
                        {
                            if (!UInt.TryParse(constantValue, ref literalValue))
                            {
                                Die(0x0B);
                            }
                        }
                        TinyGen.Comment(literalComment);
                        TinyGen.PushImmediate(IsByteType(actualType), literalValue);
                    }
                    success = true;
                    break;
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
                            break;
                        }
                        if (actualType != "func")
                        {
                            Error(token.SourcePath, token.Line, "invalid use of '" + name + "'");
                            break;
                        }
                        // untyped func pointer, arbitrarily pick "word" as return type
                        actualType = "word";
                    }
                    
                    TinyScanner.Advance(); // Skip '('
                    byte bytes;
                    if (!parseArgumentList(name, ref bytes))
                    {
                        break;
                    }
                    token = TinyScanner.Current();
                    if (token.Type != TokenType.SYM_RPAREN)
                    {
                        Error(token.SourcePath, token.Line, "expected ')' after argument list, ('" + token.Lexeme + "')");
                        break;
                    }
                    TinyScanner.Advance(); // Skip ')'
                    TinyGen.Call(name, IsByteType(actualType), actualType != "void", bytes);
                    //TinyGen.DecSP(bytes); // TinyCode.PopBytes(bytes, name + " arguments"); // arguments after returning from method call
                    //if (actualType != "void")
                    //{
                    //    TinyGen.PushTOP(IsByteType(actualType)); // TinyOps.PushTop(IsByteType(actualType));
                    //}
                }
                else if (token.Type == TokenType.SYM_LBRACKET)
                {
                    // array accessor
                    int  offset;
                    bool isGlobal;
                    if (name == "mem")
                    {
                        actualType = "byte";
                    }
                    else
                    {
                        if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                        {
                            Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                            break;
                        }
                    }
                    
                    TinyScanner.Advance(); // Skip '['
                    string indexType;
                    if (!parseExpression(ref indexType))
                    {
                        break;
                    }
                    if (!IsIndexType(indexType))
                    {
                        Error(token.SourcePath, token.Line, "integral index type expected");
                        break;
                    }
                    token = TinyScanner.Current();
                    if (token.Type != TokenType.SYM_RBRACKET)
                    {
                        Error(token.SourcePath, token.Line, "expected ']' after array index");
                        break;
                    }
                    TinyScanner.Advance(); // Skip ']'
                    if (name == "mem")
                    {
                        actualType = "byte";
                        TinyGen.PushMemory(IsByteType(indexType), true); // TinyCode.ReadMemory(IsByteType(indexType), true);
                    }
                    else
                    {
                        TinyGen.Comment("array getitem"); 
                        // calculate the address we want to access
                        if (IsByteType(indexType))
                        {
                            // for simplicity, cast the index to a word
                            CastPad(false);
                        }
                        if (!actualType.Contains('['))
                        {
                            Error(token.SourcePath, token.Line, "array identifier expected");
                            break;
                        }
                        actualType = GetArrayMemberType(actualType);
                        if (!IsByteType(actualType))
                        {
                            // index *= 2;
                            TinyGen.Dup(false);
                            TinyGen.Add(false);
                        }
                        TinyGen.PushVariable(offset, false, isGlobal);
                        TinyGen.Add(false);
                        TinyGen.PushMemory(false, IsByteType(actualType)); //TinyCode.ReadMemory(false, IsByteType(actualType));  
                    }
                }
                else
                {
                    // variable
                    int  offset;
                    bool isGlobal;
                    if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                    {
                        Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                        break;
                    }
                    if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
                    {
                        
                        TinyScanner.Advance(); // Skip '++' or '--'
                        // TinyCode.PostIncrement(name, offset, IsByteType(actualType), token.Type == TokenType.SYM_PLUSPLUS, isGlobal);
                        
                        // i++ or i-- : value before --/++ is used in the expression
                        TinyGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                        
                        TinyGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                        TinyGen.PushImmediate(IsByteType(actualType), 1);
                        if (token.Type == TokenType.SYM_PLUSPLUS)
                        {
                            TinyGen.Add(IsByteType(actualType));
                        }
                        else
                        {
                            TinyGen.Sub(IsByteType(actualType));
                        }
                        TinyGen.PopVariable(offset, IsByteType(actualType), isGlobal);
                    }
                    else
                    {
                        TinyGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                    }
                }
            }
            else if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
            {
                
                bool inc = token.Type == TokenType.SYM_PLUSPLUS;
                TinyScanner.Advance(); // Skip '++' or '--'
                token = TinyScanner.Current();
                if (token.Type == TokenType.IDENTIFIER)
                {
                    string name = token.Lexeme;
                    TinyScanner.Advance(); // Skip identifier
                    int  offset;
                    bool isGlobal;
                    if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                    {
                        Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                        break;
                    }
                    //TinyCode.PreIncrement(name, offset, IsByteType(actualType), inc, isGlobal);
                    
                    TinyGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                    TinyGen.PushImmediate(IsByteType(actualType), 1);
                    if (token.Type == TokenType.SYM_PLUSPLUS)
                    {
                        TinyGen.Add(IsByteType(actualType));
                    }
                    else
                    {
                        TinyGen.Sub(IsByteType(actualType));
                    }
                    TinyGen.PopVariable(offset, IsByteType(actualType), isGlobal);
                    
                    // ++i or --i  : value after --/++ is used in the expression
                    TinyGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                }
                else
                {
                    Error(token.SourcePath, token.Line, "expected identifier after '++' or '--'");
                    break;
                }
            }
            else if (token.Type == TokenType.KW_MEM)
            {
                TinyScanner.Advance(); // Skip 'func'
                
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_LBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected '[' after address expression");
                    break;
                }
                TinyScanner.Advance(); // Skip '['
                
                string indexType;
                if (!parseExpression(ref indexType))
                {
                    break;
                }
                if (!IsIndexType(indexType))
                {
                    Error(token.SourcePath, token.Line, "integral address type expected");
                    break;
                }
                
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after address expression");
                    break;
                }
                TinyScanner.Advance(); // Skip ']'
                actualType = "byte";
                
                TinyGen.PushMemory(IsByteType(indexType), true); // TinyCode.ReadMemory(IsByteType(indexType), true);
            }
            else if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_TRUE))
            {
                string value;
                if (!TinyConstant.parseConstantPrimary(ref value, ref actualType))
                {
                    break;
                }
                
                uint   literalValue;
                string literalComment= "literal";
                
                switch (actualType)
                {
                    case "char":
                    {
                        literalValue   = byte(value[0]);
                        literalComment = "literal '" + value + "'";
                    }
                    case "bool":
                    {
                        literalValue   = (value == "0") ? 0 : 1;
                        literalComment = "literal '" + ((value == "0") ? "false" : "true") + "'";
                    }
                    case "int":
                    {
                        int i;
                        if (!Int.TryParse(value, ref i))
                        {
                            Die(0x0B);
                            break;
                        }
                        literalValue   = UInt.FromBytes(i.GetByte(0), i.GetByte(1));
                    }
                    default:
                    {
                        if (!UInt.TryParse(value, ref literalValue))
                        {
                            Die(0x0B);
                            break;
                        }
                    }
                }
                TinyGen.Comment(literalComment);
                TinyGen.PushImmediate(IsByteType(actualType), literalValue);
            }
            else if ((token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.KW_NULL))
            {
                actualType = "const char[]";
                uint index;
                DefineStringConst(token.Lexeme, ref index);
                TinyGen.PushConst(index);
                TinyScanner.Advance(); // Skip literal
            }
            else if (token.Type == TokenType.KW_NULL)
            {
                actualType = "[]"; // any pointer
                TinyGen.Comment("null");
                TinyGen.PushImmediate(false, 0);
                TinyScanner.Advance(); // Skip literal
            }
            else if (token.Type == TokenType.SYM_LPAREN)
            {
                TinyScanner.Advance(); // Skip '('
                if (!parseExpression(ref actualType))
                {
                    break;
                }
                token = TinyScanner.Current();
                if (token.Type != TokenType.SYM_RPAREN)
                {
                    Error(token.SourcePath, token.Line, "expected ')' after expression, ('" + token.Lexeme + "')");
                    break;
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
                    break;
                }
            }
            else
            {
                Error(token.SourcePath, token.Line, "unexpected token in expression: " + TinyToken.ToString(token.Type));
                break;
            }
            success = true;
            break;
        }
        return success;
    }
    
    bool parseArgumentList(string functionName, ref byte bytes)
    {
        Token token;
        <string> argumentNames;
        <string,Variable> arguments = GetArguments(functionName, ref argumentNames);
        uint currentArgument = 0;
        loop
        {
            token = TinyScanner.Current();
            if (token.Type == TokenType.SYM_RPAREN)
            {
                break; // exit the loop when reaching ')'
            }
            if (currentArgument == argumentNames.Count)
            {
                Error(token.SourcePath, token.Line, "too many arguments for '" + functionName  + "'");
                return false;
            }
            string argType;
            if (!parseExpression(ref argType))
            {
                return false;
            }
            string argName = argumentNames[currentArgument];
            Variable argument = arguments[argName];
            if (!IsAutomaticCast(argument.Type, argType, false, false))
            {
                 Error(token.SourcePath, token.Line, "expected '" + argument.Type + "' for argument '" + argName  + "', was '" + argType + "'");
                 return false;
            }
            
            bytes += (IsByteType(argument.Type) ? 1 : 2);
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
            currentArgument++;
        }
        if (currentArgument < argumentNames.Count)
        {
            Error(token.SourcePath, token.Line, "too few arguments for '" + functionName  + "'");
            return false;
        }
        return true; // success
    }
    
    bool Expression(ref string actualType)
    {
        TinyGen.BeginStream(false);
        
        bool success = parseExpression(ref actualType);
        if (success)
        {
            TinyGen.FlushStream();
        }
        return success;
    }
}

