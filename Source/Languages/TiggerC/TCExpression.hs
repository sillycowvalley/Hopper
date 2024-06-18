unit TCExpression
{
    uses "TCToken"
    uses "TCScanner"
    uses "TCCode"
    uses "TCGen"
    uses "TCOps"
    uses "TCConstant"
    uses "TCType"
    uses "TCSymbols"
    
    bool parseExpression(ref string actualType)
    {
        bool success;
        loop
        {
            if (!parseAssignmentExpression(ref actualType))
            {
                break;
            }
    
            Token token = TCScanner.Current();        
            if (token.Type == TokenType.KW_AS)
            {
                string asType;
                TCScanner.Advance(); // Skip 'as'
                uint size;
                if (!TCCompile.parseType(ref asType, ref size))
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
        Token token = TCScanner.Current();
        if ((token.Type == TokenType.IDENTIFIER) || (token.Type == TokenType.KW_MEM))
        {
            string name = token.Lexeme;
            bool hasIndex;
            bool isByteIndex;
            bool isIncDecIndexed;
            
            Token peek  = TCScanner.Peek();
            if (peek.Type == TokenType.SYM_LLBRACKET)
            {
                TCScanner.Advance(); // <name>
                TCScanner.Advance(); // '[['
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
                token = TCScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after index expression");
                    return false;
                }
                peek  = TCScanner.Peek();
                hasIndex = true;
                isByteIndex = IsByteType(indexType);
                isIncDecIndexed = (peek.Type == TokenType.SYM_PLUSPLUS) || (peek.Type == TokenType.SYM_MINUSMINUS);
            }
           
            if ((peek.Type == TokenType.SYM_EQ) || IsCompoundAssignmentOperator(peek.Type) || isIncDecIndexed)
            {
                TCScanner.Advance(); // name or ']'
                    
                token = TCScanner.Current(); 
                string op = token.Lexeme;
                
                TCGen.Comment(name + " " + op);
                TCScanner.Advance(); // Skip '=' or compound assignment operator
                
                
                int    offset;
                bool   isGlobal;
                if (hasIndex && (name == "mem"))
                {
                    actualType = "byte";
                    if (isIncDecIndexed)
                    {
                        TCGen.Dup(isByteIndex);
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
                        TCCode.CastPad(false);
                    }
                    if (!isByte)
                    {
                        // index *= 2;
                        TCGen.Dup(false);
                        TCGen.Add(false);
                    }
                    TCGen.PushVariable(offset, false, isGlobal);
                    TCGen.Add(false);
                    if (isIncDecIndexed)
                    {
                        TCGen.Dup(false);
                    }
                    // array member address is TOS
                }
                
                if (op != "=")
                {
                    if (hasIndex && (name == "mem"))
                    {
                        TCGen.Dup(isByteIndex);
                        TCGen.PushMemory(isByteIndex, isByte); // TCCode.ReadMemory(isByteIndex, isByte);
                    }
                    else
                    {
                        if (hasIndex)
                        {
                            // push member based on address
                            TCGen.Comment("array assignment operator '" + op + "' getitem");
                            TCGen.Dup(false);
                            TCGen.PushMemory(false, isByte); // TCCode.ReadMemory(false, isByte);
                        }
                        else
                        {
                            TCGen.PushVariable(offset, isByte, isGlobal);
                        }
                    }
                }
                if (isIncDecIndexed)
                {
                    TCGen.PushImmediate(isByte, 1);
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
                        TCGen.Add(IsByteType(actualType));
                    }
                    case "--":
                    case "-=":
                    {
                        TCGen.Sub(IsByteType(actualType));
                    }
                    case "/=":
                    {
                        if (IsSignedType(actualType))
                        {
                            TCGen.DivI();
                        }
                        else
                        {
                            TCGen.Div(IsByteType(actualType));
                        }
                    }
                    case "*=":
                    {
                        if (IsSignedType(actualType))
                        {
                            TCGen.MulI();
                        }
                        else
                        {
                            TCGen.Mul(IsByteType(actualType));
                        }
                    }
                }
                
                if (hasIndex && (name == "mem"))
                {
                    TCGen.PopMemory(isByteIndex, isByte); // TCCode.WriteMemory(isByteIndex, isByte);
                    if (isIncDecIndexed)
                    {
                        TCGen.PushMemory(isByteIndex, isByte); // TCCode.ReadMemory(isByteIndex, isByte); // for the expression result
                    }
                }
                else
                {
                    if (hasIndex)
                    {
                        // pop member based on address
                        TCGen.Comment("array assignment operator '" + op + "' setitem");
                        TCGen.PopMemory(false, isByte); // TCCode.WriteMemory(false, isByte);
                        if (isIncDecIndexed)
                        {
                            TCGen.PushMemory(false, isByte); // TCCode.ReadMemory(false, isByte); // for the expression result
                        }
                    }
                    else
                    {
                        TCGen.PopVariable(offset, isByte, isGlobal);
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
        Token token = TCScanner.Current();
        if (token.Type == TokenType.SYM_QUESTION)
        {
            if (actualType != "bool")
            {
                Error(token.SourcePath, token.Line, "boolean expression expected");
                return false;
            }    
            
            //TCCode.If("if");
            //TCCode.PadOut("{", 0);
            
            TCGen.IF();
            
            TCScanner.Advance(); // Skip '?'
            string trueType;
            if (!parseExpression(ref trueType))
            {
                return false;
            }
            
            //TCCode.PadOut("}", 0);
            //TCCode.Else();
            //TCCode.PadOut("{", 0);
            
            TCGen.ELSE();
            
            token = TCScanner.Current();
            if (token.Type != TokenType.SYM_COLON)
            {
                Error(token.SourcePath, token.Line, "expected ':' in conditional expression");
                return false;
            }
            TCScanner.Advance(); // Skip ':'
            string falseType;
            if (!parseConditionalExpression(ref falseType))
            {
                return false;
            }
            
            //TCCode.PadOut("}", 0);
            TCGen.ENDIF();
          
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

        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_PIPEPIPE)
        {
            TCScanner.Advance(); // Skip '||'
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
            token = TCScanner.Current();
            // assuming both arguments are either 0 or 1
            TCGen.Or(true);
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

        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_AMPAMP)
        {
            TCScanner.Advance(); // Skip '&&'
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
            token = TCScanner.Current();
            // assuming both arguments are either 0 or 1
            TCGen.And(true);
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

        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_PIPE)
        {
            TCScanner.Advance(); // Skip '|'
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
            token = TCScanner.Current();
            TCGen.Or(IsByteType(actualType));
        }
        return true;
    }

    bool parseBitwiseXorExpression(ref string actualType)
    {
        if (!parseBitwiseAndExpression(ref actualType))
        {
            return false;
        }

        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_CARET)
        {
            TCScanner.Advance(); // Skip '^'
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
            token = TCScanner.Current();
            TCGen.Xor(IsByteType(actualType));
        }
        return true;
    }

    bool parseBitwiseAndExpression(ref string actualType)
    {
        if (!parseEqualityExpression(ref actualType))
        {
            return false;
        }

        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_AMP)
        {
            TCScanner.Advance(); // Skip '&'
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
            token = TCScanner.Current();
            TCGen.And(IsByteType(actualType));
        }
        return true;
    }

    bool parseEqualityExpression(ref string actualType)
    {
        if (!parseRelationalExpression(ref actualType))
        {
            return false;
        }

        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_EQEQ) || (token.Type == TokenType.SYM_NEQ))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip '==' or '!='
            string rhsType;
            if (!parseRelationalExpression(ref rhsType))
            {
                return false;
            }
            token = TCScanner.Current();
            
            if (!MatchTypes(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            if (op == "==")
            {
                TCGen.EQ(IsByteType(actualType));
            }
            else
            {
                // !=
                TCGen.NE(IsByteType(actualType));
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

        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_LT) || (token.Type == TokenType.SYM_LTE) || (token.Type == TokenType.SYM_GT) || (token.Type == TokenType.SYM_GTE))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip '<', '<=', '>', '>='
            string rhsType;
            if (!parseShiftExpression(ref rhsType))
            {
                return false;
            }
            token = TCScanner.Current(); // we fail if we don't call this?!
            
            
            if (!MatchNumericTypesForRelational(rhsType, ref actualType))
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
                        TCGen.LTI();
                    }
                    else
                    {
                        TCGen.LT(IsByteType(actualType));
                    }
                }
                case "<=":
                {
                    if (IsSignedType(actualType))
                    {
                        TCGen.LEI();
                    }
                    else
                    {
                        TCGen.LE(IsByteType(actualType));
                    }
                }
                case ">":
                {
                    if (IsSignedType(actualType))
                    {
                        TCGen.GTI();
                    }
                    else
                    {
                        TCGen.GT(IsByteType(actualType));
                    }
                }
                case ">=":
                {
                    if (IsSignedType(actualType))
                    {
                        TCGen.GEI();
                    }
                    else
                    {
                        TCGen.GE(IsByteType(actualType));
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

        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_LSHIFT) || (token.Type == TokenType.SYM_RSHIFT))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip '<<' or '>>'
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
            token = TCScanner.Current();
            if (op == "<<")
            {
                TCGen.Shl(IsByteType(actualType)); // assumes [top] is <= 255
            }
            else
            {
                TCGen.Shr(IsByteType(actualType)); // assumes [top] is <= 255
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

        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_MINUS))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip '+' or '-'
            string rhsType;
            if (!parseMultiplicativeExpression(ref rhsType))
            {
                return false;
            }
            token = TCScanner.Current(); // we fail if we don't call this?!
            
            if (!MatchNumericTypesForAddition(rhsType, ref actualType))
            {
                TypeError(actualType, rhsType);
                return false;
            }
            if (op == "+")
            {
                TCGen.Add(IsByteType(actualType));
            } 
            else
            {
                TCGen.Sub(IsByteType(actualType));
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

        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_STAR) || (token.Type == TokenType.SYM_SLASH) || (token.Type == TokenType.SYM_PERCENT))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip '*', '/', '%'
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
            token = TCScanner.Current();
            if (op == "*")
            {
                if (IsSignedType(actualType))
                {
                    TCGen.MulI();
                }
                else
                {
                    TCGen.Mul(IsByteType(actualType));
                }
            }
            else if (op == "/")
            {
                if (IsSignedType(actualType))
                {
                    TCGen.DivI();
                }
                else
                {
                    TCGen.Div(IsByteType(actualType));
                }
            }
            else if (op == "%")
            {
                if (IsSignedType(actualType))
                {
                    TCGen.ModI();
                }
                else
                {
                    TCGen.Mod(IsByteType(actualType));
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
        Token token = TCScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            string op = token.Lexeme;
            if (op == "-")
            {
                TCGen.Comment("0 -");
                TCGen.PushImmediate(false, 0);
            }
            TCScanner.Advance(); // Skip unary operator
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
                TCGen.Sub(IsByteType(actualType));
            }
            else if (op == "~")
            {
                if (!IsNumericType(actualType))
                {
                    Error(token.SourcePath, token.Line, "numeric expression expected");
                    return false;
                }
                TCGen.Not(IsByteType(actualType));
            }
            else if (op == "!")
            {
                if (actualType != "bool")
                {
                    Error(token.SourcePath, token.Line, "boolean expression expected");
                    return false;
                }
                TCGen.BoolNot();
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
            Token token = TCScanner.Current();
            if (token.Type == TokenType.IDENTIFIER)
            {
                string name = token.Lexeme;
                TCScanner.Advance(); // Skip identifier
                
                string constantValue;
                if (GetConst(name, ref actualType, ref constantValue))
                {
                    if (actualType.EndsWith(']'))
                    {
                        // const char[] palette = ".,'~=+:;*%&$OXB#@ ";
                        token = TCScanner.Current();
                        if (token.Type == TokenType.SYM_LBRACKET)
                        {
                            // pick out an array member
                            TCScanner.Advance(); // Skip '['
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
                            token = TCScanner.Current();
                            if (token.Type != TokenType.SYM_RBRACKET)
                            {
                                Error(token.SourcePath, token.Line, "expected ']' after array index");
                                break;
                            }
                            TCScanner.Advance(); // Skip ']'
                            
                            if (IsByteType(indexType))
                            {
                                CastPad(false);
                            }
                            
                            uint address;
                            _ = UInt.TryParse(constantValue, ref address);
                            TCGen.PushConst(address);
                            
                            TCGen.Add(false);  // const [] access in parsePrimaryExpression : add the index to the address (assumes char/byte array)
                            TCGen.PushMemory(false, true); // TCCode.ReadMemory(false, true); 
                        }
                        else
                        {
                            // return the entire array
                            uint address;
                            _ = UInt.TryParse(constantValue, ref address);
                            TCGen.PushConst(address);
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
                        TCGen.Comment(literalComment);
                        TCGen.PushImmediate(IsByteType(actualType), literalValue);
                    }
                    success = true;
                    break;
                }
            
            
                token = TCScanner.Current();
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
                    
                    TCScanner.Advance(); // Skip '('
                    byte bytes;
                    if (!parseArgumentList(name, ref bytes))
                    {
                        break;
                    }
                    token = TCScanner.Current();
                    if (token.Type != TokenType.SYM_RPAREN)
                    {
                        Error(token.SourcePath, token.Line, "expected ')' after argument list, ('" + token.Lexeme + "')");
                        break;
                    }
                    TCScanner.Advance(); // Skip ')'
                    TCGen.Call(name, IsByteType(actualType), actualType != "void", bytes);
                    //TCGen.DecSP(bytes); // TCCode.PopBytes(bytes, name + " arguments"); // arguments after returning from method call
                    //if (actualType != "void")
                    //{
                    //    TCGen.PushTOP(IsByteType(actualType)); // TCOps.PushTop(IsByteType(actualType));
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
                    
                    TCScanner.Advance(); // Skip '['
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
                    token = TCScanner.Current();
                    if (token.Type != TokenType.SYM_RBRACKET)
                    {
                        Error(token.SourcePath, token.Line, "expected ']' after array index");
                        break;
                    }
                    TCScanner.Advance(); // Skip ']'
                    if (name == "mem")
                    {
                        actualType = "byte";
                        TCGen.PushMemory(IsByteType(indexType), true); // TCCode.ReadMemory(IsByteType(indexType), true);
                    }
                    else
                    {
                        TCGen.Comment("array getitem"); 
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
                            TCGen.Dup(false);
                            TCGen.Add(false);
                        }
                        TCGen.PushVariable(offset, false, isGlobal);
                        TCGen.Add(false);
                        TCGen.PushMemory(false, IsByteType(actualType)); //TCCode.ReadMemory(false, IsByteType(actualType));  
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
                        
                        TCScanner.Advance(); // Skip '++' or '--'
                        // TCCode.PostIncrement(name, offset, IsByteType(actualType), token.Type == TokenType.SYM_PLUSPLUS, isGlobal);
                        
                        // i++ or i-- : value before --/++ is used in the expression
                        TCGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                        
                        TCGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                        TCGen.PushImmediate(IsByteType(actualType), 1);
                        if (token.Type == TokenType.SYM_PLUSPLUS)
                        {
                            TCGen.Add(IsByteType(actualType));
                        }
                        else
                        {
                            TCGen.Sub(IsByteType(actualType));
                        }
                        TCGen.PopVariable(offset, IsByteType(actualType), isGlobal);
                    }
                    else
                    {
                        TCGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                    }
                }
            }
            else if ((token.Type == TokenType.SYM_PLUSPLUS) || (token.Type == TokenType.SYM_MINUSMINUS))
            {
                
                bool inc = token.Type == TokenType.SYM_PLUSPLUS;
                TCScanner.Advance(); // Skip '++' or '--'
                token = TCScanner.Current();
                if (token.Type == TokenType.IDENTIFIER)
                {
                    string name = token.Lexeme;
                    TCScanner.Advance(); // Skip identifier
                    int  offset;
                    bool isGlobal;
                    if (!GetVariable(name, ref actualType, ref offset, ref isGlobal))
                    {
                        Error(token.SourcePath, token.Line, "undefined identifier '" + name + "'");
                        break;
                    }
                    //TCCode.PreIncrement(name, offset, IsByteType(actualType), inc, isGlobal);
                    
                    TCGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                    TCGen.PushImmediate(IsByteType(actualType), 1);
                    if (token.Type == TokenType.SYM_PLUSPLUS)
                    {
                        TCGen.Add(IsByteType(actualType));
                    }
                    else
                    {
                        TCGen.Sub(IsByteType(actualType));
                    }
                    TCGen.PopVariable(offset, IsByteType(actualType), isGlobal);
                    
                    // ++i or --i  : value after --/++ is used in the expression
                    TCGen.PushVariable(offset, IsByteType(actualType), isGlobal);
                }
                else
                {
                    Error(token.SourcePath, token.Line, "expected identifier after '++' or '--'");
                    break;
                }
            }
            else if (token.Type == TokenType.KW_MEM)
            {
                TCScanner.Advance(); // Skip 'func'
                
                token = TCScanner.Current();
                if (token.Type != TokenType.SYM_LBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected '[' after address expression");
                    break;
                }
                TCScanner.Advance(); // Skip '['
                
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
                
                token = TCScanner.Current();
                if (token.Type != TokenType.SYM_RBRACKET)
                {
                    Error(token.SourcePath, token.Line, "expected ']' after address expression");
                    break;
                }
                TCScanner.Advance(); // Skip ']'
                actualType = "byte";
                
                TCGen.PushMemory(IsByteType(indexType), true); // TCCode.ReadMemory(IsByteType(indexType), true);
            }
            else if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.LIT_CHAR) || (token.Type == TokenType.KW_FALSE) || (token.Type == TokenType.KW_TRUE))
            {
                string value;
                if (!TCConstant.parseConstantPrimary(ref value, ref actualType))
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
                TCGen.Comment(literalComment);
                TCGen.PushImmediate(IsByteType(actualType), literalValue);
            }
            else if ((token.Type == TokenType.LIT_STRING) || (token.Type == TokenType.KW_NULL))
            {
                actualType = "const char[]";
                uint index;
                DefineStringConst(token.Lexeme, ref index);
                TCGen.PushConst(index);
                TCScanner.Advance(); // Skip literal
            }
            else if (token.Type == TokenType.KW_NULL)
            {
                actualType = "[]"; // any pointer
                TCGen.Comment("null");
                TCGen.PushImmediate(false, 0);
                TCScanner.Advance(); // Skip literal
            }
            else if (token.Type == TokenType.SYM_LPAREN)
            {
                TCScanner.Advance(); // Skip '('
                if (!parseExpression(ref actualType))
                {
                    break;
                }
                token = TCScanner.Current();
                if (token.Type != TokenType.SYM_RPAREN)
                {
                    Error(token.SourcePath, token.Line, "expected ')' after expression, ('" + token.Lexeme + "')");
                    break;
                }
                TCScanner.Advance(); // Skip ')'
            }
            else if (token.Type == TokenType.KW_FUNC)
            {
                TCScanner.Advance(); // Skip 'func'
                token = TCScanner.Current();
                if (token.Type == TokenType.IDENTIFIER)
                {
                    TCScanner.Advance(); // Skip identifier
                }
                else
                {
                    Error(token.SourcePath, token.Line, "expected identifier after 'func'");
                    break;
                }
            }
            else
            {
                Error(token.SourcePath, token.Line, "unexpected token in expression: " + TCToken.ToString(token.Type));
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
            token = TCScanner.Current();
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
            token = TCScanner.Current();
            if (token.Type == TokenType.SYM_COMMA)
            {
                TCScanner.Advance(); // Skip ','
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
        TCGen.BeginStream(false);
        
        bool success = parseExpression(ref actualType);
        if (success)
        {
            TCGen.FlushStream();
        }
        return success;
    }
}
