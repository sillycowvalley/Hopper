unit TinyConstant
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    uses "TinyType"
    
    record Constant
    {
        string Type;
        string Value;
    }
    
    uint blockLevel;
    < <string, Constant> > constants;
    
    EnterBlock()
    {
        <string, Constant> level;
        constants.Append(level);
        blockLevel++;
    }
    LeaveBlock()
    {
        constants.Remove(constants.Count-1);
        blockLevel--;
    }
    bool DefineConst(string constantType, string constantName, string constantValue)
    {
        uint level = constants.Count -1;
        <string, Constant> scopeConstants = constants[level];
        if (scopeConstants.Contains(constantName))
        {
            // exists already
            Token token = TinyScanner.Current();
            Error(token.SourcePath, token.Line, "constant with name '" + constantName + "' already exists '");
            return false;
        }
        Constant newConstant;
        newConstant.Type  = constantType;
        newConstant.Value = constantValue;
        
        scopeConstants[constantName] = newConstant;
        
        constants[level] = scopeConstants;   
        //PrintLn(constantType + " " + constantName + " = " + constantValue);
        return true;
    }
    
    bool GetConst(string constantName, ref string constantType, ref string constantValue)
    {
        uint level = constants.Count -1;
        loop
        {
            <string, Constant> scopeConstants = constants[level];
            if (scopeConstants.Contains(constantName))
            {
                Constant constant = scopeConstants[constantName];
                constantType  = constant.Type;
                constantValue = constant.Value;
                return true;
            }
            if (level == 0) { break; }
            level--;
        }
        return false; // not found
    }
    
    
    bool evaluateArithmeticOperation(string leftValue, string rightValue,  string op, ref string resultValue, ref string resultType)
    {
        Token token = TinyScanner.Current();
        
        long left;
        long right;
        _ = Long.TryParse(rightValue, ref right);
        _ = Long.TryParse(leftValue, ref left);
        long result;

        switch (op)
        {
            case "+":
            {
                result = left + right;
            }
            case "-":
            {
                result = left - right;
            }
            case "*":
            {
                result = left * right;
            }
            case "/":
            {
                if (right == 0)
                {
                    Error(token.SourcePath, token.Line, "division by zero in constant expression");
                    return false;
                }
                result = left / right;
            }
            case "%":
            {
                if (right == 0)
                {
                    Error(token.SourcePath, token.Line, "division by zero in constant expression");
                    return false;
                }
                result = left % right;
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unsupported binary operator in constant expression");
                return false;
            }
        }

        if ((result >= -32768) && (result < 0))
        {
            resultType = "int";
        }
        else if ((result >= 0) && (result < 256))
        {
            resultType = "byte";
        }
        else if ((result >= 0) && (result < 32768))
        {
            resultType = "+int";
        }
        else if ((result >= 0) && (result <= 65535))
        {
            resultType = "word";
        }
        else
        {
            Error(token.SourcePath, token.Line, "result of binary operation out of range");
            return false;
        }
        resultValue = result.ToString();
        return true;
    }
    
    bool evaluateBitwiseOperation(string leftValue, string rightValue, string op, ref string resultValue, ref string resultType)
    {
        Token token = TinyScanner.Current();

        uint left;
        uint right;
        _ = UInt.TryParse(rightValue, ref right);
        _ = UInt.TryParse(leftValue, ref left);
        uint result;
        switch (op)
        {
            case "&":
            {
                result = left & right;
            }
            case "|":
            {
                result = left | right;
            }
            case "^":
            {
                result = left ^ right;
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unsupported bitwise operator in constant expression");
                return false;
            }
        }
        resultType = (result < 256) ? "byte" : "word";
        resultValue = result.ToString();
        return true;
    }
    
    
    bool parseConstantPrimary(ref string value, ref string actualType)
    {
        Token token = TinyScanner.Current();
        
        if (token.Type == TokenType.LIT_NUMBER)
        {
            value = token.Lexeme;
            long lv;
            if (!Long.TryParse(value, ref lv))
            {
                Error(token.SourcePath, token.Line, "error in integral constant, ('" + token.Lexeme + "')");
                return false;
            }
            if ((lv >= -32768) && (lv < 0))
            {
                actualType = "int";
            }
            else if ((lv >= 0) && (lv < 256))
            {
                actualType = "byte";
            }
            else if ((lv >= 0) && (lv < 32768))
            {
                actualType = "+int";
            }
            else if ((lv >= 0) && (lv <= 65535))
            {
                actualType = "word";
            }
            else
            {
                Error(token.SourcePath, token.Line, "integral constant out of range, ('" + token.Lexeme + "')");
                return false;
            }
            TinyScanner.Advance(); // Skip number or identifier
        }
        else if (token.Type == TokenType.IDENTIFIER)
        {
            if (!GetConst(token.Lexeme, ref actualType, ref value))
            {
                Error(token.SourcePath, token.Line, "undefined constant identifier in constant expression, ('" + token.Lexeme + "')");
                return false;
            }
            TinyScanner.Advance(); // Skip identifier
        }
        else if (token.Type == TokenType.LIT_STRING)
        {
            value = token.Lexeme;
            actualType = "const char[]";
            TinyScanner.Advance(); // Skip string literal
        }
        else if (token.Type == TokenType.LIT_CHAR)
        {
            value = token.Lexeme;
            actualType = "char";
            TinyScanner.Advance(); // Skip char literal
        }
        else
        {
            Error(token.SourcePath, token.Line, "expected number, identifier, string, or char in constant expression, ('" + token.Lexeme + "')");
            return false;
        }
        return true;
    }
    
    
    bool parseUnary(ref string value, ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            if (!parseConstantPrimary(ref value, ref actualType))
            {
                return false;
            }
            if (op == "-")
            {
                if (!IsNumericType(actualType))
                {
                    Error(token.SourcePath, token.Line, "numeric type expected in constant expression");
                    return false;
                }
                long lv;
                _ = Long.TryParse(value, ref lv);   
                lv = -lv;
                if ((lv >= -32768) && (lv < 0))
                {
                    actualType = "int";
                }
                else if ((lv >= 0) && (lv < 256))
                {
                    actualType = "byte";
                }
                else if ((lv >= 0) && (lv < 32768))
                {
                    actualType = "+int";
                }
                else if ((lv >= 0) && (lv <= 65535))
                {
                    actualType = "word";
                }
                else
                {
                    Error(token.SourcePath, token.Line, "integer constant out of range, ('" + token.Lexeme + "')");
                    return false;
                }
            }
            else
            {
                if (!IsNumericType(actualType))
                {
                    Error(token.SourcePath, token.Line, "numeric type expected in constant expression");
                    return false;
                }
                uint ui;
                _ = UInt.TryParse(value, ref ui);
                switch (op)
                {
                    case "+":
                    {
                        // Do nothing
                    }
                    case "!":
                    {
                        ui = (ui == 0) ? 1 : 0;
                    }
                    case "~":
                    {
                        ui = ~ui;
                    }
                }
                value = ui.ToString();
            }
        }
        else
        {
            if (!parseConstantPrimary(ref value, ref actualType))
            {
                return false;
            }
        }
        return true;
    }
    
    bool parseFactor(ref string value, ref string actualType)
    {
        if (!parseUnary(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_STAR) || (token.Type == TokenType.SYM_SLASH) || (token.Type == TokenType.SYM_PERCENT))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseUnary(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateArithmeticOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }
    
    bool parseTerm(ref string value, ref string actualType)
    {
        if (!parseFactor(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_MINUS))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseFactor(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateArithmeticOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }

    bool parseBitwiseShift(ref string value, ref string actualType)
    {
        if (!parseTerm(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while ((token.Type == TokenType.SYM_LSHIFT) || (token.Type == TokenType.SYM_RSHIFT))
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseTerm(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }

    bool parseBitwiseAnd(ref string value, ref string actualType)
    {
        if (!parseBitwiseShift(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_AMP)
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseBitwiseShift(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }

    bool parseBitwiseXor(ref string value, ref string actualType)
    {
        if (!parseBitwiseAnd(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_CARET)
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseBitwiseAnd(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }

    bool parseBitwiseOr(ref string value, ref string actualType)
    {
        if (!parseBitwiseXor(ref value, ref actualType))
        {
            return false;
        }
        Token token = TinyScanner.Current();
        while (token.Type == TokenType.SYM_PIPE)
        {
            string op = token.Lexeme;
            TinyScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseBitwiseXor(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!IsNumericType(actualType) || !IsNumericType(rightType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TinyScanner.Current();
        }
        return true;
    }

    bool parseConstantExpression(ref string value, ref string actualType)
    {
        return parseBitwiseOr(ref value, ref actualType);
    }
}
