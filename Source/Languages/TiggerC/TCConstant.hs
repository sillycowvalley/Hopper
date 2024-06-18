unit TCConstant
{
    friend TCStatement, TCExpression, TCCompile;
    
    uses "TCToken"
    uses "TCType"
    uses "TCCode"
    
    record Constant
    {
        string Type;
        string Value;
    }
    
    uint blockLevel;
    < <string, Constant> > constants;
    
    string stringConstants;
    
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
    DefineStringConst(string constantValue, ref uint index)
    {
        index = 0;
        string constantValuePlus = constantValue + char(0);
        if (!stringConstants.IndexOf(constantValuePlus, ref index))
        {
            index = stringConstants.Length /*+ romAddress*/;
            stringConstants += constantValuePlus;
        }
    }
    string GetStringConstants() { return stringConstants; }
    
    bool DefineConst(string constantType, string constantName, string constantValue)
    {
        uint level = constants.Count -1;
        <string, Constant> scopeConstants = constants[level];
        if (scopeConstants.Contains(constantName))
        {
            // exists already
            Token token = TCScanner.Current();
            Error(token.SourcePath, token.Line, "constant with name '" + constantName + "' already exists");
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
        Token token = TCScanner.Current();
        
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
        Token token = TCScanner.Current();

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
            case "<<":
            {
                result = left << right;
            }
            case ">>":
            {
                result = left >> right;
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
    
    bool evaluateComparisonOperation(string leftValue, string rightValue, string op, ref string resultValue, ref string resultType)
    {
        Token token = TCScanner.Current();
        
        long left;
        long right;
        _ = Long.TryParse(rightValue, ref right);
        _ = Long.TryParse(leftValue, ref left);
        bool result;
        switch (op)
        {
            case "<":
            {
                result = left < right;
            }
            case "<=":
            {
                result = left <= right;
            }
            case ">":
            {
                result = left > right;
            }
            case ">=":
            {
                result = left >= right;
            }
            case "==":
            {
                result = left == right;
            }
            case "!=":
            {
                result = left != right;
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unsupported comparison operator in constant expression");
                return false;
            }
        }
        resultType = "bool";
        resultValue = result ? "1" : "0";
        return true;
    }
    
    bool evaluateBooleanOperation(string leftValue, string rightValue, string op, ref string resultValue, ref string resultType)
    {
        Token token = TCScanner.Current();
        bool left = (leftValue == "true");
        bool right = (rightValue == "true");
        bool result;
        switch (op)
        {
            case "&&":
            {
                result = left && right;
            }
            case "||":
            {
                result = left || right;
            }
            default:
            {
                Error(token.SourcePath, token.Line, "unsupported boolean operator in constant expression");
                return false;
            }
        }
        resultType = "bool";
        resultValue = result ? "true" : "false";
        return true;
    }
    
    
    
    bool parseConstantPrimary(ref string value, ref string actualType)
    {
        Token token = TCScanner.Current();
        
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
            TCScanner.Advance(); // Skip number or identifier
        }
        else if (token.Type == TokenType.IDENTIFIER)
        {
            if (!GetConst(token.Lexeme, ref actualType, ref value))
            {
                Error(token.SourcePath, token.Line, "undefined constant identifier in constant expression, ('" + token.Lexeme + "')");
                return false;
            }
            TCScanner.Advance(); // Skip identifier
        }
        else if (token.Type == TokenType.LIT_STRING)
        {
            value = token.Lexeme;
            actualType = "const char[]";
            TCScanner.Advance(); // Skip string literal
        }
        else if (token.Type == TokenType.LIT_CHAR)
        {
            value = token.Lexeme;
            actualType = "char";
            TCScanner.Advance(); // Skip char literal
        }
        else if (token.Type == TokenType.KW_TRUE)
        {
            value = "1";
            actualType = "bool";
            TCScanner.Advance(); // Skip bool literal
        }
        else if (token.Type == TokenType.KW_FALSE)
        {
            value = "0";
            actualType = "bool";
            TCScanner.Advance(); // Skip bool literal
        }
        else
        {
            Error(token.SourcePath, token.Line, "expected number, identifier, string, or char in constant expression, ('" + token.Lexeme + "')");
            return false;
        }
        return true;
    }
    
    
    bool parseConstantUnary(ref string value, ref string actualType)
    {
        Token token = TCScanner.Current();
        if ((token.Type == TokenType.SYM_MINUS) || (token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_BANG) || (token.Type == TokenType.SYM_TILDE))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
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
                value = "-" + value;
            }
            else if (op == "!")
            {
                if ((actualType != "bool") && (actualType != "const bool"))
                {
                    Error(token.SourcePath, token.Line, "type mismatch in in constant expression");
                    return false;
                }
                value = (value == "true") ? "false" : "true";
                actualType = "bool";   
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
                    case "~":
                    {
                        ui = ~ui;
                        if (actualType == "byte")
                        {
                            ui = (ui & 0xFF);
                        }
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
    
    bool parseConstantFactor(ref string value, ref string actualType)
    {
        if (!parseConstantUnary(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_STAR) || (token.Type == TokenType.SYM_SLASH) || (token.Type == TokenType.SYM_PERCENT))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantUnary(ref rightValue, ref rightType))
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
            token = TCScanner.Current();
        }
        return true;
    }
    
    bool parseConstantTerm(ref string value, ref string actualType)
    {
        if (!parseConstantFactor(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_PLUS) || (token.Type == TokenType.SYM_MINUS))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantFactor(ref rightValue, ref rightType))
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
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantBitwiseShift(ref string value, ref string actualType)
    {
        if (!parseConstantTerm(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_LSHIFT) || (token.Type == TokenType.SYM_RSHIFT))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantTerm(ref rightValue, ref rightType))
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
            token = TCScanner.Current();
        }
        return true;
    }
    
    bool parseConstantComparison(ref string value, ref string actualType)
    {
        if (!parseConstantBitwiseShift(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_LT) || (token.Type == TokenType.SYM_LTE) || (token.Type == TokenType.SYM_GT) || (token.Type == TokenType.SYM_GTE))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantBitwiseShift(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchNumericTypes(rightType, ref actualType))
            {
                Error(token.SourcePath, token.Line, "numeric types expected in constant expression");
                return false;
            }
            if (!evaluateComparisonOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }
    
    bool parseConstantEquality(ref string value, ref string actualType)
    {
        if (!parseConstantComparison(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while ((token.Type == TokenType.SYM_EQEQ) || (token.Type == TokenType.SYM_NEQ))
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantComparison(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchTypes(rightType, ref actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateComparisonOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantBitwiseAnd(ref string value, ref string actualType)
    {
        if (!parseConstantEquality(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_AMP)
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantEquality(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchNumericTypes(rightType, ref actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantBitwiseXor(ref string value, ref string actualType)
    {
        if (!parseConstantBitwiseAnd(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_CARET)
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantBitwiseAnd(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchNumericTypes(rightType, ref actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantBitwiseOr(ref string value, ref string actualType)
    {
        if (!parseConstantBitwiseXor(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_PIPE)
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantBitwiseXor(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchNumericTypes(rightType, ref actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateBitwiseOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }
    
    bool parseConstantLogicalAnd(ref string value, ref string actualType)
    {
        if (!parseConstantBitwiseOr(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_AMPAMP)
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantBitwiseOr(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchBoolTypes(rightType, actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateBooleanOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantLogicalOr(ref string value, ref string actualType)
    {
        if (!parseConstantLogicalAnd(ref value, ref actualType))
        {
            return false;
        }
        Token token = TCScanner.Current();
        while (token.Type == TokenType.SYM_PIPEPIPE)
        {
            string op = token.Lexeme;
            TCScanner.Advance(); // Skip operator
            string rightValue;
            string rightType;
            if (!parseConstantLogicalAnd(ref rightValue, ref rightType))
            {
                return false;
            }
            if (!MatchBoolTypes(rightType, actualType))
            {
                Error(token.SourcePath, token.Line, "type mismatch constant expression");
                return false;
            }
            if (!evaluateBooleanOperation(value, rightValue, op, ref value, ref actualType))
            {
                return false;
            }
            token = TCScanner.Current();
        }
        return true;
    }

    bool parseConstantExpression(ref string value, ref string actualType)
    {
        if (!parseConstantLogicalOr(ref value, ref actualType))
        {
            return false;
        }
        if (actualType == "bool")
        {
            value = (value == "0") ? "false" : "true";
        }
        return true;
    }
}
