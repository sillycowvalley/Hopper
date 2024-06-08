unit TinyConstant
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    
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
    bool DefineLocalConst(string constantType, string constantName, string constantValue)
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
    
    bool parseConstantExpression(ref string value, ref string actualType)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.IDENTIFIER))
        {
            value += token.Lexeme;
            TinyScanner.Advance(); // Skip number or identifier
    
            token = TinyScanner.Current();
            while (TinyToken.IsBinaryOperator(token.Type))
            {
                value += TinyToken.ToString(token.Type);
                TinyScanner.Advance(); // Skip operator
    
                token = TinyScanner.Current();
                if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.IDENTIFIER))
                {
                    value += token.Lexeme;
                    TinyScanner.Advance(); // Skip number or identifier
                }
                else
                {
                    Error(token.SourcePath, token.Line, "expected number or identifier in constant expression, ('" + token.Lexeme + "')");
                    return false;
                }
    
                token = TinyScanner.Current();
            }
            return true;
        }
        Error(token.SourcePath, token.Line, "expected number or identifier in constant expression, ('" + token.Lexeme + "')");
        return false;
    }
}
