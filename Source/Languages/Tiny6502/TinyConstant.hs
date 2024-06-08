unit TinyConstant
{
    friend TinyStatement, TinyExpression, TinyCompile;
    
    uses "TinyToken"
    
    bool parseConstantExpression(ref string expr)
    {
        Token token = TinyScanner.Current();
        if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.IDENTIFIER))
        {
            expr += token.Lexeme;
            TinyScanner.Advance(); // Skip number or identifier
    
            token = TinyScanner.Current();
            while (TinyToken.IsBinaryOperator(token.Type))
            {
                expr += TinyToken.ToString(token.Type);
                TinyScanner.Advance(); // Skip operator
    
                token = TinyScanner.Current();
                if ((token.Type == TokenType.LIT_NUMBER) || (token.Type == TokenType.IDENTIFIER))
                {
                    expr += token.Lexeme;
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
