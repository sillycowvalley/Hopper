unit Tokens
{
    // Token types - EndOfFile = 0 for easy checking
    enum Token    
    {
        EndOfFile    = 0,   // Makes if (Z) work for EOF check
        
        // Literals and identifiers
        Identifier   = 1,
        IntegerLiteral = 2,
        StringLiteral = 3,
        CharLiteral  = 4,
        
        // Keywords (10-29)
        Void         = 10,
        Char         = 11,
        Int          = 12,
        Long         = 13,
        If           = 14,
        Else         = 15,
        For          = 16,
        While        = 17,
        Return       = 18,
        Break        = 19,
        Continue     = 20,
        
        // Operators (30-49)
        Plus         = 30,  // +
        Minus        = 31,  // -
        Star         = 32,  // *
        Slash        = 33,  // /
        Percent      = 34,  // %
        Assign       = 35,  // =
        Equal        = 36,  // ==
        NotEqual     = 37,  // !=
        Less         = 38,  // 
        Greater      = 39,  // >
        LessEqual    = 40,  // <=
        GreaterEqual = 41,  // >=
        LogicalAnd   = 42,  // &&
        LogicalOr    = 43,  // ||
        Not          = 44,  // !
        Increment    = 45,  // ++
        Decrement    = 46,  // --
        
        // Punctuation (50-59)
        Semicolon    = 50,  // ;
        Comma        = 51,  // ,
        LeftParen    = 52,  // (
        RightParen   = 53,  // )
        LeftBrace    = 54,  // {
        RightBrace   = 55,  // }
        LeftBracket  = 56,  // [
        RightBracket = 57,  // ]
        Ampersand    = 58,  // & (for address-of)
    }
    
    // Keyword table for recognition
    // Each entry: null-terminated string followed by token type
    const string kwVoid   = "void";
    const string kwChar   = "char";
    const string kwInt    = "int";
    const string kwLong   = "long";
    const string kwIf     = "if";
    const string kwElse   = "else";
    const string kwFor    = "for";
    const string kwWhile  = "while";
    const string kwReturn = "return";
    
    // Table of keyword addresses and their token types
    // Would be cleaner with a struct, but using parallel arrays
    const byte keywordCount = 9;
    
    // Check if string at [tokenBuffer] matches keyword
    // Input: ZP.STR points to keyword string to check
    // Output: C set if match, clear if no match
    matchKeyword()
    {
        PHY
        
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z)  // End of keyword
            {
                LDA [Lexer.TokenType], Y
                if (Z)  // Also end of token
                {
                    SEC  // Match!
                    PLY
                    return;
                }
                else
                {
                    CLC  // Token is longer
                    PLY
                    return;
                }
            }
            
            CMP [Lexer.TokenType], Y
            if (NZ)
            {
                CLC  // No match
                PLY
                return;
            }
            
            INY
        }
    }
    
    // Check token buffer against all keywords
    // Updates Lexer.tokenType if keyword found
    CheckKeywords()
    {
        // Check "void"
        LDA #(kwVoid % 256)
        STA ZP.STRL
        LDA #(kwVoid / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Void
            STA Lexer.TokenType
            return;
        }
        
        // Check "char"
        LDA #(kwChar % 256)
        STA ZP.STRL
        LDA #(kwChar / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA # Token.Char
            STA Lexer.TokenType
            return;
        }
        
        // Check "int"
        LDA #(kwInt % 256)
        STA ZP.STRL
        LDA #(kwInt / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Int
            STA Lexer.TokenType
            return;
        }
        
        // Check "long"
        LDA #(kwLong % 256)
        STA ZP.STRL
        LDA #(kwLong / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Long
            STA Lexer.TokenType
            return;
        }
        
        // Check "if"
        LDA #(kwIf % 256)
        STA ZP.STRL
        LDA #(kwIf / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.If
            STA Lexer.TokenType
            return;
        }
        
        // Check "else"
        LDA #(kwElse % 256)
        STA ZP.STRL
        LDA #(kwElse / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Else
            STA Lexer.TokenType
            return;
        }
        
        // Check "for"
        LDA #(kwFor % 256)
        STA ZP.STRL
        LDA #(kwFor / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.For
            STA Lexer.TokenType
            return;
        }
        
        // Check "while"
        LDA #(kwWhile % 256)
        STA ZP.STRL
        LDA #(kwWhile / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.While
            STA Lexer.TokenType
            return;
        }
        
        // Check "return"
        LDA #(kwReturn % 256)
        STA ZP.STRL
        LDA #(kwReturn / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Return
            STA Lexer.TokenType
            return;
        }
        
        // Not a keyword, must be identifier
        LDA #Token.Identifier
        STA Lexer.TokenType
    }
    
    // Debug: Print token type name (for testing)
    PrintType()  // Input: A = token type
    {
        switch (A)
        {
            case Token.EndOfFile:      { Print.Char(); LDA #'E' Print.Char(); LDA #'O' Print.Char(); LDA #'F' Print.Char(); }
            case Token.Identifier:     { Print.Char(); LDA #'I' Print.Char(); LDA #'D' Print.Char(); }
            case Token.IntegerLiteral: { Print.Char(); LDA #'N' Print.Char(); LDA #'U' Print.Char(); LDA #'M' Print.Char(); }
            case Token.StringLiteral:  { Print.Char(); LDA #'S' Print.Char(); LDA #'T' Print.Char(); LDA #'R' Print.Char(); }
            case Token.Plus:           { Print.Char(); LDA #'+' Print.Char(); }
            case Token.Minus:          { Print.Char(); LDA #'-' Print.Char(); }
            case Token.Star:           { Print.Char(); LDA #'*' Print.Char(); }
            case Token.Semicolon:      { Print.Char(); LDA #';' Print.Char(); }
            // ... etc
            default:                   { Print.Hex(); }
        }
    }
}
