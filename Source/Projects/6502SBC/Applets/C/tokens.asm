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
        Less         = 38,  // <
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
        
        // Pointer types
        CharPtr      = 59,    // char* type
        FilePtr      = 60,    // FILE* type
        
        // New keyword
        Null         = 61,    // null constant
        
        PlusAssign   = 62,  // +=
        MinusAssign  = 63,  // -=
        
        Const        = 64, // "const"
    }
    
    // Keyword table for recognition
    const string kwVoid   = "void";
    const string kwChar   = "char";
    const string kwInt    = "int";
    const string kwLong   = "long";
    const string kwIf     = "if";
    const string kwElse   = "else";
    const string kwFor    = "for";
    const string kwWhile  = "while";
    const string kwReturn = "return";
    const string kwNull   = "null";
    const string kwFILE   = "FILE";
    const string kwConst  = "const";
    
    
    
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
                LDA [Lexer.TokenBuffer], Y
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
            
            CMP [Lexer.TokenBuffer], Y
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
            LDA #Token.Char
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
        
        // Check "const"
        LDA #(kwConst % 256)
        STA ZP.STRL
        LDA #(kwConst / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Const
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
        
        // Check "null"
        LDA #(kwNull % 256)
        STA ZP.STRL
        LDA #(kwNull / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            LDA #Token.Null
            STA Lexer.TokenType
            return;
        }
        
        // Check "FILE"
        LDA #(kwFILE % 256)
        STA ZP.STRL
        LDA #(kwFILE / 256)
        STA ZP.STRH
        matchKeyword();
        if (C)
        {
            // we're on a '*'?
            Lexer.CurrentChar();
            CMP #'*'
            if (Z)
            {
                Lexer.advance();  // Consume the *
                LDA #Token.FilePtr
                STA Lexer.TokenType
                SEC
                return;
            }
            else
            {
                LDA #Token.Star
                Errors.Expected();
                return;
            }
        }
        
        // Not a keyword, must be identifier
        LDA #Token.Identifier
        STA Lexer.TokenType
        SEC
    }
    
#ifdef DEBUG    
    // Token type names for debugging
    const string nameEOF         = "EOF";
    const string nameIdentifier  = "ID";
    const string nameInteger     = "NUM";
    const string nameString      = "STR";
    const string nameCharLit     = "CHR";
    
    const string nameVoid        = "void";
    const string nameChar        = "char";
    const string nameInt         = "int";
    const string nameLong        = "long";
    const string nameIf          = "if";
    const string nameElse        = "else";
    const string nameFor         = "for";
    const string nameWhile       = "while";
    const string nameReturn      = "return";
    const string nameBreak       = "break";
    const string nameContinue    = "continue";
    const string nameConst       = "const";
    
    const string namePlus        = "+";
    const string nameMinus       = "-";
    const string nameStar        = "*";
    const string nameSlash       = "/";
    const string namePercent     = "%";
    const string nameAssign      = "=";
    const string nameEqual       = "==";
    const string nameNotEqual    = "!=";
    const string nameLess        = "<";
    const string nameGreater     = ">";
    const string nameLessEqual   = "<=";
    const string nameGreaterEqual = ">=";
    const string nameLogicalAnd  = "&&";
    const string nameLogicalOr   = "||";
    const string nameNot         = "!";
    const string nameIncrement   = "++";
    const string nameDecrement   = "--";
    
    const string nameSemicolon   = ";";
    const string nameComma       = ",";
    const string nameLeftParen   = "(";
    const string nameRightParen  = ")";
    const string nameLeftBrace   = "{";
    const string nameRightBrace  = "}";
    const string nameLeftBracket = "[";
    const string nameRightBracket = "]";
    const string nameAmpersand   = "&";
    
    const string nameUnknown     = "?";
    
    const string nameCharPtr     = "char*";
    const string nameFilePtr     = "FILE*";
    const string nameNull        = "null";
    
    
    PrintToken()  // Input: A = token
    {
        switch (A)
        {
            // Literals and special
            case Token.EndOfFile:      { LDA #(nameEOF % 256)         STA ZP.STRL  LDA #(nameEOF / 256)         STA ZP.STRH }
            case Token.Identifier:     { LDA #(nameIdentifier % 256)  STA ZP.STRL  LDA #(nameIdentifier / 256)  STA ZP.STRH }
            case Token.IntegerLiteral: { LDA #(nameInteger % 256)     STA ZP.STRL  LDA #(nameInteger / 256)     STA ZP.STRH }
            case Token.StringLiteral:  { LDA #(nameString % 256)      STA ZP.STRL  LDA #(nameString / 256)      STA ZP.STRH }
            case Token.CharLiteral:    { LDA #(nameCharLit % 256)     STA ZP.STRL  LDA #(nameCharLit / 256)     STA ZP.STRH }
            
            case Token.CharPtr:        { LDA #(nameCharPtr % 256)     STA ZP.STRL  LDA #(nameCharPtr / 256)     STA ZP.STRH }
            case Token.FilePtr:        { LDA #(nameFilePtr % 256)     STA ZP.STRL  LDA #(nameFilePtr / 256)     STA ZP.STRH }
            case Token.Null:           { LDA #(nameNull % 256)        STA ZP.STRL  LDA #(nameNull / 256)        STA ZP.STRH }
            
            // Keywords
            case Token.Void:           { LDA #(nameVoid % 256)        STA ZP.STRL  LDA #(nameVoid / 256)        STA ZP.STRH }
            case Token.Char:           { LDA #(nameChar % 256)        STA ZP.STRL  LDA #(nameChar / 256)        STA ZP.STRH }
            case Token.Int:            { LDA #(nameInt % 256)         STA ZP.STRL  LDA #(nameInt / 256)         STA ZP.STRH }
            case Token.Long:           { LDA #(nameLong % 256)        STA ZP.STRL  LDA #(nameLong / 256)        STA ZP.STRH }
            case Token.If:             { LDA #(nameIf % 256)          STA ZP.STRL  LDA #(nameIf / 256)          STA ZP.STRH }
            case Token.Else:           { LDA #(nameElse % 256)        STA ZP.STRL  LDA #(nameElse / 256)        STA ZP.STRH }
            case Token.For:            { LDA #(nameFor % 256)         STA ZP.STRL  LDA #(nameFor / 256)         STA ZP.STRH }
            case Token.While:          { LDA #(nameWhile % 256)       STA ZP.STRL  LDA #(nameWhile / 256)       STA ZP.STRH }
            case Token.Return:         { LDA #(nameReturn % 256)      STA ZP.STRL  LDA #(nameReturn / 256)      STA ZP.STRH }
            case Token.Break:          { LDA #(nameBreak % 256)       STA ZP.STRL  LDA #(nameBreak / 256)       STA ZP.STRH }
            case Token.Continue:       { LDA #(nameContinue % 256)    STA ZP.STRL  LDA #(nameContinue / 256)    STA ZP.STRH }
            case Token.Const:          { LDA #(nameConst % 256)       STA ZP.STRL  LDA #(nameConst / 256)       STA ZP.STRH }
            
            // Operators
            case Token.Plus:           { LDA #(namePlus % 256)        STA ZP.STRL  LDA #(namePlus / 256)        STA ZP.STRH }
            case Token.Minus:          { LDA #(nameMinus % 256)       STA ZP.STRL  LDA #(nameMinus / 256)       STA ZP.STRH }
            case Token.Star:           { LDA #(nameStar % 256)        STA ZP.STRL  LDA #(nameStar / 256)        STA ZP.STRH }
            case Token.Slash:          { LDA #(nameSlash % 256)       STA ZP.STRL  LDA #(nameSlash / 256)       STA ZP.STRH }
            case Token.Percent:        { LDA #(namePercent % 256)     STA ZP.STRL  LDA #(namePercent / 256)     STA ZP.STRH }
            case Token.Assign:         { LDA #(nameAssign % 256)      STA ZP.STRL  LDA #(nameAssign / 256)      STA ZP.STRH }
            case Token.Equal:          { LDA #(nameEqual % 256)       STA ZP.STRL  LDA #(nameEqual / 256)       STA ZP.STRH }
            case Token.NotEqual:       { LDA #(nameNotEqual % 256)    STA ZP.STRL  LDA #(nameNotEqual / 256)    STA ZP.STRH }
            case Token.Less:           { LDA #(nameLess % 256)        STA ZP.STRL  LDA #(nameLess / 256)        STA ZP.STRH }
            case Token.Greater:        { LDA #(nameGreater % 256)     STA ZP.STRL  LDA #(nameGreater / 256)     STA ZP.STRH }
            case Token.LessEqual:      { LDA #(nameLessEqual % 256)   STA ZP.STRL  LDA #(nameLessEqual / 256)   STA ZP.STRH }
            case Token.GreaterEqual:   { LDA #(nameGreaterEqual % 256) STA ZP.STRL LDA #(nameGreaterEqual / 256) STA ZP.STRH }
            case Token.LogicalAnd:     { LDA #(nameLogicalAnd % 256)  STA ZP.STRL  LDA #(nameLogicalAnd / 256)  STA ZP.STRH }
            case Token.LogicalOr:      { LDA #(nameLogicalOr % 256)   STA ZP.STRL  LDA #(nameLogicalOr / 256)   STA ZP.STRH }
            case Token.Not:            { LDA #(nameNot % 256)         STA ZP.STRL  LDA #(nameNot / 256)         STA ZP.STRH }
            case Token.Increment:      { LDA #(nameIncrement % 256)   STA ZP.STRL  LDA #(nameIncrement / 256)   STA ZP.STRH }
            case Token.Decrement:      { LDA #(nameDecrement % 256)   STA ZP.STRL  LDA #(nameDecrement / 256)   STA ZP.STRH }
            
            // Punctuation
            case Token.Semicolon:      { LDA #(nameSemicolon % 256)   STA ZP.STRL  LDA #(nameSemicolon / 256)   STA ZP.STRH }
            case Token.Comma:          { LDA #(nameComma % 256)       STA ZP.STRL  LDA #(nameComma / 256)       STA ZP.STRH }
            case Token.LeftParen:      { LDA #(nameLeftParen % 256)   STA ZP.STRL  LDA #(nameLeftParen / 256)   STA ZP.STRH }
            case Token.RightParen:     { LDA #(nameRightParen % 256)  STA ZP.STRL  LDA #(nameRightParen / 256)  STA ZP.STRH }
            case Token.LeftBrace:      { LDA #(nameLeftBrace % 256)   STA ZP.STRL  LDA #(nameLeftBrace / 256)   STA ZP.STRH }
            case Token.RightBrace:     { LDA #(nameRightBrace % 256)  STA ZP.STRL  LDA #(nameRightBrace / 256)  STA ZP.STRH }
            case Token.LeftBracket:    { LDA #(nameLeftBracket % 256) STA ZP.STRL  LDA #(nameLeftBracket / 256) STA ZP.STRH }
            case Token.RightBracket:   { LDA #(nameRightBracket % 256) STA ZP.STRL LDA #(nameRightBracket / 256) STA ZP.STRH }
            case Token.Ampersand:      { LDA #(nameAmpersand % 256)   STA ZP.STRL  LDA #(nameAmpersand / 256)   STA ZP.STRH }
            
            default:                       { LDA #(nameUnknown % 256)     STA ZP.STRL  LDA #(nameUnknown / 256)     STA ZP.STRH }
        }
        
        Print.String();
    }
#endif    
}
