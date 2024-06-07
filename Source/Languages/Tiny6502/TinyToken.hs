unit TinyToken
{
    enum TokenType
    {
        // Keywords
        KW_FUNC, KW_IF, KW_ELSE, KW_WHILE, KW_FOR, KW_BREAK, KW_CONTINUE, KW_SWITCH, KW_CASE, KW_DEFAULT,
        KW_CONST, KW_TRUE, KW_FALSE, KW_NULL, KW_IMPORT, KW_RETURN,
        KW_BYTE, KW_WORD, KW_CHAR, KW_BOOL, KW_INT, KW_UINT,

        // Symbols
        SYM_LPAREN, SYM_RPAREN, SYM_LBRACE, SYM_RBRACE, SYM_LBRACKET, SYM_RBRACKET,
        SYM_SEMICOLON, SYM_COLON, SYM_COMMA, SYM_DOT,
        SYM_PLUS, SYM_MINUS, SYM_STAR, SYM_SLASH, SYM_PERCENT,
        SYM_AMP, SYM_PIPE, SYM_CARET, SYM_TILDE, SYM_BANG, SYM_QUESTION,
        SYM_EQ, SYM_EQEQ, SYM_NEQ, SYM_LT, SYM_LTE, SYM_GT, SYM_GTE,
        SYM_PLUSPLUS, SYM_MINUSMINUS, SYM_PLUSEQ, SYM_MINUSEQ, SYM_STAREQ, SYM_SLASHEQ,
        SYM_AMPAMP, SYM_PIPEPIPE,

        // Literals
        LIT_NUMBER, LIT_STRING, LIT_CHAR,

        // Identifiers
        IDENTIFIER,

        // Preprocessor Directives
        PRE_INCLUDE, PRE_DEFINE, PRE_UNDEF, PRE_IFDEF, PRE_IFNDEF, PRE_IF, PRE_ELIF, PRE_ELSE, PRE_ENDIF, PRE_PRAGMA, PRE_UNKNOWN,

        // End of File
        EOF
    }

    record Token
    {
        TokenType Type;
        string Lexeme;
        uint Line;
    }

    // ToString method for TokenType enum
    string ToString(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.KW_FUNC: { return "KW_FUNC"; }
            case TokenType.KW_IF: { return "KW_IF"; }
            case TokenType.KW_ELSE: { return "KW_ELSE"; }
            case TokenType.KW_WHILE: { return "KW_WHILE"; }
            case TokenType.KW_FOR: { return "KW_FOR"; }
            case TokenType.KW_BREAK: { return "KW_BREAK"; }
            case TokenType.KW_CONTINUE: { return "KW_CONTINUE"; }
            case TokenType.KW_SWITCH: { return "KW_SWITCH"; }
            case TokenType.KW_CASE: { return "KW_CASE"; }
            case TokenType.KW_DEFAULT: { return "KW_DEFAULT"; }
            case TokenType.KW_CONST: { return "KW_CONST"; }
            case TokenType.KW_TRUE: { return "KW_TRUE"; }
            case TokenType.KW_FALSE: { return "KW_FALSE"; }
            case TokenType.KW_NULL: { return "KW_NULL"; }
            case TokenType.KW_IMPORT: { return "KW_IMPORT"; }
            case TokenType.KW_RETURN: { return "KW_RETURN"; }
            case TokenType.KW_BYTE: { return "KW_BYTE"; }
            case TokenType.KW_WORD: { return "KW_WORD"; }
            case TokenType.KW_CHAR: { return "KW_CHAR"; }
            case TokenType.KW_BOOL: { return "KW_BOOL"; }
            case TokenType.KW_INT: { return "KW_INT"; }
            case TokenType.KW_UINT: { return "KW_UINT"; }
            case TokenType.SYM_LPAREN: { return "SYM_LPAREN"; }
            case TokenType.SYM_RPAREN: { return "SYM_RPAREN"; }
            case TokenType.SYM_LBRACE: { return "SYM_LBRACE"; }
            case TokenType.SYM_RBRACE: { return "SYM_RBRACE"; }
            case TokenType.SYM_LBRACKET: { return "SYM_LBRACKET"; }
            case TokenType.SYM_RBRACKET: { return "SYM_RBRACKET"; }
            case TokenType.SYM_SEMICOLON: { return "SYM_SEMICOLON"; }
            case TokenType.SYM_COLON: { return "SYM_COLON"; }
            case TokenType.SYM_COMMA: { return "SYM_COMMA"; }
            case TokenType.SYM_DOT: { return "SYM_DOT"; }
            case TokenType.SYM_PLUS: { return "SYM_PLUS"; }
            case TokenType.SYM_MINUS: { return "SYM_MINUS"; }
            case TokenType.SYM_STAR: { return "SYM_STAR"; }
            case TokenType.SYM_SLASH: { return "SYM_SLASH"; }
            case TokenType.SYM_PERCENT: { return "SYM_PERCENT"; }
            case TokenType.SYM_AMP: { return "SYM_AMP"; }
            case TokenType.SYM_PIPE: { return "SYM_PIPE"; }
            case TokenType.SYM_CARET: { return "SYM_CARET"; }
            case TokenType.SYM_TILDE: { return "SYM_TILDE"; }
            case TokenType.SYM_BANG: { return "SYM_BANG"; }
            case TokenType.SYM_QUESTION: { return "SYM_QUESTION"; }
            case TokenType.SYM_EQ: { return "SYM_EQ"; }
            case TokenType.SYM_EQEQ: { return "SYM_EQEQ"; }
            case TokenType.SYM_NEQ: { return "SYM_NEQ"; }
            case TokenType.SYM_LT: { return "SYM_LT"; }
            case TokenType.SYM_LTE: { return "SYM_LTE"; }
            case TokenType.SYM_GT: { return "SYM_GT"; }
            case TokenType.SYM_GTE: { return "SYM_GTE"; }
            case TokenType.SYM_PLUSPLUS: { return "SYM_PLUSPLUS"; }
            case TokenType.SYM_MINUSMINUS: { return "SYM_MINUSMINUS"; }
            case TokenType.SYM_PLUSEQ: { return "SYM_PLUSEQ"; }
            case TokenType.SYM_MINUSEQ: { return "SYM_MINUSEQ"; }
            case TokenType.SYM_STAREQ: { return "SYM_STAREQ"; }
            case TokenType.SYM_SLASHEQ: { return "SYM_SLASHEQ"; }
            case TokenType.SYM_AMPAMP: { return "SYM_AMPAMP"; }
            case TokenType.SYM_PIPEPIPE: { return "SYM_PIPEPIPE"; }
            case TokenType.LIT_NUMBER: { return "LIT_NUMBER"; }
            case TokenType.LIT_STRING: { return "LIT_STRING"; }
            case TokenType.LIT_CHAR: { return "LIT_CHAR"; }
            case TokenType.IDENTIFIER: { return "IDENTIFIER"; }
            case TokenType.PRE_INCLUDE: { return "PRE_INCLUDE"; }
            case TokenType.PRE_DEFINE: { return "PRE_DEFINE"; }
            case TokenType.PRE_UNDEF: { return "PRE_UNDEF"; }
            case TokenType.PRE_IFDEF: { return "PRE_IFDEF"; }
            case TokenType.PRE_IFNDEF: { return "PRE_IFNDEF"; }
            case TokenType.PRE_IF: { return "PRE_IF"; }
            case TokenType.PRE_ELIF: { return "PRE_ELIF"; }
            case TokenType.PRE_ELSE: { return "PRE_ELSE"; }
            case TokenType.PRE_ENDIF: { return "PRE_ENDIF"; }
            case TokenType.PRE_PRAGMA: { return "PRE_PRAGMA"; }
            case TokenType.PRE_UNKNOWN: { return "PRE_UNKNOWN"; }
            case TokenType.EOF: { return "EOF"; }
        }
        return "UNDEFINED";
    }
}

