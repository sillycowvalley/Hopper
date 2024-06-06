unit TinyToken
{
    enum TokenType
    {
        // Keywords
        KW_FUNC, KW_IF, KW_ELSE, KW_WHILE, KW_FOR, KW_BREAK, KW_CONTINUE, KW_SWITCH, KW_CASE, KW_DEFAULT,
        KW_CONST, KW_TRUE, KW_FALSE, KW_NULL, KW_IMPORT,
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
        PRE_INCLUDE, PRE_DEFINE, PRE_UNDEF, PRE_IFDEF, PRE_IFNDEF, PRE_IF, PRE_ELIF, PRE_ELSE, PRE_ENDIF, PRE_PRAGMA,

        // End of File
        EOF
    }

    record Token
    {
        TokenType Type;
        string Lexeme;
        uint Line;
    }
}
