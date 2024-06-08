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
        SYM_PLUSPLUS, SYM_MINUSMINUS, SYM_PLUSEQ, SYM_MINUSEQ, SYM_STAREQ, SYM_SLASHEQ, SYM_PERCENTEQ,
        SYM_AMPAMP, SYM_PIPEPIPE,
        // Literals
        LIT_NUMBER, LIT_STRING, LIT_CHAR,
        // Identifiers
        IDENTIFIER,
        // End of File
        EOF
    }
    
    record Token
    {
        TokenType Type;
        string    Lexeme;
        uint      Line;
        string    SourcePath;
    } 
    
    <string,TokenType> keywords;
    
    Initialize()
    {
        // in case Initialize is called more than once
        keywords.Clear();
        keywords["func"] = TokenType.KW_FUNC;
        keywords["if"] = TokenType.KW_IF;
        keywords["else"] = TokenType.KW_ELSE;
        keywords["while"] = TokenType.KW_WHILE;
        keywords["for"] = TokenType.KW_FOR;
        keywords["break"] = TokenType.KW_BREAK;
        keywords["continue"] = TokenType.KW_CONTINUE;
        keywords["switch"] = TokenType.KW_SWITCH;
        keywords["case"] = TokenType.KW_CASE;
        keywords["default"] = TokenType.KW_DEFAULT;
        keywords["const"] = TokenType.KW_CONST;
        keywords["true"] = TokenType.KW_TRUE;
        keywords["false"] = TokenType.KW_FALSE;
        keywords["null"] = TokenType.KW_NULL;
        keywords["import"] = TokenType.KW_IMPORT;
        keywords["byte"] = TokenType.KW_BYTE;
        keywords["word"] = TokenType.KW_WORD;
        keywords["char"] = TokenType.KW_CHAR;
        keywords["bool"] = TokenType.KW_BOOL;
        keywords["int"] = TokenType.KW_INT;
        keywords["uint"] = TokenType.KW_UINT;
        keywords["return"] = TokenType.KW_RETURN;
    }
    
    bool IsKeyword(string candidate, ref TokenType kw)
    {
        if (keywords.Contains(candidate))
        {
            kw = keywords[candidate];
            return true;
        }
        return false;
    }
    
    bool IsTypeKeyword(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.KW_BYTE:
            case TokenType.KW_WORD:
            case TokenType.KW_CHAR:
            case TokenType.KW_BOOL:
            case TokenType.KW_INT:
            case TokenType.KW_UINT:
            {
                return true;
            }
            default:
            {
                return false;
            }
        }
        return false; // Unreachable but required by Hopper
    }
    
    
    bool IsBinaryOperator(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.SYM_PLUS:
            case TokenType.SYM_MINUS:
            case TokenType.SYM_STAR:
            case TokenType.SYM_SLASH:
            case TokenType.SYM_PERCENT:
            case TokenType.SYM_AMP:
            case TokenType.SYM_PIPE:
            case TokenType.SYM_CARET:
            case TokenType.SYM_EQEQ:
            case TokenType.SYM_NEQ:
            case TokenType.SYM_LT:
            case TokenType.SYM_LTE:
            case TokenType.SYM_GT:
            case TokenType.SYM_GTE:
            {
                return true;
            }
            default:
            {
                return false;
            }
        }
        return false; // unreachable but required by Hopper
    }
    
    bool IsCompoundAssignmentOperator(TokenType tp)
    {
        switch (tp)
        {
            case TokenType.SYM_PLUSEQ:
            case TokenType.SYM_MINUSEQ:
            case TokenType.SYM_STAREQ:
            case TokenType.SYM_SLASHEQ:
            {
                return true;
            }
            default:
            {
                return false;
            }
        }
        return false; // unreachable but required by Hopper
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
            case TokenType.SYM_PERCENTEQ: { return "SYM_PERCENTEQ"; }
            case TokenType.SYM_AMPAMP: { return "SYM_AMPAMP"; }
            case TokenType.SYM_PIPEPIPE: { return "SYM_PIPEPIPE"; }
            case TokenType.LIT_NUMBER: { return "LIT_NUMBER"; }
            case TokenType.LIT_STRING: { return "LIT_STRING"; }
            case TokenType.LIT_CHAR: { return "LIT_CHAR"; }
            case TokenType.IDENTIFIER: { return "IDENTIFIER"; }
            case TokenType.EOF: { return "EOF"; }
        }
        return "UNDEFINED";
    }
}

