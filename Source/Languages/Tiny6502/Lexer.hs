unit Lexer
{
    uses "TinyToken"
    uses "/Source/System/IO"
    uses "/Source/System/Char"

    
    const TokenType[] keywordTokens = {
        TokenType.KW_FUNC, TokenType.KW_IF, TokenType.KW_ELSE, TokenType.KW_WHILE, TokenType.KW_FOR,
        TokenType.KW_BREAK, TokenType.KW_CONTINUE, TokenType.KW_SWITCH, TokenType.KW_CASE, TokenType.KW_DEFAULT,
        TokenType.KW_CONST, TokenType.KW_TRUE, TokenType.KW_FALSE, TokenType.KW_NULL, TokenType.KW_IMPORT,
        TokenType.KW_BYTE, TokenType.KW_WORD, TokenType.KW_CHAR, TokenType.KW_BOOL, TokenType.KW_INT, TokenType.KW_UINT
    }

    const TokenType[] preprocessorTokens = {
        TokenType.PRE_INCLUDE, TokenType.PRE_DEFINE, TokenType.PRE_UNDEF, TokenType.PRE_IFDEF,
        TokenType.PRE_IFNDEF, TokenType.PRE_IF, TokenType.PRE_ELIF, TokenType.PRE_ELSE, TokenType.PRE_ENDIF, TokenType.PRE_PRAGMA
    }
    
    <string> keywords = {
        "func", "if", "else", "while", "for", "break", "continue", "switch", "case", "default",
        "const", "true", "false", "null", "import",
        "byte", "word", "char", "bool", "int", "uint"
    }
    
    <string> preprocessorDirectives = {
        "include", "define", "undef", "ifdef", "ifndef", "if", "elif", "else", "endif", "pragma"
    }

    record Lexer
    {
        string Source;
        uint start;
        uint current;
        uint line;
    }

    Lexer Lexer(string src)
    {
        Lexer result;
        result.Source = src;
        result.line   = 1;
        return result;
    }

    bool isAtEnd(Lexer lexer)
    {
        return lexer.current >= lexer.Source.Length;
    }

    char advance(ref Lexer lexer)
    {
        return lexer.Source[lexer.current++];
    }

    char peek(Lexer lexer)
    {
        if (isAtEnd(lexer)) { return '\0'; }
        return lexer.Source[lexer.current];
    }

    char peekNext(Lexer lexer)
    {
        if ((lexer.current + 1) >= lexer.Source.Length) { return '\0'; }
        return lexer.Source[lexer.current + 1];
    }

    bool match(ref Lexer lexer, char expected)
    {
        if (isAtEnd(lexer)) { return false; }
        if (lexer.Source[lexer.current] != expected) { return false; }
        lexer.current++;
        return true;
    }

    Token addToken(Lexer lexer, TokenType type)
    {
        string text = lexer.Source.Substring(lexer.start, lexer.current - lexer.start);
        return { Type = type, Lexeme = text, Line = lexer.line };
    }

    Token stringToken(ref Lexer lexer)
    {
        while ((peek(lexer) != '"') && !isAtEnd(lexer))
        {
            if (peek(lexer) == '\n') { lexer.line++; }
            advance(ref lexer);
        }

        if (isAtEnd(lexer))
        {
            IO.WriteLn("Unterminated string.");
            Diagnostics.Die(1);
        }

        advance(ref lexer); // The closing "

        return addToken(lexer, TokenType.LIT_STRING);
    }

    Token number(ref Lexer lexer)
    {
        while (Char.IsDigit(peek(lexer))) { advance(ref lexer); }

        if ((peek(lexer) == '.') && Char.IsDigit(peekNext(lexer)))
        {
            advance(ref lexer);

            while (Char.IsDigit(peek(lexer))) { advance(ref lexer); }
        }

        return addToken(lexer, TokenType.LIT_NUMBER);
    }

    Token identifier(ref Lexer lexer)
    {
        while (Char.IsAlphaNumeric(peek(lexer))) { advance(ref lexer); }

        string text = lexer.Source.Substring(lexer.start, lexer.current - lexer.start);

        for (uint i = 0; i < keywords.Length; i++)
        {
            if (text == keywords[i])
            {
                return addToken(lexer, keywordTokens[i]);
            }
        }

        return addToken(lexer, TokenType.IDENTIFIER);
    }

    Token preprocessorDirective(ref Lexer lexer)
    {
        advance(ref lexer); // skip the '#'

        while (Char.IsAlpha(peek(lexer))) { advance(ref lexer); }

        string text = lexer.Source.Substring(lexer.start + 1, lexer.current - lexer.start - 1); // exclude '#'

        for (uint i = 0; i < preprocessorDirectives.Length; i++)
        {
            if (text == preprocessorDirectives[i])
            {
                return addToken(lexer, preprocessorTokens[i]);
            }
        }

        IO.WriteLn("Unknown preprocessor directive: " + text);
        Diagnostics.Die(1);
    }

    Token scanToken(ref Lexer lexer)
    {
        char c = advance(ref lexer);

        switch (c)
        {
            case '(': { return addToken(lexer, TokenType.SYM_LPAREN); }
            case ')': { return addToken(lexer, TokenType.SYM_RPAREN); }
            case '{': { return addToken(lexer, TokenType.SYM_LBRACE); }
            case '}': { return addToken(lexer, TokenType.SYM_RBRACE); }
            case '[': { return addToken(lexer, TokenType.SYM_LBRACKET); }
            case ']': { return addToken(lexer, TokenType.SYM_RBRACKET); }
            case ';': { return addToken(lexer, TokenType.SYM_SEMICOLON); }
            case ':': { return addToken(lexer, TokenType.SYM_COLON); }
            case ',': { return addToken(lexer, TokenType.SYM_COMMA); }
            case '.': { return addToken(lexer, TokenType.SYM_DOT); }
            case '+': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_PLUSEQ : (match(ref lexer, '+') ? TokenType.SYM_PLUSPLUS : TokenType.SYM_PLUS)); }
            case '-': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_MINUSEQ : (match(ref lexer, '-') ? TokenType.SYM_MINUSMINUS : TokenType.SYM_MINUS)); }
            case '*': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_STAREQ : TokenType.SYM_STAR); }
            case '/': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_SLASHEQ : TokenType.SYM_SLASH); }
            case '%': { return addToken(lexer, TokenType.SYM_PERCENT); }
            case '&': { return addToken(lexer, match(ref lexer, '&') ? TokenType.SYM_AMPAMP : TokenType.SYM_AMP); }
            case '|': { return addToken(lexer, match(ref lexer, '|') ? TokenType.SYM_PIPEPIPE : TokenType.SYM_PIPE); }
            case '^': { return addToken(lexer, TokenType.SYM_CARET); }
            case '~': { return addToken(lexer, TokenType.SYM_TILDE); }
            case '!': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_NEQ : TokenType.SYM_BANG); }
            case '=': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_EQEQ : TokenType.SYM_EQ); }
            case '<': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_LTE : TokenType.SYM_LT); }
            case '>': { return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_GTE : TokenType.SYM_GT); }
            case '"': { return stringToken(ref lexer); }
            case '#': { return preprocessorDirective(ref lexer); }
            default:
            {
                if (Char.IsDigit(c))
                {
                    return number(ref lexer);
                }
                if (Char.IsAlpha(c))
                {
                    return identifier(ref lexer);
                }
                IO.WriteLn("Unexpected character: " + c);
                Diagnostics.Die(1);
                break;
            }
        }
    }
}
