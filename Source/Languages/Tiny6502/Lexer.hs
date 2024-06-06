unit Lexer
{
    uses "TinyToken"
    uses "/Source/System/IO"
    uses "/Source/System/Char"

    <string> keywords;
    <string> preprocessorDirectives;

    const TokenType[] keywordTokens = {
        TokenType.KW_FUNC, TokenType.KW_IF, TokenType.KW_ELSE, TokenType.KW_WHILE, TokenType.KW_FOR,
        TokenType.KW_BREAK, TokenType.KW_CONTINUE, TokenType.KW_SWITCH, TokenType.KW_CASE, TokenType.KW_DEFAULT,
        TokenType.KW_CONST, TokenType.KW_TRUE, TokenType.KW_FALSE, TokenType.KW_NULL, TokenType.KW_IMPORT,
        TokenType.KW_BYTE, TokenType.KW_WORD, TokenType.KW_CHAR, TokenType.KW_BOOL, TokenType.KW_INT, TokenType.KW_UINT
    };

    const TokenType[] preprocessorTokens = {
        TokenType.PRE_INCLUDE, TokenType.PRE_DEFINE, TokenType.PRE_UNDEF, TokenType.PRE_IFDEF,
        TokenType.PRE_IFNDEF, TokenType.PRE_IF, TokenType.PRE_ELIF, TokenType.PRE_ELSE, TokenType.PRE_ENDIF, TokenType.PRE_PRAGMA
    };

    Initialize()
    {
        // in case Initialize is called more than once
        keywords.Clear(); 
        preprocessorDirectives.Clear(); 
        
        keywords.Append("func");
        keywords.Append("if");
        keywords.Append("else");
        keywords.Append("while");
        keywords.Append("for");
        keywords.Append("break");
        keywords.Append("continue");
        keywords.Append("switch");
        keywords.Append("case");
        keywords.Append("default");
        keywords.Append("const");
        keywords.Append("true");
        keywords.Append("false");
        keywords.Append("null");
        keywords.Append("import");
        keywords.Append("byte");
        keywords.Append("word");
        keywords.Append("char");
        keywords.Append("bool");
        keywords.Append("int");
        keywords.Append("uint");
        
        preprocessorDirectives.Append("include");
        preprocessorDirectives.Append("define");
        preprocessorDirectives.Append("undef");
        preprocessorDirectives.Append("ifdef");
        preprocessorDirectives.Append("ifndef");
        preprocessorDirectives.Append("if");
        preprocessorDirectives.Append("elif");
        preprocessorDirectives.Append("else");
        preprocessorDirectives.Append("endif");
        preprocessorDirectives.Append("pragma");
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
        result.start = 0;
        result.current = 0;
        result.line = 1;
        return result;
    }

    bool isAtEnd(Lexer lexer)
    {
        return lexer.current >= (lexer.Source).Length;
    }

    char advance(ref Lexer lexer)
    {
        char ch = (lexer.Source).GetChar(lexer.current);
        lexer.current++;
        return ch;
    }

    char peek(Lexer lexer)
    {
        if (isAtEnd(lexer)) { return char(0); }
        return (lexer.Source).GetChar(lexer.current);
    }

    char peekNext(Lexer lexer)
    {
        if ((lexer.current + 1) >= (lexer.Source).Length) { return char(0); }
        return (lexer.Source).GetChar(lexer.current + 1);
    }

    bool match(ref Lexer lexer, char expected)
    {
        if (isAtEnd(lexer)) { return false; }
        if ((lexer.Source).GetChar(lexer.current) != expected) { return false; }
        lexer.current++;
        return true;
    }

    Token addToken(Lexer lexer, TokenType tp)
    {
        string text = (lexer.Source).Substring(lexer.start, lexer.current - lexer.start);
        Token token;
        token.Type = tp;
        token.Lexeme = text;
        token.Line = lexer.line;
        return token;
    }

    Token stringToken(ref Lexer lexer)
    {
        while ((peek(lexer) != '"') && !isAtEnd(lexer))
        {
            if (peek(lexer) == Char.EOL) { lexer.line++; }
            _ = advance(ref lexer);
        }

        if (isAtEnd(lexer))
        {
            IO.WriteLn("Unterminated string.");
            Diagnostics.Die(1);
        }

        _ = advance(ref lexer); // The closing "

        return addToken(lexer, TokenType.LIT_STRING);
    }

    Token number(ref Lexer lexer)
    {
        while (Char.IsDigit(peek(lexer)))     { _ = advance(ref lexer); }

        if ((peek(lexer) == '.') && Char.IsDigit(peekNext(lexer)))
        {
            _ = advance(ref lexer);

            while (Char.IsDigit(peek(lexer))) { _ = advance(ref lexer); }
        }

        return addToken(lexer, TokenType.LIT_NUMBER);
    }

    Token identifier(ref Lexer lexer)
    {
        while (Char.IsLetterOrDigit(peek(lexer))) { _ = advance(ref lexer); }

        string text = (lexer.Source).Substring(lexer.start, lexer.current - lexer.start);

        TokenType[] kt = keywordTokens;

        for (uint i = 0; i < keywords.Count; i++)
        {
            if (text == keywords[i])
            {
                WriteLn(i.ToString() + " " + (kt.Count).ToString());
                return addToken(lexer, kt[i]);
            }
        }

        return addToken(lexer, TokenType.IDENTIFIER);
    }

    Token preprocessorDirective(ref Lexer lexer)
    {
        _ = advance(ref lexer); // skip the '#'

        while (Char.IsLetter(peek(lexer))) { _ = advance(ref lexer); }

        string text = (lexer.Source).Substring(lexer.start + 1, lexer.current - lexer.start - 1); // exclude '#'

        TokenType[] ppt = preprocessorTokens;

        for (uint i = 0; i < preprocessorDirectives.Count; i++)
        {
            if (text == preprocessorDirectives[i])
            {
                return addToken(lexer, ppt[i]);
            }
        }

        IO.WriteLn("Unknown preprocessor directive: " + text);
        Diagnostics.Die(1);
        
        Token unreachable;
        return unreachable;
    }

    skipLineComment(ref Lexer lexer)
    {
        while ((peek(lexer) != Char.EOL) && !isAtEnd(lexer))
        {
            _ = advance(ref lexer);
        }
    }

    skipBlockComment(ref Lexer lexer)
    {
        while (!isAtEnd(lexer))
        {
            if ((peek(lexer) == '*') && (peekNext(lexer) == '/'))
            {
                _ = advance(ref lexer);
                _ = advance(ref lexer);
                return;
            }
            if (peek(lexer) == Char.EOL)
            {
                lexer.line++;
            }
            _ = advance(ref lexer);
        }
        IO.WriteLn("Unterminated block comment.");
        Diagnostics.Die(1);
    }

    Token ScanToken(ref Lexer lexer)
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
            case '/':
            {
                if (match(ref lexer, '/'))
                {
                    skipLineComment(ref lexer);
                    return ScanToken(ref lexer);
                }
                else if (match(ref lexer, '*'))
                {
                    skipBlockComment(ref lexer);
                    return ScanToken(ref lexer);
                }
                else
                {
                    return addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_SLASHEQ : TokenType.SYM_SLASH);
                }
            }
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
                if (Char.IsLetter(c))
                {
                    return identifier(ref lexer);
                }
                IO.WriteLn("Unexpected character: 0x" + (byte(c)).ToHexString(2));
                Diagnostics.Die(1);
            }
        }
        Token unreachable;
        return unreachable;
    }
}

