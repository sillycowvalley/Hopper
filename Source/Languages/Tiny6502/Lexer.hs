unit Lexer
{
    uses "TinyToken"
    uses "/Source/System/IO"
    uses "/Source/System/Char"

    enum LexerErrorType
    {
        NONE,
        UNTERMINATED_STRING,
        UNTERMINATED_CHAR,
        UNKNOWN_PREPROCESSOR_DIRECTIVE,
        UNEXPECTED_CHARACTER
    }

    record LexerError
    {
        LexerErrorType Type;
        uint Line;
    }

    string ToString(LexerError error)
    {
        string result = "UNKNOWN_ERROR"; // Default value
        switch (error.Type)
        {
            case LexerErrorType.NONE:
            {
                result = "NONE";
            }
            case LexerErrorType.UNTERMINATED_STRING:
            {
                result = "UNTERMINATED_STRING";
            }
            case LexerErrorType.UNTERMINATED_CHAR:
            {
                result = "UNTERMINATED_CHAR";
            }
            case LexerErrorType.UNKNOWN_PREPROCESSOR_DIRECTIVE:
            {
                result = "UNKNOWN_PREPROCESSOR_DIRECTIVE";
            }
            case LexerErrorType.UNEXPECTED_CHARACTER:
            {
                result = "UNEXPECTED_CHARACTER";
            }
        }
        result = result + " at line " + (error.Line).ToString();
        return result;
    }

    <string,TokenType> keywords;
    <string,TokenType> preprocessorDirectives;

    Initialize()
    {
        // in case Initialize is called more than once
        keywords.Clear();
        preprocessorDirectives.Clear();
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
        preprocessorDirectives["include"] = TokenType.PRE_INCLUDE;
        preprocessorDirectives["define"] = TokenType.PRE_DEFINE;
        preprocessorDirectives["undef"] = TokenType.PRE_UNDEF;
        preprocessorDirectives["ifdef"] = TokenType.PRE_IFDEF;
        preprocessorDirectives["ifndef"] = TokenType.PRE_IFNDEF;
        preprocessorDirectives["if"] = TokenType.PRE_IF;
        preprocessorDirectives["elif"] = TokenType.PRE_ELIF;
        preprocessorDirectives["else"] = TokenType.PRE_ELSE;
        preprocessorDirectives["endif"] = TokenType.PRE_ENDIF;
        preprocessorDirectives["pragma"] = TokenType.PRE_PRAGMA;
    }
    

    record Lexer
    {
        string Source;
        uint Start;
        uint Current;
        uint Line;
    }

    Lexer Lexer(string src)
    {
        Lexer result;
        result.Source = src;
        result.Start = 0;
        result.Current = 0;
        result.Line = 1;
        return result;
    }

    bool isAtEnd(Lexer lexer)
    {
        return lexer.Current >= (lexer.Source).Length;
    }

    char advance(ref Lexer lexer)
    {
        char ch = (lexer.Source).GetChar(lexer.Current);
        lexer.Current++;
        return ch;
    }

    char peek(Lexer lexer)
    {
        if (isAtEnd(lexer)) { return char(0); }
        return (lexer.Source).GetChar(lexer.Current);
    }

    char peekNext(Lexer lexer)
    {
        if ((lexer.Current + 1) >= (lexer.Source).Length) { return char(0); }
        return (lexer.Source).GetChar(lexer.Current + 1);
    }

    bool match(ref Lexer lexer, char expected)
    {
        if (isAtEnd(lexer)) { return false; }
        if ((lexer.Source).GetChar(lexer.Current) != expected) { return false; }
        lexer.Current++;
        return true;
    }

    Token addToken(Lexer lexer, TokenType tp)
    {
        string text = (lexer.Source).Substring(lexer.Start, lexer.Current - lexer.Start);
        Token token;
        token.Type = tp;
        token.Lexeme = text;
        token.Line = lexer.Line;
        return token;
    }

    Token stringToken(ref Lexer lexer)
    {
        while ((peek(lexer) != '"') && !isAtEnd(lexer))
        {
            if (peek(lexer) == Char.EOL) { lexer.Line++; }
            _ = advance(ref lexer);
        }
        if (isAtEnd(lexer))
        {
            Token token;
            token = addToken(lexer, TokenType.LIT_STRING);
            return token;
        }
        _ = advance(ref lexer); // The closing "
        return addToken(lexer, TokenType.LIT_STRING);
    }

    Token charToken(ref Lexer lexer)
    {
        if (isAtEnd(lexer))
        {
            Token token;
            token = addToken(lexer, TokenType.LIT_CHAR);
            return token;
        }

        char value = advance(ref lexer);

        if (isAtEnd(lexer) || (advance(ref lexer) != '\''))
        {
            Token token;
            token = addToken(lexer, TokenType.LIT_CHAR);
            return token;
        }

        return addToken(lexer, TokenType.LIT_CHAR);
    }

    Token number(ref Lexer lexer)
    {
        while (Char.IsDigit(peek(lexer))) { _ = advance(ref lexer); }
        if ((peek(lexer) == '.') && Char.IsDigit(peekNext(lexer)))
        {
            _ = advance(ref lexer);
            while (Char.IsDigit(peek(lexer))) { _ = advance(ref lexer); }
        }
        return addToken(lexer, TokenType.LIT_NUMBER);
    }

    bool isValidIdentifierChar(char ch)
    {
        return Char.IsLetterOrDigit(ch) || (ch == '_');
    }

    Token identifier(ref Lexer lexer)
    {
        while (isValidIdentifierChar(peek(lexer))) { _ = advance(ref lexer); }
        string text = (lexer.Source).Substring(lexer.Start, lexer.Current - lexer.Start);
        if (keywords.Contains(text))
        {
            return addToken(lexer, keywords[text]);
        }
        return addToken(lexer, TokenType.IDENTIFIER);
    }

    Token preprocessorDirective(ref Lexer lexer)
    {
        _ = advance(ref lexer); // skip the '#'
        while (Char.IsLetter(peek(lexer))) { _ = advance(ref lexer); }
        string text = (lexer.Source).Substring(lexer.Start + 1, lexer.Current - lexer.Start - 1); // exclude '#'
        if (preprocessorDirectives.Contains(text))
        {
            return addToken(lexer, preprocessorDirectives[text]);
        }
        Token token;
        token = addToken(lexer, TokenType.PRE_UNKNOWN);
        return token;
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
                lexer.Line++;
            }
            _ = advance(ref lexer);
        }
    }

     LexerError ScanToken(ref Lexer lexer, ref Token token)
    {
        loop
        {
            lexer.Start = lexer.Current;  // Reset start position for the new token
    
            // Skip whitespace and handle EOF
            loop
            {
                if (isAtEnd(lexer))
                {
                    token = addToken(lexer, TokenType.EOF);
                    LexerError error;
                    error.Type = LexerErrorType.NONE;
                    error.Line = lexer.Line;
                    return error;
                }
                char c = peek(lexer);
                if (!c.IsWhitespace())
                {
                    break; // non-whitespace character
                }
                if (c == Char.EOL) { lexer.Line++; }
                _ = advance(ref lexer); // consume the peeked whitespace character
            } // loop
    
            lexer.Start = lexer.Current; // Correctly update the start position after whitespace
    
            if (isAtEnd(lexer))
            {
                token = addToken(lexer, TokenType.EOF);
                LexerError error;
                error.Type = LexerErrorType.NONE;
                error.Line = lexer.Line;
                return error;
            }
    
            char c = advance(ref lexer);
    
            switch (c)
            {
                case '(': { token = addToken(lexer, TokenType.SYM_LPAREN); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case ')': { token = addToken(lexer, TokenType.SYM_RPAREN); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '{': { token = addToken(lexer, TokenType.SYM_LBRACE); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '}': { token = addToken(lexer, TokenType.SYM_RBRACE); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '[': { token = addToken(lexer, TokenType.SYM_LBRACKET); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case ']': { token = addToken(lexer, TokenType.SYM_RBRACKET); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case ';': { token = addToken(lexer, TokenType.SYM_SEMICOLON); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case ':': { token = addToken(lexer, TokenType.SYM_COLON); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case ',': { token = addToken(lexer, TokenType.SYM_COMMA); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '.': { token = addToken(lexer, TokenType.SYM_DOT); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '+': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_PLUSEQ : (match(ref lexer, '+') ? TokenType.SYM_PLUSPLUS : TokenType.SYM_PLUS)); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '-': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_MINUSEQ : (match(ref lexer, '-') ? TokenType.SYM_MINUSMINUS : TokenType.SYM_MINUS)); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '*': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_STAREQ : TokenType.SYM_STAR); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '/':
                {
                    if (match(ref lexer, '/'))
                    {
                        skipLineComment(ref lexer);
                        continue;
                    }
                    else if (match(ref lexer, '*'))
                    {
                        skipBlockComment(ref lexer);
                        continue;
                    }
                    else
                    {
                        token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_SLASHEQ : TokenType.SYM_SLASH);
                        LexerError error;
                        error.Type = LexerErrorType.NONE;
                        error.Line = lexer.Line;
                        return error;
                    }
                }
                case '%': { token = addToken(lexer, TokenType.SYM_PERCENT); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '&': { token = addToken(lexer, match(ref lexer, '&') ? TokenType.SYM_AMPAMP : TokenType.SYM_AMP); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '|': { token = addToken(lexer, match(ref lexer, '|') ? TokenType.SYM_PIPEPIPE : TokenType.SYM_PIPE); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '^': { token = addToken(lexer, TokenType.SYM_CARET); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '~': { token = addToken(lexer, TokenType.SYM_TILDE); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '!': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_NEQ : TokenType.SYM_BANG); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '=': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_EQEQ : TokenType.SYM_EQ); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '<': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_LTE : TokenType.SYM_LT); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '>': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_GTE : TokenType.SYM_GT); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '"': { token = stringToken(ref lexer); LexerError error; error.Type = LexerErrorType.NONE; error.Line = lexer.Line; return error; }
                case '\'':
                {
                    if (isAtEnd(lexer))
                    {
                        LexerError error;
                        error.Type = LexerErrorType.UNTERMINATED_CHAR;
                        error.Line = lexer.Line;
                        return error;
                    }
    
                    char value = advance(ref lexer);
    
                    if (isAtEnd(lexer) || (advance(ref lexer) != '\''))
                    {
                        LexerError error;
                        error.Type = LexerErrorType.UNTERMINATED_CHAR;
                        error.Line = lexer.Line;
                        return error;
                    }
    
                    token = addToken(lexer, TokenType.LIT_CHAR);
                    LexerError noError;
                    noError.Type = LexerErrorType.NONE;
                    noError.Line = lexer.Line;
                    return noError;
                }
                case '#': 
                { 
                    token = preprocessorDirective(ref lexer); 
                    if (token.Type == TokenType.PRE_UNKNOWN)
                    {
                        LexerError error;
                        error.Type = LexerErrorType.UNKNOWN_PREPROCESSOR_DIRECTIVE;
                        error.Line = lexer.Line;
                        return error;
                    }
                    LexerError noError;
                    noError.Type = LexerErrorType.NONE;
                    noError.Line = lexer.Line;
                    return noError; 
                }
                default:
                {
                    if (Char.IsDigit(c))
                    {
                        token = number(ref lexer);
                        LexerError error;
                        error.Type = LexerErrorType.NONE;
                        error.Line = lexer.Line;
                        return error;
                    }
                    if (Char.IsLetter(c))
                    {
                        token = identifier(ref lexer);
                        LexerError error;
                        error.Type = LexerErrorType.NONE;
                        error.Line = lexer.Line;
                        return error;
                    }
                    LexerError error;
                    error.Type = LexerErrorType.UNEXPECTED_CHARACTER;
                    error.Line = lexer.Line;
                    return error;
                }
            } // switch
        } // loop
        LexerError noError;
        noError.Type = LexerErrorType.NONE;
        noError.Line = lexer.Line;
        return noError;
    }
}   

