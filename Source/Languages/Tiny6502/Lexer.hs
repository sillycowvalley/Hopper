unit Lexer
{
    uses "TinyToken"
    uses "/Source/System/IO"
    uses "/Source/System/Char"
    
    enum LexerError
    {
        NONE,
        UNTERMINATED_STRING,
        UNTERMINATED_CHAR,
        UNKNOWN_PREPROCESSOR_DIRECTIVE,
        UNEXPECTED_CHARACTER
    }
    
    string ToString(LexerError error)
    {
        switch (error)
        {
            case LexerError.NONE: { return "NONE"; }
            case LexerError.UNTERMINATED_STRING: { return "UNTERMINATED_STRING"; }
            case LexerError.UNTERMINATED_CHAR: { return "UNTERMINATED_CHAR"; }
            case LexerError.UNKNOWN_PREPROCESSOR_DIRECTIVE: { return "UNKNOWN_PREPROCESSOR_DIRECTIVE"; }
            case LexerError.UNEXPECTED_CHARACTER: { return "UNEXPECTED_CHARACTER"; }
        }
        return "UNKNOWN_ERROR";
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
    
    Token charToken(ref Lexer lexer)
    {
        if (isAtEnd(lexer)) 
        {
            IO.WriteLn("Unterminated character literal.");
            Diagnostics.Die(1);
        }
    
        char value = advance(ref lexer);
    
        if (isAtEnd(lexer) || (advance(ref lexer) != '\'')) 
        {
            IO.WriteLn("Unterminated character literal.");
            Diagnostics.Die(1);
        }
    
        return addToken(lexer, TokenType.LIT_CHAR);
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
    bool isValidIdentifierChar(char ch)
    {
        return Char.IsLetterOrDigit(ch) || (ch == '_');
    }

    Token identifier(ref Lexer lexer)
    {
        while (isValidIdentifierChar(peek(lexer))) { _ = advance(ref lexer); }

        string text = (lexer.Source).Substring(lexer.start, lexer.current - lexer.start);
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

        string text = (lexer.Source).Substring(lexer.start + 1, lexer.current - lexer.start - 1); // exclude '#'
        if (preprocessorDirectives.Contains(text))
        {
            return addToken(lexer, preprocessorDirectives[text]);
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

    LexerError ScanToken(ref Lexer lexer, ref Token token)
    {
        loop
        {
            lexer.start = lexer.current;  // Reset start position for the new token
            
            // Skip whitespace and handle EOF
            loop
            {
                if (isAtEnd(lexer)) 
                {
                    token = addToken(lexer, TokenType.EOF);
                    return LexerError.NONE;
                }
                char c = peek(lexer);
                if (!c.IsWhitespace()) 
                {
                    break; // non-whitespace character
                }
                if (c == Char.EOL) { lexer.line++; }
                _ = advance(ref lexer); // consume the peeked whitespace character
            } // loop
    
            lexer.start = lexer.current; // Correctly update the start position after whitespace
            
            if (isAtEnd(lexer)) 
            {
                token = addToken(lexer, TokenType.EOF);
                return LexerError.NONE;
            }
    
            char c = advance(ref lexer);
    
            switch (c)
            {
                case '(': { token = addToken(lexer, TokenType.SYM_LPAREN); return LexerError.NONE; }
                case ')': { token = addToken(lexer, TokenType.SYM_RPAREN); return LexerError.NONE; }
                case '{': { token = addToken(lexer, TokenType.SYM_LBRACE); return LexerError.NONE; }
                case '}': { token = addToken(lexer, TokenType.SYM_RBRACE); return LexerError.NONE; }
                case '[': { token = addToken(lexer, TokenType.SYM_LBRACKET); return LexerError.NONE; }
                case ']': { token = addToken(lexer, TokenType.SYM_RBRACKET); return LexerError.NONE; }
                case ';': { token = addToken(lexer, TokenType.SYM_SEMICOLON); return LexerError.NONE; }
                case ':': { token = addToken(lexer, TokenType.SYM_COLON); return LexerError.NONE; }
                case ',': { token = addToken(lexer, TokenType.SYM_COMMA); return LexerError.NONE; }
                case '.': { token = addToken(lexer, TokenType.SYM_DOT); return LexerError.NONE; }
                case '+': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_PLUSEQ : (match(ref lexer, '+') ? TokenType.SYM_PLUSPLUS : TokenType.SYM_PLUS)); return LexerError.NONE; }
                case '-': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_MINUSEQ : (match(ref lexer, '-') ? TokenType.SYM_MINUSMINUS : TokenType.SYM_MINUS)); return LexerError.NONE; }
                case '*': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_STAREQ : TokenType.SYM_STAR); return LexerError.NONE; }
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
                        return LexerError.NONE;
                    }
                }
                case '%': { token = addToken(lexer, TokenType.SYM_PERCENT); return LexerError.NONE; }
                case '&': { token = addToken(lexer, match(ref lexer, '&') ? TokenType.SYM_AMPAMP : TokenType.SYM_AMP); return LexerError.NONE; }
                case '|': { token = addToken(lexer, match(ref lexer, '|') ? TokenType.SYM_PIPEPIPE : TokenType.SYM_PIPE); return LexerError.NONE; }
                case '^': { token = addToken(lexer, TokenType.SYM_CARET); return LexerError.NONE; }
                case '~': { token = addToken(lexer, TokenType.SYM_TILDE); return LexerError.NONE; }
                case '!': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_NEQ : TokenType.SYM_BANG); return LexerError.NONE; }
                case '=': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_EQEQ : TokenType.SYM_EQ); return LexerError.NONE; }
                case '<': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_LTE : TokenType.SYM_LT); return LexerError.NONE; }
                case '>': { token = addToken(lexer, match(ref lexer, '=') ? TokenType.SYM_GTE : TokenType.SYM_GT); return LexerError.NONE; }
                case '"': { token = stringToken(ref lexer); return LexerError.NONE; }
                case '\'': 
                { 
                    if (isAtEnd(lexer)) 
                    {
                        return LexerError.UNTERMINATED_CHAR;
                    }
                
                    char value = advance(ref lexer);
                
                    if (isAtEnd(lexer) || (advance(ref lexer) != '\'')) 
                    {
                        return LexerError.UNTERMINATED_CHAR;
                    }
                
                    token = addToken(lexer, TokenType.LIT_CHAR);
                    return LexerError.NONE;
                }
                case '#': { token = preprocessorDirective(ref lexer); return LexerError.NONE; }
                default:
                {
                    if (Char.IsDigit(c))
                    {
                        token = number(ref lexer);
                        return LexerError.NONE;
                    }
                    if (Char.IsLetter(c))
                    {
                        token = identifier(ref lexer);
                        return LexerError.NONE;
                    }
                    return LexerError.UNEXPECTED_CHARACTER;
                }
            } // switch
        } // loop
        return LexerError.NONE;
    }
    
    
}

