unit Lexer
{
    uses "TinyToken"
    uses "/Source/System/IO"
    uses "/Source/System/Char"

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

    Token ScanToken(ref Lexer lexer)
    {
        loop
        {
            lexer.start = lexer.current;  // Reset start position for the new token
            
            // Skip whitespace and handle EOF
            loop
            {
                if (isAtEnd(lexer)) 
                {
                    return addToken(lexer, TokenType.EOF);
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
                return addToken(lexer, TokenType.EOF);
            }
    
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
                        continue;
                    }
                    else if (match(ref lexer, '*'))
                    {
                        skipBlockComment(ref lexer);
                        continue;
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
                case '\'': { return charToken(ref lexer); }
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
            } // switch
        } // loop
        Token unreachable;
        return unreachable;
    }
    
}

