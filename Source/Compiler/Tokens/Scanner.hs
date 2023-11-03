unit Scanner
{

    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/Compiler/Tokens/Token"

    <string> sourceLines;
    long currentPos;
    uint currentLine;
    long sourceLength;
    uint iCurrentSourceLine;
    long currentStartPos;
    string currentSourcePath;
    char lastChar;
    long lastPos;
    bool lastPosSet;
    
    bool dumpTokens;
    
    <string,string> peekedToken;
    bool isPeekedToken = false;
    
    // public methods defined in Scanner:
    //   New()
    //   <string,string> Peek()
    //   <string,string> Next()
    //   Reset(uint pos, uint line)
    //   Load(string sourcePath)
       
    New()
    {
        sourceLines.Clear();
        Token.Initialize();
    }
    
    Load(string sourcePath)
    {
        byte b;
        string ln;
                
        sourceLines.Clear();
        sourceLength = 0;
        file sourceFile = File.Open(sourcePath);
        uint progress = 0;
        if (sourceFile.IsValid())
        {
            loop
            {
                String.Build(ref ln); // clear
                loop
                {
                    b = sourceFile.Read();
                    if (b == 0)
                    {
                        break; // EOF
                    }
                    Build(ref ln, char(b));
                    if (b == 0x0A)
                    {
                        break;
                    } 
                } // loop
                if ((ln.Length == 0) && !sourceFile.IsValid())
                {
                    break; // not a line
                }
                sourceLength = sourceLength + ln.Length;
                sourceLines.Append(ln);
                
                progress = progress + ln.Length;
                if (progress > 10240)
                {
                    Parser.ProgressTick("+");
                    progress = 0;
                }
            } // loop
            if (progress > 2560)
            {
                Parser.ProgressTick("+");
            }
        }
        currentSourcePath = sourcePath;
    }
    Reset(long pos, uint ln, string sourcePath)
    {
        if (sourcePath != currentSourcePath)
        {
            Load(sourcePath);
        }
        currentPos = pos;
        currentLine = ln;
        <string,string> empty;
        peekedToken = empty;
        isPeekedToken = false;
        
        // only used in sourceGetFromPos
        iCurrentSourceLine = 0;
        currentStartPos = 0;
        lastPos = 0;
        lastPosSet = false;
    }
    Reset(<string, string> startToken)
    {
        loop
        {
            long pos;
            if (!Long.TryParse(startToken["pos"], ref pos))
            {
                Parser.Error("invalid pos: '" + startToken["pos"] + "'");
            }
            uint ln;
            if (!UInt.TryParse(startToken["line"], ref ln))
            {
                Parser.Error("invalid line: '" + startToken["line"] + "'");
            }
            Scanner.Reset(pos, ln, startToken["source"]);
            break;
        }
    }        
    
    DumpTokens(bool dump)
    {
        dumpTokens = true;
    }
    bool isAtEnd()
    {
        //PrintLn("isAtEnd: currentPos=" + currentPos.ToString() + ", sourceLength=" + sourceLength.ToString());
        return (currentPos >= sourceLength);
    }
    <string,string> errorToken(string message)
    {
        <string,string> token = Token.New(HopperToken.Error, message, currentLine, currentPos, currentSourcePath);
        return token;
    }
    char sourceGetFromPos(long pos, bool updateCache)
    {
        char c;
        uint iLine;
        uint maxLines;
        uint length;
        uint uindex;
        string ln;
        long current;
        long limit;
        long index;
        
        loop
        {
            if (lastPosSet)
            {
                if (pos == lastPos)
                {
                    c = lastChar;
                    break;
                }
            }
            if (pos < currentStartPos)
            {
                currentStartPos = 0;
                iCurrentSourceLine = 0;
            }
            
            current = currentStartPos;
            iLine = iCurrentSourceLine;
            maxLines = sourceLines.Length;
            loop
            {
                ln = sourceLines[iLine];
                length = ln.Length;
                limit = current+length;
                if ((pos >= current) && (pos < limit))
                {
                    index = pos - current;
                    uindex = uint(index);
                    c = ln[uindex];
                    break;
                }
                current = current + length;
                iLine = iLine + 1;
                if (iLine == maxLines)
                {
                    break;
                }
                if (updateCache)
                {
                    currentStartPos = current;
                    iCurrentSourceLine = iLine;
                }
            }
            lastPos = pos;
            lastChar = c;
            lastPosSet = true;
            break;
        }
        return c;
    }
    char advance()
    {
        char c = sourceGetFromPos(currentPos, true);
        currentPos = currentPos + 1;
        return c;
    }
    char peek()
    {
        return sourceGetFromPos(currentPos, true);
    }
    char peekNext()
    {
        if (isAtEnd())
        {
            return char(0);
        }
        long currentPlusOne = currentPos+1;
        return sourceGetFromPos(currentPlusOne, false);
    }
    bool match(char expected)
    {
        if (isAtEnd())
        {
            return false;
        }
        if (sourceGetFromPos(currentPos, true) != expected)
        {
            return false;
        }
        currentPos = currentPos + 1;
        return true;
    }
    skipWhitespace()
    {
        loop
        {
            char c = peek();
            if ((c == ' ') || (c == char(0x0D)) || (c == char(0x09)))
            {
                c = advance();
            }
            else if (c == char(0x0A))
            {
                currentLine++;
                c = advance();
            }
            else if (c == '/')
            {
                char n = peekNext();
                if (n == '/')
                {
                    // comment to end of line
                    loop
                    {
                        c = peek();
                        if ((c == char(0)) || (c == char(0x0A)) || isAtEnd())
                        {
                            break;
                        }
                        c = advance(); // gobble gobble
                    }
                }
                else if (n == '*')
                {
                    // block comment until '*/'
                    // TODO: deal with ignoring '*/' within strings
                    c = advance();
                    c = advance();
                    loop
                    {
                        c = peek();
                        if ((c == char(0)) || isAtEnd())
                        {
                            break;
                        }
                        n = peekNext();
                        if ((n == char(0)) || isAtEnd())
                        {
                            break;
                        }
                        if ((c == '*') && (n == '/'))
                        {
                            // end of block comment
                            c = advance();
                            c = advance();
                            break;
                        }
                        c = advance(); // gobble gobble   
                        if (c == char(0x0A))
                        {
                            currentLine++;    
                        }
                    }
                }
                else
                {
                    break; // just a single '/'
                }
            }
            else
            {
                break;
            }
        }
    }
    <string,string> scanString()
    {
        string value;
        while ((peek() != '"') && (peek() != char(0)))
        {
            if (peek() == char(0x0A))
            {
                return errorToken("unexpected EOL in string");
            }
            char c = advance();
            if (c == char(0x5C)) // \
            {
                if (   (peek() == '"') // \"
                    || (peek() == char(0x5C)) // \\
                    )
                {
                    c = advance(); // gooble the \
                }
            }
            value = value + c;
        }
        if (isAtEnd())
        {
            if (value.Length > 80)
            {
                value = value.Substring(0, 80);
            }
            return errorToken("unterminated string: '" + value + "'");
        }
        char c = advance(); // consume the '"'
        return Token.New(HopperToken.StringConstant, value, currentLine, currentPos, currentSourcePath);
    }
    <string,string> scanChar()
    {
        char c = advance();
        if (c == char(0x5C)) 
        {
            if (   (peek() == char(0x27)) // \'
                || (peek() == char(0x5C)) // \\
                )
            {
                c = advance();         // gobble the \
            }
        }
        char d = advance();
        if (isAtEnd() || (d != char(0x27))) // '
        {
            return errorToken("' expected");
        }
        return Token.New(HopperToken.Char, c.ToString(), currentLine, currentPos, currentSourcePath);
    }
    <string,string> scanNumber(char c)
    {
        <string,string> token;
        string value = c.ToString();
        bool hexOk = false;
        bool binaryOk = false;
        bool floatOk = false;
        
        HopperToken ttype = HopperToken.Integer;
        loop
        {
            loop
            {
                c = peek();
                if ((c == 'x') && (value == "0"))
                {
                    hexOk = true;
                    value = value + advance();
                    continue;
                }
                if ((c == 'b') && (value == "0"))
                {
                    binaryOk = true;
                    value = value + advance();
                    continue;
                }
                if (hexOk && c.IsHexDigit())
                {
                    // hex
                }
                else if (binaryOk && ((c == '0') || (c == '1')))
                {
                    // binary
                }
                else if ((c == '.') && !floatOk && (value.Length > 0))
                {
                    floatOk = true;
                }
                else if (!c.IsDigit())
                {
                    break;
                }
                value = value + advance();
            }
            if (hexOk)
            {
                if (value.Length < 3)
                {
                    token = errorToken("invalid hex integer literal");
                    break;
                }
                uint hexValue = 0;
                if (!UInt.TryParse(value, ref hexValue))
                {
                    token = errorToken("invalid hex integer literal");
                    break;
                }
                value = hexValue.ToString();
            }
            else if (binaryOk)
            {
                if (value.Length < 3)
                {
                    token = errorToken("invalid binary integer literal");
                    break;
                }
                uint binaryValue = 0;
                if (!UInt.TryParse(value, ref binaryValue))
                {
                    token = errorToken("invalid binary integer literal");
                    break;
                }
                value = binaryValue.ToString();
            }
            else if (floatOk)
            {
                float f;
                if (!Float.TryParse(value, ref f))
                {
                    token = errorToken("invalid float literal");
                    break;       
                }
                value = f.ToString();
                ttype = HopperToken.Float;
            }
            else // just Integer
            {
                long l;
                if (!Long.TryParse(value, ref l))
                {
                    token = errorToken("invalid integer literal");
                    break;       
                }
                value = l.ToString();
            }
            token = Token.New(ttype, value, currentLine, currentPos, currentSourcePath);
            
            break;
        }
        return token; 
    }
    <string,string> scanIdentifier(char c)
    {
        uint dotSeen = 0;
        string value = c.ToString();
        HopperToken ttype = HopperToken.Identifier;
        loop
        {
            c = peek();
            if (!c.IsLetter() && !c.IsDigit())
            {
                if ((c == '.') && (dotSeen == 0))
                {
                    dotSeen++;
                }
                else
                { 
                    break;
                }
            }
            value = value + advance();
        }
        if (dotSeen > 0)
        {
            ttype = HopperToken.DottedIdentifier;
        }
        if (Token.IsKeyword(value))
        {
            switch (value)
            {
                case "true":
                {
                    ttype = HopperToken.Bool;
                }
                case "false":
                {
                    ttype = HopperToken.Bool;
                }
                default:
                {
                    ttype = HopperToken.Keyword;
                }
            }
        }
        if (Token.IsDirectiveKeyword(value))
        {
            ttype = HopperToken.Directive;
        }
        return Token.New(ttype, value, currentLine, currentPos, currentSourcePath);
    }
    <string,string> Next()
    {   
        <string,string> token = Token.New(HopperToken.Undefined, "", currentLine, currentPos, currentSourcePath);
        loop
        {
            if (isPeekedToken)
            {
                token = peekedToken;
                isPeekedToken = false;
                break;
            }
            if (isAtEnd())
            {
                token = Token.New(HopperToken.EOF, "", currentLine, currentPos, currentSourcePath);
                break;
            }
            loop
            {
                skipWhitespace();
                char c = advance();
                HopperToken htoken = HopperToken.Undefined;
                switch (c)
                {
                    case '#':
                    case 'a':
                    case 'b':
                    case 'c':
                    case 'd':
                    case 'e':
                    case 'f':
                    case 'g':
                    case 'h':
                    case 'i':
                    case 'j':
                    case 'k':
                    case 'l':
                    case 'm':
                    case 'n':
                    case 'o':
                    case 'p':
                    case 'q':
                    case 'r':
                    case 's':
                    case 't':
                    case 'u':
                    case 'v':
                    case 'w':
                    case 'x':
                    case 'y':
                    case 'z':
                    case 'A':
                    case 'B':
                    case 'C':
                    case 'D':
                    case 'E':
                    case 'F':
                    case 'G':
                    case 'H':
                    case 'I':
                    case 'J':
                    case 'K':
                    case 'L':
                    case 'M':
                    case 'N':
                    case 'O':
                    case 'P':
                    case 'Q':
                    case 'R':
                    case 'S':
                    case 'T':
                    case 'U':
                    case 'V':
                    case 'W':
                    case 'X':
                    case 'Y':
                    case 'Z':
                    {
                        token = scanIdentifier(c);
                        break;
                    }
                
                    
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                    {
                        token = scanNumber(c);
                        break;
                    }
                    case '{': { htoken = HopperToken.LBrace; }
                    case '}': { htoken = HopperToken.RBrace; }
                    case ';': { htoken = HopperToken.SemiColon; }
                    case '(': { htoken = HopperToken.LParen; }
                    case ')': { htoken = HopperToken.RParen; }
                    case '[': { htoken = HopperToken.LBracket; }
                    case ']': { htoken = HopperToken.RBracket; }
                    case ':': { htoken = HopperToken.Colon; }
                    case ',': { htoken = HopperToken.Comma; }
                    case '.': { htoken = HopperToken.Dot; }
                    case '*': { htoken = HopperToken.Multiply; }
                    case '/': { htoken = HopperToken.Divide; }
                    case '%': { htoken = HopperToken.Modulus; }
                    case '~': { htoken = HopperToken.BitNot; }
                    case '^': { htoken = HopperToken.BitXor; }
                    case '?': { htoken = HopperToken.Question; }
                    
                    case '-':
                    {
                        if (match('-'))
                        { htoken = HopperToken.Decrement; }
                        else
                        { htoken = HopperToken.Subtract; }
                    }
                    case '+':
                    {
                        if (match('+'))
                        { htoken = HopperToken.Increment; }
                        else
                        { htoken = HopperToken.Add; }
                    }
                    case '!':
                    {
                        if (match('='))
                        { htoken = HopperToken.NE; }
                        else
                        { htoken = HopperToken.BooleanNot; }
                    }
                    case '=':
                    {
                        if (match('='))
                        { htoken = HopperToken.EQ; }
                        else
                        { htoken = HopperToken.Assign; }
                    }
                    case '<':
                    {
                        if (match('='))
                        { htoken = HopperToken.LE; }
                        else if (match('<'))
                        { htoken = HopperToken.ShiftLeft; }
                        else
                        { htoken = HopperToken.LT; }
                    }
                    case '>':
                    {
                        if (match('='))
                        { htoken = HopperToken.GE; }
                        else if (match('>'))
                        { htoken = HopperToken.ShiftRight; }
                        else
                        { htoken = HopperToken.GT; }
                    }
                    case '|':
                    {
                        if (match('|'))
                        { htoken = HopperToken.BooleanOr; }
                        else
                        { htoken = HopperToken.BitOr; }
                    }
                    case '&':
                    {
                        if (match('&'))
                        { htoken = HopperToken.BooleanAnd; }
                        else
                        { htoken = HopperToken.BitAnd; }
                    }
                    case '"':
                    {
                        token = scanString();
                    }
                    case '\'':
                    {
                        token = scanChar();
                    }
                    default:
                    {
                        uint ui = uint(c);
                        if ((ui == 0) && isAtEnd())
                        {
                            token = Token.New(HopperToken.EOF, "", currentLine, currentPos, currentSourcePath);
                        }
                        else
                        {
                            token = errorToken("unexpected character: '" + c + "' (0x" + ui.ToHexString(2) + ")");
                        }
                    }
                } // switch
                if (htoken != HopperToken.Undefined)
                {
                    token = Token.New(htoken, "", currentLine, currentPos, currentSourcePath);
                }
                break;
            }
            break;
        }
        if (dumpTokens)
        {
            PrintLn("  " + Token.ToString(token), Color.Red, Color.Black);
        }
        return token;
    }
    <string,string> Peek()
    {
        if (!isPeekedToken)
        {
            peekedToken = Next();
            isPeekedToken = true;
        }
        return peekedToken;
    }
}
