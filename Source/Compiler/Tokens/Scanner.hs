unit Scanner
{

    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "Token"

    <string> sourceLines;
    long currentPos;
    long currentPosEOL; // position of most recently seen EOL (to calculate 'col' in error messages from currentPos;
    uint currentLine;
    long sourceLength;
    uint iCurrentSourceLine;
    long currentStartPos;
    string currentSourcePath;
    char lastChar;
    long lastPos;
    bool lastPosSet;
    
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
        Token.InitializeHopper();
    }
    
    long PosEOL { get { return currentPosEOL; } }
    
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
                    String.Build(ref ln, char(b));
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
                if (progress > 40960)
                {
                    Parser.ProgressTick("s"); // scanner
                    progress = 0;
                }
            } // loop
            if (progress > 5120)
            {
                Parser.ProgressTick("s"); // scanner
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
        currentPosEOL = -1;
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
            maxLines = sourceLines.Count;
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
    bool skipWhitespace()
    {
        char c;
        char n;
        bool inString;
        uint nesting;
        loop
        {
            c = sourceGetFromPos(currentPos, true); // peek
            if ((c == ' ') || (c == char(0x0D)) || (c == char(0x09)))
            {
                c = advance();
            }
            else if (c == Char.EOL)
            {
                currentLine++;
                currentPosEOL = currentPos;
                c = advance();
            }
            else if (c == '/')
            {
                n = peekNext();
                if (n == '/')
                {
                    // comment to end of line
                    loop
                    {
                        c = sourceGetFromPos(currentPos, true); // peek
                        if ((c == char(0)) || (c == Char.EOL) || isAtEnd())
                        {
                            break;
                        }
                        c = advance(); // gobble gobble
                    }
                }
                else if (n == '*')
                {
                    // block comment until '*/'
                    c = advance();
                    c = advance();
                    inString = false;
                    nesting = 1;
                    loop
                    {
                        c = sourceGetFromPos(currentPos, true); // peek
                        if ((c == char(0)) || isAtEnd())
                        {
                            return false; // unexpected EOF in block comment
                        }
                        n = peekNext();
                        if ((n == char(0)) || isAtEnd())
                        {
                            return false; // unexpected EOF in block comment
                        }
                        if ((c == '*') && (n == '/') && !inString)
                        {
                            // end of block comment
                            nesting--;
                            c = advance();
                            c = advance();
                            if (nesting > 0)
                            {
                                continue;
                            }
                            break;
                        }
                        else if ((c == '/') && (n == '*') && !inString)
                        {
                            nesting++;
                            c = advance();
                            c = advance();
                            continue;
                        }
                        //else if (inString && (c == '\\') && (n == '"'))
                        else if (inString && (c == '\\'))
                        {
                            // ignore \" in string
                            c = advance(); // gobble
                            c = advance(); // gobble
                            continue;
                        }
                        else if (c == '"')
                        {
                            inString = !inString;
                        }
                        c = advance(); // gobble gobble   
                        if (c == Char.EOL)
                        {
                            currentLine++;    
                            currentPosEOL = currentPosEOL;
                        }
                    } // loop
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
        return true;
    }
    <string,string> scanString()
    {
        char c;
        char p;
        string value;
        loop
        {
            p = sourceGetFromPos(currentPos, true); // peek
            if ((p == '"') || (p == char(0)))
            {
                break;
            }
            if (p == Char.EOL)
            {
                return errorToken("unexpected EOL in string");
            }
            c = advance();
            if (c == char(0x5C)) // \
            {
                p = sourceGetFromPos(currentPos, true); // peek
                if (   (p == '"') // \"
                    || (p == char(0x5C)) // \\
                    )
                {
                    c = advance(); // gooble the \
                }
            }
            String.Build(ref value, c);
        }
        if (isAtEnd())
        {
            if (value.Length > 80)
            {
                value = value.Substring(0, 80);
            }
            return errorToken("unterminated string: '" + value + "'");
        }
        c = advance(); // consume the '"'
        return Token.New(HopperToken.StringConstant, value, currentLine, currentPos, currentSourcePath);
    }
    <string,string> scanChar()
    {
        char p;
        char d;
        char c = advance();
        if (c == char(0x5C)) 
        {
            p = sourceGetFromPos(currentPos, true); // peek
            if (   (p == char(0x27)) // \'
                || (p == char(0x5C)) // \\
                )
            {
                c = advance();         // gobble the \
            }
        }
        d = advance();
        if (isAtEnd() || (d != char(0x27))) // '
        {
            return errorToken("' expected");
        }
        return Token.New(HopperToken.Char, c.ToString(), currentLine, currentPos, currentSourcePath);
    }
    <string,string> scanNumber(char c)
    {
        bool hexOk;
        bool binaryOk;
        bool floatOk;
        char c2;
        HopperToken ttype = HopperToken.Integer;
        
        <string,string> token;
        string value = c.ToString();
        
        loop
        {
            loop
            {
                c = sourceGetFromPos(currentPos, true); // peek
                if ((c == 'x') && (value == "0"))
                {
                    hexOk = true;
                    String.Build(ref value, advance());
                    continue;
                }
                if ((c == 'b') && (value == "0"))
                {
                    binaryOk = true;
                    String.Build(ref value, advance());
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
                else if ((c == '.') && !floatOk && (value.Length != 0))
                {
                    c2 = peekNext();
                    if (c2 == '.') // ..
                    {
                        break;
                    }
                    floatOk = true;
                }
                else if (!c.IsDigit())
                {
                    break;
                }
                String.Build(ref value, advance());
            }
            if (hexOk)
            {
                if (value.Length < 3)
                {
                    token = errorToken("invalid hex integer literal");
                    break;
                }
                long hexValue = 0;
                if (!Long.TryParse(value, ref hexValue))
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
        uint dotSeen;
        HopperToken ttype = HopperToken.Identifier;
        string value = c.ToString();
        loop
        {
            c = sourceGetFromPos(currentPos, true); // peek
#ifdef ASSEMBLER
            if ((c == ':') && !Token.IsKeyword(value) && !value.Contains('.')) 
            {
                // labels can end in ':',
                //    - but not keywords like "default:")   
                //    - and not dotted identifiers (used for "case xxx.yyy:")
                String.Build(ref value, advance());
                break;
            }
#endif            
            if (!c.IsLetter() && !c.IsDigit() && (c != '_'))
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
            String.Build(ref value, advance());
        }
        if (dotSeen > 0)
        {
            ttype = HopperToken.DottedIdentifier;
        }
#ifdef ASSEMBLER
        if (value.EndsWith(':'))
        {
            ttype = HopperToken.LabelIdentifier;
        }
#endif
        if (Token.IsKeyword(value))
        {
            ttype = HopperToken.Keyword;
            if  ((value == "true") || (value == "false"))
            {
                ttype = HopperToken.Bool;
            }
        }
        else if (Token.IsDirectiveKeyword(value))
        {
            ttype = HopperToken.Directive;
        }
        else if (Token.IsInstructionKeyword(value))
        {
            ttype = HopperToken.Instruction;
        }
        else if (Token.IsRegisterKeyword(value))
        {
            ttype = HopperToken.Register;
        }
        else if (Token.IsConditionKeyword(value))
        {
            ttype = HopperToken.Condition;
        }
        return Token.New(ttype, value, currentLine, currentPos, currentSourcePath);
    }
    <string,string> Next()
    {   
        char c;
        uint ui;
        HopperToken htoken;
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
                htoken = HopperToken.Undefined;
                if (!skipWhitespace())
                {
                    token = errorToken("unexpected EOF in block comment");
                    break;
                }
                c = advance();
                switch (c)
                {
                    case '#':
                    {
                        c = sourceGetFromPos(currentPos, true); // peek
                        if (c.IsLower() || c.IsUpper() || (c == '_'))
                        {
                            token = scanIdentifier('#');
                            break;
                        }
                        else
                        {
                            htoken = HopperToken.Hash;
                        }
                    }
                    case 'a'..'z', 'A'..'Z':
                    {
                        token = scanIdentifier(c);
                        break;
                    }
                    case '0'..'9':
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
                    case '~': { htoken = HopperToken.BitNot; }
                    case '^': { htoken = HopperToken.BitXor; }
                    case '?': { htoken = HopperToken.Question; }
                    case '_': { htoken = HopperToken.Discarder; }
                    
                    case '*':
                    {
                        htoken = HopperToken.Multiply;
                        if (match('=')) { htoken = HopperToken.AssignMultiply; }
                    }
                    case '/':
                    {
                        htoken = HopperToken.Divide;
                        if (match('=')) { htoken = HopperToken.AssignDivide; }
                    }
                    case '%':
                    {
                        htoken = HopperToken.Modulus;
                        if (match('=')) { htoken = HopperToken.AssignModulus; }
                    }
                    case '-':
                    {
                        htoken = HopperToken.Subtract;
                        if (match('-'))      { htoken = HopperToken.Decrement; }
                        else if (match('=')) { htoken = HopperToken.AssignSubtract; }
                    }
                    case '+':
                    {
                        htoken = HopperToken.Add;
                        if (match('+'))      { htoken = HopperToken.Increment; }
                        else if (match('=')) { htoken = HopperToken.AssignAdd; }
                    }
                    case '!':
                    {
                        htoken = HopperToken.BooleanNot;
                        if (match('=')) { htoken = HopperToken.NE; }
                    }
                    case '=':
                    {
                        htoken = HopperToken.Assign;
                        if (match('=')) { htoken = HopperToken.EQ; }
                    }
                    case '<':
                    {
                        htoken = HopperToken.LT;
                        if (match('='))      { htoken = HopperToken.LE; }
                        else if (match('<')) { htoken = HopperToken.ShiftLeft; }
                    }
                    case '>':
                    {
                        htoken = HopperToken.GT;
                        if (match('='))      { htoken = HopperToken.GE; }
                        else if (match('>')) { htoken = HopperToken.ShiftRight; }
                    }
                    case '|':
                    {
                        htoken = HopperToken.BitOr;
                        if (match('|'))      { htoken = HopperToken.BooleanOr; }
                        else if (match('=')) { htoken = HopperToken.AssignBitOr; }
                    }
                    case '&':
                    {
                        htoken = HopperToken.BitAnd;
                        if (match('&'))      { htoken = HopperToken.BooleanAnd; }
                        else if (match('=')) { htoken = HopperToken.AssignBitAnd; }
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
                        ui = uint(c);
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
    HopperToken PeekTokenType()
    {
        if (!isPeekedToken)
        {
            peekedToken = Next();
            isPeekedToken = true;
        }
        return Token.GetType(peekedToken);
    }
}
