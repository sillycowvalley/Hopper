unit Parser
{

    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    
    uses "Token"
    uses "Scanner"
    
    <string, string> previousToken;
    <string, string> currentToken;
    
    // <string, string>
    // ["type"]    - HopperToken
    // ["lexeme"]  - string
    // ["line"]    - uint
    // ["source"]  - string
    // ["pos"]     - uint - index in current parsed content string
    // ["literal"] - depends
    
    bool hadError;
    bool interactive;
    
    string progressTicks = "-\\|/-\\|/";
    
    const string errorPath = "/Temp/Errors.txt";
    file errorFile;
    byte currentTick;
    
    byte tickRow;
    byte tickColumn;
    
    SetInteractive(byte column, byte row)
    {
        tickColumn = column;
        tickRow = row;
        interactive = true;
        File.Delete(errorPath);
        errorFile = File.Create(errorPath);
        currentTick = 0;
        Diagnostics.SetError(0x00);
    }
    bool IsInteractive ()
    { 
        return interactive; 
    }
    
    emitError(string errorMessage)
    {
        if (IsInteractive())
        {
            ProgressDone();
            string eol = "" + Char.EOL;
            errorFile.Append(errorMessage + eol);
            errorFile.Flush();
            Diagnostics.SetError(0x0E);
        }
        else
        {
            PrintLn();
            PrintLn(errorMessage, Colour.MatrixRed, Colour.Black);
        }
    }
    ProgressTick(string str)
    {
        if (IsInteractive())
        {
            char c = progressTicks[currentTick];
            DrawChar(tickColumn, tickRow, c, Colour.StatusText, Colour.StatusFace);
            currentTick++;
            if (currentTick == progressTicks.Length)
            {
                currentTick = 0;
            }
        }
        else
        {
            Print(str, Colour.ProgressTick, Colour.ProgressBackground);
        }
    }
    ProgressDone()
    {
        if (IsInteractive())
        {
            DrawChar(tickColumn, tickRow, ' ', Colour.ProgressText, Colour.ProgressFace);
        }
    }
        
    bool HadError { get { return hadError; } }
    
    <string, string> CurrentToken { get { return currentToken; }}    
    <string, string> PreviousToken { get { return previousToken; }}    
    
    DumpToken(string name, <string,string> token)
    {
        OutputDebug(name + ":"); // DumpToken() DumpCurrent() DumpPrevious() DumpPeek()
        foreach (var kv in token)
        {
            OutputDebug("  " + kv.key + "->" + kv.value);
        }
    }
    DumpCurrent()
    {
        DumpToken("CurrentToken", currentToken);
    }    
    DumpPrevious()
    {
        DumpToken("PreviousToken", previousToken);
    }    
    DumpPeek()
    {
        <string, string> token = Scanner.Peek();
        DumpToken("PeekToken", token);
    }
    Reset()
    {
        <string, string> empty;
        previousToken = empty;
        currentToken = empty;
        hadError = false;
    }
        
    ErrorAt(<string, string> token, string message, bool prefix)
    {
        if (!hadError)
        {
            string errorMessage;
            string lexeme = token["lexeme"];
            if (token.Contains("line"))
            {
                string ln = token["line"];
                string path = token["source"];
                errorMessage = "[" + path + ":" + ln;
                long eolPos = Scanner.PosEOL;
                if (eolPos != -1)
                {
                    long pos;
                    if (Long.TryParse(token["pos"], ref pos))
                    {
                        long col = pos - eolPos - 1;
                        errorMessage += "," + col.ToString();        
                    }
                }
                errorMessage += "]";
            }
            HopperToken ttype = Token.GetType(token);
            if (ttype == HopperToken.EOF)
            {
                errorMessage = errorMessage + " Error at end:";
            }
            else if (ttype == HopperToken.Error)
            {
                // nothing
            }
            else if ((lexeme.Length != 0) && prefix)
            {
                errorMessage = errorMessage + " Error at '" + lexeme + "':";
            }
            errorMessage = errorMessage + " " + message;
            emitError(errorMessage);
        }
        hadError = true;
    }
    ErrorAt(<string, string> token, string message)
    {
        ErrorAt(token, message, true);
    }
    Error(char ch)
    {
        ErrorAt(previousToken, "'" + ch + "' expected");
    }
    Error(string message, bool prefix)
    {
        ErrorAt(previousToken, message, prefix);
    }
    Error(string message)
    {
        ErrorAt(previousToken, message, true);
    }
    ErrorAtCurrent(char ch)
    {
        ErrorAt(currentToken, "'" + ch + "' expected");
    }
    ErrorAtCurrent(string message)
    {
        ErrorAt(currentToken, message, true);
    }
    ErrorAtCurrent(string message, bool prefix)
    {
        ErrorAt(currentToken, message, prefix);
    }
    
    Advance()
    {
        previousToken = currentToken;
        loop
        {
            currentToken = Scanner.Next();
            if (Token.GetType(currentToken) != HopperToken.Error)
            {
                break;
            }
            ErrorAtCurrent(currentToken["lexeme"]);
            break;
        }
    }
    Consume(HopperToken consumeType, string message)
    {
        loop
        {
            if (HadError)
            {
                break;
            }
            if (Token.GetType(currentToken) == consumeType)
            {
                Advance();
                break;
            }
            ErrorAtCurrent(message);
            break;             
        }
    }  
    ConsumeKeyword(string keyword)
    {
        loop
        {
            if (HadError)
            {
                break;
            }
            if (Token.GetType(currentToken) == HopperToken.Keyword)
            {
                keyword = "|" + keyword + "|";
                string search = "|" + currentToken["lexeme"] + "|";
                if (keyword.Contains(search))
                {
                    Advance();
                    break;
                }
            }
            ErrorAtCurrent("'" + keyword + "' expected");
            break;             
        }
    }
    Consume(HopperToken consumeType)
    {
        loop
        {
            if (HadError)
            {
                break;
            }
            if (consumeType == Token.GetType(currentToken))
            {
                Advance();
                break;
            }
            switch (consumeType)
            {
                case HopperToken.Comma:
                {
                    ErrorAtCurrent("',' expected");
                }
                case HopperToken.Colon:
                {
                    ErrorAtCurrent("':' expected");
                }
                case HopperToken.Assign:
                {
                    ErrorAtCurrent("'=' expected");
                }
                case HopperToken.SemiColon:
                {
                    ErrorAtCurrent("';' expected");
                }
                case HopperToken.LBracket:
                {
                    ErrorAtCurrent("'[' expected");
                }
                case HopperToken.RBracket:
                {
                    ErrorAtCurrent("']' expected");
                }
                case HopperToken.LBrace:
                {
                    ErrorAtCurrent("'{' expected");
                }
                case HopperToken.RBrace:
                {
                    ErrorAtCurrent("'}' expected");
                }
                case HopperToken.LParen:
                {
                    ErrorAtCurrent("'(' expected");
                }
                case HopperToken.RParen:
                {
                    ErrorAtCurrent("')' expected");
                }
                default:
                {   
                    ErrorAtCurrent("Consume not implemented");
                    Die(0x0A);
                }
            }
            break;             
        }
    }
    
    
    bool Check(HopperToken checkType)
    {
        return (checkType == Token.GetType(currentToken));
    }
    bool CheckDirective(string directive)
    {
        bool result = (HopperToken.Directive == Token.GetType(currentToken));
        if (result)
        {
            result = directive == currentToken["lexeme"];
        }
        return result;
    }
    bool CheckKeyword(string keyword)
    {
        bool result = (HopperToken.Keyword == Token.GetType(currentToken));
        if (result)
        {
            keyword = "|" + keyword + "|";
            string search = "|" + currentToken["lexeme"] + "|";
            result = keyword.Contains(search);
        }
        return result;
    }  
    HopperToken PeekTokenType()
    {
        return Scanner.PeekTokenType();
    }
}
