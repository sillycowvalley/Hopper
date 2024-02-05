unit Parser
{

    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    
    <string, string> previousToken;
    <string, string> currentToken;
    
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
    
    EmitError(string errorMessage)
    {
        if (IsInteractive())
        {
            ProgressDone();
            string eol = "" + char(0x0A);
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
    
    // ["type"]    - HopperToken
    // ["lexeme"]  - string
    // ["line"]    - uint
    // ["source"]  - string
    // ["pos"]     - uint - index in current parsed content string
    // ["literal"] - depends


    DumpToken(string name, <string,string> token)
    {
        OutputDebug(name + ":");
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
        <string, string> token = Peek();
        DumpToken("PeekToken", token);
    }
    Reset()
    {
        <string, string> empty;
        previousToken = empty;
        currentToken = empty;
        hadError = false;
    }
        
    nextToken()
    {
        currentToken = Scanner.Next();
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
                errorMessage = "[" + path + ":" + ln + "]";
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
            EmitError(errorMessage);
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
            nextToken();
            HopperToken ttype = Token.GetType(currentToken);
            if (ttype != HopperToken.Error)
            {
                break;
            }
            ErrorAtCurrent(currentToken["lexeme"]);
            break;
        }
    }
    Consume(HopperToken consumeType, char ch)
    {
        Consume(consumeType, "", "'" + ch + "' expected");
    }
    Consume(HopperToken consumeType, string message)
    {
        Consume(consumeType, "", message);
    }  
    Consume(HopperToken consumeType, string keyword, string message)
    {
        loop
        {
            if (HadError)
            {
                break;
            }
            HopperToken ttype = Token.GetType(currentToken);
            if (ttype == consumeType)
            {
                if (ttype == HopperToken.Keyword)
                {
                    keyword = "|" + keyword + "|";
                    string search = "|" + currentToken["lexeme"] + "|";
                    if (keyword.Contains(search))
                    {
                        Advance();
                        break;
                    }
                }
                else
                {
                    Advance();
                    break;
                }
            }
            ErrorAtCurrent(message);
            break;             
        }
    }
    bool Check(HopperToken checkType)
    {
        return Check(checkType, "");
    }
    bool Check(HopperToken checkType, string keyword)
    {
        bool result = false;
        loop
        {
            HopperToken ttype = Token.GetType(currentToken);
            result = (checkType == ttype);
            if (!result)
            {
                break;
            }
            if (checkType == HopperToken.Keyword)
            {
                keyword = "|" + keyword + "|";
                string search = "|" + currentToken["lexeme"] + "|";
                result = keyword.Contains(search);
            }
            else if ((checkType == HopperToken.Identifier) && (keyword.Length != 0))
            {
                result = keyword == currentToken["lexeme"];
            }
            else if ((checkType == HopperToken.Directive) && (keyword.Length != 0))
            {
                result = keyword == currentToken["lexeme"];
            }
            break;             
        }
        return result;
    }  
    
    <string,string> Peek()
    {
        <string,string> peek = Scanner.Peek();
        return peek;
    }
    <string,string> PeekNext()
    {
        <string,string> peekNext = Scanner.PeekNext();
        return peekNext;
    }
}
