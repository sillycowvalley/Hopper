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
    
    SetInteractive(bool value)
    {
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
            PrintLn(errorMessage);
        }
    }
    ProgressTick(string str)
    {
        if (IsInteractive())
        {
            char c = progressTicks[currentTick];
            DrawChar(1, Screen.Rows-1, c, Color.ButtonText, Color.Button);
            currentTick++;
            if (currentTick == progressTicks.Length)
            {
                currentTick = 0;
            }
        }
        else
        {
            Print(str, Color.MatrixBlue, Color.PaleBlue);
        }
    }
    ProgressDone()
    {
        if (IsInteractive())
        {
            DrawChar(1, Screen.Rows-1, ' ', Color.ButtonText, Color.Button);
        }
    }
        
    bool HadError { get { return hadError; } }
    
    <string, string> CurrentToken { get { return currentToken; }}    
    <string, string> PreviousToken { get { return previousToken; }}    

    DumpCurrent()
    {
        OutputDebug("CurrentToken:");
        foreach (var kv in currentToken)
        {
            OutputDebug("  " + kv.key + "->" + kv.value);
        }
    }    
    DumpPrevious()
    {
        OutputDebug("PreviousToken:");
        foreach (var kv in previousToken)
        {
            OutputDebug("  " + kv.key + "->" + kv.value);
        }
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
    ErrorAt(<string, string> token, string message)
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
                errorMessage = errorMessage + " Error at end";
            }
            else if (ttype == HopperToken.Error)
            {
                // nothing
            }
            else if (lexeme.Length > 0)
            {
                errorMessage = errorMessage + " Error at '" + lexeme + "'";
            }
            errorMessage = errorMessage + ": " + message;
            EmitError(errorMessage);
        }
        hadError = true;
    }
    Error(string message)
    {
        ErrorAt(previousToken, message);
    }
    ErrorAtCurrent(string message)
    {
        ErrorAt(currentToken, message);
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
