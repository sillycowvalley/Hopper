unit Commands
{
    uses "/Source/Basic/Parse"
    uses "/Source/Basic/Errors"
    uses "/Source/Basic/Instructions"
    
    // 12 bytes of globals:
    
    <uint, string>  sourceCode;
    uint lastLine;
    bool exit;
    bool ended = false;
    
    delegate CommandDelegate(string content);
    <string, CommandDelegate> commandDelegates;
    
#ifdef CHECKED
    bool traceOn = false;
#endif        


    uint LastLine { get { return lastLine; } set { lastLine = value; } }
    
    
    bool Ended { get { return ended; } set { ended = value; } }

#ifdef CHECKED    
    bool TraceOn { get { return traceOn; } set { traceOn = value; } }
#endif    
    
    Trace(string text, uint color)
    {
#ifdef H6502
        Write(text);
#else
        Print(text, color, Black);
#endif        
    }
    Trace(char ch, uint color)
    {
#ifdef H6502
        Write(ch);
#else
        Print(ch, color, Black);
#endif        
    }
    TraceLn()
    {
#ifdef H6502
        WriteLn();
#else
        PrintLn();
#endif        
    }
    
    
    Initialize()
    {
        CommandDelegate commandDelegate = Commands.Bye;
        commandDelegates["BYE"] = commandDelegate;
        commandDelegate = Commands.New;
        commandDelegates["NEW"] = commandDelegate;
        commandDelegate = Commands.List;
        commandDelegates["LIST"] = commandDelegate;
        commandDelegate = Commands.Run;
        commandDelegates["RUN"] = commandDelegate;
#ifdef H6502        
        commandDelegate = Commands.Mem;
        commandDelegates["MEM"] = commandDelegate;
#endif
#ifdef CHECKED        
        commandDelegate = Commands.Tron;
        commandDelegates["TRON"] = commandDelegate;
        commandDelegate = Commands.Troff;
        commandDelegates["TROFF"] = commandDelegate;
#endif        
    }
    Clear()
    {
        sourceCode.Clear();
        LastLine = 0;
        Instructions.Reset(true);
    }
    bool LineExists(uint lineNumber)
    {
        return sourceCode.Contains(lineNumber);
    }
    bool ExpectNoArgument(string content)
    {
        Write(content);
        if (content.Length > 0)
        {
            Error(2, content);
            return false;
        }
        return true;
    }
#ifdef CHECKED
    Tron(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        TraceOn = true;
    }
    Troff(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        TraceOn = false;
    }
#endif
#ifdef H6502
    Mem(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        uint available = Memory.Available();
        string availableString;
        UInt.ToString(available, ref availableString);
        Write(availableString);
        WriteLn(" Bytes free");
    }
#endif
    New(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        Commands.Clear();
        Welcome();
    }
    
    List(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        uint lineNumber;
        string lineNumberString;
        while (GetNextLine(ref lineNumber))
        {
            UInt.ToString(lineNumber, ref lineNumberString);
            uint len = lineNumberString.Length;
            uint pad = 4-len;
            while (pad > 0)
            {
                Write(' ');
                pad--;
            }
            Write(lineNumberString);
            Write(' ');
            WriteLn(sourceCode[lineNumber]);
#ifdef CHECKED
#ifndef H6502
            Instructions.DASM(lineNumber);
#endif
#endif            
        }
    }
    Bye(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        Commands.Clear();
        exit = true;
    }
    
    bool GetNextLine(ref uint lineNumber)
    {
        bool result = false;
        uint lastLine = LastLine;
        loop
        {
            lineNumber++;
            if (sourceCode.Contains(lineNumber))
            {
                string currentLine = sourceCode[lineNumber];
                if (currentLine.Length != 0) // ignore empty lines
                {
                    result = true;
                    break;
                }
            }
            if (lineNumber > lastLine)
            {
                break;
            }
        }
        return result;
    }
    
    Run(string content)
    {
        if (!ExpectNoArgument(content))
        {
            return;
        }
        Commands.Ended = false;
        
        Instructions.Reset(false);
        Instructions.Run(ref sourceCode);
    }
    
    bool Execute(string currentLine)
    {
        exit = false;
        loop
        {
            uint iSpace;
            string command;
            string content;
            uint clength = currentLine.Length;
            if (currentLine.IndexOf(' ', ref iSpace))
            {
                for (uint i=0; i < iSpace; i++)
                {
                    char ch = currentLine[i];
                    ch = ch.ToUpper();
                    String.Build(ref command, ch);
                }
                for (uint i=iSpace+1; i < clength; i++)
                {
                    char ch = currentLine[i];    
                    String.Build(ref content, ch);
                }
            }
            else
            {
                for (uint i=0; i < clength; i++)
                {
                    char ch = currentLine[i];
                    ch = ch.ToUpper();
                    String.Build(ref command, ch);
                }
            }
        
            uint lineNumber;
            WasError = false;
            if (TryParseLineNumber(command, ref lineNumber))
            {
                Instructions.Reset(true);
                sourceCode[lineNumber] = content;
                if (lineNumber > LastLine)
                {
                    LastLine = lineNumber;
                }
                break;
            }
            
            if (commandDelegates.Contains(command))
            {
                CommandDelegate cmd = commandDelegates[command];
                cmd(content);
                break;
            }
            if (!WasError)
            {
                Error(1, command); // unknown command
            }
            break;
        } // loop
        return exit;
    }
}
