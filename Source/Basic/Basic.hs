program Basic
{
    
    #define CHECKED
    #define TERSE // errors are numbers only, no text messages
    
    uses "/Source/6502/System"
    //uses "/Source/System/System"
    
    // IO:
    //   - write either to Screen or Screen and Serial,
    //   - read from Keyboard or Keyboard and Serial
    uses "/Source/System/IO" 

    uses "/Source/Basic/Errors"
    uses "/Source/Basic/Commands"
    uses "/Source/Basic/Instructions"
    
    Welcome()
    {
        IO.Clear();
        WriteLn("Tigger BASIC 1.0");
        Commands.Initialize();
        Instructions.Initialize();
#ifdef H6502
        Commands.Mem("");
#endif
        WriteLn("Ok");
    }
    
    GetInputLine(ref string currentLine)
    {
        String.Build(ref currentLine);
        uint clength = 0;
        bool refresh = true;
        Write('>');
        loop
        {
            char ch = Read();
            if (ch == char(0x0D))
            {
                break;
            }
            else if (ch == char(0x1B))
            {
                // clear the current line
                foreach (var c in currentLine)
                {
                    // backspace
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                }
                String.Build(ref currentLine);
                clength = 0;
            }
            else if (ch == char(0x08))
            {
                if (clength != 0)
                {
                    currentLine = currentLine.Substring(0, clength-1);
                    clength--;
                    // backspace
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                }
            }
            else // alphanumeric
            {
                if (clength < IO.LineMax)
                {
                    String.Build(ref currentLine, ch);
                    clength++;
                    Write(ch);
                }
            } // alphanumeric
            
        } // loop
        WriteLn();
        if (clength != 0)
        {
            if ((currentLine[0] == ' ') || (currentLine[clength-1] == ' '))
            {
                currentLine = currentLine.Trim();
            }
        }
    }
    
    {
        Welcome();
        string inputLine;
        loop
        {
            GetInputLine(ref inputLine);
            if (inputLine.Length > 0)
            {
                if (Commands.Execute(inputLine))
                {
                    break;
                }
            }
        }
    }
}
