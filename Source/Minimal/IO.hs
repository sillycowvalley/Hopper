unit IO
{
    uses "/Source/Minimal/System"
    
    bool echoToLCD;
    bool EchoToLCD     { set { echoToLCD = value; } get { return echoToLCD; } }
    
    Write(char c)
    {
        Serial.WriteChar(c);
#ifdef DISPLAY_DRIVER        
        if (echoToLCD)
        {
            if (Char.EOL == c)
            {
                Screen.PrintLn();
            }
            else if (Char.Formfeed == c)
            {
                Screen.Clear();
            }
            else
            {
                Screen.Print(c);
            }
        }
#endif        
    }
    Write(string s)
    {
        uint length = s.Length;
        for (uint i=0; i < length; i++)
        {
            Write(s[i]);
        }
    }
    WriteLn()
    {
        Write(Char.EOL);
    }
    WriteLn(string s)
    {
        Write(s);
        WriteLn();
    }
    
    bool IsAvailable
    {
        get
        {
            return Serial.IsAvailable;
        }
    }
    
    char Read()
    {
        char ch;
        loop
        {
            ch = Serial.ReadChar();
            if ((ch == Char.Backspace) || (ch == Char.EOL) || (ch == Char.Escape))
            {
                // from above : ok
            }
            else if (ch == Char.Break)
            {
                // <ctrl><C> from Read() ?
            }
            else if ((ch >= ' ') && (ch <= '~')) 
            {
                // ASCII 32 to 126 : ok
            }
            else if (ch == char(0xE0))
            {
                // Maker
            }
            else
            {
                continue;
            }
            break;
        }
        return ch;    
    }
    
    
    bool ReadLn(ref string str)
    {
        char ch;
        bool result;
        String.Build(ref str);
        loop
        {
            loop
            {
                if (Serial.IsAvailable)
                {
                    break;
                }
            }
            ch = Serial.ReadChar();
            if (ch == Char.Break)
            {
                break; // return false
            }
            else if (ch == Char.EOL) 
            { 
                WriteLn();
                result = true; // good
                break; 
            }
            else if (ch == Char.Escape) 
            {
                while (str.Length > 0)
                {
                    Write(Char.Backspace);
                    Write(' ');
                    Write(Char.Backspace);
                    str = str.Substring(0, str.Length-1);   
                }
                continue;
            }
            else if (ch == Char.Backspace)
            {
                if (str.Length > 0)
                {
                    Write(Char.Backspace);
                    Write(' ');
                    Write(Char.Backspace);
                    str = str.Substring(0, str.Length-1);   
                }
                continue;
            }
            String.Build(ref str, ch);
            Write(ch);    
        }
        return result;
    }
}
