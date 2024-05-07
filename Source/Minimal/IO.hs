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
    bool ReadLn(ref string str)
    {
        char ch;
        bool result;
        String.Build(ref str);
        loop
        {
            ch = Serial.ReadChar();
            if (ch == Char.EOL) 
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
