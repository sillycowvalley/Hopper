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
}
