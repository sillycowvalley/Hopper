unit IO
{
    uses "/Source/Minimal/System"
    
    bool echoToLCD;
    bool EchoToLCD     { set { echoToLCD = value; } get { return echoToLCD; } }
    
    Write(char c)
    {
        Serial.WriteChar(c);
    }
    Write(string s)
    {
        uint length = s.Length;
        for (uint i=0; i < length; i++)
        {
            Serial.WriteChar(s[i]);
        }
    }
    WriteLn()
    {
        Serial.WriteChar(Char.EOL);
    }
    WriteLn(string s)
    {
        Write(s);
        WriteLn();
    }
}
