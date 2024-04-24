unit IO
{
    uses "/Source/Minimal/System"
    
    Write(char c)
    {
        Serial.WriteChar(c);
    }
    Write(string s)
    {
        uint length = s.Length;
        uint i;
        for (i=0; i < length; i++)
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
