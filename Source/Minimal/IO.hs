unit IO
{
    uses "/Source/Minimal/System"
    
    Write(char c)
    {
        Serial.WriteChar(c);
    }
    Write(string s)
    {
        foreach (var c in s)
        {
            Serial.WriteChar(c);
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
