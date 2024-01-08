unit Char
{
    char ToUpper(char this) system;
    bool IsDigit(char this) system;
    bool IsHexDigit(char this) system;
    bool IsLetterOrDigit(char this) system;
    bool IsLower(char this) system;
    bool IsUpper(char this) system;
    char ToLower(char this) system;
    char ToDigit(byte d) system; // TODO : should be in Byte
    char ToHex(byte h) system;   // TODO : should be in Byte
    
  #ifdef H6502
    string ToString(char this)
    {
        string result;
        String.Build(ref result, this);
        return result;
    }
  #else
    string ToString(char this) system;
  #endif

    bool IsLetter(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 65) && (b <= 90))   // A..Z
            || ((b >= 97) && (b <= 122)); // a..z
    }
    bool IsWhitespace(char this)
    {
        switch(byte(this))
        {
            case 0x09: // tab
            case 0x0A: // line feed
            case 0x0C: // form feed
            case 0x0D: // return
            case 0x20: // space
            {
                return true;
            }
        }
        return false;
    }
}
