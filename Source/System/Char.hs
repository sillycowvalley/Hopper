unit Char
{

    const char EOL        = char(0x0A);
    const char Escape     = char(0x1B);
    const char Slash      = char(0x5C);
    const char Formfeed   = char(0x0C);
    const char Backspace  = char(0x08);
    const char Break      = char(0x03);
    const char Tab        = char(0x09);

    char ToUpper(char this) system;
    bool IsDigit(char this) system;
    bool IsHexDigit(char this) system;
    bool IsLetterOrDigit(char this) system;
    bool IsLower(char this) system;
    bool IsUpper(char this) system;
    char ToLower(char this) system;
    
    string ToString(char this) system;

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
