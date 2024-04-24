unit Char
{
    const char EOL        = char(0x0A);
    const char Escape     = char(0x1B);
    const char Slash      = char(0x5C);
    const char Formfeed   = char(0x0C);
    const char Backspace  = char(0x08);
    const char Break      = char(0x03);
    const char Tab        = char(0x09);
    
    bool IsUpper(char this)
    {
        return (this >= 'A') && (this <= 'Z');
    }

    bool IsLower(char this)
    {
        return (this >= 'a') && (this <= 'z');
    }

    char ToUpper(char this)
    {
        if (this >= 'a' && this <= 'z')
        {
            this = (char(byte(this) - (byte('a') - byte('A'))));
        }
        return this;
    }
    bool IsDigit(char this)
    {
        return (this >= '0') && (this <= '9');
    }
    bool IsLetterOrDigit(char this)
    {
        return (this >= '0' && this <= '9') ||
               (this >= 'A' && this <= 'Z') ||
               (this >= 'a' && this <= 'z');
    }

    char ToLower(char this) 
    {
        if ((this >= 'A') && (this <= 'Z'))
        {
            this = (char(byte(this) + (byte('a') - byte('A'))));
        }
        return this;
    }

    bool IsHexDigit(char this)
    {
        return ((this >= '0' && this <= '9') || 
                (this >= 'A' && this <= 'F') || 
                (this >= 'a' && this <= 'f'));
    }
    
    bool IsLetter(char this)
    {
        return (this >= 'A' && this <= 'Z') || (this >= 'a' && this <= 'z');
    }
    bool IsWhitespace(char this)
    {
        return (this == ' ') || (this == Char.Tab) || (this == Char.EOL) || (this == Char.Formfeed) || (this == char(0x0D));
    }     
    byte FromHex(char this)
    {
        byte hex = byte(this);
        if (hex > 96) // 'a'
        {
            hex -= 87;
        }
        else if (hex > 64) // 'A'
        {
            hex -= 55;
        }
        else
        {
            hex -= 48; // '0'
        }
    }
    string ToString(char this)
    {
        string result;
        String.Build(ref result, this);
        return result;
    }
}
