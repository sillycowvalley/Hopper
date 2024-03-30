unit Char
{
    bool IsUpper(char this)
    {
        return this >= 'A' && this <= 'Z';
    }

    bool IsLower(char this)
    {
        return this >= 'a' && this <= 'z';
    }

    char ToUpper(char this)
    {
        if (this >= 'a' && this <= 'z')
        {
            return this - ('a' - 'A');
        }
        return this;
    }
    bool IsDigit(char this)
    {
        return this >= '0' && this <= '9';
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
            return this + ('a' - 'A');
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
        return this == ' ' || this == char(0x09) || this == char(0x0A) || this == char(0x0C) || this == char(0x0D);
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
    
}
