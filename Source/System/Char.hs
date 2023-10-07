unit Char
{
#ifdef H6502
    bool IsUpper(char this) system;
    bool IsLower(char this) system;
    char ToUpper(char this) system;
    bool IsDigit(char this) system;
    bool IsLetterOrDigit(char this) system;
    char ToDigit(byte d) system;
    char ToHex(byte h) system;
    char ToLower(char this) system;
    bool IsHexDigit(char this) system;
#else    
    bool IsUpper(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 65) && (b <= 90));  // A..Z
    }
    bool IsLower(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 97) && (b <= 122)); // a..z
    }
    char ToUpper(char this) 
    {
        byte b;
        if (IsLower(this))
        {
            b = byte(this) - 97 + 65; // -a + A;
            this = char(b);
        }
        return this;
    }
    char ToDigit(byte d)
    {
        d = d + 48; // +0
        return char(d);
    }
    bool IsDigit(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 48) && (b <= 57)); // 0..9
    }
    bool IsLetterOrDigit(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 48) && (b <= 57)) || // 0..9
               ((b >= 65) && (b <= 90)) || // A..Z
               ((b >= 97) && (b <= 122));  // a..z
    }
    char ToHex(byte h)
    {
        if (h < 10)
        {
            h = h + 48; // +0
        }
        else
        {
            h = h + 55; // +A - 10
        }
        return char(h);
    }
    char ToLower(char this) 
    {
        byte b;
        if (IsUpper(this))
        {
            b = byte(this) - 65 + 97; // -A + a;
            this = char(b); 
        }
        return this;
    }
    bool IsHexDigit(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 48) && (b <= 57)) || // 0..9
               ((b >= 65) && (b <= 70)) || // A..F
               ((b >= 97) && (b <= 102));  // a..f
    }
    
#endif
    
    bool IsLetter(char this)
    {
        byte b;
        b = byte(this);
        return ((b >= 65) && (b <= 90))   // A..Z
            || ((b >= 97) && (b <= 122)); // a..z
    }
    
    
    
    string ToString(char this)
    {
        string result;
        return result.Append(this);
    }
        
    
    
    
}
