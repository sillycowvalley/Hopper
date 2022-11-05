unit Char
{
    bool IsUpper(char this)
    {
        byte b = byte(this);
        return ((b >= 65) && (b <= 90));  // A..Z
    }
    bool IsLower(char this)
    {
        byte b = byte(this);
        return ((b >= 97) && (b <= 122)); // a..z
    }
    bool IsLetter(char this)
    {
        byte b = byte(this);
        return ((b >= 65) && (b <= 90))   // A..Z
            || ((b >= 97) && (b <= 122)); // a..z
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
    
    string ToString(char this)
    {
        string result;
        return result.Append(this);
    }
        
    char ToLower(char this) 
    {
        if (IsUpper(this))
        {
            byte b = byte(this) - 65 + 97; // -A + a;
            this = char(b); 
        }
        return this;
    }
    
    char ToUpper(char this) 
    {
        if (IsLower(this))
        {
            byte b = byte(this) - 97 + 65; // -a + A;
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
        byte b = byte(this);
        return ((b >= 48) && (b <= 57)); // 0..9
    }
    bool IsLetterOrDigit(char this)
    {
        byte b = byte(this);
        return ((b >= 48) && (b <= 57)) || // 0..9
               ((b >= 65) && (b <= 90)) || // A..Z
               ((b >= 97) && (b <= 122));  // a..z
    }
    bool IsHexDigit(char this)
    {
        byte b = byte(this);
        return ((b >= 48) && (b <= 57)) || // 0..9
               ((b >= 65) && (b <= 70)) || // A..F
               ((b >= 97) && (b <= 102));  // a..f
    }
}
