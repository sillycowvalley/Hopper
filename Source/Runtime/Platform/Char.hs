unit HRChar
{
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
    byte FromHex(char this)
    {
        byte hex = byte(this);
        if (hex >= 97) // 'a'
        {
            hex -= 87; // 97 - 87 = 10 ('a' - 10)
        }
        else if (hex >= 65) // 'A'
        {
            hex -= 55; // 65 - 55 = 10 ('A' - 10)
        }
        else
        {
            hex -= 48; // '0'
        }
        return hex;
    }
}
