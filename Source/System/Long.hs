unit Long
{
    bool TryParse(string content, ref long returnValue)
    {
        long result;
        bool makeNegative;
        if (content.Length < 1)
        {
            return false;
        }
        if (content.StartsWith("0x"))
        {
            return tryParseHex(content, ref returnValue);
        }
        if (content.StartsWith('+'))
        {
            String.Substring(ref content, 1);
        }
        else if (content.StartsWith('-'))
        {
            String.Substring(ref content, 1);
            makeNegative = true;
        }
        foreach (var c in content)
        {
            result = result * 10;
            if (!c.IsDigit())
            {
                return false;
            }
            result = result + (byte(c) - 48); // 48 is ASCII for '0'
        }
        if (makeNegative)
        {
            result = -result;
        }
        returnValue = result;
        return true;
    }
    bool tryParseHex(string content, ref long returnValue)
    {
        bool success;
        uint length;
        uint i;
        char c;
        loop
        {
            returnValue = 0;
            if (!content.StartsWith("0x"))
            {
                break;
            }
            length = content.Length;
            if (length < 3)
            {
                break;
            }
            success = true;
            for (i=0; i < length-2; i++)
            {
                returnValue = returnValue * 16;
                c = content.GetChar(i+2);
                if (c.IsDigit())
                {
                    returnValue = returnValue + (byte(c) - 48); // 48 is ASCII for '0'
                }
                else if (c.IsHexDigit())
                {
                    returnValue = returnValue + (byte(c.ToLower()) - 87); // 97 is ASCII for 'a', -97+10 = -87
                }
                else
                {
                    success = false;
                    break;
                }
            }
            break;
        }
        return success;
    }
    
#ifndef HOPPER_6502  
    string ToString(long this) system;
#else    
    string ToString(long this)
    {
        bool negative;
        uint udigit;
        char c;
        long digit;
        string result;
        if (this < 0)
        {
            negative = true;
            this = 0 - this;
        }
        else if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            digit = this % 10;
            udigit = uint(digit);
            c = Byte.ToDigit(byte(udigit));
            String.BuildFront(ref result, c);
            this = this / 10;
        }
        if (negative)
        {
            String.BuildFront(ref result, '-');
        }
        return result;
    }
#endif    

    byte GetByte(long this, byte index) system;
    long FromBytes(byte b0, byte b1, byte b2, byte b3) system;
    
#ifndef HOPPER_6502    
    float ToFloat(long this) system;
#endif    

    string ToHexString(long this, byte digits)
    {
        int idigit;
        char c;
        int i;
        long digit;
        string result;
        for (i = digits; i > 0; i--)
        {
            digit = this % 16;
            idigit = int(digit);
            c = Byte.ToHex(byte(idigit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
    string ToBinaryString(long this, byte digits)
    {
        char c;
        int i;
        string result;
        long digit;
        for ( i = digits; i > 0; i--)
        {
            digit = this % 2;
            c = '0';
            if (1 == digit)
            {
                c = '1';
            }
            String.BuildFront(ref result, c);
            this = this / 2;
        }
        return result;
    }
    
    long Abs(long value) { return (value >= 0) ? value : -value; }
    long Min(long a, long b) { return (a < b) ? a : b; }
    long Max(long a, long b) { return (a > b) ? a : b; }
}
