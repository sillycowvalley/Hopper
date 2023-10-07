unit Long
{
#ifndef H6502    
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
            c = Char.ToDigit(byte(udigit));
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
    
    <byte> ToBytes(long this) system;
#ifndef H6502    
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
            c = Char.ToHex(byte(idigit));
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
}
