unit Int
{
    string ToString(int this)
    {
        int digit;
        char c;
        bool negative;
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
            c = Char.ToDigit(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 10;
        }
        if (negative)
        {
            String.BuildFront(ref result, '-');
        }
        return result;
    }
#ifndef ZOPPER    
    float ToFloat(int this) system;
    long ToLong(int this) system;
#endif
    string ToHexString(int this, byte digits)
    {
        int digit;
        char c;
        int i;
        string result;
        for (i = digits; i > 0; i--)
        {
            digit = this % 16;
            c = Char.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
}
