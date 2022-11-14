unit Int
{
    string ToString(int this)
    {
        string result;
        bool negative = false;
        if (this < 0)
        {
            negative = true;
            this = 0 - this;
        }
        else if (0 == this)
        {
            result = "0";
        }
        while (0 != this)
        {
            int digit = this % 10;
            char c = Char.ToDigit(byte(digit));
            result = result.InsertChar(0,c);
            this = this / 10;
        }
        if (negative)
        {
            result = result.InsertChar(0,'-');
        }
        return result;
    }
#ifndef ZOPPER    
    float ToFloat(int this) system;
    long ToLong(int this) system;
#endif
    string ToHexString(int this, byte digits)
    {
        string result;
        for (int i = digits; i > 0; i--)
        {
            int digit = this % 16;
            char c = Char.ToHex(byte(digit));
            result = result.InsertChar(0, c);
            this = this / 16;
        }
        return result;
    }
    
}
