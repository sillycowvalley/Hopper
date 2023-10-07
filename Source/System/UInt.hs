unit UInt
{
    string ToString(uint this)
    {
        uint digit;
        char c;
        string result;
        if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            digit = this % 10;
            digit = digit + 48;
            c = char(digit);
            String.BuildFront(ref result, c);
            this = this / 10;
        }
        return result;
    }
    ToString(uint this, ref string result)
    {
        uint digit;
        char c;
        String.Build(ref result);
        if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            digit = this % 10;
            digit = digit + 48;
            c = char(digit);
            String.BuildFront(ref result, c);
            this = this / 10;
        }
    }
#ifndef ZOPPER
    long ToLong(uint this) system;
    string ToHexString(uint this, byte digits)
    {
        uint digit;
        char c;
        uint i;
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
#endif
}
