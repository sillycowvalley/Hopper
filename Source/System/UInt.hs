unit UInt
{
    string ToString(uint this)
    {
        string result;
        if (this == 0)
        {
            result = "0";
        }
        while (this != 0)
        {
            uint digit = this % 10;
            digit = digit + 48;
            char c = char(digit);
            result = result.InsertChar(0,c);
            this = this / 10;
        }
        return result;
    }
#ifndef ZOPPER
    long ToLong(uint this) system;
    string ToHexString(uint this, byte digits)
    {
        string result;
        for (uint i = digits; i > 0; i--)
        {
            uint digit = this % 16;
            char c = Char.ToHex(byte(digit));
            result = result.InsertChar(0, c);
            this = this / 16;
        }
        return result;
    }
#endif

}