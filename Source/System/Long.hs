unit Long
{
    string ToString(long this) system;
    <byte> ToBytes(long this) system;
#ifndef TINYHOPPER    
    float ToFloat(long this) system;
#endif    

    string ToHexString(long this, byte digits)
    {
        string result;
        for (int i = digits; i > 0; i--)
        {
            long digit = this % 16;
            int idigit = int(digit);
            char c = Char.ToHex(byte(idigit));
            result = result.InsertChar(0, c);
            this = this / 16;
        }
        return result;
    }
    string ToBinaryString(long this, byte digits)
    {
        string result;
        for (int i = digits; i > 0; i--)
        {
            long digit = this % 2;
            char c = '0';
            if (1 == digit)
            {
                c = '1';
            }
            result = result.InsertChar(0, c);
            this = this / 2;
        }
        return result;
    }
}
