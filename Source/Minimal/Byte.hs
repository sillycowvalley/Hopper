unit Byte
{
    string ToString(byte this)
    {
        int value;
        value = int(this);
        return value.ToString();
    }
    string ToHexString(byte this, byte digits)
    {
        int digit;
        char c;
        string result;
        for (int i = digits; i > 0; i--)
        {
            digit = this % 16;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
    string ToBinaryString(byte this)
    {
        int digit;
        char c;
        string result;
        for (int i = 8; i > 0; i--)
        {
            digit = this % 2;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 2;
        }
        return result;
    }
    char ToDigit(byte d)
    {
        d = d + 48; // +0
        return char(d);
    }
    char ToHex(byte this)
    {
        if (this > 0x09)
        {
            // +'A' - 10   = 55
            // + 48 below  = 7
            this += 7;
        }
        this += 48; // +'0'
        return char(this);
    } 
}
