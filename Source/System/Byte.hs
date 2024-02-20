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
        int i;
        string result;
        for (i = digits; i > 0; i--)
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
        int i;
        string result;
        for (i = 8; i > 0; i--)
        {
            digit = this % 2;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 2;
        }
        return result;
    }
    
    char ToHex(byte this) system; 
    char ToDigit(byte this) system;
    
}
