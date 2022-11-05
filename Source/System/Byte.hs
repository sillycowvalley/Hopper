unit Byte
{
    string ToString(byte this)
    {
        int value = int(this);
        return value.ToString();
    }
    string ToHexString(byte this, byte digits)
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