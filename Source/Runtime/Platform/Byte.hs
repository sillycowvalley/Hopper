unit HRByte
{
    char ToHex(byte h)
    {
        if (h < 10)
        {
            h = h + 48; // +0
        }
        else
        {
            h = h + 55; // +A - 10
        }
        return char(h);
    }
    char ToDigit(byte d)
    {
        d = d + 48; // +0
        return char(d);
    }
}