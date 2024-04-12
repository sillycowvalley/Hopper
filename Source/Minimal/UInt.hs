unit UInt
{
    byte GetByte(uint this, byte index)
    {
        if (index == 1)
        {
            return byte(this >> 8);
        }
        else
        {
            return byte(this & 0xFF);
        }
    }
    uint FromBytes(byte b0, byte b1)
    {
        return b0 + b1 << 8;
    }
    string ToString(uint this)
    {
        string result;
        if (this == 0)
        {
            String.Build(ref result, '0');
        }
        while (this != 0)
        {
            String.BuildFront(ref result, char((this % 10) + 48));
            this = this / 10;
        }
        return result;
    }
}
