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
}
