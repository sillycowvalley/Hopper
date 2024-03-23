unit UInt
{
    byte GetByte(uint this, byte index)
    {
        switch (index)
        {
            case 0:
            {
                return byte(this & 0xFF);
            }
            case 1:
            {
                return byte(this >> 8);
            }
            default:
            {
                Diagnostics.Die(0x02); // array index out of range
            }
        }
        return 0;
    }
    uint FromBytes(byte b0, byte b1)
    {
        return b0 + b1 << 8;
    }
}
