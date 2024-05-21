unit HRInt
{    
    uint ToLong(uint ichunk)
    {
        uint this = HRLong.New();
        WriteWord(this+2, ichunk);
        if ((0x8000 & ichunk) != 0)
        {
            WriteWord(this+4, 0xFFFF);
        }
        else
        {
            WriteWord(this+4, 0);
        }
        return this;
    }
    uint FromBytes(byte b0, byte b1)
    {
        return b0 + (b1 << 8);
    }
    byte GetByte(uint ichunk, uint i)
    {
        return (i == 0) ? byte(ichunk & 0xFF) : byte(ichunk >> 8);
    }
    
}
