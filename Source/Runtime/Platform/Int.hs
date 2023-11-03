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
    uint ToBytes(uint ichunk)
    {
        byte lsb = byte(ichunk & 0xFF);
        byte msb = byte(ichunk >> 8);
        
        uint lst = HRList.New(Type.Byte);
        HRList.Append(lst, lsb, Type.Byte);
        HRList.Append(lst, msb, Type.Byte);
        return lst;
    }
    
}
