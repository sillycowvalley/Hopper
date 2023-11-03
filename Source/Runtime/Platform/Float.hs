unit HRFloat
{
    
    // Float memory map:
    //   0000 heap allocator size
    //   0F   type = tFloat
    //   00   GC reference count
    //   00   LSB
    //   00
    //   00
    //   00   MSB
    
    
    uint New()
    {
        uint address = GC.New(4, Type.Float);
        WriteWord(address+2, 0);
        WriteWord(address+4, 0);
        return address;
    }
    uint NewFromConstant(uint location)
    {
        uint address = GC.New(4, Type.Float);
        WriteWord(address+2, ReadWord(location));
        WriteWord(address+4, ReadWord(location+2));
        return address;
    }
    
    Dump(uint address, uint indent)
    {
        for (uint i = 0; i < indent; i++)
        {
            IO.Write(' ');
        }
        uint lsw = ReadWord(address+2);
        uint msw = ReadWord(address+4);
        IO.WriteHex(msw); IO.WriteHex(lsw); IO.Write(' ');
    }
    uint Clone(uint original)
    {
        uint address = GC.New(4, Type.Float);
        WriteWord(address+2, ReadWord(original+2));
        WriteWord(address+4, ReadWord(original+4));
        
        return address;    
    }
    
    uint ToBytes(uint ichunk)
    {
        uint lst = HRList.New(Type.Byte);
        for (byte i=0; i < 4; i++)
        {
            byte b = ReadByte(ichunk+2+i);
            HRList.Append(lst, b, Type.Byte);
        }
        return lst;
    }
    
    
    
}
