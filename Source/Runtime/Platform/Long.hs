unit HRLong
{
    
    // Long memory map:
    //   0000 heap allocator size
    //   0F   type = tLong
    //   00   GC reference count
    //   00   LSB
    //   00
    //   00
    //   00   MSB
    
    
    uint New()
    {
        uint address = GC.New(4, Type.Long);
        WriteWord(address+2, 0);
        WriteWord(address+4, 0);
        return address;
    }
    uint NewFromConstant(uint location)
    {
        uint address = GC.New(4, Type.Long);
        WriteWord(address+2, ReadCodeWord(location));
        WriteWord(address+4, ReadCodeWord(location+2));
        return address;
    }
    
    uint FromBytes(byte b0, byte b1, byte b2, byte b3)
    {
        uint address = GC.New(4, Type.Long);
        WriteByte(address+2, b0);
        WriteByte(address+2+1, b1);
        WriteByte(address+2+2, b2);
        WriteByte(address+2+3, b3);
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
        uint address = GC.New(4, Type.Long);
        WriteWord(address+2, ReadWord(original+2));
        WriteWord(address+4, ReadWord(original+4));
        
        return address;    
    }
    uint ToUInt(uint this)
    {
        uint value = ReadWord(this+4);
#ifdef CHECKED
        if (value != 0)
        {
            ErrorDump(62);
            Error = 0x0D; // numeric type out of range / overflow
        }
#endif        
        return ReadWord(this+2);
    }
    uint LongNegate(uint top)
    {
        uint zero = HRUInt.ToLong(0);
        uint result = LongSub(zero, top);
        GC.Release(zero);
        return result;
    }
    uint LongAddB(uint next, uint top)
    {
        uint argument = HRUInt.ToLong(top);
        uint result = LongAdd(next, argument);
        GC.Release(argument);
        return result;
    }
    uint LongSubB(uint next, uint top)
    {
        uint argument = HRUInt.ToLong(top);
        uint result = LongSub(next, argument);
        GC.Release(argument);
        return result;
    }
    
    byte GetByte(uint ichunk, uint i)
    {
        return ReadByte(ichunk+2+i);    
    }
}
