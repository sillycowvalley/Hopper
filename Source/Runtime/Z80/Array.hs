unit Array
{
    // Array memory map:
    //   0000 heap allocator size
    //   0F   type = tArray
    //   00   GC reference count
    //   0000 number of elements
    //   xx   type of elements
    //   0000 first element in array
    //   ..
    //   <nn>  last element in array
    
    const uint aiCount    = 2;
    const uint aiType     = 4;
    const uint aiElements = 5;
    
    const uint ConstantStart = 0x000A; // location of first byte of constant data in ROM
    
    // This constant always exists for Z80 as first constant: (see Preprocess.hs to modify)
    //const byte[] bitMasks  = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
    //                           0b00010000, 0b00100000, 0b01000000, 0b10000000 };
    
    uint GetCount(uint this)
    {
        return ReadWord(this+aiCount);
    }
    uint GetItem(uint this, uint index)
    {
        uint value;
        uint bit;
        byte mask;
        uint count = ReadWord(this+aiCount);
        if (index >= count)
        {
            Die(0x02); // array index out of range
        }
        
        Type etype = Type(ReadByte(this+aiType));
        if ((etype == Type.UInt) || (etype == Type.Int))
        {
            value = ReadWord(this + aiElements + (index << 1));
        }
        else 
        {
            value = ReadByte(this + aiElements + index);
            if (etype == Type.Bool)
            {
                bit = index & 0x07;
                mask = bitMasks[bit];
                value = (value & mask != 0) ? 1 : 0;
            }
        }
        return value;
    }
    SetItem(uint this, uint index, uint value)
    {
        uint bit;
        byte mask;
        uint count = ReadWord(this+aiCount);
        if (index >= count)
        {
            Die(0x02); // array index out of range
        }
        Type etype = Type(ReadByte(this+aiType));
        if ((etype == Type.UInt) || (etype == Type.Int))
        {
            WriteWord(this + aiElements + (index << 1), value);
        }
        else 
        {
            if (etype == Type.Bool)
            {
                bit = index & 0x07;
                mask = bitMasks[bit];
                if (value == 0)
                {
                    value = ReadByte(this + aiElements + index) & ~mask;
                }
                else
                {
                    value = ReadByte(this + aiElements + index) | mask;
                }
            }
            WriteByte(this + aiElements + index, byte(value & 0xFF));
        }
    }
    uint New(uint count, Type elementType)
    {
        uint size = count;
        if ((elementType == Type.UInt) || (elementType == Type.Int))
        {
            size = size << 2;
        }
        else if (elementType == Type.Bool)
        {
            size = (size >> 3) + ((size & 0x0007 != 0) ? 1 : 0);
        }
        uint this = GC.Create(Type.Array, size+3);
        WriteWord(this+aiType, byte(elementType));
        WriteWord(this+aiCount, count);
        return this;
    }
    uint NewFromConstant(uint location, uint length, Type elementType)
    {
        if ((elementType != Type.Byte) && (elementType != Type.Char))
        {
            Die(0x0A);
        }
        uint this = Array.New(length, elementType);
        uint destination = this + aiElements;
        uint source      = ConstantStart + location;
        uint i;
        for (i=0; i < length; i++)
        {
            WriteByte(destination, ReadByte(source));
            destination++;
            source++;
        }
        return this;
    }
}
