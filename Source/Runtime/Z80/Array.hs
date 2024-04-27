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
    
    const uint ConstantStart = 0x0008; // location of first byte of constant data in ROM
    
    uint GetCount(uint this)
    {
        return ReadWord(this+aiCount);
    }
    uint New(uint count, Type elementType)
    {
        uint size = count;
        if ((elementType == Type.UInt) || (elementType == Type.Int))
        {
            size = size << 1;
        }
        else if (elementType == Type.Bool)
        {
            size = (size >> 3);
            if ((count & 0x0007) != 0)
            {
                size++;
            }
        }
        uint this = GC.Create(Type.Array, size+3);
        WriteWord(this+aiCount, count);
        WriteByte(this+aiType, byte(elementType));
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
        uint constantLocation = ReadWord(ConstantStart) + 2;
        uint source      = constantLocation + location;
        uint i;
        for (i=0; i < length; i++)
        {
            WriteByte(destination, ReadByte(source));
            destination++;
            source++;
        }
        return this;
    }
    uint GetItem(uint this, uint index)
    {
        uint value;
        uint bit;
        byte mask;
        Type etype;
        uint count = ReadWord(this+aiCount);
        if (index >= count)
        {
            Die(0x02); // array index out of range
        }
        etype = Type(ReadByte(this+aiType));
        if ((etype == Type.UInt) || (etype == Type.Int))
        {
            value = ReadWord(this + aiElements + (index << 1));
        }
        else if (etype == Type.Bool)
        {
            bit = (index & 0x07); // 0..7
            mask = (0b00000001 << bit);
            value = ReadByte(this + aiElements + (index >> 3)) & mask;
            if (value != 0)
            {
                value = 1;
            }
        }
        else
        {
            value = ReadByte(this + aiElements + index);
        }
        return value;
    }
    SetItem(uint this, uint index, uint value)
    {
        uint bit;
        byte mask;
        Type etype;
        uint count = ReadWord(this+aiCount);
        if (index >= count)
        {
            Die(0x02); // array index out of range
        }
        etype = Type(ReadByte(this+aiType));
        if ((etype == Type.UInt) || (etype == Type.Int))
        {
            WriteWord(this + aiElements + (index << 1), value);
        }
        else if (etype == Type.Bool)
        {
            bit = (index & 0x07); // 0..7
            index = this + aiElements + (index >> 3);
            mask = (0b00000001 << bit);
            if (value == 0)
            {
                value = ReadByte(index) & ~mask;
            }
            else
            {
                value = ReadByte(index) | mask;
            }
            WriteByte(index, byte(value & 0xFF));   
        }
        else
        {
            WriteByte(this + aiElements + index, byte(value & 0xFF));
        }
    }
}
