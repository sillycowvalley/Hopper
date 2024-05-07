unit HRArray
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
    
    uint setSlots;
    uint clearSlots;
    
    Initialize()
    {
        setSlots = Memory.Allocate(8);
        clearSlots = Memory.Allocate(8);
        
        Memory.WriteByte(setSlots + 0, 0x01);
        Memory.WriteByte(setSlots + 1, 0x02);
        Memory.WriteByte(setSlots + 2, 0x04);
        Memory.WriteByte(setSlots + 3, 0x08);
        Memory.WriteByte(setSlots + 4, 0x10);
        Memory.WriteByte(setSlots + 5, 0x20);
        Memory.WriteByte(setSlots + 6, 0x40);
        Memory.WriteByte(setSlots + 7, 0x80);
        Memory.WriteByte(clearSlots + 0, 0xFE);
        Memory.WriteByte(clearSlots + 1, 0xFD);
        Memory.WriteByte(clearSlots + 2, 0xFB);
        Memory.WriteByte(clearSlots + 3, 0xF7);
        Memory.WriteByte(clearSlots + 4, 0xEF);
        Memory.WriteByte(clearSlots + 5, 0xDF);
        Memory.WriteByte(clearSlots + 6, 0xBF);
        Memory.WriteByte(clearSlots + 7, 0x7F);
    }
    Release()
    {
        if (setSlots != 0)
        {
            Memory.Free(setSlots);
        }
        if (clearSlots != 0)
        {
            Memory.Free(clearSlots);
        }
    }
    uint New(Type htype, uint count)
    {
        uint elementbytes;
        switch (htype)
        {
            case Type.Bool:
            {
                elementbytes = (count+7) >> 3;
            }
            case Type.Char:
            case Type.Byte:
            {
                elementbytes = count;
            }
            default:
            {
                elementbytes = count *2;
            }
        }
        
        uint this = GC.New(3 + elementbytes, Type.Array);
        WriteWord(this+aiCount, count);
        WriteByte(this+aiType, byte(htype));
        
        // zero initialize
        uint address = this + aiElements;
        for (uint i = 0; i < elementbytes; i++)
        {
            WriteByte(address, 0);
            address++;
        }
        return this;
    }
    uint Clone(uint original)
    {
        uint count = ReadWord(original+aiCount);
        Type etype = Type(ReadWord(original+aiType));
        uint address = HRArray.New(etype, count);
        uint elementbytes;
        switch (etype)
        {
            case Type.Bool:
            {
                elementbytes = (count+7) >> 3;
            }
            case Type.Char:
            case Type.Byte:
            {
                elementbytes = count;
            }
            default:
            {
                elementbytes = count *2;
            }
        }
        for (uint i = 0; i < elementbytes; i++)
        {
            WriteByte(address+aiElements+i, ReadByte(original+aiElements+i));
        }
        return address;    
    }
    
    uint NewFromConstant(uint location, Type htype, uint length)
    {
        uint this = HRArray.New(htype, length);
        for (uint i = 0; i < length; i++)
        {
            WriteByte(this + aiElements + i, ReadCodeByte(location+i));
        }
        return this;
    }
    
    
    
    
    
    Type GetValueType(uint this)
    {
        return Type(ReadByte(this+aiType));
    }
    Dump(uint address, uint indent)
    {
        for (uint i = 0; i < indent; i++)
        {
            IO.Write(' ');
        }
        
        uint elements = ReadWord(address+aiCount);
        byte etype    = ReadByte(address+aiType);
        IO.WriteHex(elements); IO.Write(' '); IO.WriteHex(etype);
    }
    
    uint GetCount(uint this)
    {
        return ReadWord(this+aiCount);
    }
    uint GetItemType(uint this)
    {
        return ReadByte(this+aiType);
    }
    
    uint GetItem(uint this, uint index, ref Type etype)
    {
        uint elements = ReadWord(this+aiCount);
        etype    = Type(ReadByte(this+aiType));
        if (index >= elements)
        {
            Error = 0x02; // array index out of range
            return 0;
        }
        uint address = this + aiElements;
        uint value;
        switch (etype)
        {
            case Type.Bool:
            {
                uint offset = address + (index >> 3);
                byte slotIndex = byte(index & 0x07);
                byte b = ReadByte(offset);
                byte mask = Memory.ReadByte(setSlots + slotIndex);
                value = b & mask;
                if (value != 0)
                {
                    value = 1;
                }
            }
            case Type.Char:
            case Type.Byte:
            {
                value = ReadByte(address + index);
            }
            default:
            {
                uint offset = address + (index << 1);
                byte msb = ReadByte(offset);
                byte lsb = ReadByte(offset+1);
                value = (msb << 8) | lsb;
            }
        }
        return value;     
    }
    
    SetItem(uint this, uint index, uint value)
    {
        uint elements = ReadWord(this+aiCount);
        Type etype    = Type(ReadByte(this+aiType));
        if (index >= elements)
        { 
            Error  = 0x02; // array index out of range
            return;
        }
        uint address = this + aiElements;
        switch (etype)
        {
            case Type.Bool:
            {
                uint offset = address + (index >> 3);
                byte slotIndex = byte(index & 0x07);
                byte b = ReadByte(offset);
                if (value == 0)
                {
                    // clear the bit
                    byte mask = Memory.ReadByte(clearSlots+slotIndex);
                    b = b & mask;
                }
                else
                {
                    // set the bit
                    byte mask = Memory.ReadByte(setSlots+slotIndex);
                    b = b | mask;
                }
                WriteByte(offset, b);
            }
            case Type.Char:
            case Type.Byte:
            {
                WriteByte(address + index,   byte(value & 0xFF));
            }
            default:
            {
                uint offset = address + (index << 1);
                WriteByte(offset,   byte(value >> 8));
                WriteByte(offset+1, byte(value & 0xFF));
            }
        }
    }
    
}
