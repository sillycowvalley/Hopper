unit Array
{
    uint Count { get system; }
#ifndef CPU_Z80    
    
    SetItem(V[] this, uint index, V value) system;
    V GetItem(V[] this, uint index) system;
#endif

#ifdef GENERATING

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
    
    uint GetCount(uint this)
    {
        return ReadWord(this+aiCount);
    }
    
    uint New(uint count, Type elementType)
    {
        uint this;
        uint size = count;
        if ((elementType ==Type.UInt) || (elementType ==Type.Int))
        {
            size = size << 2;
        }
        else if (elementType == Type.Bool)
        {
            size = (size + 7) >> 3;
        }
        this = GC.Create(Type.Array, size+3);
        WriteWord(this+aiType, byte(elementType));
        WriteWord(this+aiCount, count);
        return this;
    }
    uint NewFromConstant(uint location, uint length, Type elementType)
    {
        uint a = 0;
        a = a + 10;
        return 0;
    }
#endif

}
