unit GC
{
    // Object memory map:
    //   0000 heap allocator size
    //   00   type
    //   00   GC reference count
    
    const uint iType      = 0;
    const uint iReference = 1;
        
    uint Create(Type objectType, uint size)
    {
        uint this;
        uint i;
        size += 2; // type and ref
        this = Memory.Allocate(size);
        WriteByte(this+iType,      byte(objectType));
        WriteByte(this+iReference, 0); // it will get its first reference when pushed to the stack via R0/HL
        
        // zero initialize:
        for (i = iReference+1; i < size; i++)
        {
            WriteByte(this+i, 0);
        }
        return this;
    }
    Release(uint this)
    {
        byte count = ReadByte(this+iReference);
        count--;
        if (count == 0)
        {
            Memory.Free(this);
        }
        else
        {
            WriteByte(this+iReference, count);
        }
    }
    uint Clone(uint source)
    {
        uint i;
        uint size = ReadWord(source-2) - 2; // Allocate size is -2 from object size
        uint copy = Memory.Allocate(size);
        for (i = 0; i < size; i++)
        {
            WriteByte(copy+i, ReadByte(source+i));
        }
        WriteByte(copy+iReference, 1);
        return copy;
    }
}
