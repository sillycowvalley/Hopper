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
        size += 2;
        this = Memory.Allocate(size+2);
        WriteByte(this+iType,      byte(objectType));
        WriteByte(this+iReference, 0); // it will get its first reference when pushed to the stack via R0/HL
        for (i= iReference+1; i < size; i++)
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
        uint size = ReadWord(source-2);
        uint copy = Memory.Allocate(size+2);
        size-=2;
        for (i = source; i < size; i++)
        {
            WriteByte(copy+i, ReadByte(source+i));
        }
        WriteByte(copy+iReference, 1);
        return copy;
    }
}
