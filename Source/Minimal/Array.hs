unit Array
{
    uint Count { get system; }
#ifndef CPU_Z80    
    SetItem(V[] this, uint index, V value) system;
    V GetItem(V[] this, uint index) system;
    
    V[] New(uint length, type elementType) system;
    type ItemType { get system; }
    V[] Slice(V[] this, uint start)
    {
        uint length;
        uint sLength = this.Count;
        if (sLength > start)
        {
            length = sLength - start;
        }
        V[] result = Array.New(length, this.ItemType);
        for (uint i = 0; i < length; i++)
        {
            Array.SetItem(result, i, Array.GetItem(this, i+start));
        }
        return result;
    }
    V[] Slice(V[] this, uint start, uint length)
    {
        uint sLength = this.Count;
        if (start + length > sLength)
        {
            length = sLength - start;
        }
        V[] result = Array.New(length, this.ItemType);
        for (uint i = 0; i < length; i++)
        {
            Array.SetItem(result, i, Array.GetItem(this, i+start));
        }
        return result;
    }
    
    
#endif

}
