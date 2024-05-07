unit Array
{
    uint Count { get system; }
    
    SetItem(V[] this, uint index, V value) system;
    V GetItem(V[] this, uint index) system;
    V[] Slice(V[] this, uint start) system;
    V[] Slice(V[] this, uint start, uint length) system;
    type ItemType { get system; }
}
