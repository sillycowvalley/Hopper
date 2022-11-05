unit Array
{
    uint Count { get system; }
    
    SetItem(V[] this, uint index, V value) system;
    V GetItem(V[] this, uint index) system;
}