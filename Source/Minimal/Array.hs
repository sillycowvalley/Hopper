unit Array
{
    uint Count { get system; }
#ifndef CPU_Z80    
    SetItem(V[] this, uint index, V value) system;
    V GetItem(V[] this, uint index) system;
#endif

}
