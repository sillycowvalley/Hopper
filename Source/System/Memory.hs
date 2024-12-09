unit Memory
{
    byte ReadByte(uint address) system;
    WriteByte(uint address, byte value) system;
    
    uint Allocate(uint size) system;
    Free(uint address) system;
    uint Available() system;
    uint Maximum() system;
}
