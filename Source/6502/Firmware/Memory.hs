unit Memory
{
    byte ReadByte(uint address) system;
    WriteByte(uint address, byte value) system;

    uint Available() system;
    uint Maximum() system;
    
    Free(uint address) system;
    uint Allocate(uint size) system;
    
    byte ReadBit(uint address, uint index) system;
    WriteBit(uint address, uint index, byte value) system;
}

