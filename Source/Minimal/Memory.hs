unit Memory
{
    byte ReadByte(uint address) system;
    WriteByte(uint address, byte value) system;
#ifdef CPU_Z80    
    uint ReadWord(uint address) system;
    WriteWord(uint address, uint value) system;
#else
    uint ReadWord(uint address)         { return ReadByte(address)  + ReadByte(address+1) << 8; }
    WriteWord(uint address, uint value) { WriteByte(address, byte(value & 0xFF)); WriteByte(address+1, byte(value>> 8)); }    
    uint Allocate(uint size) system;
    Free(uint address) system;
    uint Available() system;
    uint Maximum() system;
#endif
}
