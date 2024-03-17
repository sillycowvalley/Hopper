unit Z80
{
    byte[0xFFFA] memory;
    
    uint pc;
    
    uint PC    { get { return pc;          } set { pc = value; } }
    
    byte GetMemory(uint address)
    {
        return memory[address];
    }
    SetMemory(uint address, byte value)
    {
        memory[address] = value;
    }
    Reset()
    {
        Die(0x0A);
    }
    string GetRegisterNames()
    {
        Die(0x0A);
        return ""; 
    }
    string GetRegisters()
    {
        Die(0x0A);
        return ""; 
    }
    ShowStack()
    {
        Die(0x0A);
    }
    Execute()
    {
        Die(0x0A);
    }
}
