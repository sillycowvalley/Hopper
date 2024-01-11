program TinyHopper
{
    #define TINY_HOPPER
    
    uses "/Source/6502/System"
    
    Function()
    {
        uint address = Allocate(10);
        uint a;
        
        WriteByte(address,   0x1A);     // PUSHIB
        WriteByte(address+1, 0x0A);     // 10
        WriteByte(address+2, 0x1B);     // POPLOCAL
        WriteByte(address+3, byte(&a)); // offset of 'a'
        WriteByte(address+2, 0x63);     // EXIT
        uint result = Inline(address);
        
        if (a == 10)
        {
            Print('!');
        }
        else
        {
            Print('x');
        }
        
    }
    {
        Function();
    }
}
