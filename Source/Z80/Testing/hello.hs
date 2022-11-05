program Hello
{
    uses "/Source/Z80/Firmware/LCDDriver" // 20x4 LCD
    uses "/Source/Z80/Firmware/Memory"    // Heap Manager
    uses "/Source/Z80/Firmware/Utilities" 
    {
        LCDInitialize();
        //MemoryInitialize();
        //uint oneThing = MemoryAllocate(10);
        
        LCDClear();
        PrintAt(0, 0, 'H', 'e', 'l', 'l', 'o', char(0), char(0), char(0), char(0), char(0));
                
        //MemoryFree(oneThing);
    }
}
