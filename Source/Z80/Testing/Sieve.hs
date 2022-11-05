program Sieve
{
    uses "/Source/Z80/Firmware/LCDDriver"
    uses "/Source/Z80/Firmware/Utilities"
    uses "/Source/Z80/Firmware/Memory"
    
    const uint size = 8190;
    const uint sizepl = 8191;
    
    {
        LCDInitialize();
        MemoryInitialize();
        uint flagsGlobal = MemoryAllocate(sizepl);
        
        uint i; 
        uint prime;
        uint k;
        uint count; 
        uint iter;

        LCDClear();
        PrintAt(0, 0, 'S', 't', 'a', 'r', 't', char(0), char(0), char(0), char(0), char(0));

        iter = 1;
        loop
        {
            if (iter > 10)
            {
                break;
            }
            count=0 ; 
            i = 0;
            loop
            {
                if (i > size)
                {
                    break;
                }
                memory[flagsGlobal + i] = 1;
                i++;
            }
            i = 0;
            loop
            { 
                if (i > size)
                {
                    break;
                }
                if (1 == memory[flagsGlobal + i])
                {
                    prime = i + i + 3; 
                    k = i + prime; 
                    loop
                    { 
                        if (k > size)
                        {
                            break;
                        }
                        memory[flagsGlobal+k] = 0; 
                        k = k + prime; 
                    }
                    count++;
                }
                i++;
            }
            iter++;
        }
        PrintAt(0, 1, 'P', 'r', 'i', 'm', 'e', 's', ':', char(0), char(0), char(0));
        PrintHexAt(10,1, count);
        
        MemoryFree(flagsGlobal);
    }
}
