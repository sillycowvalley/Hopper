program Sieve
{
    uses "/Source/System/String"
    uses "/Source/System/UInt"
    uses "/Source/System/Float"
    uses "/Source/System/Long"
    uses "/Source/System/Screen"
    uses "/Source/System/Time"
    
    const uint size   = 8190;
    const uint sizepl = 8191;
    
    bool[sizepl] flagsGlobal;
        
    {
        uint i; 
        uint prime;
        uint k;
        uint count;
        uint iter;

        Screen.Clear();
        PrintLn("100 iterations");

        long start  = Millis;
        
        for (iter = 1; iter <= 100; iter ++)
        {
            count=0 ; 
            for (i = 0; i <= size; i++)
            {
                flagsGlobal[i] = true;
            }
            for (i = 0; i <= size; i++) 
            { 
                if (flagsGlobal[i])
                {
                    prime = i + i + 3; 
                    k = i + prime; 
                    while (k <= size) 
                    { 
                        flagsGlobal[k] = false; 
                        k = k + prime; 
                    }
                    count = count + 1;
                }
            }
        }
        PrintLn("Done.");
        PrintLn(count.ToString() + " primes");

        float ms = ((Millis - start) / 100.0); 
        PrintLn(ms.ToString() + " " + " ms");
    }
}
