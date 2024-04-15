program Sieve
{
    //#define CPU_Z80
    //#define MCU

    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    const uint size = 200; // 8191;  // 200 -> 78
    const uint iterations = 1;
    
    bool[size] flagsGlobal;
    
    uint Sieve()
    {
        uint i; 
        uint k;
        uint prime;
        uint count;
        uint iter = iterations;
        
        for (iter = 1; iter <= iterations; iter ++)
        {
            count=0 ; 
            
            for (i = 0; i < size; i++)
            {
                flagsGlobal[i] = true;
            }
            for (i = 0; i < size; i++) 
            { 
                if (flagsGlobal[i])
                {
                    prime = i + i + 3; 
                    k = i + prime; 
                    while (k < size) 
                    { 
                        flagsGlobal[k] = false; 
                        k = k + prime; 
                    }
                    count = count + 1;
                }
            }
        }
        return count;
    }
    
    Hopper()
    {
        IO.WriteLn();
        IO.WriteLn((uint(iterations)).ToString() + " iterations");
        
        uint start = Time.Seconds;
        uint result = Sieve();
        uint elapsed = Time.Seconds - start;
        
        IO.WriteLn("Done.");
        IO.WriteLn(result.ToString() + " primes");
        IO.WriteLn(elapsed.ToString() + " " + " seconds");
    }
}
