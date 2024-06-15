program Sieve
{
    //#define CPU_Z80
    //#define ROM_8K
    
    //#define MCU
    //#define EXPERIMENTAL

    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    const uint size = 8191;  // 200 -> 78
    const uint iterations = 1;
    
    bool[size] flagsGlobal;
    
    Hopper()
    {
        IO.WriteLn();
        IO.WriteLn((uint(iterations)).ToString() + " iterations");
        
        uint i; 
        uint k;
        uint prime;
        uint count;
        uint iter = iterations;
        
        uint start = Time.Seconds;
        
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
        
        uint elapsed = Time.Seconds - start;
        
        IO.WriteLn("Done.");
        IO.WriteLn(count.ToString() + " primes");
        IO.WriteLn(elapsed.ToString() + " " + " seconds");
    }
}
