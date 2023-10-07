program Sieve
{
    uses "/Source/6502/System"
    
    const uint size = 8191;
    const uint iterations = 1;
    
    bool[size] flagsGlobal;
        
    {
        uint i; 
        uint k;
        uint prime;
        uint count;
        uint iter = iterations;
        
        // only the LCD, IO.Clear(..) would clear both LCD and Serial (form feed)
        Screen.Clear(); 
        WriteBoth(iter.ToString() + " iterations", true);
        
        long start  = Millis; // start timing
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
                    //WriteLnBoth(prime.ToString(), true);
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
        long ms = ((Millis - start) / iterations); // stop timing
        
        WriteLnBoth(": Done", true);
        WriteBoth(count.ToString() + " primes, ", true);
        WriteLnBoth(ms.ToString() + "ms", true);
    }
}
