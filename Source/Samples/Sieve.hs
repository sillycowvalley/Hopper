program Sieve
{
//#define TINYHOPPER    
#define PORTABLE
#define SERIALCONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    const uint sizepl = 8191;
    bool[sizepl] flagsGlobal;
        
    {
        uint i; 
        uint prime;
        uint k;
        uint count;
        uint iter;

        IO.Clear();
        IO.WriteLn("10 iterations");

        long start  = Millis;
        
        for (iter = 1; iter <= 10; iter ++)
        {
            count=0 ; 
            for (i = 0; i < sizepl; i++)
            {
                flagsGlobal[i] = true;
            }
            for (i = 0; i < sizepl; i++) 
            { 
                if (flagsGlobal[i])
                {
                    prime = i + i + 3; 
                    k = i + prime; 
                    while (k < sizepl) 
                    { 
                        flagsGlobal[k] = false; 
                        k = k + prime; 
                    }
                    count = count + 1;
                }
            }
        }
        IO.WriteLn("Done.");
        IO.WriteLn(count.ToString() + " primes");

        float ms = ((Millis - start) / 10.0); 
        IO.WriteLn(ms.ToString() + " " + " ms");
    }
}
