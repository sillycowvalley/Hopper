program Sieve
{
    uses "/Source/System/String"
    uses "/Source/System/UInt"
    uses "/Source/System/Float"
    uses "/Source/System/Long"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Time"
    
    const uint sizepl = 8191;
    bool[sizepl] flagsGlobal;
        
    {
        uint i; 
        uint prime;
        uint k;
        uint count;
        uint iter;

        IO.Clear();
        IO.WriteLn("1 iterations");

        long start  = Millis;
        
        for (iter = 1; iter <= 1; iter ++)
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

        float ms = ((Millis - start) / 100.0); 
        IO.WriteLn(ms.ToString() + " " + " ms");
    }
}
