program Sieve
{
//uses "/Source/Library/Boards/PiPico"
    uses "/Source/Library/Boards/PiPico2"
    
    const uint sizepl = 8191;
    bool[sizepl] flagsGlobal;
                
    Hopper()
    {
        //ClockSpeed = RPClockSpeed.Slow133;
        //ClockSpeed = RPClockSpeed.Default; // 150 MHz
        //ClockSpeed = RPClockSpeed.Overclock300;  
        
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
