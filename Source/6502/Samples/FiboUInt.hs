program FiboUInt
{
    uses "/Source/6502/System"
    uses "/Source/System/IO"
    
    // https://forums.raspberrypi.com/viewtopic.php?t=303458
    
    delegate uint FiboDelegate(uint n);
    
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
        
    Benchmark(string name, FiboDelegate func, uint arg, uint loops)
    {
        long start  = Millis; // start timing
        uint result = 0;
        for(uint count=0; count < loops; count++)
        {
            result = func(arg);
        }
        long ms = (Millis - start); // stop timing
        WriteLn(name + "(" + arg.ToString() + ") = " + result.ToString() + " in " + (ms / loops).ToString() + "ms");
    }
        
    {
        FiboDelegate fiboDelegate = Fibo;
        Benchmark("Fibo", fiboDelegate, 24, 1); // Fibo(24) = 46368
    }
}
