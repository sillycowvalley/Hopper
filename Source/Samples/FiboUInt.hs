program FiboUInt
{
    //#define NO_PACKED_INSTRUCTIONS
    
    uses "/Source/Library/Boards/PiPico2W"
    
#if !defined(MCU)
    uses "/Source/System/Screen"
#endif    
    
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
        float seconds = ms / 1000.0;
        WriteLn(name + "(" + arg.ToString() + ") = " + result.ToString() + " in " + (seconds / loops).ToString() + " seconds");
    }
     
    Hopper()   
    {
        //ClockSpeed = RPClockSpeed.Overclock300;  
                
        FiboDelegate fiboDelegate = Fibo;
        Benchmark("Fibo", fiboDelegate, 24, 1); // Fibo(24) = 46368
    }
}
