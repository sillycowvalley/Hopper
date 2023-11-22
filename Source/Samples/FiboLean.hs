program FiboUInt
{
#define PORTABLE      // use .hs runtime methods where they exist (rather than 'system')
//#define CHECKED     // mainly stack checks, range checks and division by zero
#define SERIALCONSOLE // Source/System/IO uses serial only (for MCU's etc)

    uses "/Source/Runtime/Emulation/Minimal" // minimal use of actual 'system' APIs
    uses "/Source/Runtime/Emulation/Memory"
    
    uses "/Source/Runtime/Platform/Types"
    uses "/Source/Runtime/Platform/GC"
    
    uses "/Source/Runtime/Platform/Long"
    uses "/Source/Runtime/Platform/External"
    
    // https://forums.raspberrypi.com/viewtopic.php?t=303458
    
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
        
    Benchmark(uint arg, uint loops)
    {
        uint start  = GetMillis(); // start timing
        uint result = 0;
        for(uint count=0; count < loops; count++)
        {
            result = Fibo(arg);
        }
        uint ms = (GetMillis() - start); // stop timing
        WriteLn();
        Write('('); WriteUInt(arg); Write(')');Write(' ');Write('=');Write(' ');
        WriteUInt(result); Write(' ');Write('i');Write('n');Write(' ');
        uint uims = uint(ms / loops);
        WriteUInt(uims);
        Write('m');Write('s');
    }
        
    {
        Memory.Initialize(0x4000, 0x4000);
        
        PinMode(32, 1);
        
        loop
        {
            DigitalWrite(32, 1);
            
            Benchmark(24, 1); // Fibo(24) = 46368
            
            DigitalWrite(32, 0);
            Delay(500);
        }
    }
}
