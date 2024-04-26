program Fibo
{
    #define CPU_Z80
    //#define MCU

    uses "/Source/Minimal/IO"
      
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
    Benchmark(string name, uint arg, uint loops)
    {
        //uint start = Time.Seconds;
        uint result = 0;
        for(uint count=0; count < loops; count++)
        {
            result = Fibo(arg);
        }
        //uint elapsed = Time.Seconds - start;
        WriteLn();
        WriteLn(name + " (" + arg.ToString() + ") = " + result.ToString() /*+ " in " + elapsed.ToString() + " seconds"*/);
    }
    Hopper()
    {
        // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
        Benchmark("Fibo", 10, 1); // Fibo(24) = 46368
    }
}
