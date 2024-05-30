program Fibo
{
    //#define MCU

    uses "/Source/Minimal/IO"
      
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
        uint start = Time.Seconds;
        uint result = 0;
        for(uint count=0; count < loops; count++)
        {
            result = func(arg);
        }
        uint elapsed = Time.Seconds - start;
        WriteLn();
        WriteLn(name + " (" + arg.ToString() + ") = " + result.ToString() + " in " + elapsed.ToString() + " seconds");
    }
    Hopper()
    {
        FiboDelegate fiboDelegate = Fibo;
        // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
        Benchmark("Fibo", fiboDelegate, 24, 1); // Fibo(24) = 46368
    }
}
