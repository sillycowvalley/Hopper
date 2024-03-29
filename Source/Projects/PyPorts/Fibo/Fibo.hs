program Fibo
{
#define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    delegate long FiboDelegate(long n);
    
    long Fibo(long n)
    {
        if ( n <= 1 ) 
        { return n; }
        else          
        { return Fibo(n-1) + Fibo(n-2); }
    }
        
    Benchmark(string name, FiboDelegate func, long arg, uint loops)
    {
        long start  = Millis; // start timing
        long result = 0;
        for(uint count=0; count < loops; count++)
        {
            result = func(arg);
        }
        long ms = (Millis - start); // stop timing
        WriteLn(name + "(" + arg.ToString() + ") = " + result.ToString() + " in " + (ms / loops).ToString() + "ms");
    }
        
    {
        FiboDelegate fiboDelegate = Fibo;
        Benchmark("Fibo", fiboDelegate, long(24), 5); // Fibo(24) = 46368
    }
}
