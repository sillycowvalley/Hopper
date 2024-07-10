program Ahl
{
    // https://armchairarcade.com/perspectives/2023/07/02/creative-computing-benchmark-from-1984/
    
    // #define FAST_6502_RUNTIME
    
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    Hopper()
    {
        UInt.Seed();
        long start = Millis;
        
        float a = 0;
        float r = 0;
        float s = 0;

        for (uint n = 1; n <= 100; n++)
        {
            a = n;
            for (uint i = 1; i <= 10; i++)
            {
                a = Float.Sqrt(a);
                r = r + Float.Random();
            }
            for (uint i = 1; i <= 10; i++)
            {
                a = a * a;
                r = r + Float.Random();
            }
            s = s + a;
            WriteLn(n.ToString() + ": " + a.ToString());
        }

        float result1 = Float.Abs(1010 - s / 5);
        float result2 = Float.Abs(1000 - r);

        WriteLn();
        WriteLn("ABS(1010 - s / 5): " + result1.ToString());
        WriteLn("ABS(1000 - r): " + result2.ToString());
        
        long elapsed = Millis - start;
        IO.WriteLn(elapsed.ToString() + " ms");       
    }
}
