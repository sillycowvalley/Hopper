program Newton
{
    uses "/Source/System/System"
    uses "/Source/System/IO"
        
    const long interations = 100000;
    
    {
        IO.Clear();
        IO.WriteLn();
        IO.WriteLn(" Newton's method for approximating pi - floating point benchmark");
        
        long start = Millis;
        
        float x = 1.0;
        float temp;
        float pi=1.0;
        
        for (long i = 2; i < interations; i++) 
        {
           x = x * -1.0;
           pi = pi + x / (2.0 * i - 1.0);
           temp = 40000000.0 * pi;
        }
        pi = pi * 4;
        
        long ms = (Millis - start); 
        IO.WriteLn(" pi = " + pi.ToString());
        IO.WriteLn(" interations = " + (interations).ToString());
        IO.WriteLn(" " + ms.ToString() + " " + " ms");
         
    }
}
