program TestFloat
{
    //#define FAST_6502_RUNTIME
    
    uses "/Source/Library/Boards/Hopper6502"
    
    Hopper()
    {
        long start = Millis;
        for (float i=1; i <= 10; i += 0.25)
        {
            float f = i;
            float g = f * i;
            float h = g / i;
            
            float j = f * f;
            LED = true;
            float k = Float.Sqrt(j);
            LED = false;
            
            IO.WriteLn();
            IO.Write(i.ToString() + ": " + f.ToString() + " " + g.ToString() + " " + h.ToString() + " " + j.ToString() + " " + k.ToString());
        }
        long elapsed = Millis - start;
        WriteLn();
        WriteLn(elapsed.ToString() + " ms");
    }
}
