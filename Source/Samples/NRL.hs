program NRL
{
#define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    {
        uint i;
        uint j;
        long s;
        
        long start  = Millis; // start timing

        for (i=1; i <= 10; i++)
        {
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
            Write('.');
        }
        WriteLn(s.ToString()); // should be '500500'
        long ms = (Millis - start); // stop timing
        WriteLn(ms.ToString() + "ms");
    }
}
