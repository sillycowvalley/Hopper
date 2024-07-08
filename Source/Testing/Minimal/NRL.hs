program NRL
{
//#define CPU_Z80
    /#define FAST_6502_RUNTIME

    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    NRL()
    {
        uint i;
        uint j;
        uint s;
        
        IO.WriteLn("NRL:");
        
        //uint start = Seconds;
        long start = Millis;
        
        for (i=1; i <= 10; i++)
        {
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
            IO.Write('.');
        }
        IO.WriteLn();
        IO.WriteLn(s.ToString());
        
        //uint elapsed = (Seconds - start); 
        long elapsed = (Millis - start);
        IO.WriteLn(elapsed.ToString() + " ms");
    }
    NRL32()
    {
        uint i;
        uint j;
        long s;
        
        IO.WriteLn("NRL32:");
        
        //uint start = Seconds;
        long start = Millis;
        
        for (i=1; i <= 10; i++)
        {
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
            IO.Write('.');
        }
        IO.WriteLn();
        IO.WriteLn(s.ToString());
        
        //uint elapsed = (Seconds - start); 
        long elapsed = (Millis - start);
        IO.WriteLn(elapsed.ToString() + " ms");
    }
    Hopper()
    {
        NRL();
        NRL32();
    }
}
