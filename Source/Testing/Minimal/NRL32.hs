program NRL
{
//#define CPU_Z80
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    {
        uint i;
        uint j;
        long s;
        
        IO.WriteLn();
        
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
        IO.WriteLn();
        IO.WriteLn(elapsed.ToString() + " " + " ms");
    }
}
