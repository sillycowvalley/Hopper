program NRL
{
//#define CPU_Z80

    uses "/Source/Minimal/System"
    uses "/Source/Minimal/IO"
    
    {
        uint i;
        uint j;
        uint s;
        
        IO.WriteLn();
        
        uint start = Seconds;
        
        for (i=1; i <= 10; i++)
        {
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
            IO.Write('.');
        }
        
        uint elapsed = (Seconds - start); 
        IO.WriteLn();
        IO.WriteLn(elapsed.ToString() + " " + " seconds");
    }
}
