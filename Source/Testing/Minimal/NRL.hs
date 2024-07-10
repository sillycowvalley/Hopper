program NRL
{
//#define CPU_Z80
    
    uses "/Source/Library/Boards/Hopper6502"
    
    NRL()
    {
        uint i;
        uint j;
        long s;
        
        IO.WriteLn("NRL:");
        
        long start = Millis;
        for (i=1; i <= 10; i++)
        {
            LED = !LED;
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
        }
        float elapsed = (Millis - start) / 1000.0;
        
        LED = false;
        IO.WriteLn();
        IO.WriteLn(s.ToString());
        IO.WriteLn(elapsed.ToString() + " seconds");
    }
    NRL32()
    {
        long i;
        long j;
        long s;
        
        IO.WriteLn("NRL32:");
        
        long start = Millis;
        for (i=1; i <= 10; i++)
        {
            LED = !LED;
            s = 0;
            for (j=1; j <= 1000; j++)
            {
                s = s + j;
            }
        }
        float elapsed = (Millis - start) / 1000.0;
        
        LED = false;
        IO.WriteLn();
        IO.WriteLn(s.ToString());
        IO.WriteLn(elapsed.ToString() + " seconds");
    }
    Hopper()
    {
        NRL32();
        NRL();
    }
}
