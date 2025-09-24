program Mandelbrot
{
    //uses "/Source/Library/Boards/PiPico"
    //uses "/Source/Library/Boards/PiPico2"
    //uses "/Source/Library/Boards/Hopper6502"
    uses "/Source/Library/Boards/MECB6502"
    //uses "/Source/System/System"
    //uses "/Source/System/IO"
        
    Hopper()
    {
        //ClockSpeed = RPClockSpeed.Slow133;
        //ClockSpeed = RPClockSpeed.Default;
        //ClockSpeed = RPClockSpeed.Overclock300;
        
        //IO.Clear();
        IO.WriteLn();
        
        // https://www.dos4ever.com/SCMP/NIBL.html
        
        IO.WriteLn(" Mandelbrot - ported from Gordon's TinyBasic - Integers");
        IO.WriteLn("    Ported to Hopper, running on a VM/Runtime written in Hopper.");
        IO.WriteLn();
        IO.Write(' ');
        
        long start = Millis;
        string palette = ".,'~=+:;*%&$OXB#@ ";
        long a; long b; long c; long d;
        long q; long p; long t; long s; long i;
        long y; long x;
        long f = 50;
        for (y = -12; y <= 12; y++)
        {
            for (x = -49; x <= 29; x++)
            {
                c = x * 229 / 100;
                IO.Write(c.ToString() + " ");
                /*
                d = y * 416 / 100;
                a = c; b = d; i = 0;
                loop
                {
                    q = b / f; s = b - (q * f);
                    t = ((a * a) - (b * b)) / f + c;
                    b = 2 * ((a * q) + (a * s / f)) + d;
                    a = t; p = a / f; q = b / f;
                    if (((p * p) + (q * q)) >= 5)
                    {
                        IO.Write(palette[i]);
                    }
                    else
                    {
                        i++; 
                        if (i < 16) { continue; }
                        IO.Write(' ');                   
                    }
                    break;
                }
                */
            } // next x
            IO.WriteLn(); IO.Write(' ');
        } // next y
        long ms = (Millis - start); 
        IO.WriteLn(ms.ToString() + " " + " ms");
        
        Delay(3000);
    }
}
