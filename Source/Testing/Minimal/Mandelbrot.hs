program Mandelbrot
{
    uses "/Source/Minimal/IO"
        
    {
        //IO.Clear();
        IO.WriteLn();
        
        // https://www.dos4ever.com/SCMP/NIBL.html
        
        IO.WriteLn(" Mandelbrot - ported from Gordon's TinyBasic - Integers");
        IO.WriteLn("    Ported to Hopper, running on a VM/Runtime built with the Hopper 6502 Assembler");
        IO.WriteLn();
        IO.Write(' ');
        
        uint start = Time.Seconds;
        string palette = ".,'~=+:;*%&$OXB#@ ";
        int a; int b; int c; int d;
        int q; int p; int t; int s; byte i;
        int y; int x;
        int f = 50;
        for (y = -12; y <= 12; y++)
        {
            for (x = -49; x <= 29; x++)
            {
                c = x * 229 / 100;
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
            } // next x
            IO.WriteLn(); IO.Write(' ');
        } // next y
        uint elapsed = (Seconds - start); 
        IO.WriteLn(elapsed.ToString() + " " + " seconds");
    }
}
