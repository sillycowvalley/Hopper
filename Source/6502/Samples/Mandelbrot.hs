program Mandelbrot
{

    uses "/Source/6502/System"
    uses "/Source/System/Screen"

    ClearScreen()
    {
#ifdef HOPPER_6502
        Serial.WriteChar(char(0x0C)); // form feed
#else
        Screen.Clear();
#endif        
    }
    NewLine()
    {
#ifdef HOPPER_6502
        Serial.WriteChar(char(0x0D));
#else
        PrintLn();
#endif
    }
    OutChar(char ch)
    {
#ifdef HOPPER_6502
        Serial.WriteChar(ch);
#else
        Print(ch);
#endif
    }
    OutLn(string s)
    {
        foreach (var ch in s)
        {
            OutChar(ch);
        }
        NewLine();
    }
        
    {
        
        ClearScreen();
        
        NewLine();
        OutLn(" Mandelbrot - ported from Gordon's TinyBasic - Integers");
        OutLn("    Ported to Hopper on 6502 running at 4.9152 mHz");
        NewLine();
        OutChar(' ');
        
        long start = Millis;
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
                        OutChar(palette[i]);
                    }
                    else
                    {
                        i++; 
                        if (i < 16) { continue; }
                        OutChar(' ');                   
                    }
                    break;
                }
            } // next x
            NewLine(); OutChar(' ');
        } // next y
        long ms = (Millis - start); 
        OutLn(ms.ToString() + " " + " ms");
    }
}
