program Mandelbrot
{
    uses "/Source/Library/Devices/WSPicoePaper4200"
    uses "/Source/Library/Fonts/Hitachi5x7"
        
    ButtonISR(byte pin, PinStatus status) 
    { 
        string pinName = PinToButton(pin);
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        Mandelbrot(pinName == "1");
    }
    
    Mandelbrot(bool invert)
    {
        LED = true;
        
        EchoToLCD = true;
        Screen.ForeColour = invert ? Colour.White : Colour.Black;
        Screen.BackColour = invert ? Colour.Black : Colour.White;
        Display.Suspend();
        IO.Clear();
        
        IO.WriteLn();
        
        // https://www.dos4ever.com/SCMP/NIBL.html
        IO.WriteLn(" Mandelbrot - ported from Gordon's TinyBasic - Integers");
        IO.WriteLn("    Ported to Hopper, running a VM/Runtime written in Hopper.");
        IO.WriteLn();
        IO.Write(' ');
        
        long start = Millis;
        string palette = ".,'~=+:;*%&$OXB#@ ";
        int a; int b; int c; int d;
        int q; int p; int t; int s; byte i;
        int y; int x;
        int f = 50;
        
        for (y = -15; y <= 15; y++)
        {
            for (x = -44; x <= 19; x++)
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
        long ms = (Millis - start); 
        IO.WriteLn(ms.ToString() + " " + " ms");
        Display.Resume();
        EchoToLCD = false;
        
        LED = false;
    }
    {
        // Setup code:
        ISRDelegate buttonDelegate = ButtonISR;
        DisplayDriver.FlipY = true;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-ePaper-4.2");
            return;
        }
        
        
        LED = false;
        Display.Clear(Colour.White);
        // Infinite loop: (events ..)
        loop
        {
            Delay(10);
        }
        
        
    }
}
