program LED8x8Matrix
{
    #define RP2040_PICO
    uses "/Source/Library/Displays/LEDMatrixDriver"
    
    uses "/Source/Library/Fonts/Verdana5x8"

    const byte ClockPin = 1; // White
    const byte CSPin    = 2; // Gray
    const byte DataPin  = 3; // Purple
    
    TestDrawRect()
    {
        Screen.Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i< ph / 2; i += 1)
        {
            Rectangle(i, i, uint(pw-2*i), uint(ph-2*i), Color.White);
            Delay(250);
        }
    }

    TestFillRect()
    {
        Screen.Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i<ph / 2; i += 1)
        {
            // Color.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, uint(pw-i*2), uint(ph-i*2), Color.Invert);
            Delay(250);
        }
    }
    DrawText(string str)
    {
        foreach (var ch in str)
        {
            Screen.Clear(); // resets text cursor
            Screen.DrawChar(0, 0, ch, Color.Red, Color.Black);
            Delay(250);
        }
    }
    
    TestDrawLines() 
    {
        int i;
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        Screen.Clear();
        for(i=0; i<pw; i += 1) 
        {
            Line(0, 0, i, ph-1, Color.White);
            Delay(50);
        }
        for(i=0; i< ph; i += 1) 
        {
            Line(0, 0, pw-1, i, Color.White);
            Delay(50);
        }
        Delay(250);
        
        Screen.Clear();
        for(i=0; i< pw; i += 1)
        {
            Line(0, ph-1, i, 0, Color.Red);
            Delay(50);
        }
        for(i= ph-1; i>=0; i -= 1)
        {
            Line(0, ph-1, pw-1, i, Color.Red);
            Delay(50);
        }
        Delay(250);
        
        Screen.Clear();
        for(i= pw-1; i>=0; i -= 1)
        {
            Line(pw-1, ph-1, i, 0, Color.Green);
            Delay(50);
        }
        for(i= ph-1; i>=0; i -= 1)
        {
            Line(pw-1, ph-1, 0, i, Color.Green);
            Delay(50);
        }
        Delay(250);

        Screen.Clear();
        for(i=0; i< ph; i += 1)
        {
            Line(pw-1, 0, 0, i, Color.Blue);
            Delay(50);
        }
        for(i=0; i< pw; i += 1)
        {
            Line(pw-1, 0, i, ph-1, Color.Blue);
            Delay(50);
        }
    }
    
    {

        DisplayDriver.ClockPin = 1; // white
        DisplayDriver.CSPin    = 2; // gray
        DisplayDriver.DataPin  = 3; // purple
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize");
        }
        Brightness = 8;
        
        loop
        {
            Display.Clear(Color.Black);
            for (byte row = 0; row < 8; row++)
            {
                for (byte column = 0; column < 8; column++)
                {
                    Display.SetPixel(column, row, Color.Red);
                    Delay(2);
                }
            }
            Delay(500);
            
            TestDrawRect();
            Delay(500);
            
            TestFillRect();
            Delay(500);
            
            TestDrawLines() ;
            Display.Clear(Color.Black);
            
            for (uint i=0; i < 6; i++)
            {
                DrawText("HOPPER ");
            }
        }
    }
}


