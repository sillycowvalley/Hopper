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
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        for(int i=0; i< ph / 2; i += 1)
        {
            Rectangle(i, i, pw-2*i, ph-2*i, Colour.White);
            Delay(250);
        }
    }

    TestFillRect()
    {
        Screen.Clear();
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        for(int i=0; i<ph / 2; i += 1)
        {
            // Colour.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, pw-i*2, ph-i*2, Colour.Invert);
            Delay(250);
        }
    }
    DrawText(string str)
    {
        foreach (var ch in str)
        {
            Screen.Clear(); // resets text cursor
            Screen.DrawChar(0, 0, ch, Colour.Red, Colour.Black);
            Delay(250);
        }
    }
    
    TestDrawLines() 
    {
        int i;
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        Screen.Clear();
        for(i=0; i<pw; i += 1) 
        {
            Line(0, 0, i, ph-1, Colour.White);
            Delay(50);
        }
        for(i=0; i< ph; i += 1) 
        {
            Line(0, 0, pw-1, i, Colour.White);
            Delay(50);
        }
        Delay(250);
        
        Screen.Clear();
        for(i=0; i< pw; i += 1)
        {
            Line(0, ph-1, i, 0, Colour.Red);
            Delay(50);
        }
        for(i= ph-1; i>=0; i -= 1)
        {
            Line(0, ph-1, pw-1, i, Colour.Red);
            Delay(50);
        }
        Delay(250);
        
        Screen.Clear();
        for(i= pw-1; i>=0; i -= 1)
        {
            Line(pw-1, ph-1, i, 0, Colour.Green);
            Delay(50);
        }
        for(i= ph-1; i>=0; i -= 1)
        {
            Line(pw-1, ph-1, 0, i, Colour.Green);
            Delay(50);
        }
        Delay(250);

        Screen.Clear();
        for(i=0; i< ph; i += 1)
        {
            Line(pw-1, 0, 0, i, Colour.Blue);
            Delay(50);
        }
        for(i=0; i< pw; i += 1)
        {
            Line(pw-1, 0, i, ph-1, Colour.Blue);
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
            Display.Clear(Colour.Black);
            for (byte row = 0; row < 8; row++)
            {
                for (byte column = 0; column < 8; column++)
                {
                    Display.SetPixel(column, row, Colour.Red);
                    Delay(2);
                }
            }
            Delay(500);
            
            TestDrawRect();
            Delay(500);
            
            TestFillRect();
            Delay(500);
            
            TestDrawLines() ;
            Display.Clear(Colour.Black);
            
            for (uint i=0; i < 6; i++)
            {
                DrawText("HOPPER ");
            }
        }
    }
}


