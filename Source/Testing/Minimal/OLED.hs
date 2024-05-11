program Test
{
    #define MCU
    
    uses "/Source/Minimal/System"
    
    uses "/Source/Minimal/Wire" // just for fast navigation for now
    
    uses "/Source/Library/Displays/OLEDSSD1306"    
    uses "/Source/Library/Fonts/Verdana5x8"
    
    CrossBox(int x, int y, int size)
    {
        Display.FilledRectangle       (x, y, size, size, Colour.Black);
        Display.Line(x + 4, y + 4,        x + size - 6, y + size - 6, Colour.White);
        Display.Line(x + 4, y + size - 6, x + size - 6, y + 4, Colour.White);
    }
    
    Hopper()
    {
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        uint start = Seconds;
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        uint elapsed = Seconds - start;
        WriteLn();
        WriteLn("Clear Time:   " + elapsed.ToString() + " seconds");
        
        start = Seconds;
        
        Display.Suspend();
        Display.FilledRoundedRectangle(0,0, Display.PixelWidth-1, Display.PixelHeight-1, 12, Colour.White);
        
        Display.FilledRoundedRectangle(17,  10, 75, 18, 8, Colour.Black);
        CrossBox(100, 12, 14);
        
        
        Screen.DrawText(20, 15, " Verdana x1 ", Colour.White, Colour.Black, 1);
        Screen.DrawText(4,  38, "Verdana x2", Colour.Black, Colour.White, 2);
        elapsed = Seconds - start;
        start = Seconds;
        Display.Resume();
        uint elapsed2 = Seconds - start;
        WriteLn("Draw Time:   " + elapsed.ToString() + " seconds");
        WriteLn("Update Time: " + elapsed2.ToString() + " seconds");
    }
}

