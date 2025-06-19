program Test
{
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Minimal/System"
    uses "/Source/Library/Boards/Hopper6502"
    
    uses "/Source/Library/Displays/OLEDSSD1306"    
    
    
    CrossBox(int x, int y, int size)
    {
        Display.FilledRectangle       (x, y, size, size, Colour.Black);
        Display.Line(x + 4, y + 4,        x + size - 6, y + size - 6, Colour.White);
        Display.Line(x + 4, y + size - 6, x + size - 6, y + 4, Colour.White);
    }
    
    Hopper()
    {
        DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        long start = Millis;
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        long elapsed = Millis - start;
        WriteLn();
        WriteLn("Clear Time:   " + elapsed.ToString() + " ms");
        
        start = Millis;
        
        Display.Suspend();
        Display.FilledRoundedRectangle(0,0, Display.PixelWidth-1, Display.PixelHeight-1, 12, Colour.White);
        
        Display.FilledRoundedRectangle(17,  10, 75, 18, 8, Colour.Black);
        CrossBox(100, 12, 14);
        
        
        Screen.DrawText(20, 15, " Verdana x1 ", Colour.White, Colour.Black, 1);
        Screen.DrawText(4,  38, "Verdana x2", Colour.Black, Colour.White, 2);
        elapsed = Millis - start;
        start = Millis;
        Display.Resume();
        long elapsed2 = Millis - start;
        WriteLn("Draw Time:   " + elapsed.ToString() + " ms");
        WriteLn("Update Time: " + elapsed2.ToString() + " ms");
    }
}

