program Matrix
{
    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/Library/Graphics"
    
    bool DisplayLEDMatrix()
    {
        ConfigureDisplay(Display.LEDMatrix, 8, 8);
        ConfigureMatrix(14, 13, 4);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    RunningLights()
    {
        int x = 0;
        int y = 0;
        int dx = 1;
        int dy = 0;
        int width  = int(Width);
        int height = int(Height);
        Graphics.Clear(Colour.Black);
        loop
        {
            SetPixel(uint(x), uint(y), Colour.Invert);
            Delay(5);
            x = x + dx;
            y = y + dy;
            if (x >= width)
            {
                x = width-1;
                dx = 0;
                dy = 1;
            }
            if (y >= height)
            {
                y = height-1;
                dx = -1;
                dy = 0;
            }
            if (x < 0)
            {
                x = 0;
                dx = 0;
                dy = -1;
            }
            if (y < 0)
            {
                y = 0;
                dx = 1;
                dy = 0;
            }
        }     
    }
    PrettyLights()
    {
        Graphics.Clear(Colour.Black);
        loop
        {
            for (byte y = 0; y < Height; y++)
            {
                Line(0, 0, Width-1, y, Colour.Invert);
                Delay(100);
            }
            for (byte y = 0; y < Height; y++)
            {
                Line(0, y, Width-1, 0, Colour.Invert);
                Delay(100);
            }
            for (byte y = 0; y < Height; y++)
            {
                Line(0, Height-1, Width-1, y, Colour.Invert);
                Delay(100);
            }
            for (byte y = 0; y < Height; y++)
            {
                Line(0, y, Width-1, Height-1, Colour.Invert);
                Delay(100);
            }
            for (byte y = 0; y < Height; y++)
            {
                for (byte x = 0; x < Width; x++)
                {
                    SetPixel(x, y, Colour.White);
                    Delay(10);
                }
            }
        }
    }
    {
        if (!DisplayLEDMatrix())
        {
            WriteLn("Failed to initialize Display");
            return;
        }
        
        //RunningLights();
        PrettyLights();
    }
}
