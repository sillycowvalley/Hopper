program Displays
{
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    uses "/Source/Library/Graphics"
    
    // https://www.i4cy.com/sinclair/
    // https://www.codingame.com/playgrounds/2358/how-to-plot-the-mandelbrot-set/mandelbrot-set
    
    bool DisplayILI9341xPiPico28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(17, 21);
        ConfigureReset(20);
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplaySSD1306x96()
    {
        ConfigureDisplay(Display.SSD1306, 128, 64);
        ConfigureI2C(0x3C);
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    
    const uint maxIterations = 32;
    
    uint HSVToColour(uint H, uint S, uint V)
    {
        float r;
        float g;
        float b;
        float h = H / 255.0; // 360
        float s = S / 255.0; // 100
        float v = V / 255.0; // 100

        uint i = uint(h*6);
        float f = h * 6 - i;
        float p = v * (1 - s);
        float q = v * (1 - f * s);
        float t = v * (1 - (1 - f) * s);
        
        switch (i % 6) 
        {
            case 0: { r = v; g = t; b = p; }
            case 1: { r = q; g = v; b = p; }
            case 2: { r = p; g = v; b = t; }
            case 3: { r = p; g = q; b = v; }
            case 4: { r = t; g = p; b = v; }
            case 5: { r = v; g = p; b = q; }
        }
        uint colour = (uint(r * 15) << 8) | (uint(g * 15) << 4) | (uint(b * 15));
        return colour;
    }
    Cell(uint x, uint y, uint iterations, bool isColour)
    {
        if (isColour)
        {
            uint hue = 255 * iterations / maxIterations;
            uint saturation = 255;
            uint value = (iterations < maxIterations) ? 255 : 0;
            
            uint colour = HSVToColour(hue, saturation, value);
            Graphics.SetPixel(x,y, colour);
        }
        else if (iterations >= maxIterations)
        {
            Graphics.SetPixel(x,y, Colour.White);
        }
    }
    
    uint Mandelbrot(float rC, float iC)
    {
        float rZ = 0;
        float iZ = 0;
        uint iterations = 0;
        float rZ2 = rZ*rZ;
        float iZ2 = iZ*iZ;
        while (((rZ2 + iZ2) < 4.0) && (iterations < maxIterations))
        {
            iZ = rZ * iZ + iZ * rZ + iC;
            rZ = rZ2 - iZ2 + rC;
            rZ2 = rZ*rZ;
            iZ2 = iZ*iZ;
            iterations++;
        }
        return iterations;
    }
    Plot(bool isColour)
    {
        long start = Millis;
        int rStart = -2;
        int rEnd   =  1;
        int iStart = -1;
        int iEnd   =  1;
        
        // subset of display for faster testing:
        //uint width  = (Graphics.Width  * 2) / 4;
        //uint height = (Graphics.Height * 2) / 4;
        
        // full display area:
        uint width  = Graphics.Width;
        uint height = Graphics.Height;
        
        uint x0 = (Graphics.Width  - width) / 2;
        uint y0 = (Graphics.Height - height) / 2;
        for (uint x = 0; x < width; x++)
        {
            // convert x pixel coordinate to complex number
            float rC = rStart + (float(x) / width)  * (rEnd - rStart);
                
            for (uint y = 0; y < height; y++)
            {
                // convert y pixel coordinate to complex number
                float iC = iStart + (float(y) / height) * (iEnd - iStart);
                
                // compute the number of iterations
                uint iterations = Mandelbrot(rC, iC);
                
                // plot point
                Cell(x0+x,y0+y, iterations, isColour);
            }
        }
        long ms = (Millis - start); 
        float mpp = ms / (1.0 * width * height);
        PrintLn((isColour ? "Colour, " : "Mono,   ") + ms.ToString() + " " + " ms (" + mpp.ToString() + " " + " mspp)");
    }
    TFTDemo()
    {
        if (!DisplayILI9341xPiPico28())
        {
            WriteLn("Failed to initialize TFT display");
            return;
        }
        Graphics.Clear(Colour.Black);
        Screen.Suspend();
        Plot(true);
        Screen.Resume(false);
        Graphics.End();
    }
    OLEDDemo()
    {
        if (!DisplaySSD1306x96())
        {
            WriteLn("Failed to initialize OLED display");
            return;
        }
        Graphics.Clear(Colour.Black);
        Screen.Suspend();
        Plot(false);   
        Screen.Resume(false);
        Graphics.End();
    }
    {
        OLEDDemo();
        TFTDemo();
    }
}
