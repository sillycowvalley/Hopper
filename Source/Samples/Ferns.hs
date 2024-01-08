program Ferns
{
    // https://en.wikipedia.org/wiki/Barnsley_fern#History
    
    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/System/Color"
    uses "/Source/System/Screen"
    
    uses "/Source/Library/Graphics"
    
    bool DisplayILI9341xPiPico28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(17, 21);
        ConfigureReset(20);
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayST7796xPiPico40()
    {
        ConfigureDisplay(Display.ST7796, 480, 320);
        ConfigureSPI(4, 2);
        ConfigureReset(0);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    
    bool DisplaySSD1306x96()
    {
        ConfigureDisplay(Display.SSD1306, 128, 64);
        ConfigureI2C(0x3C);
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    
    uint rnd;
    uint random(uint max)
    {
        // Pseudorandom generator from here: (fast and good enough)
        // https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
        rnd = rnd ^ (rnd << 7);
        rnd = rnd ^ (rnd >> 9);
        rnd = rnd ^ (rnd << 8);
        return (rnd % max);
    }
    randomSeed(uint seed)
    {
        rnd = seed == 0 ? 1 : seed;
    }
    
    BarnsleyFern(uint samples, float offset, float scale)
    {
        long start = Millis;
        float x = 0;
        float y = 0;
        for (long count = 0; count < samples; count++)
        {
            uint roll = random(100); // 0..99
            float xn;
            float yn;
            if (roll < 1)// 1%
            {
                xn = 0.0;
                yn = 0.16 * y;
            } 
            else if (roll < 86)// 85%
            {
                xn =  0.85 * x + 0.04 * y;
                yn = -0.04 * x + 0.85 * y + 1.6;
            }
            else if (roll < 93)// 7%
            {
                xn = 0.2  * x - 0.26 * y;
                yn = 0.23 * x + 0.22 * y + 1.6;
            }
            else// 7%
            {
                xn = -0.15 * x + 0.28 * y;
                yn =  0.26 * x + 0.24 * y + 0.44;
            }
            SetPixel(uint(scale * yn), uint(offset + scale * xn), Color.Green);
            x = xn;
            y = yn;
        }
        
        long ms = (Millis - start); 
        float mpp = ms / (1.0 * samples);
        PrintLn(ms.ToString() + " " + " ms (" + mpp.ToString() + " " + " mspp)");
    }
    TFTDemo()
    {
        if (!DisplayILI9341xPiPico28())
        {
            WriteLn("Failed to initialize TFT display");
            return;
        }
        Graphics.Clear(Color.Black);
        Screen.Suspend();
        BarnsleyFern(25000, Width * 0.333, 30);
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
        Graphics.Clear(Color.Black);
        Screen.Suspend();
        BarnsleyFern(8000,  Width * 0.25, 10);   
        Screen.Resume(false);
        Graphics.End();
    }
    {
        randomSeed(uint(Millis % 0xFFFF));
        
        loop
        {
            TFTDemo();
        }
        //OLEDDemo();
    }
}
