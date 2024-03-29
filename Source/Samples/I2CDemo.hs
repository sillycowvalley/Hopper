program I2CDemo
{
#define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    uses "/Source/Library/Graphics"
    
    bool DisplayILI9341xPiPico28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(17, 21);
        ConfigureReset(20);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayST7796xPiPico40()
    {
        ConfigureDisplay(Display.ST7796, 480, 320);
        ConfigureSPI(17, 21);
        ConfigureReset(20);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplayILI9341xD1Mini28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(4, 2);
        ConfigureReset(0);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayILI9341xD1Mini24()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(16, 15);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayST7796xD1Mini40()
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
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplaySSD1306x66()
    {
        ConfigureDisplay(Display.SSD1306, 64, 48);
        ConfigureI2C(0x3C);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    Mandelbrot()
    {
        Screen.Suspend();
        uint width = Width;
        uint height = Height;
        uint xOffset = 0;
        uint yOffset = 0;
        
        if (Width > 128)
        {
            width = Width / 3;
            height = Height / 3;
            xOffset = width / 2;
            yOffset = height / 2;
        }
        
        byte  max   =  15;
        float w4 = 4.0 / width;
        float w2 = width / 2.0;
        float h2 = height / 2.0;
        for (byte row = 0; row < height; row++) 
        {
            for (byte col = 0; col < width; col++) 
            {
                float cre = (col - w2)  * w4;
                float cim = (row - h2) * w4;
                float x; 
                float y;
                byte iteration;
                loop
                {
                    float x2 = x*x;
                    float y2 = y*y;
                    if (x2 + y2 > 4)      { break; }
                    y = 2 * x * y + cim;
                    x = x2 - y2 + cre;
                    iteration++;
                    if (iteration >= max) { break; }
                }
                if (iteration == max)
                {
                    Graphics.SetPixel(xOffset + col, yOffset + row, Colour.White);
                }
            }
        } 
        Screen.Resume(false);
    } 
    TestAxes()
    {
        Line(10,10,10,40, Colour.Red);
        Line(10,40,15,20, Colour.Red);
        Line(10,10,40,10, Colour.Blue);
        Line(40,10,20,15, Colour.Blue);
        Delay(2000);
        for (uint i = 0; i < 10; i++)
        {
            Delay(250);
            Line(10,10,10,40, Colour.Invert);
            Line(10,40,15,20, Colour.Invert);
            Line(10,10,40,10, Colour.Invert);
            Line(40,10,20,15, Colour.Invert);
        }
    }
    TestDrawLines() 
    {
        int i;
        Screen.Clear();

        for(i=0; i<int(Width); i = i + 4) 
        {
            Line(0, 0, uint(i), Height-1, Colour.White);
        }
        for(i=0; i<int(Height); i = i + 4) 
        {
            Line(0, 0, Width-1, uint(i), Colour.White);
        }

        Screen.Clear();

        for(i=0; i<int(Width); i = i + 4) 
        {
            Line(0, Height-1, uint(i), 0, Colour.Red);
        }
        for(i=int(Height)-1; i>=0; i = i - 4)
        {
            Line(0, Height-1, Width-1, uint(i), Colour.Red);
        }
 
        Screen.Clear();

        for(i=int(Width)-1; i>=0; i = i - 4) 
        {
            Line(Width-1, Height-1, uint(i), 0, Colour.Green);
        }
        for(i=int(Height)-1; i>=0; i = i - 4) 
        {
            Line(Width-1, Height-1, 0, uint(i), Colour.Green);
        }

        Clear();

        for(i=0; i<int(Height); i = i + 4) 
        {
            Line(Width-1, 0, 0, uint(i), Colour.Blue);
        }
        for(i=0; i<int(Width); i = i + 4)
        {
            Line(Width-1, 0, uint(i), Height-1, Colour.Blue);
        }
    }
    
    TestDrawRect() 
    {
        Clear();
        for(uint i=0; i< Height/2; i = i +2) 
        {
            Rectangle(i, i, Width-2*i,Height-2*i, Colour.White);
        }
    }

    TestFillRect() 
    {
        Clear();
        for(uint i=0; i<Height/2; i = i +3) 
        {
            // Colour.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, Width-i*2, Height-i*2, Colour.Invert);
        }
    }
    
    {
        long start;
        long elapsed;
        
        if (!DisplaySSD1306x96())
        {
            WriteLn("Failed to initialize Display");
            return;
        }
        
        loop
        {
            Graphics.FlipDisplay(true);
            Graphics.InvertDisplay(false);
            
            start = Millis;
            Graphics.Clear(Colour.White);
            elapsed = Millis - start;
            WriteLn("White: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Red);
            elapsed = Millis - start;
            WriteLn("Red: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Green);
            elapsed = Millis - start;
            WriteLn("Green: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Blue);
            elapsed = Millis - start;
            WriteLn("Blue: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Black);
            elapsed = Millis - start;
            WriteLn("Blank: " + elapsed.ToString());
            Delay(500);
            
            //start = Millis;
            //TestAxes();
            //elapsed = Millis - start;
            //WriteLn("TestAxes: " + elapsed.ToString());
            //Delay(250);
            
            start = Millis;
            TestDrawRect();
            elapsed = Millis - start;
            WriteLn("TestDrawRect: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            TestFillRect();
            elapsed = Millis - start;
            WriteLn("TestFillRect: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            TestDrawLines();
            elapsed = Millis - start;
            WriteLn("TestDrawLines: " + elapsed.ToString());
            
            Delay(500);
            
            //start = Millis;
            //Clear();
            //Mandelbrot();
            //elapsed = Millis - start;
            //WriteLn("Mandelbrot: " + elapsed.ToString());
            
            //Delay(3000);
            
        }
    }    
}
