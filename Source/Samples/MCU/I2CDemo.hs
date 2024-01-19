program I2CDemo
{
#define SERIAL_CONSOLE

    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    uses "/Source/Library/Graphics"
    
    bool DisplayILI9341xPiPico28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(17, 21); // CS, DC
        ConfigureReset(20);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayST7796xPiPico40()
    {
        ConfigureDisplay(Display.ST7796, 480, 320);
        ConfigureSPI(17, 21); // CS, DC
        ConfigureReset(20);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplayST7735xPiPico144() // Pico-LCD-1.44
    {
        ConfigureDisplay(Display.ST7735, 128, 128);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplayST7735xPiPico096() // Waveshare Pico-LCD-0.96
    {
        ConfigureDisplay(Display.ST7735, 160, 80);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplayST7789xPiPico114() // Waveshare Pico-LCD-1.14
    {
        ConfigureDisplay(Display.ST7789, 240, 135);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    
    bool DisplayILI9341xD1Mini28()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(4, 2); // CS, DC
        ConfigureReset(0);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayILI9341xD1Mini24()
    {
        ConfigureDisplay(Display.ILI9341, 320, 240);
        ConfigureSPI(16, 15); // CS, DC
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

        for(i=0; i<int(Width); i += 4) 
        {
            Line(0, 0, uint(i), Height-1, Colour.White);
        }
        for(i=0; i<int(Height); i += 4) 
        {
            Line(0, 0, Width-1, uint(i), Colour.White);
        }

        Screen.Clear();

        for(i=0; i<int(Width); i += 4)
        {
            Line(0, Height-1, uint(i), 0, Colour.Red);
        }
        for(i=int(Height)-1; i>=0; i -= 4)
        {
            Line(0, Height-1, Width-1, uint(i), Colour.Red);
        }
 
        Screen.Clear();

        for(i=int(Width)-1; i>=0; i -= 4)
        {
            Line(Width-1, Height-1, uint(i), 0, Colour.Green);
        }
        for(i=int(Height)-1; i>=0; i -= 4)
        {
            Line(Width-1, Height-1, 0, uint(i), Colour.Green);
        }

        Clear();

        for(i=0; i<int(Height); i += 4)
        {
            Line(Width-1, 0, 0, uint(i), Colour.Blue);
        }
        for(i=0; i<int(Width); i += 4)
        {
            Line(Width-1, 0, uint(i), Height-1, Colour.Blue);
        }
    }
    
    TestDrawRect()
    {
        Clear();
        for(uint i=0; i< Height/2; i += 2)
        {
            Rectangle(i, i, Width-2*i,Height-2*i, Colour.White);
        }
    }

    TestFillRect() 
    {
        Clear();
        for(uint i=0; i<Height/2; i += 3)
        {
            // Colour.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, Width-i*2, Height-i*2, Colour.Invert);
        }
    }
    
    TestFilledCircles()
    {
        Clear();
        uint r  = Width/10;
        uint r2 = Width/20;
        for(uint x=r2; x< Width*4/5; x += r2) 
        {
            for(uint y=r2; y< Height*4/5; y += r2) 
            {
                FilledCircle(x, y, r, Colour.Green);
            }
        }
    }
    TestCircles()
    {
        Clear();
        uint r  = Width/10;
        uint r2 = Width/20;
        for(uint x=r2; x< Width*4/5; x += r2) 
        {
            for(uint y=r2; y< Height*4/5; y += r2) 
            {
                Circle(x, y, r, Colour.Blue);
            }
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
            
            /*
            
            Graphics.Clear(Colour.Blue);
            FilledRectangle(0, 0, Width, Height, Colour.Red);
            FilledRectangle(4, 4, Width-8, Height-8, Colour.Green);
            Line(0, 0, Width-1, Height-1, Colour.Black);
            Line(0, Height-1, Width-1, 0, Colour.Black);
            Rectangle(0,0, Width/2, Height/2, Colour.Blue);
            
            Graphics.DrawChar(0,  0, 'A', Colour.MatrixRed, Colour.Black, 3, false);
            Graphics.DrawChar(18, 0, 'B', Colour.MatrixRed, Colour.Black, 3, false);
            Graphics.DrawChar(36, 0, 'C', Colour.MatrixRed, Colour.Black, 3, false);
            
            Graphics.DrawChar(60, 60, 'A', Colour.MatrixGreen, Colour.Black, 3, false);
            Graphics.DrawChar(78, 60, 'B', Colour.MatrixGreen, Colour.Black, 3, false);
            Graphics.DrawChar(96, 60, 'C', Colour.MatrixGreen, Colour.Black, 3, false);
            
            break;
            */
            
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
            Graphics.Clear(Colour.Black);
            TestCircles();
            elapsed = Millis - start;
            WriteLn("TestCircles: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Black);
            TestFilledCircles();
            elapsed = Millis - start;
            WriteLn("TestFilledCircles: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Black);
            TestFillRect();
            elapsed = Millis - start;
            WriteLn("TestFillRect: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Black);
            TestDrawRect();
            elapsed = Millis - start;
            WriteLn("TestDrawRect: " + elapsed.ToString());
            Delay(500);
            
            start = Millis;
            Graphics.Clear(Colour.Black);
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
