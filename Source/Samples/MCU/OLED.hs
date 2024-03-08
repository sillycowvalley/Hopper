program OLEDdemo
{
    //uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    uses "/Source/Library/Boards/AdafruitQTPy"
    uses "/Source/Library/Displays/OLEDSSD1306"    
    
    uses "/Source/Library/Fonts/System5x7"
    
    testTextChars()
    {
        Screen.BackColour = Colour.Black;
        Screen.Clear();
        
        long start = Time.Millis;
        Screen.ForeColour = Colour.White;
        Screen.TextScale = 1;
        Screen.SetCursor(0,0);
        Display.Suspend();
        for (byte c = 33; c < 128; c++)
        {
            Screen.Print(char(c));
        }
        Display.Resume();        
        IO.WriteLn(("Text (chars)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testText()
    {
        Screen.BackColour = Colour.Black;
        Screen.Clear();
        
        long start = Time.Millis;
        Screen.ForeColour = Colour.White;
        Screen.TextScale = 1;
        Screen.SetCursor(0,0);
        
        Display.Suspend();
        Screen.PrintLn("Failure is always an option");
        
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        Screen.PrintLn((3.141592).ToString());
        
        Screen.TextScale = 2;
        Screen.ForeColour = Colour.White;
        Screen.BackColour = Colour.Black;
        Screen.Print((0xDEAD).ToHexString(4));
        Screen.PrintLn((0xBEEF).ToHexString(4));
        Display.Resume();
                      
        IO.WriteLn(("Text").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testScreenClear() 
    {
        long start = Time.Millis;
        Display.Clear(Colour.Black);
        IO.WriteLn(("Screen clear").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testScreenInvert() 
    {
        long start = Time.Millis;
        Display.Clear(Colour.Invert);
        IO.WriteLn(("Screen invert").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testLines() 
    {
        int w = Display.PixelWidth;
        int h = Display.PixelHeight;

        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for (int i = 0; i < w; i += 4) { Display.Line(0, 0, i, h-1, Colour.White); Time.Delay(1); }
        for (int i = 0; i < h; i += 4) { Display.Line(0, 0, w-1, i, Colour.White); Time.Delay(1); }

        Display.Clear(Colour.Black);
        for (int i = 0; i < w; i += 4)    { Display.Line(0, h-1, i, 0, Colour.White); Time.Delay(1); }
        for (int i = h-1; i >= 0; i -= 4) { Display.Line(0, h-1, w-1, i, Colour.White); Time.Delay(1); }
        
        Display.Clear(Colour.Black);
        for (int i = w-1; i >= 0; i -= 4) { Display.Line(w-1, h-1, i, 0, Colour.White); Time.Delay(1); }
        for (int i = h-1; i >= 0; i -= 4) { Display.Line(w-1, h-1, 0, i, Colour.White); Time.Delay(1); }
        
        Display.Clear(Colour.Black);
        for (int i = 0; i < h; i += 4) { Display.Line(w-1, 0, 0, i, Colour.White); Time.Delay(1); }
        for (int i = 0; i < w; i += 4) { Display.Line(w-1, 0, i, h-1, Colour.White); Time.Delay(1); }
        
        IO.WriteLn(("Lines").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testRects() 
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;

        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for (int i = 0; i < cy; i += 2) 
        {
            Display.Rectangle(i, i, Display.PixelWidth - 2*i, Display.PixelHeight - 2*i, Colour.White);
            Time.Delay(1);
        }

        IO.WriteLn(("Rectangles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }

    testFilledRects()
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        uint colour = Colour.White;
        for (int i = 0; i < cy - 2; i += 3) 
        {
            Display.FilledRectangle(i, i, Display.PixelWidth - i*2, Display.PixelHeight - i*2, colour);
            Time.Delay(1);
            colour = (colour == Colour.White) ? Colour.Black : Colour.White;
        }
        IO.WriteLn(("Rectangles (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testTriangles() 
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;
        int n = Int.Min(cx, cy);
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int i=0; i<n; i+=5) 
        {
            Display.Triangle(cx, cy-i, cx-i, cy+i, cx+i, cy+i, Colour.White);
            Time.Delay(1);
        }
        IO.WriteLn(("Triangles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    testFilledTriangles()
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;
        int n = Int.Min(cx, cy);
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        uint colour = Colour.White;
        for(int i=n; i > 0; i-=5) 
        {
            Display.FilledTriangle(cx, cy-i, cx-i, cy+i, cx+i, cy+i, colour);
            Time.Delay(1);
            colour = (colour == Colour.White) ? Colour.Black : Colour.White;
        }
        IO.WriteLn(("Triangles (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());
    } 
    
    testCircles()
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int i=0; i<Display.PixelHeight; i += 2) 
        {
            Display.Circle(cx, cy, i, Colour.White);
            Time.Delay(1);
        }
     
        IO.WriteLn(("Circles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    testFilledCircle()
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        Display.FilledCircle(cx, cy, 10, Colour.White);
        
        IO.WriteLn(("Circle (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    
    testRoundedRectangles()
    {
        int cy = Display.PixelHeight / 2;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for (int i=0; i<cy-2; i+=2) 
        {
            Display.RoundedRectangle(i, i, Display.PixelWidth - 2*i, Display.PixelHeight - 2*i, Display.PixelHeight/4, Colour.White);
            Time.Delay(1);
        }
        
        IO.WriteLn(("Rounded rects (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    testFilledRoundedRectangles()
    {
        int cy = Display.PixelHeight / 2;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        uint colour = Colour.White;
        for (int i=0; i<cy-2; i+=2) 
        {
            Display.FilledRoundedRectangle(i, i, Display.PixelWidth - 2*i, Display.PixelHeight - 2*i, Display.PixelHeight/4, colour);
            colour = (colour == Colour.White) ? Colour.Black : Colour.White;
            Time.Delay(1);
        }       
        IO.WriteLn(("Rounded rects (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
        
    {
    
// SH110X_OLED_128x64    
// SSD1306_OLED_128x64

        //IsPortrait = true;
        FlipY = true;
        FlipX = true;
        
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
              
        IO.WriteLn("Columns: " + (Screen.Columns).ToString() + ", Rows: " + (Screen.Rows).ToString());
        loop
        {
            IO.WriteLn(("Benchmark").Pad(' ', 25) + "Time (milliseconds)");
            testScreenClear();
            Delay(500);
            
            testTextChars();
            Delay(500);
            testText();
            Delay(500);
            break;
 
            testLines();
            Delay(500);
            
            testRects();
            Delay(500);
            testFilledRects();
            Delay(500);
            
            testCircles();
            Delay(500);
            testFilledCircle();  
            Delay(500);
            
            testRoundedRectangles();
            Delay(500);
            testFilledRoundedRectangles();
            Delay(500);
            
            testTriangles();
            Delay(500);
            testFilledTriangles();
            Delay(500);
            
            testTextChars();
            Delay(500);
            testText();
            Delay(500);
            
            testScreenInvert();
            Delay(500);
            testScreenInvert();
            Delay(500);
            
            IO.WriteLn("Done!");
        }
    }
}
