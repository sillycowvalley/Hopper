program TFTandSDdemo
{
    uses "/Source/Library/Fonts/System5x7"
    
    
    //uses "/Source/Library/Boards/Pi"
    uses "/Source/Library/Boards/PiPico2W"
    uses "/Source/Library/Devices/Adafruit240x135ColorTFT"
    
    const string lorumIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse iaculis tortor vitae imperdiet tempus. Quisque eget sapien ex. Donec molestie tincidunt sem imperdiet condimentum. Nulla facilisi. Class aptent taciti sociosqu ad litora vestibulum.";

    DrawText()
    {
        IO.WriteLn("  DrawText");
        EchoToDisplay = true;
        Display.ForeColour = Colour.White;
        //Display.BackColour = Colour.White;
        Display.Clear();
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        IO.WriteLn();
        Display.ForeColour = Colour.MatrixRed;
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        IO.WriteLn();
        Display.ForeColour = Colour.MatrixBlue;
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        IO.WriteLn();
        Display.ForeColour = Colour.MatrixGreen;
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        IO.WriteLn();
        Display.ForeColour = Colour.MatrixOrange;
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        IO.WriteLn();
        //Display.ForeColour = Colour.Black;
        EchoToDisplay = false;
    }
    
    DrawTextScroll()
    {
        IO.WriteLn("  DrawText");
        EchoToDisplay = true;
        Display.Clear();
        for (uint i = 0; i < 100; i++)
        {
            switch (i % 4)
            {
                case 0: { Display.ForeColour = Colour.White; }
                case 1: { Display.ForeColour = Colour.Red; }
                case 2: { Display.ForeColour = Colour.Green; }
                case 3: { Display.ForeColour = Colour.Blue; }
            }
            IO.WriteLn(i.ToString());
        }
        EchoToDisplay = false;
    }
    
    DrawRGB()
    {
        IO.WriteLn("  DrawRGB");
        int pw3  = Display.PixelWidth/3;
        int pw23 = Display.PixelWidth*2/3;
        int w = pw3;
        if (Display.PixelWidth % 3 != 0)
        {
            w++;
        }
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.Red);
        FilledRectangle(pw3,  0, w, Display.PixelHeight, Colour.Green);
        FilledRectangle(pw23, 0, w, Display.PixelHeight, Colour.Blue);
    }
    DrawCMYK()
    {
        IO.WriteLn("  DrawCMYK");
        int pw2  = Display.PixelWidth/2;
        int pw34 = Display.PixelWidth*3/4;
        int pw4  = Display.PixelWidth/4;
        int w = pw4;
        if (Display.PixelWidth % 4 != 0)
        {
            w++;
        }
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.Cyan);
        FilledRectangle(pw4,  0, w, Display.PixelHeight, Colour.Magenta);
        FilledRectangle(pw2,  0, w, Display.PixelHeight, Colour.Yellow);
        FilledRectangle(pw34, 0, w, Display.PixelHeight, Colour.Black);
    }
    
    DrawShades()
    {
        IO.WriteLn("  DrawShades");
        int pw2  = Display.PixelWidth/2;
        int pw34 = Display.PixelWidth*3/4;
        int pw4  = Display.PixelWidth/4;
        int w = pw4;
        if (Display.PixelWidth % 4 != 0)
        {
            w++;
        }
        Display.Suspend();
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.White);
        FilledRectangle(pw4,  0, w, Display.PixelHeight, Colour.LightGray);
        FilledRectangle(pw2,  0, w, Display.PixelHeight, Colour.DarkGray);
        FilledRectangle(pw34, 0, w, Display.PixelHeight, Colour.Black);
        Display.Resume(); 
    }
    
    testText()
    {
        Display.BackColour = Colour.Black;
        Display.Clear();
        
        long start = Time.Millis;
        Display.ForeColour = Colour.White;
        Display.PrintLn("Hello World!");
        Display.ForeColour = Colour.Yellow; Display.TextScale = 2;
        Display.PrintLn((1234.56).ToString());
        Display.ForeColour = Colour.Red;  Display.TextScale = 3;
        Display.PrintLn((0xDEAD).ToHexString(4) + (0xBEEF).ToHexString(4)); // Hopper only has 'uint' or signed 'long'
        Display.PrintLn();
        Display.ForeColour = Colour.Green;  Display.TextScale = 5;
        Display.PrintLn("Groop");
        Display.TextScale = 2;
        Display.PrintLn("I implore thee,");
        Display.TextScale = 1;
        Display.PrintLn("my foonting turlingdromes.");
        Display.PrintLn("And hooptiously drangle me");
        Display.PrintLn("with crinkly bindlewurdles,");
        Display.PrintLn("Or I will rend thee");
        Display.PrintLn("in the gobberwarts");
        Display.PrintLn("with my blurglecruncheon,");
        Display.PrintLn("see if I don't!");
        
        IO.WriteLn(("Text").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testFillScreen() 
    {
        long start = Time.Millis;
        Display.Clear(Colour.Black);
        Display.Clear(Colour.Red);
        Display.Clear(Colour.Green);
        Display.Clear(Colour.Blue);
        Display.Clear(Colour.Black);
        IO.WriteLn(("Screen fill").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testLines(uint colour) 
    {
        int w = Display.PixelWidth;
        int h = Display.PixelHeight;

        long start = Time.Millis;
        Display.Clear(Colour.Black);
        long clearTime = Time.Millis - start; // clear time doesn't count
        byte cleared = 1;
        
        int x1;
        int y1;
        int y2 = h - 1;
        int x2;
        
        for(x2=0; x2<w; x2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        
        x2 = w - 1;
        for(y2=0; y2<h; y2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        
        Display.Clear(Colour.Black);
        cleared++;
        
        x1    = w - 1;
        y1    = 0;
        y2    = h - 1;
        for(x2=0; x2<w; x2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        x2    = 0;
        for(y2=0; y2<h; y2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        
        Display.Clear(Colour.Black);
        cleared++;
        
        x1    = 0;
        y1    = h - 1;
        y2    = 0;
        for(x2=0; x2<w; x2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        x2    = w - 1;
        for(y2=0; y2<h; y2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        
        Display.Clear(Colour.Black);
        cleared++;
        
        x1    = w - 1;
        y1    = h - 1;
        y2    = 0;
        for(x2=0; x2<w; x2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        x2    = 0;
        for(y2=0; y2<h; y2+=6) { Display.Line(x1, y1, x2, y2, colour);}
        
        IO.WriteLn(("Lines").Pad(' ', 25) + (Time.Millis - start - (cleared * clearTime)).ToString());
    }

    testFastLines(uint colour1, uint colour2) 
    {
        int w = Display.PixelWidth;
        int h = Display.PixelHeight;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int y=0; y<h; y+=5) { Display.HorizontalLine(0, y, w, colour1);}
        for(int x=0; x<w; x+=5) { Display.VerticalLine(x, 0, h, colour2);}
        
        IO.WriteLn(("Horiz/Vert Lines").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    
    testRects(uint colour) 
    {
        int cx = Display.PixelWidth  / 2;
        int cy = Display.PixelHeight / 2;

        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        int i2;
        int n = Int.Min(Display.PixelWidth, Display.PixelHeight);
        for(int i=2; i<n; i+=6) 
        {
            i2 = i / 2;
            Display.Rectangle(cx-i2, cy-i2, i, i, colour);
        }

        IO.WriteLn(("Rectangles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }

    testFilledRects(uint colour1, uint colour2)
    {
        int cx = Display.PixelWidth  / 2 - 1;
        int cy = Display.PixelHeight / 2 - 1;

        
        Display.Clear(Colour.Black);
        long elapsed;
        
        int i2;
        int n = Int.Min(Display.PixelWidth, Display.PixelHeight);
        for(int i=n; i>0; i-=6) 
        {
            i2 = i / 2;
            long start = Time.Millis;
            Display.FilledRectangle(cx-i2, cy-i2, i, i, colour1);
            elapsed += Time.Millis - start;
            
            // Outlines are not included in timing results
            Display.Rectangle(cx-i2, cy-i2, i, i, colour2);
        }

        IO.WriteLn(("Rectangles (filled)").Pad(' ', 25) + elapsed.ToString());
    }
    
    testTriangles() 
    {
        int cx = Display.PixelWidth  / 2 - 1;
        int cy = Display.PixelHeight / 2 - 1;
        int n = Int.Min(cx, cy);
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int i=0; i<n; i+=5) 
        {
            byte colourComponent = (byte(i) & 0x3F) >> 2;
            uint colour = (colourComponent << 8) + (colourComponent << 4) + colourComponent;
            Display.Triangle(cx, cy-i, cx-i, cy+i, cx+i, cy+i, colour);
        }
        IO.WriteLn(("Triangles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());
    }
    testFilledTriangles()
    {
        int cx = Display.PixelWidth  / 2 - 1;
        int cy = Display.PixelHeight / 2 - 1;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int i=Int.Min(cx, cy); i>10; i-=5) 
        {
            byte colourComponent = (byte(i*10) & 0x3F) >> 2;

            uint colour = (colourComponent << 4) + colourComponent;
            Display.FilledTriangle(cx, cy - i, cx - i, cy + i, cx + i, cy + i,colour);
            
            colour = (colourComponent << 8) + (colourComponent << 4);
            Display.FilledTriangle(cx, cy - i, cx - i, cy + i, cx + i, cy + i, colour);
        }
        IO.WriteLn(("Triangles (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());
    } 
    
    testCircles(byte radius, uint colour)
    {
        int r2 = radius * 2;
        int w  = Display.PixelWidth  + radius;
        int h  = Display.PixelHeight + radius;
        
        long start = Time.Millis;
        
        for(int x=0; x<w; x+=r2) 
        {
            for(int y=0; y<h; y+=r2) 
            {
                Display.Circle(x, y, radius, colour);
            }
        }
     
        IO.WriteLn(("Circles (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    testFilledCircles(byte radius, uint colour)
    {
        int r2 = radius * 2;
        int w  = Display.PixelWidth;
        int h  = Display.PixelHeight;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for(int x=radius; x<w; x+=r2) 
        {
            for(int y=radius; y<h; y+=r2) 
            {
                Display.FilledCircle(x, y, radius, colour);
            }
        }
        IO.WriteLn(("Circles (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    
    testRoundedRectangles()
    {
        int cx = Display.PixelWidth  / 2 - 1;
        int cy = Display.PixelHeight / 2 - 1;
        int w  = Int.Min(Display.PixelWidth, Display.PixelHeight);
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for (int i=0; i<w; i+=6) 
        {
            int i2 = i / 2;
            byte colourComponent = (byte(i) & 0x3F) >> 2;
            uint colour = (colourComponent << 8);
            Display.RoundedRectangle(cx-i2, cy-i2, i, i, i/8, colour);
        }
        
        IO.WriteLn(("Rounded rects (outline)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
    testFilledRoundedRectangles()
    {
        int cx = Display.PixelWidth  / 2 - 1;
        int cy = Display.PixelHeight / 2 - 1;
        
        Display.Clear(Colour.Black);
        long start = Time.Millis;
        
        for (int i=Int.Min(Display.PixelWidth, Display.PixelHeight); i>20; i-=6)
        {
            int i2 = i / 2;
            byte colourComponent = (byte(i) & 0x3F) >> 2;
            uint colour = (colourComponent << 4);
            Display.FilledRoundedRectangle(cx-i2, cy-i2, i, i, i/8, colour);
        }
        
        IO.WriteLn(("Rounded rects (filled)").Pad(' ', 25) + (Time.Millis - start).ToString());      
    }
        
    Hopper()
    {

#if defined(RPIPICO2W)
        // Adafruit240x135ColorTFT
        DeviceDriver.CS   = Board.SPI0SS;
        DeviceDriver.DC   = Board.GP12;
        IsPortrait = true;
        FlipX = true;
        FlipY = true;
#endif        
#if defined(RPI)
        // Adafruit240x135ColorTFT
        DeviceDriver.CS   = Board.SPI0SS;
        DeviceDriver.DC   = Board.GP12;
        IsPortrait = true;
        FlipX = true;
        FlipY = true;
#endif
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
              
        IO.WriteLn("Columns: " + (Display.Columns).ToString() + ", Rows: " + (Display.Rows).ToString());
        loop
        {
            IO.WriteLn(("Benchmark").Pad(' ', 25) + "Time (milliseconds)");
            testFillScreen();
            Delay(500);
            testText();
            Delay(3000);
            testLines(Colour.Cyan);
            Delay(500);
            testFastLines(Colour.Red, Colour.Blue);
            Delay(500);
            testRects(Colour.Green);
            Delay(500);
            testFilledRects(Colour.Yellow, Colour.Magenta);
            Delay(500);
            testFilledCircles(10, Colour.Magenta);  
            Delay(500);
            testCircles(10, Colour.White);
            Delay(500);
            testTriangles();
            Delay(500);
            testFilledTriangles();
            Delay(500);
            testRoundedRectangles();
            Delay(500);
            testFilledRoundedRectangles();
            Delay(500);
            IO.WriteLn("Done!");
   
            /*
            DrawRGB();
            DelaySeconds(1);
            
            DrawCMYK();
            DelaySeconds(1);
            
            DrawShades();
            DelaySeconds(1);
            DrawText();
            DelaySeconds(1);
            break;
            */
            //DrawTextScroll();           
        }
    }
}
