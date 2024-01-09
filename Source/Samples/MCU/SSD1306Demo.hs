program SSD1306Demo
{
    #define RP2040PICOW
    
    //#define DISPLAYDIAGNOSTICS
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    TestDrawRect()
    {
        Clear();
        for(int i=0; i< PixelHeight / 2; i += 2)
        {
            Rectangle(i, i, PixelWidth-2*i,PixelHeight-2*i, Color.White);
        }
    }

    TestFillRect()
    {
        Clear();
        for(int i=0; i<PixelHeight / 2; i += 3)
        {
            // Color.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, PixelWidth-i*2, PixelHeight-i*2, Color.Invert);
        }
    }
    
    TestDrawLines() 
    {
        int i;
        Clear();

        for(i=0; i<PixelWidth; i += 4) 
        {
            Line(0, 0, i, PixelHeight-1, Color.White);
        }
        for(i=0; i< PixelHeight; i += 4) 
        {
            Line(0, 0, PixelWidth-1, i, Color.White);
        }

        Clear();

        for(i=0; i< PixelWidth; i += 4)
        {
            Line(0, PixelHeight-1, i, 0, Color.Red);
        }
        for(i= PixelHeight-1; i>=0; i -= 4)
        {
            Line(0, PixelHeight-1, PixelWidth-1, i, Color.Red);
        }
 
        Clear();

        for(i= PixelWidth-1; i>=0; i -= 4)
        {
            Line(PixelWidth-1, PixelHeight-1, i, 0, Color.Green);
        }
        for(i= PixelHeight-1; i>=0; i -= 4)
        {
            Line(PixelWidth-1, PixelHeight-1, 0, i, Color.Green);
        }

        Clear();

        for(i=0; i< PixelHeight; i += 4)
        {
            Line(PixelWidth-1, 0, 0, i, Color.Blue);
        }
        for(i=0; i< PixelWidth; i += 4)
        {
            Line(PixelWidth-1, 0, i, PixelHeight-1, Color.Blue);
        }
    }
    
    
    {
        Display.I2CAddress = 0x3C;
        
        Display.I2CController = 1;
        Display.I2CSDAPin = 14;
        Display.I2CSCLPin = 15;
        
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        Display.Clear();
        loop
        {
            TestDrawLines();
            TestDrawRect();
            TestFillRect();        
        }
    }
}
