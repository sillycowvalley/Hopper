program SSD1306Demo
{
    //#define TINY2040
    #define RP2040PICOW
    
    //#define DISPLAYDIAGNOSTICS
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Screen.hs"
    
    TestDrawRect()
    {
        Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i< ph / 2; i += 2)
        {
            Rectangle(i, i, uint(pw-2*i), uint(ph-2*i), Color.White);
        }
    }

    TestFillRect()
    {
        Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i<ph / 2; i += 3)
        {
            // Color.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, uint(pw-i*2), uint(ph-i*2), Color.Invert);
        }
    }
    
    TestDrawLines() 
    {
        int i;
        Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(i=0; i<pw; i += 4) 
        {
            Line(0, 0, i, ph-1, Color.White);
        }
        for(i=0; i< ph; i += 4) 
        {
            Line(0, 0, pw-1, i, Color.White);
        }

        Clear();

        for(i=0; i< pw; i += 4)
        {
            Line(0, ph-1, i, 0, Color.Red);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(0, ph-1, pw-1, i, Color.Red);
        }
 
        Clear();

        for(i= pw-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, i, 0, Color.Green);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, 0, i, Color.Green);
        }

        Clear();

        for(i=0; i< ph; i += 4)
        {
            Line(pw-1, 0, 0, i, Color.Blue);
        }
        for(i=0; i< pw; i += 4)
        {
            Line(pw-1, 0, i, ph-1, Color.Blue);
        }
    }
    
    
    {
        EchoToLCD = true;
        Screen.FontData = HitachiFont5x7.Data;
        
        Display.I2CAddress = 0x3C;
        
        Display.I2CController = 1;
        Display.I2CSDAPin = 14;
        Display.I2CSCLPin = 15;
        
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        long start;
        long elapsed;
        
        Display.Clear();
        loop
        {
            start = Millis;
            Display.Clear();
            elapsed = Millis - start;
            WriteLn("Clear: " + elapsed.ToString());
            Delay(300);
            
            start = Millis;
            TestDrawRect();
            elapsed = Millis - start;
            WriteLn("TestDrawRect: " + elapsed.ToString());
            Delay(300);
            
            start = Millis;
            TestFillRect();        
            elapsed = Millis - start;
            WriteLn("TestFillRect: " + elapsed.ToString());
            Delay(300);
 
            continue;           
            
            start = Millis;
            TestDrawLines();
            elapsed = Millis - start;
            WriteLn("TestDrawLines: " + elapsed.ToString());
        }
    }
}
