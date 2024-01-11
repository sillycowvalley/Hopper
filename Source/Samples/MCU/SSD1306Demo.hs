program SSD1306Demo
{
    //#define PIMORONI_TINY2040
    #define RP2040_PICOW
    
    //#define DISPLAY_DIAGNOSTICS
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    TestDrawRect()
    {
        Screen.Clear();
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i< ph / 2; i += 2)
        {
            Rectangle(i, i, uint(pw-2*i), uint(ph-2*i), Color.White);
        }
    }

    TestFillRect()
    {
        Screen.Clear();
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
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        Display.Suspend();
        Screen.Clear();
        for(i=0; i<pw; i += 4) 
        {
            Line(0, 0, i, ph-1, Color.White);
        }
        for(i=0; i< ph; i += 4) 
        {
            Line(0, 0, pw-1, i, Color.White);
        }
        Display.Resume();
        
        Display.Suspend();
        Screen.Clear();
        for(i=0; i< pw; i += 4)
        {
            Line(0, ph-1, i, 0, Color.Red);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(0, ph-1, pw-1, i, Color.Red);
        }
        Display.Resume();
        
        Display.Suspend(); 
        Screen.Clear();
        for(i= pw-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, i, 0, Color.Green);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, 0, i, Color.Green);
        }
        Display.Resume();

        Display.Suspend();
        Screen.Clear();
        for(i=0; i< ph; i += 4)
        {
            Line(pw-1, 0, 0, i, Color.Blue);
        }
        for(i=0; i< pw; i += 4)
        {
            Line(pw-1, 0, i, ph-1, Color.Blue);
        }
        Display.Resume();
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
        long laps;
        
        Screen.Clear();
        loop
        {
            Display.Suspend();
            WriteLn("Laps: " + laps.ToString());
            Display.Resume();
            laps++;
            start = Millis;
            Display.Clear(Color.Black);
            elapsed = Millis - start;
            Display.Suspend();
            WriteLn("Clear: " + elapsed.ToString());
            Display.Resume();
            Delay(1000);
            
            start = Millis;
            TestDrawRect();
            elapsed = Millis - start;
            Display.Suspend();
            WriteLn("TestDrawRect: " + elapsed.ToString());
            Display.Resume();
            Delay(1000);
            
            start = Millis;
            TestFillRect();   
            elapsed = Millis - start;
            Display.Suspend();
            WriteLn("TestFillRect: " + elapsed.ToString());
            Display.Resume();
            Delay(1000);
            
            start = Millis;
            TestDrawLines();
            elapsed = Millis - start;
            Display.Suspend();
            WriteLn("TestDrawLines: " + elapsed.ToString());
            Display.Resume();
            Delay(1000);
        }
    }
}
