program SSD1306Demo
{   
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Boards/PiPico2W"
    //uses "/Source/Library/Boards/Pi"
    uses "/Source/Library/Displays/OLEDSSD1306"    
    
    
    TestDrawRect()
    {
        Display.Clear();
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        for(int i=0; i< ph / 2; i += 2)
        {
            Rectangle(i, i, pw-2*i, ph-2*i, Colour.White);
        }
    }

    TestFillRect()
    {
        Display.Clear();
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        for(int i=0; i<ph / 2; i += 3)
        {
            // Colour.Inverse is used so rectangles alternate white/black
            FilledRectangle(i, i, pw-i*2, ph-i*2, Colour.Invert);
        }
    }
    
    TestDrawLines() 
    {
        int i;
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        Display.Suspend();
        Display.Clear();
        for(i=0; i<pw; i += 4) 
        {
            Line(0, 0, i, ph-1, Colour.White);
        }
        for(i=0; i< ph; i += 4) 
        {
            Line(0, 0, pw-1, i, Colour.White);
        }
        Display.Resume();
        
        Display.Suspend();
        Display.Clear();
        for(i=0; i< pw; i += 4)
        {
            Line(0, ph-1, i, 0, Colour.Red);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(0, ph-1, pw-1, i, Colour.Red);
        }
        Display.Resume();
        
        Display.Suspend(); 
        Display.Clear();
        for(i= pw-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, i, 0, Colour.Green);
        }
        for(i= ph-1; i>=0; i -= 4)
        {
            Line(pw-1, ph-1, 0, i, Colour.Green);
        }
        Display.Resume();

        Display.Suspend();
        Display.Clear();
        for(i=0; i< ph; i += 4)
        {
            Line(pw-1, 0, 0, i, Colour.Blue);
        }
        for(i=0; i< pw; i += 4)
        {
            Line(pw-1, 0, i, ph-1, Colour.Blue);
        }
        Display.Resume();
    }
    
    
    Hopper()
    {
        // PiPicoW
        //DisplayDriver.I2CController = 1;
        //DisplayDriver.I2CSDAPin = Board.GP14;
        //DisplayDriver.I2CSCLPin = Board.GP15;

        // Sparkfun ThingPlus RP2040
        //DisplayDriver.I2CController = 1;
        //DisplayDriver.I2CSDAPin = Board.I2CSDA1;
        //DisplayDriver.I2CSCLPin = Board.I2CSCL1;
        
#ifdef BITBANG_I2C
        DisplayDriver.I2CSDAPin = 17;
        DisplayDriver.I2CSCLPin = 16;
#endif        
        
        //IsPortrait = true;
        //FlipX = true;
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        
        long start;
        long elapsed;
        long laps;
        
        Display.Clear();
        loop
        {
            Display.Suspend();
            IO.WriteLn("Laps: ");
            IO.WriteLn(laps.ToString());
            Display.Resume();
            laps++;
            
            start = Millis;
            Display.Clear(Colour.Black);
            elapsed = Millis - start;
            Display.Suspend();
            IO.WriteLn("Clear: ");
            IO.WriteLn(elapsed.ToString());
            Display.Resume();
            Delay(500);
            
            Display.SetCursor(0,0);
            
            start = Millis;
            TestDrawRect();
            elapsed = Millis - start;
            Display.Suspend();
            EchoToDisplay = true;
            IO.WriteLn("TestDrawRect: ");
            EchoToDisplay = false;
            IO.WriteLn(elapsed.ToString());
            Display.Resume();
            Delay(500);
            
            start = Millis;
            TestFillRect();   
            elapsed = Millis - start;
            Display.Suspend();
            EchoToDisplay = true;
            IO.WriteLn("TestFillRect: ");
            EchoToDisplay = false;
            IO.WriteLn(elapsed.ToString());
            Display.Resume();
            Delay(500);
            
            start = Millis;
            TestDrawLines();
            elapsed = Millis - start;
            Display.Suspend();
            EchoToDisplay = true;
            IO.WriteLn("TestDrawLines: ");
            EchoToDisplay = false;
            IO.WriteLn(elapsed.ToString());
            Display.Resume();
            Delay(500);
            
            if (IO.IsAvailable)
            {
                break;
            }
        }
    }
}
