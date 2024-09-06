program WaveShareLCD144Demo
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Devices/WSPicoLCD144"
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
    }
    
    {
        EchoToLCD = true;
        
        // Setup code:
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-LCD-1.44");
            return;
        }
        
        // Demo code:
                
        long start;
        long laps;
        loop
        {
            Screen.SetCursor(0,0);
            WriteLn("Laps: " + laps.ToString());
            laps++;
            
            start = Millis;
            Display.Clear(Colour.Black);
            IO.WriteLn("Black Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            Display.Clear(Colour.Red);
            IO.WriteLn("Red Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            Display.Clear(Colour.Green);
            IO.WriteLn("Green Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            Display.Clear(Colour.Blue);
            IO.WriteLn("Blue Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            TestFillRect();
            IO.WriteLn("FilledRectangle: " + (Millis - start).ToString());
            
        }
    }
    
    TestFillRect()
    {
        int ph = int(Display.PixelHeight);
        int pw = int(Display.PixelWidth);
        for(int i=0; i<ph / 2; i += 3)
        {
            FilledRectangle(i, i, pw-i*2, ph-i*2, (i/3 % 3 == 0) ? Colour.Blue : (i/3 % 3 == 1) ? Colour.Red : Colour.Green);
        }
    }
}
