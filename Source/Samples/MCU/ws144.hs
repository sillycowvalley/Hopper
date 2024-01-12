program WaveShareLCD144Demo
{
    uses "/Source/Library/Devices/WSPicoLCD144"
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        IO.WriteLn("    Button " + (PinToButton(pin)).ToString()); 
    }
    
    {
        // Setup code:
        ISRDelegate buttonDelegate = ButtonISR;
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
            WriteLn("Laps: " + laps.ToString());
            laps++;
            
            start = Millis;
            Display.Clear(Color.Black);
            IO.WriteLn("Black Screen: " + (Millis - start).ToString());
            
            start = Millis;
            
            start = Millis;
            Display.Clear(Color.Red);
            IO.WriteLn("Red Screen: " + (Millis - start).ToString());
            
            start = Millis;
            Display.Clear(Color.Green);
            IO.WriteLn("Green Screen: " + (Millis - start).ToString());
            
            start = Millis;
            Display.Clear(Color.Blue);
            IO.WriteLn("Blue Screen: " + (Millis - start).ToString());
            
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
            FilledRectangle(i, i, uint(pw-i*2), uint(ph-i*2), (i/3 % 3 == 0) ? Color.Blue : (i/3 % 3 == 1) ? Color.Red : Color.Green);
        }
    }
}
