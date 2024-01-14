program WaveShareRP2040LCD096Demo
{
    uses "/Source/Library/Devices/WSRP2040LCD096"
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    {
        EchoToLCD = true;
        
        // Setup code:
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize Waveshare RP2040-LCD-0.96");
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
            Delay(250);
            
            start = Millis;
            Display.Clear(Color.Red);
            IO.WriteLn("Red Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            Display.Clear(Color.Green);
            IO.WriteLn("Green Screen: " + (Millis - start).ToString());
            Delay(250);
            
            start = Millis;
            Display.Clear(Color.Blue);
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
        for(int i=0; i<pw / 2; i += 3)
        {
            FilledRectangle(i, i, uint(pw-i*2), uint(ph-i*2), (i/3 % 3 == 0) ? Color.Blue : (i/3 % 3 == 1) ? Color.Red : Color.Green);
        }
    }
}
