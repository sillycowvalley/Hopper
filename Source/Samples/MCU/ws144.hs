program WaveShareLCD144Demo
{
    #define RP2040_PICOW
    
    uses "/Source/Library/Displays/WSPicoLCD144"
    
    {
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        long start;
        long elapsed;
        long laps;
        
        loop
        {
            WriteLn("Laps: " + laps.ToString());
            laps++;
            
            start = Millis;
            Display.Clear(Color.Black);
            elapsed = Millis - start;
            IO.WriteLn("Black Screen: " + elapsed.ToString());
            //Delay(250);
            
            start = Millis;
            Display.Clear(Color.Red);
            elapsed = Millis - start;
            IO.WriteLn("Red Screen: " + elapsed.ToString());
            //Delay(250);
            
            start = Millis;
            Display.Clear(Color.Green);
            elapsed = Millis - start;
            IO.WriteLn("Green Screen: " + elapsed.ToString());
            //Delay(250);
            
            start = Millis;
            Display.Clear(Color.Blue);
            elapsed = Millis - start;
            IO.WriteLn("Blue Screen: " + elapsed.ToString());
            //Delay(250);
            
        }
    }
}
