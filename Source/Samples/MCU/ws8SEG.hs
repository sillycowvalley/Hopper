program Waveshare8SEGLED
{
    uses "/Source/Library/Devices/WSPico8SEGLED"
    StopWatch()
    {
        Pico8SEGLED.Hex = false;
        Pico8SEGLED.DecimalPosition = 0;
        Pico8SEGLED.LeadingZeroes = false;
        for (long tick = 0; tick < 99999; tick++)
        {
            for (uint i = 1; i < 100; i ++)
            {
                Pico8SEGLED.Show(tick);
                // Show(..) takes 8ms so +2ms gives us a 1/100 second counter
                Time.Delay(2); 
            }
        }
    }
    HexCounter()
    {
        Pico8SEGLED.Hex = true;
        Pico8SEGLED.DecimalPosition = 0;
        Pico8SEGLED.LeadingZeroes = false;
        for (long value = 0; value <= 0xFFFF; value++)
        {
            Pico8SEGLED.Show(value);
            // Show(..) takes 8ms so +2ms gives us a 1/100 second counter
            Time.Delay(2); 
        }
    }
    
    {
        if (!Pico8SEGLED.Begin())
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-8SEG-LED");
            return;
        }
        loop
        {
            StopWatch();
            //HexCounter();
        }
    }
}
