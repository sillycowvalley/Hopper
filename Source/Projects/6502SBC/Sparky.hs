program Sparky
{
    uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    
    {
        PinMode(GP2, PinModeOption.Output);
        PinMode(GP4, PinModeOption.Output);
        NeoPixel.BuiltIn();
        
        // For 25 cycles, reset is asserted low.
        // During reset, the NeoPixel blinks the
        // clock ticks as purple. After reset,
        // it blinks blue.
        
        DigitalWrite(GP4, false); // reset ..
        uint resetCycles = 25;
        uint delay = 50;
        byte reset = 255;
        loop
        {
            NeoPixel.SetColor(0, reset, 0, 255);
            NeoPixel.Show();
            DigitalWrite(GP2, true);
            Delay(delay);
            
            NeoPixel.SetColor(0, 0, 0, 0);
            NeoPixel.Show();
            DigitalWrite(GP2, false);
            Delay(delay);
            if (resetCycles != 0)
            {
                resetCycles--;
                if (resetCycles == 0)
                {
                    DigitalWrite(GP4, true);
                    delay = 50;
                    reset = 0;
                }
            }
        }
    }
}
