program Sparky
{
    uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    
    {
        PinMode(GP2, PinModeOption.Output);
        PinMode(GP4, PinModeOption.Output);
        NeoPixel.BuiltIn();
        
        DigitalWrite(GP4, false); // reset ..
        
        uint resetCycles = 25;
        uint delay = 1;
        loop
        {
            NeoPixel.SetColor(0, 0, 255, 0);
            NeoPixel.Show();
            DigitalWrite(GP2, true);
            Delay(delay);
            
            NeoPixel.SetColor(0, 0, 0, 255);
            NeoPixel.Show();
            DigitalWrite(GP2, false);
            Delay(delay);
            if (resetCycles != 0)
            {
                resetCycles--;
                if (resetCycles == 0)
                {
                    DigitalWrite(GP4, true);
                    delay = 5;
                }
            }
        }
    }
}
