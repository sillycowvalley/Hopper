program LEDSweep
{
    uses "/Source/Library/Boards/CytronMakerNanoRP2040"
    
    const byte[] ledPins = {2, 3, 4, 5, 6, 7, 8, 9, 17, 19, 16};
    const uint delayTime = 100; // Delay in milliseconds

    InitializeLEDs()
    {
        foreach (var pin in (ledPins))
        {
            MCU.PinMode(pin, MCU.PinModeOption.Output);
            MCU.DigitalWrite(pin, false);
        }
    }
    
    SweepLEDs()
    {
        byte[] pinsArray = ledPins;
        loop
        {
            // Sweep from left to right
            foreach (var pin in pinsArray)
            {
                MCU.DigitalWrite(pin, true);
                Time.Delay(delayTime);
                MCU.DigitalWrite(pin, false);
            }
            
            // Sweep from right to left
            for (byte i = byte(pinsArray.Count); i > 0; i--)
            {
                MCU.DigitalWrite(pinsArray[i-1], true);
                Time.Delay(delayTime);
                MCU.DigitalWrite(pinsArray[i-1], false);
            }
        }
    }

    Hopper()
    {
        InitializeLEDs();
        loop
        {
            SweepLEDs();
        }
    }
}

