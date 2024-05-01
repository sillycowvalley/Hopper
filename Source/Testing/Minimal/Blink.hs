program Blink
{
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/MCU"
    
    Hopper()
    {
        PinMode(GP0, PinModeOption.Output);
        PinMode(GP8, PinModeOption.Output);
        loop
        {
            DigitalWrite(GP0, true);
            DigitalWrite(GP8, false);
            Delay(500);
            DigitalWrite(GP0, false);
            DigitalWrite(GP8, true);
            Delay(500);
        }
    }
}
