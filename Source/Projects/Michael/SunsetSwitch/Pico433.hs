program Pico433
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        PinMode(GP16, PinModeOption.Output);
        
        loop
        {
            Delay(500);
            DigitalWrite(GP16, true);
            Delay(500);
            DigitalWrite(GP16, false);
        }
    }
}
