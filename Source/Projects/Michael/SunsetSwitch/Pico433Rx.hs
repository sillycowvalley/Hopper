program Pico433Rx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        PinMode(GP16, PinModeOption.Input);
        PinMode(GP17, PinModeOption.Output);
        
        DigitalWrite(GP16, false);
        
        loop
        {
            bool signal = DigitalRead(GP16);
            DigitalWrite(GP17, signal);
        }
    }
}
