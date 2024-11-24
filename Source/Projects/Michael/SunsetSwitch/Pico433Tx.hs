program Pico433Tx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        PinMode(GP16, PinModeOption.Output);
        PinMode(GP17, PinModeOption.Output);
        
        DigitalWrite(GP16, false);
        
        loop
        {
            DigitalWrite(GP16, true);
            DigitalWrite(GP17, true);
            Delay(50);
            DigitalWrite(GP16, false);
            DigitalWrite(GP17, false);
            Delay(950);
            
        }
    }
}
