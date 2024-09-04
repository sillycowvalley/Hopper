program TestGPIO
{
    uses "/Source/Library/Boards/AdafruitFeather"
        
    Hopper()
    {
        byte pin = GP16;
        PinMode(pin, PinModeOption.Output);
        
        loop
        {
            DigitalWrite(pin, true);
            Delay(250);    
            DigitalWrite(pin, false);
            Delay(250);    
        }
    }
}
