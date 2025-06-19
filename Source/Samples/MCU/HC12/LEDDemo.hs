program LEDDemo
{
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    Tiny2350LEDOff()
    {
        // Pimoroni Tiny 2350 built in RGB is active low
        MCU.PinMode(GP18,  PinModeOption.Output);
        MCU.PinMode(GP19,  PinModeOption.Output);
        MCU.PinMode(GP20,  PinModeOption.Output);
        MCU.DigitalWrite(GP18, true);
        MCU.DigitalWrite(GP19, true);
        MCU.DigitalWrite(GP20, true);
    }
    
    Hopper()
    {
        Tiny2350LEDOff();
        
        PinMode(GP5, PinModeOption.Output);
        PinMode(GP6, PinModeOption.Output);
        PinMode(GP7, PinModeOption.Output);
        
        DigitalWrite(GP5, false);
        DigitalWrite(GP6, false);
        DigitalWrite(GP7, false);
        
        loop
        {
            DigitalWrite(GP5, true);
            Delay(500);
            DigitalWrite(GP5, false);    
            DigitalWrite(GP6, true);
            Delay(500);
            DigitalWrite(GP6, false);    
            DigitalWrite(GP7, true);
            Delay(500);
            DigitalWrite(GP7, false);    
        }
    }
}
