program TestGPIO
{
    //uses "/Source/Library/Boards/AdafruitFeather"
    //uses "/Source/Library/Boards/AdafruitItsyBitsy"
    //uses "/Source/Library/Boards/AdafruitQTPy"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    
    //uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    
    //uses "/Source/Library/Boards/PimoroniTiny2040"
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    Hopper()
    {
        byte pin = GP0;
        PinMode(pin, PinModeOption.Output);
        
        for (byte iterations = 0; iterations < 10; iterations++)
        {
            Write(pin.ToString() + " ");
            DigitalWrite(pin, true);
            Delay(250);    
            DigitalWrite(pin, false);
            Delay(250);    
        }
        
        WriteLn();
#ifdef BOARD_HAS_USER_BUTTON     
        PinMode(UserButton, PinModeOption.Input);
        loop
        {
            bool button = DigitalRead(UserButton);
            WriteLn(button.ToString());
            DigitalWrite(BuiltInLED, button);
            Delay(300);
        }
#endif    
    }
}
