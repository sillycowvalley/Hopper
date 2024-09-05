program TestGPIO
{
    //uses "/Source/Library/Boards/AdafruitFeather"
    //uses "/Source/Library/Boards/AdafruitItsyBitsy"
    //uses "/Source/Library/Boards/AdafruitQTPy"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    
    uses "/Source/Library/Boards/SparkfunProMicroRP2040"
        
    Hopper()
    {
        
        byte pin = GP21;
        PinMode(pin, PinModeOption.Output);
        
        /*        
        byte button = GP13;
        PinMode(pin, PinModeOption.InputPulldown);
        loop
        {
            DigitalWrite(pin, DigitalRead(button));
        }
        */
        
        for (byte iterations = 0; iterations < 10; iterations++)
        {
            Write(pin.ToString() + " ");
            DigitalWrite(pin, true);
            Delay(250);    
            DigitalWrite(pin, false);
            Delay(250);    
        }
        WriteLn();
    
    }
}
