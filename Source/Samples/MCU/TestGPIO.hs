program TestGPIO
{
    uses "/Source/Library/Boards/AdafruitFeather"
    uses "/Source/Library/Boards/Challenger2040WiFi"
        
    Hopper()
    {
        
        byte pin = GP13;
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
    
    }
}
