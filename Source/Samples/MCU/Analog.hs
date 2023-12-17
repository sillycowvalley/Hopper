program Analog
{
    const byte ledA = 15;
    const byte ledB = 16;
    
    const byte button = 22;
    
    #define RP2040
    uses "/Source/Library/MCU"
    
    {
        PinMode(button, PinModeOption.InputPullup);
        AnalogWriteResolution(10);
        loop
        {
            uint x = A0;
            uint y = A1;
            
            WriteLn(x.ToString() + ", " + y.ToString() + (DigitalRead(button) ? " Up" : " Down"));
            
            AnalogWrite(ledA, x);
            AnalogWrite(ledB, y);
            Delay(50);
        }
    }
}
