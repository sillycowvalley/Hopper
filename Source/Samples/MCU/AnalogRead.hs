program AnalogRead
{
    #define SERIAL_CONSOLE
    #define RP2040_PICOW
    uses "/Source/Library/MCU"
    
    {
        loop
        {
            uint x = A0;
            uint y = A1;
            
            WriteLn(x.ToString() + ", " + y.ToString());
            Delay(50);
        }
    }
}
