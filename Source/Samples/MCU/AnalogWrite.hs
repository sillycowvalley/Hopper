program AnalogWrite
{
    #define SERIALCONSOLE
    #define RP2040PICOW
    uses "/Source/Library/MCU"
    
    const byte ledA = 15;
    const byte ledB = 16;
    {
        AnalogWriteResolution(10);
        uint fade;
        bool flip;
        loop
        {
            fade = (flip ? fade - 1 : fade + 1);
            if ((fade & 0x03FF) == 0x0000)
            {
                flip = !flip;
                fade = (flip ? fade - 1 : fade + 1);   
            }
            AnalogWrite(ledA, fade);
            AnalogWrite(ledB, 0x03FF-fade);
            Delay(5);
        }
    }
}
