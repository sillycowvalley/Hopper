unit DeviceDriver
{
    // https://www.waveshare.com/wiki/RP2040-LCD-0.96
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
    #define BUFFER_TEXT
    #define ST77XX_CONTROLLER
    
    uses "/Source/Library/Boards/WaveshareRP2040LCD096"
    
    uses "/Source/Library/Displays/TFTDriver"
    
    friend DisplayDriver;
    
    const int pw = 160;
    const int ph = 80;
    
    const byte spiController = 1; // this device uses SPI1 on Raspberry Pi Pico
    const byte dcPin   = 8;
    const byte csPin   = 9;
    const byte clkPin  = 10;
    const byte txPin   = 11;
    const int  rstPin  = 12;
    const byte blPin   = 13;
    
    int xFudge;
    int yFudge;
    
    byte getMAD()
    {
        byte madArgument = MADCTL_BGR;
        if (DisplayDriver.IsPortrait)
        {
            if (FlipX)
            {
                madArgument |= MADCTL_MX;
            }
            if (FlipY)
            {
                madArgument |= MADCTL_MY;
            }
            xFudge = 26;
            yFudge = 1;
        }
        else
        {
            madArgument |= MADCTL_MV;
            if (!FlipX)
            {
                madArgument |= MADCTL_MY;
            }
            if (FlipY)
            {
                madArgument |= MADCTL_MX;
            }
            xFudge = 1;
            yFudge = 26;
    
        }
        return madArgument;
    }
    
    bool Begin()
    {
        return Display.Begin();
    }
}
