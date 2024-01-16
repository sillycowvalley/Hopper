unit DeviceDriver
{
    // https://www.waveshare.com/wiki/RP2040-LCD-0.96
    // Note: Color.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
    #define RP2040_PICO 
    #define WAVESHARE_RP2040_LCD_096
    
    uses "/Source/Library/Displays/ST7735Driver"
    
    const int PixelWidth  = 80;
    const int PixelHeight = 160;
    
    const byte SPIController = 1; // this device uses SPI1 on Raspberry Pi Pico
    const byte DCPin   = 8;
    const byte CSPin   = 9;
    const byte ClkPin  = 10;
    const byte TxPin   = 11;
    const byte RstPin  = 12;
    const byte BlPin   = 13;
    
    const int XFudge = 26;
    const int YFudge = 1;
    
    bool Begin()
    {
        return Display.Begin();
    }
}