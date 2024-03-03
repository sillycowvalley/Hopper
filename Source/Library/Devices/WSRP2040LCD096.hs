unit DeviceDriver
{
    // https://www.waveshare.com/wiki/RP2040-LCD-0.96
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
    #define HAS_RESET_PIN
    #define BUFFER_TEXT
    
    uses "/Source/Library/Boards/WaveshareRP2040LCD096"
    uses "/Source/Library/Displays/ST77XXDriver"
    
    friend DisplayDriver;
    
    const int pw  = 80;
    const int ph = 160;
    
    const byte spiController = 1; // this device uses SPI1 on Raspberry Pi Pico
    const byte dcPin   = 8;
    const byte csPin   = 9;
    const byte clkPin  = 10;
    const byte txPin   = 11;
    const byte rstPin  = 12;
    const byte blPin   = 13;
    
    const int xFudge = 26;
    const int yFudge = 1;
    
    bool Begin()
    {
        return Display.Begin();
    }
}
