unit DeviceDriver
{
    #define EPD_TWO_BUFFERS
    
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    friend DisplayDriver;
    
    const int pw = 250;
    const int ph = 122;
    
    #define EPD_NO_RX
    
    const byte dcPin  = Board.GP10;
    const byte csPin  = Board.GP9;
    const byte clkPin = Board.SPI0SCK;
    const byte txPin  = Board.SPI0Tx;
    const byte spiController  = 0;
    
    const uint defaultRefreshDelay   = 16; // seconds
    
    const bool epdBlack0 =  true;
    const bool epdRed0   = false;
    const bool epdWhite0 = false;
    
    const bool epdBlack1 =  true;
    const bool epdRed1   = false;
    const bool epdWhite1 =  true;
    
    bool Begin()
    {
        return Display.Begin();
    }
    
}
