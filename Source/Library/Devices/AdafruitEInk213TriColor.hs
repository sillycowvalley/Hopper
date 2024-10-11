unit DeviceDriver
{
#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather" // avoid modifying the automatically generated Board file
#endif

    #define EPD_TWO_BUFFERS
    
    friend DisplayDriver;
    
    const int pw = 250;
    const int ph = 122;
    
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    #define EPD_NO_RX
    
    const byte sdCS  = Board.GP7; // SD
    const byte srCS  = Board.GP8; // SRAM
    const byte csPin = Board.GP9; // E-Ink
    
    const byte dcPin  = Board.GP10;
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
        // https://learn.adafruit.com/adafruit-eink-display-breakouts/pinouts-2
        // Settings for Hopper SD unit:
        SD.SPIController = 0;
        SD.ClkPin = Board.SPI0SCK;
        SD.TxPin  = Board.SPI0Tx;
        SD.RxPin  = Board.SPI0Rx;
        SD.CSPin  = sdCS; 
        
        return Display.Begin();
    }
    
}
