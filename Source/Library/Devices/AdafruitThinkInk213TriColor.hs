unit DeviceDriver
{
    #define EPD_TWO_BUFFERS
    
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    const int PW = 250;
    const int PH = 122;

#if defined(CHALLENGER_2040_WIFI) || defined(CHALLENGER_NB_2040_WIFI)
    const byte DCPin  = Board.GP10;
    const byte CSPin  = Board.GP9;
    const byte ClkPin = Board.SPI0SCK;
    const byte TxPin  = Board.SPI0Tx;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    #define EPD_PINS_DEFINED
#endif

#if defined(SPARKFUN_THINGPLUSRP2040)
    const byte DCPin  = 19;
    const byte CSPin  = 20;
    const byte ClkPin = Board.SPI0SCK;
    const byte TxPin  = Board.SPI0Tx;
    const byte RxPin  = Board.SPI0Rx;
    const byte SPIController  = 0;
    #define EPD_PINS_DEFINED
#endif
    
#if !defined(EPD_PINS_DEFINED)
    // ADAFRUIT_FEATHER
    const byte DCPin  = Board.GP10; 
    const byte CSPin  = Board.GP9;  
    const byte ClkPin = Board.SPI0SCK;
    const byte TxPin  = Board.SPI0Tx;
    const byte RxPin  = Board.SPI0Rx;
    const byte SPIController  = 0;
#endif
    
    const uint DefaultRefreshDelay   = 16; // seconds
    
    const bool EPDBlack0 =  true;
    const bool EPDRed0   = false;
    const bool EPDWhite0 = false;
    
    const bool EPDBlack1 =  true;
    const bool EPDRed1   = false;
    const bool EPDWhite1 =  true;
    
    bool Begin()
    {
        return Display.Begin();
    }
    
}
