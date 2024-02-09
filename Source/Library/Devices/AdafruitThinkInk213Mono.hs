unit DeviceDriver
{
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    const int PW = 250;
    const int PH = 122;
    
#if defined(CHALLENGER_RP2040_WIFI)
    const byte DCPin  = 7;
    const byte CSPin  = 6;
    const byte ClkPin = 22;
    const byte TxPin  = 23;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    #define EPD_PINS_DEFINED
#endif

#if defined(SPARKFUN_THING_PLUS_RP2040)
    const byte DCPin  = 19;
    const byte CSPin  = 20;
    const byte ClkPin = 2;
    const byte TxPin  = 3;
    const byte RxPin  = 4;
    const byte SPIController  = 0;
    #define EPD_PINS_DEFINED
#endif
    
#if !defined(EPD_PINS_DEFINED)
    // ADAFRUIT_FEATHER_RP2040
    const byte DCPin  = 10;
    const byte CSPin  = 9;
    const byte ClkPin = 18;
    const byte TxPin  = 19;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
#endif
    
    const uint DefaultRefreshDelay   = 1; // seconds
    
    const bool EPDBlack0 = true;
    const bool EPDRed0   = true;
    const bool EPDWhite0 = false;
    
    bool Begin()
    {
        return Display.Begin();
    }
}
