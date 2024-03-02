unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    // TODO TFT uses "/Source/Library/Displays/EPDIL0373.hs"
    
    friend DisplayDriver;
    
    const int pw = 320;
    const int ph = 240;
    
    // Adafruit Feather defaults
    byte spi  = 0;
    byte sdCS = Board.GP7; // SD
    byte sck  = Board.GP18;
    byte tx   = Board.GP19;
    byte rx   = Board.GP20;
    
    byte SPIController { get { return spi; }  set { spi = value; } }
    byte SPISCK        { get { return sck; }  set { sck = value; } }
    byte SPITx         { get { return tx; }   set { tx = value; } }
    byte SPIRx         { get { return rx; }   set { rx = value; } }
    byte SDCS          { get { return sdCS; } set { sdCS = value; } }
    
    bool Begin()
    {
        // https://learn.adafruit.com/adafruit-1-14-240x135-color-tft-breakout
        
        // Settings for Hopper SD unit:
        SD.SPIController = spi;
        SD.ClkPin = sck;
        SD.TxPin  = tx;
        SD.RxPin  = rx;
        SD.CSPin  = sdCS; 
        
        return true; // TODO : Display.Begin();
    }
    
}
