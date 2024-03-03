unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    #define ADAFRUIT_TFT_114
    #define BUFFER_TEXT

    uses "/Source/Library/Displays/ST77XXDriver.hs"
    
    friend DisplayDriver;
    
    const int pw = 240;
    const int ph = 135;
    
    const int xFudge = 40;
    const int yFudge = 53;
    
    // Adafruit Feather defaults
    byte spiController  = 0;
    byte sdCS   = Board.GP7;  // SD
    byte blPin  = Board.GP8;  // backlight
    byte csPin  = Board.GP9;  // TFT
    byte dcPin  = Board.GP10; // TFT
    byte clkPin = Board.GP18;
    byte txPin  = Board.GP19;
    byte rxPin  = Board.GP20; // MISO - this is used for the SD card. It isn't used for the TFT display which is write-only. 
    
    byte SPIController { get { return spiController; } set { spiController  = value;  } }
    byte SPISCK        { get { return clkPin;        } set { clkPin  = value;         } }
    byte SPITx         { get { return txPin;         } set { txPin   = value;         } }
    byte SPIRx         { get { return rxPin;         } set { rxPin   = value;         } }
    byte SDCS          { get { return csPin;         } set { csPin = value;           } }
    byte CS            { get { return tftCS;         } set { tftCS = value;           } }
    byte DC            { get { return dcPin;         } set { dcPin = value;           } }
    byte LIT           { get { return blPin;         } set { blPin = value;           } }
    
    bool Begin()
    {
        // https://learn.adafruit.com/adafruit-1-14-240x135-color-tft-breakout
        
        // Settings for Hopper SD unit:
        SD.SPIController = spiController;
        SD.ClkPin = clkPin;
        SD.TxPin  = txPin;
        SD.RxPin  = rxPin;
        SD.CSPin  = csPin; 
        
        return Display.Begin();
    }
    
}
