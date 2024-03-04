unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    #define ADAFRUIT_TFT_096
    #define BUFFER_TEXT
    
    uses "/Source/Library/Displays/ST77XXDriver.hs"
    
    friend DisplayDriver, Screen;
    
    const int pw = 160;
    const int ph =  80;
    
    const int xFudge = 1;
    const int yFudge = 26;
    
    // Adafruit Feather defaults
    byte spiController  = 0;
    byte sdCS   = Board.GP7;  // SD
    byte blPin  = Board.GP8;  // backlight
    byte csPin  = Board.GP9;  // TFT
    byte dcPin  = 10;         // TFT
    byte clkPin = Board.SPI0SCK;
    byte txPin  = Board.SPI0Tx;
    byte rxPin  = Board.SPI0Rx; // MISO - this is used for the SD card. It isn't used for the TFT display which is write-only. 
    
    byte SPIController { get { return spiController; } set { spiController  = value;  } }
    byte SPISCK        { get { return clkPin;        } set { clkPin  = value;         } }
    byte SPITx         { get { return txPin;         } set { txPin   = value;         } }
    byte SPIRx         { get { return rxPin;         } set { rxPin   = value;         } }
    byte SDCS          { get { return sdCS;          } set { sdCS = value;           } }
    byte CS            { get { return csPin;         } set { csPin = value;           } }
    byte DC            { get { return dcPin;         } set { dcPin = value;           } }
    byte LIT           { get { return blPin;         } set { blPin = value;           } }
    
    bool Begin()
    {
        // https://learn.adafruit.com/adafruit-mini-tft-0-dot-96-inch-180x60-breakout/wiring-test
               
        // Settings for Hopper SD unit:
        SD.SPIController = spiController;
        SD.ClkPin = clkPin;
        SD.TxPin  = txPin;
        SD.RxPin  = rxPin;
        SD.CSPin  = sdCS; 
        
        return Display.Begin();
    }
    
}
