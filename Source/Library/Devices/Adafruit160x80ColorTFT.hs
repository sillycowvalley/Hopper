unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    #define ADAFRUIT_TFT_096
    #define BUFFER_TEXT
    #define ST77XX_CONTROLLER
    
    uses "/Source/Library/Displays/TFTDriver"
    
    friend DisplayDriver, Screen;
    
    const int pw = 160;
    const int ph =  80;
    
    int xFudge;
    int yFudge;
    
    // Adafruit Feather defaults
    byte spiController  = 0;
    byte sdCS   = Board.GP7;  // SD
    byte blPin  = Board.GP8;  // backlight
    byte csPin  = Board.GP9;  // TFT
    byte dcPin  = 10;         // TFT
    int  rstPin  = -1;  
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
    int  Reset         { get { return rstPin;        } set { rstPin = value;          } }
    
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
        // https://learn.adafruit.com/adafruit-mini-tft-0-dot-96-inch-180x60-breakout/wiring-test
        bool success = Display.Begin();
        
        if (success)        
        {
            // Settings for Hopper SD unit:
            SD.SPIController = spiController;
            SD.ClkPin = clkPin;
            SD.TxPin  = txPin;
            SD.RxPin  = rxPin;
            SD.CSPin  = sdCS; 
        }
        return success;
    }
    
}
