unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/PiPico"
#endif

    #define ILI9341_TFT_320x200
    #define BUFFER_TEXT
    #define HAS_RESET_PIN
    //#define BUFFER_TEXT
    #define ILI9341_CONTROLLER

    uses "/Source/Library/Displays/TFTDriver"
    
    friend DisplayDriver;
    
    const int pw = 320;
    const int ph = 240;
    
    const int xFudge = 0;
    const int yFudge = 0;
    
    byte spiController  = 0;
    byte csPin  = Board.SPI0SS;
    byte dcPin  = 10;       
    int  rstPin  = -1;
    byte clkPin = Board.SPI0SCK;
    byte txPin  = Board.SPI0Tx;
    byte rxPin  = Board.SPI0Rx; // MISO - this is used for the SD card. It isn't used for the TFT display which is write-only. 
    
    byte SPIController { get { return spiController; } set { spiController  = value;  } }
    byte SPISCK        { get { return clkPin;        } set { clkPin  = value;         } }
    byte SPITx         { get { return txPin;         } set { txPin   = value;         } }
    byte SPIRx         { get { return rxPin;         } set { rxPin   = value;         } }
    byte CS            { get { return csPin;         } set { csPin = value;           } }
    byte DC            { get { return dcPin;         } set { dcPin = value;           } }
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
            if (!FlipY)
            {
                madArgument |= MADCTL_MY;
            }
        }
        else
        {
            madArgument |= MADCTL_MV;
            if (FlipX)
            {
                madArgument |= MADCTL_MY;
            }
            if (FlipY)
            {
                madArgument |= MADCTL_MX;
            }
        }
        return madArgument;
    }
    
    bool Begin()
    {
        return Display.Begin(); // calls SPI.Begin(..)
    }
    
}
