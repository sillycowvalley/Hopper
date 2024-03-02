unit DeviceDriver
{
#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather" // avoid modifying the automatically generated Board file
#endif
    
    #define EPD_HAS_BUTTONS
    #define HAS_BUTTONS
    
    #define EPD_GRAY
    
    uses "/Source/Library/Displays/EPDIL0373.hs"
    
    friend DisplayDriver;
    
    const int pw = 296;
    const int ph = 128;
    
    #define EPD_NO_RX
    
    const byte sdCS  = Board.GP7; // SD
    const byte srCS  = Board.GP8; // SRAM
    const byte csPin = Board.GP9; // E-Ink
    
    const byte dcPin  = Board.GP10;
    const byte clkPin = Board.SPI0SCK;
    const byte txPin  = Board.SPI0Tx;
    //const byte rxPin  = Board.SPI0Rx;
    const byte spiController  = 0;
    
    const byte keyAPin = Board.GP11;
    const byte keyBPin = Board.GP12;
    const byte keyCPin = Board.GP13;
    
    const uint defaultRefreshDelay   = 1; // seconds (800ms)
    
    const bool epdBlack0 =  true;
    const bool epdRed0   =  true;
    const bool epdWhite0 = false;
    const bool epdLight0 = false;
    const bool epdDark0  =  true;
    
    const bool epdBlack1 =  true;
    const bool epdRed1   = false;
    const bool epdWhite1 = false;
    const bool epdLight1 =  true;
    const bool epdDark1  = false;
        
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
    bool Begin(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin()) { break; }
            MCU.PinMode(keyAPin, PinModeOption.InputPullup);
            MCU.PinMode(keyBPin, PinModeOption.InputPullup);
            MCU.PinMode(keyCPin, PinModeOption.InputPullup);
            if (!AttachToPin(keyAPin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyBPin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyCPin, buttonDelegate, PinStatus.Rising)) { break; }
            success = true;
            break;   
        }
        return success;
    }
    string PinToButton(byte pin)
    {
        switch (pin)
        {
            case keyAPin: { return "a"; }
            case keyBPin: { return "b"; }
            case keyCPin: { return "c"; }
        }
        return "";
    }
    bool ButtonA { get { return MCU.DigitalRead(keyAPin); } }
    bool ButtonB { get { return MCU.DigitalRead(keyBPin); } }
    bool ButtonC { get { return MCU.DigitalRead(keyCPin);  } }
}
