unit DeviceDriver
{
    // https://learn.adafruit.com/adafruit-1-3-color-tft-bonnet-for-raspberry-pi
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
#if !defined(SBC_BOARD_DEFINED) && !defined(MCU_BOARD_DEFINED)
    // plugs directly into the Raspberry Pi so no board defined, assume generic Raspberry Pi
    uses "/Source/Library/Boards/Pi"
#endif
    
    #define ADAFRUIT_TFT_LCD_13
    #define BUFFER_TEXT
    #define ST77XX_CONTROLLER
    
    uses "/Source/Library/Displays/TFTDriver"
    
    friend DisplayDriver;
    
    const int pw = 240;
    const int ph = 240;
    
    const byte spiController = 0; // this device uses SPI0 on Raspberry Pi
    byte dcPin   { get { return Board.GP12; /*Board.GP25;*/ } }
    byte csPin   { get { return Board.SPI0SS; } }
    byte clkPin  { get { return Board.SPI0SCK; } }
    byte txPin   { get { return Board.SPI0Tx; } }
    byte blPin   { get { return Board.GP26; } }
    int  rstPin  = -1;
    
    const byte keyAPin      = Board.GP5;
    const byte keyBPin      = Board.GP6;
    const byte keyUpPin     = Board.GP17;
    const byte keyDownPin   = Board.GP22;
    const byte keyLeftPin   = Board.GP27;
    const byte keyRightPin  = Board.GP23;
    const byte keyButtonPin = Board.GP4;
    
    int xFudge;
    int yFudge;
    
    byte getMAD()
    {
        byte madArgument = MADCTL_RGB;
        //byte madArgument = MADCTL_BGR;
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
            //xFudge = 52;
            //yFudge = 40;
        }
        else
        {
            madArgument |= MADCTL_MV;
            if (FlipX)
            {
                madArgument |= MADCTL_MY;
            }
            if (!FlipY)
            {
                madArgument |= MADCTL_MX;
            }
            //xFudge = 40;
            //yFudge = 53;
        }
        return madArgument;
    }
    
    bool Begin()
    {
        bool success = Display.Begin();
        
        // built-in 10K pullups?
        MCU.PinMode(keyAPin,      PinModeOption.Input);
        MCU.PinMode(keyBPin,      PinModeOption.Input);
        MCU.PinMode(keyUpPin,     PinModeOption.Input);
        MCU.PinMode(keyDownPin,   PinModeOption.Input);
        MCU.PinMode(keyLeftPin,   PinModeOption.Input);
        MCU.PinMode(keyRightPin,  PinModeOption.Input);
        MCU.PinMode(keyButtonPin, PinModeOption.Input);
        return success;
    }
    bool Begin(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin())  { break; }
            if (!AttachToPin(keyAPin,      buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyBPin,      buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyUpPin,     buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyDownPin,   buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyLeftPin,   buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyRightPin,  buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyButtonPin, buttonDelegate, PinStatus.Rising)) { break; }
            success = true;
            break;
        }
        return success;
    }
    string PinToButton(byte pin)
    {
        switch (pin)
        {
            case keyAPin:      { return "A"; }
            case keyBPin:      { return "B"; }
            case keyLeftPin:   { return "Left"; }
            case keyRightPin:  { return "Right"; }
            case keyUpPin:     { return "Up"; }
            case keyDownPin:   { return "Down"; }
            case keyButtonPin: { return "Button"; }
        }
        return "";
    }
    bool ButtonA { get { return !MCU.DigitalRead(keyAPin); } }
    bool ButtonB { get { return !MCU.DigitalRead(keyBPin); } }
    bool ButtonLeft { get { return !MCU.DigitalRead(keyLeftPin);  } }
    bool ButtonRight { get { return !MCU.DigitalRead(keyRightPin);  } }
    bool ButtonUp { get { return !MCU.DigitalRead(keyUpPin);  } }
    bool ButtonDown { get { return !MCU.DigitalRead(keyDownPin);  } }
    bool ButtonButton { get { return !MCU.DigitalRead(keyButtonPin);  } }
}
