unit DeviceDriver
{
    // https://www.waveshare.com/wiki/Pico-LCD-1.44
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
#if !defined(MCU_BOARD_DEFINED) && !defined(SBC_BOARD_DEFINED)
    // plugs directly into the Pi Pico so no board defined, assume generic Pi Pico
    uses "/Source/Library/Boards/PiPico"
#endif

    #define WAVESHARE_PICO_LCD_144
    #define HAS_RESET_PIN
    //#define BUFFER_TEXT
    #define ST77XX_CONTROLLER
    #define SET_TFT_INVOFF
    
    uses "/Source/Library/Displays/TFTDriver"
    
    friend DisplayDriver;
    
    const int pw = 128;
    const int ph = 128;
    
    const byte spiController = 1; // this device uses SPI1 on Raspberry Pi Pico
    byte dcPin   { get { return Board.GP8; } }
    byte csPin   { get { return Board.GP9; } }
    byte clkPin  { get { return Board.GP10; } }
    byte txPin   { get { return Board.GP11; } }
    int rstPin   { get { return Board.GP12; } }
    byte blPin   { get { return Board.GP13; } }
    
    const byte key0Pin = 15;
    const byte key1Pin = 17;
    const byte key2Pin = 2;
    const byte key3Pin = 3;
    
    int xFudge;
    int yFudge;
    
    byte getMAD()
    {
        byte madArgument = MADCTL_BGR;
        if (DisplayDriver.IsPortrait)
        {
            xFudge = 1;
            yFudge = 2;
            madArgument |= MADCTL_MV;
            if (!FlipX)
            {
                madArgument |= MADCTL_MX;
            }
            if (FlipY)
            {
                xFudge = 3;
                madArgument |= MADCTL_MY;
            }
        }
        else
        {
            xFudge = 2;
            yFudge = 1;
            if (FlipX)
            {
                yFudge = 3;
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
        bool success = Display.Begin();
        MCU.PinMode(key0Pin, PinModeOption.InputPullup);
        MCU.PinMode(key1Pin, PinModeOption.InputPullup);
        MCU.PinMode(key2Pin, PinModeOption.InputPullup);
        MCU.PinMode(key3Pin, PinModeOption.InputPullup);
        return success;
    }
    bool Begin(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin()) { break; }
            if (!AttachToPin(key0Pin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(key1Pin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(key2Pin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(key3Pin, buttonDelegate, PinStatus.Rising)) { break; }
            success = true;
            break;
        }
        return success;
    }
    string PinToButton(byte pin)
    {
        switch (pin)
        {
            case key0Pin: { return "Key0"; }
            case key1Pin: { return "Key1"; }
            case key2Pin: { return "Key2"; }
            case key3Pin: { return "Key3"; }
        }
        return "";
    }
    bool Button0 { get { return !MCU.DigitalRead(key0Pin); } }
    bool Button1 { get { return !MCU.DigitalRead(key1Pin); } }
    bool Button2 { get { return !MCU.DigitalRead(key2Pin);  } }
    bool Button3 { get { return !MCU.DigitalRead(key3Pin);  } }
}
