unit DeviceDriver
{
    // https://www.waveshare.com/wiki/Pico-LCD-1.44
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
#if !defined(RP2040_PICOW) && !defined(RP2040_PICO)    
    // plugs directly into the Pi Pico or Pi PicoW so if neither was defined, assume Pico W for now
    #define RP2040_PICOW 
#endif

    #define WAVESHARE_PICO_LCD_144
    uses "/Source/Library/Displays/ST7735Driver"
    
    const int PW  = 128;
    const int PH = 128;
    
    const byte SPIController = 1; // this device uses SPI1 on Raspberry Pi Pico
    const byte DCPin   = 8;
    const byte CSPin   = 9;
    const byte ClkPin  = 10;
    const byte TxPin   = 11;
    const byte RstPin  = 12;
    const byte BlPin   = 13;
    
    const byte key0Pin = 15;
    const byte key1Pin = 17;
    const byte key2Pin = 2;
    const byte key3Pin = 3;
    
    const int XFudge = 2;
    const int YFudge = 1;
    
    bool Begin()
    {
        bool success = Display.Begin();
        MCU.PinMode(key0Pin, PinModeOption.InputPullup);
        MCU.PinMode(key1Pin, PinModeOption.InputPullup);
        MCU.PinMode(key2Pin, PinModeOption.InputPullup);
        MCU.PinMode(key3Pin, PinModeOption.InputPullup);
        return success;
    }
    bool Begin(ISRDelegate buttonDelegate)
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
