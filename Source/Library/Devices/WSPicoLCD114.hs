unit DeviceDriver
{
    // https://www.waveshare.com/wiki/Pico-LCD-1.14
    // Note: Colour.Invert is not supported on this device because there is no MISO / Rx pin for SPI interface
    
#if !defined(MCU_BOARD_DEFINED)
    // plugs directly into the Pi Pico so no board defined, assume generic Pi Pico
    uses "/Source/Library/Boards/PiPico"
#endif
    #define WAVESHARE_PICO_LCD_114
    
    uses "/Source/Library/Displays/ST7735Driver"
    
    friend DisplayDriver;
    
    const int pw = 240;
    const int ph = 135;
    
    const byte spiController = 1; // this device uses SPI1 on Raspberry Pi Pico
    byte dcPin   { get { return Board.GP8; } }
    byte csPin   { get { return Board.GP9; } }
    byte clkPin  { get { return Board.GP10; } }
    byte txPin   { get { return Board.GP11; } }
    byte rstPin  { get { return Board.GP12; } }
    byte blPin   { get { return Board.GP13; } }
    
    const byte keyAPin      = 15;
    const byte keyBPin      = 17;
    const byte keyUpPin     = 2;
    const byte keyDownPin   = 18;
    const byte keyLeftPin   = 16;
    const byte keyRightPin  = 20;
    const byte keyButtonPin = 3;
    
    const int xFudge = 40;
    const int yFudge = 53;
    
    bool Begin()
    {
        bool success = Display.Begin();
        MCU.PinMode(keyAPin, PinModeOption.InputPullup);
        MCU.PinMode(keyBPin, PinModeOption.InputPullup);
        MCU.PinMode(keyUpPin, PinModeOption.InputPullup);
        MCU.PinMode(keyDownPin, PinModeOption.InputPullup);
        MCU.PinMode(keyLeftPin, PinModeOption.InputPullup);
        MCU.PinMode(keyRightPin, PinModeOption.InputPullup);
        MCU.PinMode(keyButtonPin, PinModeOption.InputPullup);
        return success;
    }
    bool Begin(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin())  { break; }
            if (!AttachToPin(keyAPin, buttonDelegate, PinStatus.Rising))  { break; }
            if (!AttachToPin(keyBPin, buttonDelegate, PinStatus.Rising))  { break; }
            if (!AttachToPin(keyUpPin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyDownPin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyLeftPin, buttonDelegate, PinStatus.Rising)) { break; }
            if (!AttachToPin(keyRightPin, buttonDelegate, PinStatus.Rising)) { break; }
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
