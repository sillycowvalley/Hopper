unit DeviceDriver
{
    
#if !defined(MCU_BOARD_DEFINED)
    // plugs directly into the Pi Pico so no board defined, assume generic Pi Pico
    uses "/Source/Library/Boards/PiPico"
#endif
    
    // https://www.waveshare.com/wiki/Pico-ePaper-4.2
    #define WAVESHARE_PICO_ePAPER_4200
    
    uses "/Source/Library/Displays/UC8176Driver"
    
    friend DisplayDriver;
    
    const int pw = 400;
    const int ph = 300;
    
    const byte spiController = 1;
    
    const byte dcPin   = Board.GP8;
    const byte csPin   = Board.GP9;
    const byte clkPin  = Board.GP10;
    const byte txPin   = Board.GP11;
    const byte rstPin  = Board.GP12;
    const byte busyPin = Board.GP13;
    
    const byte key0Pin = Board.GP15;
    const byte key1Pin = Board.GP17;
    
    bool Begin()
    {
        bool success = Display.Begin();
        MCU.PinMode(key0Pin, PinModeOption.InputPullup);
        MCU.PinMode(key1Pin, PinModeOption.InputPullup);
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
            success = true;
            break;
        }
        return success;
    }
    string PinToButton(byte pin)
    {
        switch (pin)
        {
            case key0Pin: { return "0"; }
            case key1Pin: { return "1"; }
        }
        return "";
    }
    bool Button0 { get { return !MCU.DigitalRead(key0Pin); } }
    bool Button1 { get { return !MCU.DigitalRead(key1Pin); } }
    
}
