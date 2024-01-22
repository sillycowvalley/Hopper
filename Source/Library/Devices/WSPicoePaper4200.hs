unit DeviceDriver
{
    
    #define RP2040_PICO // not the 'W' version
    
    // https://www.waveshare.com/wiki/Pico-ePaper-4.2
    #define WAVESHARE_PICO_ePAPER_4200
    
    uses "/Source/Library/Displays/UC8176Driver"
    
    const int PW  = 400;
    const int PH = 300;
    
    const byte SPIController = 1;
    
    const byte DCPin   =  8;
    const byte CSPin   =  9;
    const byte ClkPin  = 10;
    const byte TxPin   = 11;
    const byte RstPin  = 12;
    const byte BusyPin = 13;
    
    const byte key0Pin = 15;
    const byte key1Pin = 17;
    
    bool isPortrait;
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
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
