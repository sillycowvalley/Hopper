unit DeviceDriver
{
    #define EPD_HAS_BUTTONS
    
    uses "/Source/Library/Displays/EPDIL0373.hs"
    
    const int PW = 296;
    const int PH = 128;
    
#ifdef CHALLENGER_RP2040_WIFI
    const byte DCPin  = 7;
    const byte CSPin  = 6;
    const byte ClkPin = 22;
    const byte TxPin  = 23;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    
    const byte keyAPin = 8;
    const byte keyBPin = 9;
    const byte keyCPin = 10;
    #define EPD_PINS_DEFINED
#endif

#ifdef SPARKFUN_THING_PLUS_RP2040
    const byte DCPin  = 19;
    const byte CSPin  = 20;
    const byte ClkPin = 2;
    const byte TxPin  = 3;
    const byte RxPin  = 4;
    const byte SPIController  = 0;
    
    const byte keyAPin = 18;
    const byte keyBPin = 17;
    const byte keyCPin = 16;
    #define EPD_PINS_DEFINED
#endif 
    
#if !defined(EPD_PINS_DEFINED)
    // ADAFRUIT_FEATHER_RP2040
    const byte DCPin  = 10;
    const byte CSPin  = 9;
    const byte ClkPin = 18;
    const byte TxPin  = 19;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    
    const byte keyAPin = 11;
    const byte keyBPin = 12;
    const byte keyCPin = 13;
#endif
    
    const uint DefaultRefreshDelay   = 13; // seconds
    
    const bool EPDBlack0 =  true;
    const bool EPDRed0   = false;
    const bool EPDWhite0 = false;
    const bool EPDLight0 =  true;
    const bool EPDDark0  = false;
    
    const bool EPDBlack1 = false;
    const bool EPDRed1   =  true;
    const bool EPDWhite1 = false;
    const bool EPDLight1 =  true;
    const bool EPDDark1  =  true;
    
    bool Begin()
    {
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
