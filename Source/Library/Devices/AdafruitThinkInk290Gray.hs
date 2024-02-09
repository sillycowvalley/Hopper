unit DeviceDriver
{
    #define EPD_GRAY
    
    uses "/Source/Library/Displays/EPDIL0373.hs"
    
    const int PW = 296;
    const int PH = 128;
    
    const byte DCPin  = 10;
    const byte CSPin  = 9;
    const byte ClkPin = 18;
    const byte TxPin  = 19;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    
    const byte keyAPin = 11;
    const byte keyBPin = 12;
    const byte keyCPin = 13;
    
    const uint DefaultRefreshDelay   = 1; // seconds (800ms)
    
    const bool EPDBlack0 =  true;
    const bool EPDRed0   =  true;
    const bool EPDWhite0 = false;
    const bool EPDLight0 = false;
    const bool EPDDark0  =  true;
    
    const bool EPDBlack1 =  true;
    const bool EPDRed1   = false;
    const bool EPDWhite1 = false;
    const bool EPDLight1 =  true;
    const bool EPDDark1  = false;
        
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
