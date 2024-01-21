unit DeviceDriver
{
    //#define RP2040_PICO // not the 'W' version
    
    uses "/Source/Library/Displays/UC8151Driver"
    
    const byte SPIController = 0;
    
    const byte DCPin   = 20;
    const byte CSPin   = 17;
    const byte ClkPin  = 18;
    const byte TxPin   = 19;
    const byte RstPin  = 21;
    const byte BusyPin = 26;
    
    
    const byte vbusDetectPin = 24;
    const byte batteryPin    = 29;
    const byte enable3VPin   = 10;
    
    const byte keyAPin = 12;
    const byte keyBPin = 13;
    const byte keyCPin = 14;
    const byte keyDPin = 15;
    const byte keyEPin = 11;
    const byte userPin = 23;
    
    const int PW  = 296;
    const int PH = 128;
            
    bool Begin()
    {
        bool success = Display.Begin();
        MCU.PinMode(keyAPin, PinModeOption.InputPulldown);
        MCU.PinMode(keyBPin, PinModeOption.InputPulldown);
        MCU.PinMode(keyCPin, PinModeOption.InputPulldown);
        MCU.PinMode(keyDPin, PinModeOption.InputPulldown);
        MCU.PinMode(keyEPin, PinModeOption.InputPulldown);
        MCU.PinMode(userPin, PinModeOption.InputPulldown);
        
        MCU.PinMode(enable3VPin, PinModeOption.Output);
        MCU.DigitalWrite(enable3VPin, true);
        
        return success;
    }
    bool Begin(ISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin()) { break; }
            if (!AttachToPin(keyAPin, buttonDelegate, PinStatus.Falling)) { break; }
            if (!AttachToPin(keyBPin, buttonDelegate, PinStatus.Falling)) { break; }
            if (!AttachToPin(keyCPin, buttonDelegate, PinStatus.Falling)) { break; }
            if (!AttachToPin(keyDPin, buttonDelegate, PinStatus.Falling)) { break; }
            if (!AttachToPin(keyEPin, buttonDelegate, PinStatus.Falling)) { break; }
            if (!AttachToPin(userPin, buttonDelegate, PinStatus.Falling)) { break; }
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
            case keyDPin: { return "up"; }
            case keyEPin: { return "down"; }
            case userPin: { return "usr"; }
        }
        return "";
    }
    bool ButtonA { get { return MCU.DigitalRead(keyAPin); } }
    bool ButtonB { get { return MCU.DigitalRead(keyBPin); } }
    bool ButtonC { get { return MCU.DigitalRead(keyCPin);  } }
    bool ButtonD { get { return MCU.DigitalRead(keyDPin);  } }
    bool ButtonE { get { return MCU.DigitalRead(keyEPin);  } }
    
    bool ButtonUp   { get { return MCU.DigitalRead(keyDPin);  } }
    bool ButtonDown { get { return MCU.DigitalRead(keyEPin);  } }
    bool ButtonUsr  { get { return MCU.DigitalRead(userPin);  } }
}
