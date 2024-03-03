unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather"
#endif

    #define HAS_BUTTONS
    
    uses "/Source/Library/Displays/SH110XDriver.hs"
 
    friend DisplayDriver;
    
    const int pw =  64;
    const int ph = 128;
    
    const byte keyAPin = Board.GP9;
    const byte keyBPin = Board.GP8;
    const byte keyCPin = Board.GP7;
    
    
    bool Begin()
    {
        // https://learn.adafruit.com/adafruit-128x64-oled-featherwing/arduino-code
        // I2C address = 0x3C
        //     controller 1:
        //         I2CSDA1 and I2CSDCL1
        
        
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
            case keyAPin: { return "A"; }
            case keyBPin: { return "B"; }
            case keyCPin: { return "C"; }
        }
        return "";
    }
    bool ButtonA { get { return MCU.DigitalRead(keyAPin); } }
    bool ButtonB { get { return MCU.DigitalRead(keyBPin); } }
    bool ButtonC { get { return MCU.DigitalRead(keyCPin);  } }
    
    
}   
