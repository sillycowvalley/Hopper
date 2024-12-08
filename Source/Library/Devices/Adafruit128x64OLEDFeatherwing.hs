unit DeviceDriver
{

#if !defined(MCU_BOARD_DEFINED)
    uses "/Source/Library/Boards/AdafruitFeather"
#endif

    #define OLED_FEATHERWING_128x64
    #define HAS_BUTTONS
    #define SH110X_OLED_128x64
    
    const int pw =  64;
    const int ph = 128;
    friend DisplayDriver;
        
    uses "/Source/Library/Displays/SH110XDriver"
    
#if defined(CHALLENGER_NB_2040_WIFI) || defined(CHALLENGER_2040_WIFI)
    const byte keyAPin = Board.GP6;
    const byte keyBPin = Board.GP3;
    const byte keyCPin = Board.GP2;
#endif
#if defined(ADAFRUIT_FEATHER)
    const byte keyAPin = Board.GP9;
    const byte keyBPin = Board.GP8;
    const byte keyCPin = Board.GP7;
#endif    
#if defined(ADAFRUIT_FEATHER_RP2350_HSTX)
    const byte keyAPin = Board.GP9;
    const byte keyBPin = Board.GP6;
    const byte keyCPin = Board.GP5;
#endif        
    
    bool Begin()
    {
        // https://learn.adafruit.com/adafruit-128x64-oled-featherwing/arduino-code
        // I2C address = 0x3C
        //     controller 1:
        //         I2CSDA1 and I2CSDCL1
        MCU.PinMode(keyAPin, PinModeOption.InputPullup);
        MCU.PinMode(keyBPin, PinModeOption.InputPullup);
        MCU.PinMode(keyCPin, PinModeOption.InputPullup);
        return Display.Begin();
    }
    bool Begin(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            if (!DeviceDriver.Begin()) { break; }
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
    bool ButtonA { get { return !MCU.DigitalRead(keyAPin); } }
    bool ButtonB { get { return !MCU.DigitalRead(keyBPin); } }
    bool ButtonC { get { return !MCU.DigitalRead(keyCPin);  } }
    
    
}   
