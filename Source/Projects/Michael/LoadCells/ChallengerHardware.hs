unit Hardware
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    const byte CELLDATA = GP17;
    const byte CELLCLOCK = GP16;

    const byte textLeft = 0;
    const byte textWidth = 10;
    
    byte TextLeft { get { return textLeft; } }
    byte TextWidth { get { return textWidth; } }
    
    bool Initialize(ref LoadCell cell, PinISRDelegate buttonDelegate)
    {
        ClockSpeed = RPClockSpeed.Overclock250;
        IsPortrait = true;
        
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Adafruit 128x64 OLED Featherwing");
            return false;
        }
        cell = HX711.Create(CELLDATA, CELLCLOCK);
        return true;
    }
}
