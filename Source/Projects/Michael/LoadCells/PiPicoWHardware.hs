unit Hardware
{
    uses "/Source/Library/Boards/PiPicoW"
    
    uses "/Source/Library/Devices/Generic160x128ST7735TFT"
    
    const byte CELLDATA = GP15;
    const byte CELLCLOCK = GP14;
    const byte TAREBUTTON = GP13;
    
    const byte TFTDC = GP12;
    
    const byte textLeft = 5;
    const byte textWidth = 10;
    
    byte TextLeft { get { return textLeft; } }
    byte TextWidth { get { return textWidth; } }
    
    bool Initialize(ref LoadCell cell, PinISRDelegate buttonDelegate)
    {
        ClockSpeed = RPClockSpeed.Overclock270;
        
        PinMode(TAREBUTTON, PinModeOption.Input);
        if (!MCU.AttachToPin(TAREBUTTON, buttonDelegate, PinStatus.Rising))
        {
            IO.WriteLn("Failed to attach Tare Button");
            return false;
        }
        
        cell = HX711.Create(CELLDATA, CELLCLOCK);
        
        DisplayDriver.IsPortrait = true;
        DisplayDriver.FlipX = true;
        
        DeviceDriver.DC = TFTDC;
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return false;
        }
        
        return true;
    }
}
