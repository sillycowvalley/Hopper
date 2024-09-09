unit Hardware
{
    uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Devices/Generic160x128ST7735TFT"
    
    const byte TFTCS = Board.SPI0SS;
    const byte DATACONTROL = Board.GP13;
    
    const byte CELLCLOCK = Board.GP14;
    const byte CELLDATA = Board.GP15;
    
    const byte RESETBUTTON = Board.GP12;
    
    const byte textLeft  = 5;
    const byte textWidth = 10;
    
    byte TextLeft { get { return textLeft; } } 
    byte TextWidth { get { return textWidth; } } 
    
    bool Initialize(ref LoadCell cell, PinISRDelegate buttonDelegate)
    {
        // Overclocking to meet the clock criteria for the HX711 (see HX711.shiftIn())
        ClockSpeed = RPClockSpeed.Overclock270;
        
        Screen.ForeColour = Colour.White;
        Screen.BackColour = Colour.Black;
        
        DeviceDriver.CS   = TFTCS;
        DeviceDriver.DC   = DATACONTROL;
        IsPortrait = true;
        FlipX = true;
        FlipY = false;
        
        // configuring the Tare reset button:
        MCU.PinMode(RESETBUTTON, PinModeOption.Input);
        _ = MCU.AttachToPin(RESETBUTTON, buttonDelegate, PinStatus.Rising);
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return false;
        }
        cell = HX711.Create(CELLDATA, CELLCLOCK);
        return true;
    }
}
