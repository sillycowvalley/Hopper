unit Pico144
{
    bool DisplayST7735xPiPico144() // Pico-LCD-1.44
    {
        ConfigureDisplay(Display.ST7735, 128, 128);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool Initialize()
    {
        if (!DisplayST7735xPiPico144())
        {
            WriteLn("Failed to initialize Display");
            return false;
        }
        PinMode(15, PinModeOption.InputPullup); // key 0
        PinMode(17, PinModeOption.InputPullup); // key 1
        PinMode( 2, PinModeOption.InputPullup); // key 2
        PinMode( 3, PinModeOption.InputPullup); // key 3
        
        DisplayState result = Graphics.Begin();
        return (result == DisplayState.OK);
    }
    bool Button0 { get { return !DigitalRead(15); } }
    bool Button1 { get { return !DigitalRead(17); } }
    bool Button2 { get { return !DigitalRead(2);  } }
    bool Button3 { get { return !DigitalRead(3);  } }
}
