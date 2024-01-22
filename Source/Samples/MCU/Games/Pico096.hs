unit Pico096
{
    bool DisplayST7735xPiPico096() // Waveshare Pico-LCD-0.96
    {
        ConfigureDisplay(Display.ST7735, 160, 80);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool DisplayST7789xPiPico114() // Waveshare Pico-LCD-1.14
    {
        ConfigureDisplay(Display.ST7789, 240, 135);
        ConfigureSPI(9, 8);       // CS, DC
        ConfigureSPIPort(11, 10); // TX(MOSI), CLK
        ConfigureReset(12);
        DisplayState result = Begin();
        return (result == DisplayState.OK);
    }
    bool Initialize(PinISRDelegate buttonDelegate)
    {
        bool success;
        loop
        {
            //if (!DisplayST7735xPiPico096())
            if (!DisplayST7789xPiPico114())
            {
                WriteLn("Failed to initialize Display.");
                break;
            }
            DisplayState result = Graphics.Begin();
            if (result != DisplayState.OK)
            {
                WriteLn("Graphics.Begin() failed.");
                break;
            }
            <byte> buttonPins;
            buttonPins.Append(2);  // Up
            buttonPins.Append(3);  // Joystick Press
            buttonPins.Append(15); // A
            buttonPins.Append(16); // Left
            buttonPins.Append(17); // B
            buttonPins.Append(18); // Down
            buttonPins.Append(20); // Right
            foreach (var buttonPin in buttonPins)
            {
                if (!AttachToPin(buttonPin, buttonDelegate, PinStatus.Rising))
                {
                    WriteLn("Pin " + buttonPin.ToString() + " not valid for interrupts.");
                    return false;
                }
            }
            success = true;
            break;
        }        
        return success;
    }
    
    bool ButtonA     { get { return !DigitalRead(15); } }
    bool ButtonB     { get { return !DigitalRead(17); } }
    
    bool ButtonLeft  { get { return !DigitalRead(16); } }
    bool ButtonRight { get { return !DigitalRead(20); } }
    bool ButtonUp    { get { return !DigitalRead(2);  } }
    bool ButtonDown  { get { return !DigitalRead(18); } }
    
}
