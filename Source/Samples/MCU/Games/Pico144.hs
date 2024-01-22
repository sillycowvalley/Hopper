unit Pico144
{
    bool DisplayST7735xPiPico144() // Waveshare Pico-LCD-1.44
    {
        ConfigureDisplay(Display.ST7735, 128, 128);
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
            if (!DisplayST7735xPiPico144())
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
            
            if (!AttachToPin(15, buttonDelegate, PinStatus.Rising))
            {
                WriteLn("Pin 15 not valid for interrupts.");
                break;
            }
            if (!AttachToPin(17, buttonDelegate, PinStatus.Rising))
            {
                WriteLn("Pin 17 not valid for interrupts.");
                break;
            }
            if (!AttachToPin(2, buttonDelegate, PinStatus.Rising))
            {
                WriteLn("Pin 2 not valid for interrupts.");
                break;
            }     
            if (!AttachToPin(3, buttonDelegate, PinStatus.Rising))
            {
                WriteLn("Pin 3 not valid for interrupts.");
                break;
            } 
            success = true;
            break;
        }        
        return success;
    }
    bool Button0 { get { return !DigitalRead(15); } }
    bool Button1 { get { return !DigitalRead(17); } }
    bool Button2 { get { return !DigitalRead(2);  } }
    bool Button3 { get { return !DigitalRead(3);  } }
}
