program ScaleFeather
    uses "/Source/Library/Fonts/Verdana5x8"
    
    
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    
    uses "/Source/Library/Devices/HX711"
    
    const long gramFactor = 246; // calibrated using a gym weight (number of grams per reading from the HX711)
    
    long tare;
    LoadCell cell;
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        if (PinToButton(pin) == "C")
        {
            Reset(); // 'C' button for reset / tare
        }
        else
        {
            IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        }
    }
    
    Reset()
    {
        loop
        {
            if (HX711.IsReady(cell))
            {
                tare = HX711.Read(cell);
                break;
            }
        }  
        Display.Suspend();

        Screen.Clear(); // reset col and row to [0,0]
        Screen.PrintLn("Reset Tare");
        
        Display.Resume(); 
    }
    
    DisplayKg(long maxGrams, long totalGrams, bool last)
    {
        Display.Suspend();
        
        Display.Clear(); // seems fast enough that I can just redraw everything on each reading
        
        long kg = maxGrams / 1000;
        long grams = maxGrams - (kg * 1000);
        string gstring = (grams.ToString()).LeftPad('0', 3);
        Screen.CursorX = 0;
        Screen.CursorY = 0;
        Screen.Print(kg.ToString() + "." + gstring + " kg"); // top text line (max)
        
        if (!last)
        {
            kg = totalGrams / 1000;
            grams = totalGrams - (kg * 1000);
            gstring = (grams.ToString()).LeftPad('0', 3);
            Screen.CursorX = 0;
            Screen.CursorY = 1;
            Screen.Print(kg.ToString() + "." + gstring + " kg"); // 2nd text line (current)
        }
        
        long lineGrams = maxGrams * PixelHeight / 12000; // 12kg for entire display
        int y = PixelHeight - int(lineGrams);
        Display.HorizontalLine(0, y, PixelWidth-1, Colour.White); // max horizontal bar (full display width)
        
        if (!last)
        {
            long subLineGrams = totalGrams * PixelHeight / 12000; // 12kg for entire display
            if (subLineGrams == lineGrams)
            {
                subLineGrams--;
            }
            y = PixelHeight - int(subLineGrams);
            Display.HorizontalLine(10, y, PixelWidth-11, Colour.White); // current horizontal bar (narrower)
        }
        
        Display.Resume();
    }
    
    
    Hopper()
    {
        ////////////////////
        // Initialization:
        
        // Overclocking to meet the clock criteria for the HX711 (see HX711.shiftIn())
        ClockSpeed = RPClockSpeed.Overclock250; // Challenger RP2040 boards
        
        
        IsPortrait = true;
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Adafruit 128x64 OLED Featherwing");
            return;
        }
        
        cell = HX711.Create(GP17, GP16);
        Reset();
        
        ////////////////////
        // Main loop:
        
        bool newReading = true;
        long maxGrams = 0;
        loop
        {
            if (HX711.IsReady(cell))
            {
                long reading = HX711.Read(cell) - tare;
                long grams = reading / gramFactor;
                
                // consider anything <= 49g to be noise (or a release of the hook)
                if (grams > 49)
                {
                    if (grams > maxGrams)
                    {
                        maxGrams = grams;
                    }
                    DisplayKg(maxGrams, grams, false); // intermediate display displays grams and maxGrams
                    newReading = false;
                }
                else
                {
                    if (!newReading)
                    {
                        DisplayKg(maxGrams, grams, true); // last display only displays maxGrams     
                        newReading = true;
                        maxGrams = 0;
                    }
                    Delay(100);
                }
            }
            else
            {
                Delay(250); // HX711 not ready
            }
        }
    }
}
