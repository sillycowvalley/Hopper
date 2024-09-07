program ScalePicoW
{
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Devices/Generic160x128ST7735TFT"
    
    //uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    //uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    
    uses "/Source/Library/Devices/HX711"
    
#ifdef ST7735_TFT_160x128    
    const byte TFTCS = Board.SPI0SS;
    const byte DATACONTROL = Board.GP13;
    
    const byte CELLCLOCK = Board.GP14;
    const byte CELLDATA = Board.GP15;
    
    const byte RESETBUTTON = Board.GP12;
#endif
    
    const long gramFactor = 246; // calibrated using a gym weight (number of grams per reading from the HX711)
    
    long tare;
    LoadCell cell;
    int prevMax;
    int prevTotal;
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        Reset();
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
        Screen.PrintLn("Tare Set");
        
        prevMax = -1;
        prevTotal = -1;
        
        Display.Resume(); 
    }
    
    DisplayKg(long maxGrams, long totalGrams, bool last)
    {
        long kg = maxGrams / 1000;
        long grams = maxGrams - (kg * 1000);
        string gstring = (grams.ToString()).LeftPad('0', 3);
        if (last)
        {
            IO.WriteLn("Measured " + kg.ToString() + "." + gstring + " kg");
        }
        
        Display.Suspend();
        
        Screen.CursorX = 0;
        Screen.CursorY = 0;
        Screen.Print(kg.ToString() + "." + gstring + " kg", Colour.MatrixGreen, Colour.Black); // top text line (max)
        
        if (!last)
        {
            kg = totalGrams / 1000;
            grams = totalGrams - (kg * 1000);
            gstring = (grams.ToString()).LeftPad('0', 3);
            Screen.CursorX = 0;
            Screen.CursorY = 1;
            Screen.Print(kg.ToString() + "." + gstring + " kg", Colour.MatrixOrange, Colour.Black); // 2nd text line (current)
        }
        
        long lineGrams = maxGrams * PixelHeight / 12000; // 12kg for entire display
        int y = PixelHeight - int(lineGrams);
        if (y != prevMax)
        {
            if (prevMax != -1)
            {
                Display.HorizontalLine(0, prevMax, PixelWidth-1, Colour.Black);
            }
            Display.HorizontalLine(0, y, PixelWidth-1, Colour.MatrixGreen); // max horizontal bar (full display width)
            prevMax = y;
        }
        
        long subLineGrams = totalGrams * PixelHeight / 12000; // 12kg for entire display
        if (subLineGrams == lineGrams)
        {
            subLineGrams--;
        }
        y = PixelHeight - int(subLineGrams);
        if (y != prevTotal)
        {
            if (prevTotal != -1)
            {
                Display.HorizontalLine(0, prevTotal, PixelWidth-1, Colour.Black);
            }
            if (!last)
            {
                Display.HorizontalLine(10, y, PixelWidth-11, Colour.MatrixOrange); // current horizontal bar (narrower)
            }
            prevTotal = y;
        }
        
        Display.Resume();
    }
    
    
    Hopper()
    {
        ////////////////////
        // Initialization:
        
        // Overclocking to meet the clock criteria for the HX711 (see HX711.shiftIn())
        ClockSpeed = RPClockSpeed.Overclock250;
        
#if defined(ST7735_TFT_160x128)
        Screen.ForeColour = Colour.White;
        Screen.BackColour = Colour.Black;
        
        DeviceDriver.CS   = TFTCS;
        DeviceDriver.DC   = DATACONTROL;
        IsPortrait = true;
        FlipX = true;
        FlipY = false;

        // configuring the Tare reset button:
        MCU.PinISRDelegate buttonDelegate = ButtonISR;
        MCU.PinMode(RESETBUTTON, PinModeOption.InputPullup);
        _ = MCU.AttachToPin(RESETBUTTON, buttonDelegate, PinStatus.Rising);
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        cell = HX711.Create(CELLDATA, CELLCLOCK);
#endif   
#if defined(OLED_FEATHERWING_128x64)
        IsPortrait = true;
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Adafruit 128x64 OLED Featherwing");
            return;
        }
        cell = HX711.Create(GP17, GP16);
#endif     
        
        
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
