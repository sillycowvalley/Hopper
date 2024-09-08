program ScaleProject
{
    uses "/Source/Library/Fonts/Verdana5x8"
    
    //uses "PiPicoWHardware"
    uses "ChallengerHardware"
    
    uses "/Source/Library/Devices/HX711"
    
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
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 0;
        Screen.Print("Tare Set");
        
        prevMax = -1;
        prevTotal = -1;
        
        Display.Resume(); 
    }
    
    DisplayKg(long maxGrams, long totalGrams, bool last)
    {
        long kg = maxGrams / 1000;
        long grams = maxGrams - (kg * 1000);
        string gstring = (grams.ToString()).LeftPad('0', 3);
        string weight = (kg.ToString() + "." + gstring + " kg").LeftPad(' ', Hardware.TextWidth);
        if (last)
        {
            IO.WriteLn("Measured " + weight);
        }
        
        Display.Suspend();
        
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 0;
        Screen.Print(weight, Colour.MatrixGreen, Colour.Black); // top text line (max)
        
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 1;
        if (!last)
        {
            kg = totalGrams / 1000;
            grams = totalGrams - (kg * 1000);
            gstring = (grams.ToString()).LeftPad('0', 3);
            weight = (kg.ToString() + "." + gstring + " kg").LeftPad(' ', Hardware.TextWidth);
            Screen.Print(weight, Colour.MatrixOrange, Colour.Black); // 2nd text line (current)
        }
        else
        {
            weight = (" ").LeftPad(' ', Hardware.TextWidth);
            Screen.Print(weight, Colour.MatrixOrange, Colour.Black); // 2nd text line (current)
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
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!Hardware.Initialize(ref cell, buttonDelegate))
        {
            return; // failed
        }
        
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
