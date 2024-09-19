program Scale
{
    uses "/Source/Library/Fonts/Verdana5x8"
    
    //uses "PiPicoWHardware"
    uses "ChallengerHardware"
    
    uses "/Source/Library/Devices/HX711"
    
    LoadCell cell;
    int prevMax;
    int prevCurrent;
    
    const long gramFactor = 237; // calibrated using a gym weight (number of grams per reading value from HX711)
    
    const int titleHeight = Font.CellHeight * 2;
    
    ButtonISR(byte pin, PinStatus status)
    {
        Reset(cell);
    }
    
    Reset(LoadCell cell)
    {
        uint count;
        long total;
        
        loop
        {
            if (HX711.IsReady(cell))
            {
                long value = HX711.Read(cell, false);
                if (value != -1)
                {
                    total += value;
                    count++;
                }
            }
            Delay(50);
            if (count == 5)
            {
                break;
            }
        }
        total /= 5;
        
        HX711.SetTare(cell, total);
        IO.WriteLn("Tare: " + total.ToString());
        Screen.Clear();
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 0;
        Screen.Print("Tare Set");
        
        prevMax = -1;
        prevCurrent = -1;
    }
    
    
    DisplayKg(long maxGrams, long currentGrams, bool last)
    {
        Display.Suspend();
        
        long lineMaxGrams     = maxGrams     * (PixelHeight - titleHeight) / 10000; // 10kg load cell
        long lineCurrentGrams = currentGrams * (PixelHeight - titleHeight) / 10000; // 10kg load cell
        
        long kg = maxGrams / 1000;
        long grams = maxGrams - (kg * 1000);
        string gstring = kg.ToString() + "." + (grams.ToString()).LeftPad('0', 3) + " kg";
        string weight = gstring;
        
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 0;
        Screen.Print(weight.LeftPad(' ', Hardware.TextWidth));
        
        int y = PixelHeight - int(lineMaxGrams);
        if (y != prevMax)
        {
            if (prevMax != -1)
            {
                Display.HorizontalLine(0, prevMax, PixelWidth-1, Colour.Black);
            }
            Display.HorizontalLine(0, y, PixelWidth-1, ForeColour);
            prevMax = y;
        }
        
        Screen.CursorX = Hardware.TextLeft;
        Screen.CursorY = 1;
            
        if (last)
        {
            Screen.Print((" ").LeftPad(' ', Hardware.TextWidth), Colour.Orange, Colour.Black);
        }
        else
        {
            kg = currentGrams / 1000;
            grams = currentGrams - (kg * 1000);
            gstring = kg.ToString() + "." + (grams.ToString()).LeftPad('0', 3) + " kg";
            weight = gstring;
            
            Screen.Print(weight.LeftPad(' ', Hardware.TextWidth), Colour.Orange, Colour.Black);
            
            if (lineCurrentGrams == lineMaxGrams)
            {
                lineCurrentGrams--;
            }
        }
        y = PixelHeight - int(lineCurrentGrams);
        if (y != prevCurrent)
        {
            if (prevCurrent != -1)
            {
                Display.HorizontalLine(0, prevCurrent, PixelWidth-1, Colour.Black);
            }
            if (!last)
            {
                Display.HorizontalLine(0, y, PixelWidth-1, Colour.Orange);
            }
            prevCurrent = y;
        }
        
        
        Display.Resume();
    }
   
    Hopper()
    {
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!Hardware.Initialize(ref cell, buttonDelegate))
        {
            return;
        }
        
        Reset(cell);
        
        bool newReading = true;
        long maxGrams = 0;
        loop
        {
            if (HX711.IsReady(cell))
            {
                long reading = HX711.Read(cell, true);
                long grams = reading / gramFactor;
                if (grams >= 50)
                {
                    if (grams > maxGrams)
                    {
                        maxGrams = grams;
                    }
                    DisplayKg(maxGrams, grams, false);
                    newReading = false;
                }
                else
                {
                    if (!newReading)
                    {
                        DisplayKg(maxGrams, grams, true);
                        maxGrams = 0;
                        newReading = true;
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
