program SunsetSwitch
{   
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    uses "/Source/Library/Devices/AdafruitEInk213TriColor"
    
    byte RelayPin { get { return Board.GP7; } }
    
    const uint secondsPerLap     = 30;    // how often do we check the times and update the relay?
    const uint minutesPerRefresh = 15;    // how often to refresh the ePaper?
    
    // good defaults for NZ
    const uint dstStartDay = 267;
    const uint dstEndDay   = 97;
    
    
    // good defaults for Nelson, NZ
    <byte, <string> > mySunsetTimes;
    InitializeTimes()
    {
        mySunsetTimes.Clear();
        <string> times;
        times.Clear(); times.Append("20:00"); times.Append("22:30");
        for (byte i = 0; i <= 6; i++)   { mySunsetTimes[i] = times; }
        for (byte i = 47; i <= 53; i++) { mySunsetTimes[i] = times; }
        times.Clear(); times.Append("20:00"); times.Append("22:00");
        for (byte i = 7; i <= 10; i++)   { mySunsetTimes[i] = times; }
        for (byte i = 44; i <= 46; i++) { mySunsetTimes[i] = times; }
        times.Clear(); times.Append("19:30"); times.Append("21:00");
        for (byte i = 11; i <= 13; i++) { mySunsetTimes[i] = times; }
        for (byte i = 40; i <= 43; i++) { mySunsetTimes[i] = times; }
        times.Clear(); times.Append("18:30"); times.Append("20:30");
        for (byte i = 14; i <= 39; i++) { mySunsetTimes[i] = times; }
    }
    
    // LED 'signals' for diagnostics:
    bool greenLED = false;
    bool blueLED  = false;   // blue for cycle alive pulse
    bool redLED   = false;   // red while updating the ePaper
    
    bool lightsOn = false;   // current state of relay    
    
    long initialHeapFree;
    long initialStackFree;
    
    <string> GetSunsetTimes(uint weekOfYear)
    {
        return mySunsetTimes[weekOfYear];
    }
    
    string resetDate;    // date of last reset
    string powerDate;    // date of last power cycle
    string currentTime;  // current time string, adjusted for DST
    uint currentMinutes; // current time in minutes, adjusted for DST
    string currentDate;  // current date string
    uint dayOfYear;      // current day of year
    bool dst;            // are we currently in DST?
    
    bool TryDSTFromDay(uint dayOfYear, ref bool dst)
    {
        if (dstEndDay < dstStartDay) 
        {
            dst = !((dayOfYear > dstEndDay) && (dayOfYear < dstStartDay)); // Southern Hemisphere
        }
        else
        {
            dst = ((dayOfYear >= dstStartDay) && (dayOfYear <= dstEndDay)); // Northern Hemisphere
        }
        return true;
    }
    
    UpdateLEDs()
    {
        NeoPixel.SetColor(0, redLED ? 255 : 0, greenLED ? 255 : 0, blueLED ? 255 : 0);
        NeoPixel.Show();
    }
    
    CheckTime()
    {
        currentTime = RTC.Time;
        currentDate = RTC.Date;
        _ = DateTime.TryTimeToMinutes(currentTime, ref currentMinutes);
        _ = TryDateToDays(currentDate, ref dayOfYear);
        _ = TryDSTFromDay(dayOfYear, ref dst);
        if (dst)
        {
            currentMinutes += 60;
            if (currentMinutes >= 1440)
            {
                currentMinutes -= 1440;
            }
            uint   hours = currentMinutes / 60;
            uint minutes = currentMinutes % 60;
            currentTime = (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2)  + ":" + currentTime.Substring(6,2);
        }
        IO.WriteLn("  Current Time: " + currentTime + " (" + (dst ? "DST" : "Not DST") + ")");  
    }
    
    InitializeRTC()
    {
        if (RTC.SetFromDebugger())
        {
            // this will only update the time if we are running in debugger
            currentDate = RTC.Date;
            _ = TryDateToDays(currentDate, ref dayOfYear);
            _ = TryDSTFromDay(dayOfYear, ref dst);  
            if (dst)
            {        
                // if we are currently on DST, normalize the time in the RTC to standard time (no DST)
                currentTime = RTC.Time;
                _ = DateTime.TryTimeToMinutes(currentTime, ref currentMinutes);
                if (currentMinutes >= 60)
                {
                    currentMinutes -= 60;
                }
                else
                {
                    currentMinutes += (23 * 60);
                }
                uint   hours = currentMinutes / 60;
                uint minutes = currentMinutes % 60;
                currentTime = (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2)  + ":" + currentTime.Substring(6,2);
                RTC.Time = currentTime;
            }
            IO.WriteLn("  Set RTC Standard Time: " + currentTime + (dst ? " (+1 hour for DST)" : "")); 
            resetDate = currentDate; 
            file rtcFile = File.Create("RTC"); // store the date we last initialized the RTC (in case there is a power cycle)
            rtcFile.Append(resetDate);
            rtcFile.Flush();
        }
        else
        {
            _ = File.TryReadAllText("RTC", ref resetDate); // restore the date we last initialized the RTC (in the event of a power cycle)
        }
        powerDate = RTC.Date;
    }
        
    RefreshEPaper()
    {
        IO.Write("Refreshing Display");
        loop
        {
            // draw to the ePaper here
            Display.Suspend();
            
            Screen.ForeColour = Colour.Black;
            Screen.BackColour = Colour.White;
            Screen.Clear();
            Rectangle(0, 0, Display.PixelWidth,   Display.PixelHeight,   Colour.Black);
            Rectangle(2, 2, Display.PixelWidth-4, Display.PixelHeight-4, Colour.Red);
            
            int cellw = Font.CellWidth+1;
            int cellh = Font.CellHeight+1;
            string title = " Bigger Tigger - Sunset Lights";
            FilledRectangle(3 * cellw - cellw/2,     2 * cellh - cellh/2,     int(title.Length+1) * cellw+2, 2*cellh-2, Colour.Red);
            Rectangle      (3 * cellw - cellw/2 - 1, 2 * cellh - cellh/2 - 1, int(title.Length+1) * cellw+4, 2*cellh,   Colour.Black);
            
            string label = lightsOn ? "On " : "Off";
            int switchCol = 6 + int(title.Length);
            FilledRectangle(switchCol * cellw - cellw/2 - 3, 2 * cellh - cellh/2,     int(label.Length+1) * cellw+3, 2*cellh-2, lightsOn ? Colour.Red : Colour.Black);
            Rectangle      (switchCol * cellw - cellw/2 - 4, 2 * cellh - cellh/2 - 1, int(label.Length+1) * cellw+5, 2*cellh,   lightsOn ? Colour.Black : Colour.Red);
            Screen.SetCursor(byte(switchCol), 2);
            Screen.Print(label, Colour.White, lightsOn ? Colour.Red : Colour.Black);
            
            // Labels:
            Screen.SetCursor(3,2);
            Screen.Print(title, Colour.White, Colour.Red);
            Screen.SetCursor(3,7);
            Screen.Print("Time:", Colour.Black, Colour.White);
            Screen.SetCursor(3,8);
            Screen.Print("Date:", Colour.Black, Colour.White);
            
            Screen.SetCursor(3,10);
            Screen.Print("Lights On:  ", Colour.Black, Colour.White);
            Screen.SetCursor(3,11);
            Screen.Print("Lights Off: ", Colour.Black, Colour.White);
            
            Screen.SetCursor(18,7);
            Screen.Print(currentTime.Substring(0,5) + " (" + (dst ? "DST" : "No DST") + ")", Colour.Red, Colour.White);
            Screen.SetCursor(18,8);
            Screen.Print(currentDate, Colour.Red, Colour.White);
            
            
            <string> times = GetSunsetTimes((dayOfYear / 7) + 1); // week of year
            Screen.SetCursor(18,10);
            Screen.Print(times[0], Colour.Red, Colour.White);
            Screen.SetCursor(18,11);
            Screen.Print(times[1], Colour.Red, Colour.White);

            Screen.SetCursor(3,4);
            Screen.Print("Reset: ", Colour.Black, Colour.White);
            Screen.Print(resetDate, Colour.Red, Colour.White);
            Screen.Print("  Power: ", Colour.Black, Colour.White);
            Screen.Print(powerDate, Colour.Red, Colour.White);
            
            long currentHeapFree = MCU.HeapFree();            
            long currentStackFree = MCU.StackFree();
            if (initialHeapFree != currentHeapFree)
            {
                Screen.SetCursor(3,5);
                Screen.Print("Diagnostics: ", Colour.Black, Colour.White);
                Screen.SetCursor(16,5);
                Screen.Print("Heap difference = " + (currentHeapFree-initialHeapFree).ToString(), Colour.Red, Colour.White);
            }
            else if (initialStackFree != currentStackFree)
            {
                Screen.SetCursor(3,5);
                Screen.Print("Diagnostics: ", Colour.Black, Colour.White);
                Screen.SetCursor(16,5);
                Screen.Print("Stack difference = " + (currentStackFree-initialStackFree).ToString(), Colour.Red, Colour.White);
            }
            
            Display.Resume();
            
            break;
        }
        IO.WriteLn();        
        Delay(500); // at least 0.5 seconds so that we can see it    
    }
    
       
    Hopper() {
    
        IO.WriteLn();
        
        NeoPixel.BuiltIn();
        MCU.PinMode(RelayPin, MCU.PinModeOption.Output);
        MCU.DigitalWrite(RelayPin, false);
        
        UpdateLEDs();
        
        // allowing debugger to break-in before time call
        for (uint i=0; i < 10; i++)
        {
            Delay(500);
            Write('-');
            redLED = !redLED; greenLED = !greenLED; 
            UpdateLEDs();
        }
        greenLED = false; redLED = false; UpdateLEDs();
        IO.WriteLn();
        
        InitializeTimes();
        
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        InitializeRTC();
        
        DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
     
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        uint lastRefreshMinutes;
        uint lap = 0;
        bool refreshEPaper = true;
        initialHeapFree = MCU.HeapFree();
        initialStackFree = MCU.StackFree();
        
        loop
        {
            lap++;
            CheckTime();
            
            // update ePaper
            if (lap == 1)
            {
                refreshEPaper = true;
                continue;
            }
            
            bool oldState = lightsOn;
            
            <string> times = GetSunsetTimes((dayOfYear / 7) + 1); // week of year
            uint onTime;  
            uint offTime; 
            _ = DateTime.TryTimeToMinutes(times[0], ref onTime);
            _ = DateTime.TryTimeToMinutes(times[1], ref offTime);
            
            lightsOn = (currentMinutes >= onTime) && (currentMinutes <= offTime);
        
            MCU.DigitalWrite(RelayPin, lightsOn);
            
            // Refresh the ePaper display?
            if (refreshEPaper || (oldState != lightsOn) || (currentMinutes - lastRefreshMinutes >= minutesPerRefresh))
            {
                redLED = true;  UpdateLEDs();
                RefreshEPaper();
                redLED = false; UpdateLEDs();
                lastRefreshMinutes = currentMinutes;
                refreshEPaper = false;
            }
            DelaySeconds(secondsPerLap);
            
            // pulse the built-in LED so we can see the cycles
            blueLED = true;  UpdateLEDs();
            Delay(500); 
            blueLED = false; UpdateLEDs();
        }
        
    }
}
