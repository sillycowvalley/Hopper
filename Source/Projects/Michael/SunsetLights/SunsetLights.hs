program SunsetLights
{   
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    
    uses "/Source/Library/Devices/AdafruitEInk213TriColor"
    
    
#define HTTP_HEADER_TIME // just use the server time in the HTTP response headers (GMT) 
//#define UTC_TIME         // time server that serves up GMT time and date
//#define COMPLETE_TIME    // time server that serves up UTC, time zone offset, and daylight savings (flag and offset)
    
    uses "DateTime"
    uses "WebTime"
    
    byte RelayPin { get { return Board.GP13; } }
    
    const uint minutesPerCycle   = 1440;  // this is intended to be 24 hours (1440 minutes) but short cycles are nice for testing 
    const uint secondsPerLap     = 30;    // how often do we check the times and update the relay?
    const uint minutesPerRefresh = 15;    // how often to refresh the ePaper?
    const uint minutesPerUpdate  = 360;   // how often do we check the time?
    
    // 4 LED 'signals' for diagnostics:
    bool redLED = false;     // red while updating the ePaper
    bool greenLED = false;   // green while getting the time from the web
    bool blueLED = false;    // blue while connecting to WiFi
                             // cyan (green+blue) while processing a valid response from WebClient.GetRequest(..)
    bool builtInLED = false; // 'alive' pulse on each lap in the cycle
    
    bool   lightsOn = false;
    
    bool BlueLED { set { blueLED = value; } }
    
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
    <string> GetSunsetTimes(uint weekOfYear)
    {
        return mySunsetTimes[weekOfYear];
    }
       
    UpdateLEDs()
    {
        LED = builtInLED;
        NeoPixel.SetColor(0, redLED ? 255 : 0, greenLED ? 255 : 0, blueLED ? 255 : 0);
        NeoPixel.Show();
        //string colour = (redLED ? "R" : "") + (greenLED ? "G" : "") + (blueLED ? "B" : "");
        //WriteLn("LED=" + colour);
    }
        
    RefreshEPaper()
    {
        Write("Refreshing Display");
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
            Screen.SetCursor(3,4);
            Screen.Print("WiFi: ", Colour.Black, Colour.White);
            Screen.SetCursor(3,6);
            Screen.Print("Time Updated:", Colour.Black, Colour.White);
            Screen.SetCursor(3,7);
            Screen.Print("Last Refresh:", Colour.Black, Colour.White);
            
            Screen.SetCursor(3,9);
            Screen.Print("Lights On:  ", Colour.Black, Colour.White);
            Screen.SetCursor(3,10);
            Screen.Print("Lights Off: ", Colour.Black, Colour.White);
            
            // Data:
            Screen.SetCursor(9,4);
            
            Screen.Print(WebTime.StateMessage , Colour.Red, Colour.White);
            
            Screen.SetCursor(18,6);
            if (WebTime.IsValid)
            {
                Screen.Print(DateTime.MinutesToTime(WebTime.LocalTimeAtUpdate), Colour.Red, Colour.White);
                Screen.SetCursor(18,7);
                Screen.Print(DateTime.MinutesToTime(WebTime.LocalTimeNow) + " (" + (WebTime.DST ? "DST" : "No DST") + ")", Colour.Red, Colour.White);
            }
            else
            {
                Screen.Print("Failed: 0x" + WebTime.LastError.ToHexString(2));
            }
            
            Screen.SetCursor(18,9);
            <string> times = GetSunsetTimes(WebTime.WeekOfYear);
            Screen.Print(times[0], Colour.Red, Colour.White);
            Screen.SetCursor(18,10);
            Screen.Print(times[1], Colour.Red, Colour.White);
            
            Display.Resume();
            
            break;
        }
        WriteLn();        
        Delay(500); // at least 0.5 seconds so that we can see it    
        
        WebTime.Disconnect();
    }
    
    Hopper() {
    
        WriteLn();
        
        NeoPixel.BuiltIn();
        MCU.PinMode(RelayPin, MCU.PinModeOption.Output);
        MCU.DigitalWrite(RelayPin, false);
        
        UpdateLEDs();
        
        // allowing debugger to break-in before time call
        for (uint i=0; i < 10; i++)
        {
            Delay(500);
            Write('-');
            builtInLED = !builtInLED; redLED = !redLED; greenLED = !greenLED; UpdateLEDs();
        }
        builtInLED = false; greenLED = false; redLED = false; UpdateLEDs();
        WriteLn();
        
        InitializeTimes();
        
        //DisplayDriver.IsPortrait = true;
        DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
     
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        WebTime.Start();
        WriteLn("Start: " + (WebTime.MCUTimeAtStart).ToString() + " minutes");
        
        uint lap = 0;
        long lastUpdateMinutes;
        long lastRefreshMinutes;
        bool refreshEPaper = true;
        loop
        {
            uint elapsedMinutes = WebTime.MCUTimeNow - WebTime.MCUTimeAtStart;
            lap++;
            WriteLn("Elapsed: " + elapsedMinutes.ToString() + " minutes, " + lap.ToString());
            WriteLn("  CurrentTime: " + DateTime.MinutesToTime(WebTime.LocalTimeNow));
            
            // Health Reboots?
            if ((elapsedMinutes >= minutesPerCycle) && (WebTime.LastError == 0))
            {
                WriteLn("Rebooting");
                MCU.Reboot(false);
            }
            
            // Update the time from the web?
            if ((lap == 1) || (WebTime.MinutesSinceLastUpdate >= minutesPerUpdate))
            {
                greenLED = true;   UpdateLEDs();
                Delay(500);
                WebTime.UpdateTime();
                Delay(500);
                greenLED = false;  UpdateLEDs();
                
                refreshEPaper = true;
                continue;
            }
            bool oldState = lightsOn;
            if (WebTime.IsValid)
            {
                <string> times = GetSunsetTimes(WebTime.WeekOfYear);
                uint onTime;  
                uint offTime; 
                _ = DateTime.TryTimeToMinutes(times[0], ref onTime);
                _ = DateTime.TryTimeToMinutes(times[1], ref offTime);
                
                lightsOn = (WebTime.LocalTimeNow >= onTime) && (WebTime.LocalTimeNow <= offTime);
            }
            else
            {
                lightsOn = false;
            }
            MCU.DigitalWrite(RelayPin, lightsOn);
            
            // Refresh the ePaper display?
            if (refreshEPaper || (oldState != lightsOn) || (WebTime.MCUTimeNow - lastRefreshMinutes >= minutesPerRefresh))
            {
                redLED = true;  UpdateLEDs();
                RefreshEPaper();
                redLED = false; UpdateLEDs();
                lastRefreshMinutes = WebTime.MCUTimeNow;
                refreshEPaper = false;
            }
            WriteLn("Heap: " + (MCU.HeapFree()).ToString() + ", Stack: " + (MCU.StackFree()).ToString());
            DelaySeconds(secondsPerLap);
            LED = true; Delay(500); LED = false; // pulse the built-in LED so we can see the cycles
        }
        
    }
}
