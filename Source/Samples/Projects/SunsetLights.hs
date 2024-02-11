program SunsetLights
{   
    #define CHALLENGER_RP2040_WIFI
    
    const byte RelayPin = 10;
    
    uses "/Source/Library/Devices/AdafruitThinkInk213TriColor"
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Samples/MCU/Secrets2/Connect" // WiFi password
    uses "/Source/System/Serialize"             // to parse the JSON returned by the time webservice
    
    
    const uint minutesPerCycle   = 1440;  // this is intended to be 24 hours (1440 minutes) but short cycles are nice for testing 
    const uint secondsPerLap     = 30;    // how often do we check the times and update the relay?
    const bool updateDisplay     = true;  // don't update the display while debugging (avoid punishing the ePaper)
    const bool updateTime        = true;  // avoid hammering the webservice while testing (don't want to get blocked)
    const uint minutesPerRefresh = 15;    // how often to refresh the ePaper?
    const uint minutesPerUpdate  = 360;   // how often do we check the time?
    
    // 4 LED 'signals' for diagnostics:
    bool redLED = false;     // red while updating the ePaper
    bool greenLED = false;   // green while getting the time from the web
    bool blueLED = false;    // blue while connecting to WiFi
    bool builtInLED = false; // 'alive' pulse on each lap in the cycle
    
    bool   wifiConnected = false;
    bool   lightsOn = false;
    string wifiIP;
    string wifiZone;
    
    bool timeRetrieved = false;
    <string, variant> time;
    uint weekOfYear; // 1..52?
    bool daylightSavings;
    long dstOffsetMinutes;
    long utcOffsetMinutes;
    long utcTimeAtUpdate;
    long mcuTimeAtUpdateMinutes;
    long localTimeAtUpdateMinutes;
    
    <byte, <string> > timesForNelson;
    
    InitializeTimes()
    {
        timesForNelson.Clear();
        <string> times;
        times.Clear(); times.Append("20:00"); times.Append("22:30");
        for (byte i = 0; i <= 6; i++)   { timesForNelson[i] = times; }
        for (byte i = 47; i <= 53; i++) { timesForNelson[i] = times; }
        times.Clear(); times.Append("20:00"); times.Append("22:00");
        for (byte i = 7; i <= 9; i++)   { timesForNelson[i] = times; }
        for (byte i = 44; i <= 46; i++) { timesForNelson[i] = times; }
        times.Clear(); times.Append("19:30"); times.Append("21:00");
        for (byte i = 11; i <= 13; i++) { timesForNelson[i] = times; }
        for (byte i = 40; i <= 43; i++) { timesForNelson[i] = times; }
        times.Clear(); times.Append("18:30"); times.Append("20:30");
        for (byte i = 14; i <= 39; i++) { timesForNelson[i] = times; }
    }
    <string> GetWeekTimes(uint weekOfYear)
    {
        return timesForNelson[weekOfYear];
    }
    string MinutesToTime(long dayMinutes)
    {
        long hours   = dayMinutes / 60;
        long minutes = dayMinutes % 60;
        return (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
    }
    long TimeToMinutes(string time)
    {
        uint hours;
        uint minutes;
        uint iT;
        if (time.IndexOf('T', ref iT)) { time = time.Substring(iT+1); }
        _ = UInt.TryParse(time.Substring(0, 2), ref hours);
        _ = UInt.TryParse(time.Substring(3, 2), ref minutes);
        long totalMinutes = hours * 60 + minutes;
        return totalMinutes;
    }
    long NormalizeMinutes(long minutes)
    {
        while (minutes > 24*60) { minutes -= 24*60; }
        while (minutes < 0)     { minutes += 24*60; }
        return minutes;
    } 
    
    UpdateLEDs()
    {
        LED = builtInLED;
        NeoPixel.SetColor(0, redLED ? 255 : 0, greenLED ? 255 : 0, blueLED ? 255 : 0);
        NeoPixel.Show();
    }
        
    RefreshEPaper()
    {
        redLED = true; UpdateLEDs();
        Write("Refreshing Display");
        loop
        {
            if (!updateDisplay) { Write(" : skipped"); break; }
            
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
            Screen.Print("Last Update:", Colour.Black, Colour.White);
            Screen.SetCursor(3,7);
            Screen.Print("Last Refresh:", Colour.Black, Colour.White);
            
            Screen.SetCursor(3,9);
            Screen.Print("Lights On:  ", Colour.Black, Colour.White);
            Screen.SetCursor(3,10);
            Screen.Print("Lights Off: ", Colour.Black, Colour.White);
            
            
            long nowMinutes     = Millis / 60000;
            long deltaMinutes   = nowMinutes - mcuTimeAtUpdateMinutes;
            long currentMinutes = NormalizeMinutes(localTimeAtUpdateMinutes + deltaMinutes);
            
            // Data:
            Screen.SetCursor(11,4);
            Screen.Print(wifiIP + " (" + wifiZone + ")", Colour.Red, Colour.White);
            
            Screen.SetCursor(18,6);
            if (timeRetrieved)
            {
                Screen.Print(MinutesToTime(localTimeAtUpdateMinutes), Colour.Red, Colour.White);
                Screen.SetCursor(18,7);
                Screen.Print(MinutesToTime(currentMinutes) + " (" + (daylightSavings ? "DST" : "No DST") + ")", Colour.Red, Colour.White);
            }
            else
            {
                Screen.Print("Failed");
            }
            
            Screen.SetCursor(18,9);
            <string> times = GetWeekTimes(weekOfYear);
            Screen.Print(times[0], Colour.Red, Colour.White);
            Screen.SetCursor(18,10);
            Screen.Print(times[1], Colour.Red, Colour.White);
            
            Display.Resume();
            
            DisplayDriver.PowerDown();
            break;
        }
        WriteLn();        
        Delay(500); // at least 0.5 seconds so that we can see it    
        redLED = false; UpdateLEDs();
    }
    
    WiFiConnect()
    {
        uint attempts = 0;
        wifiConnected = false; 
        wifiIP = " . . . ";
        wifiZone = "none";
        blueLED = true;  UpdateLEDs();
        loop
        {
            bool success;
            success = WiFi.Connect(SSID2, Password2);
            if (success)
            {
                WriteLn("WiFi connected");
                wifiZone = "Garage";
                wifiIP = WiFi.IP;
                WriteLn("IP: " + wifiIP);
                WriteLn("Zone: " + wifiZone);
                wifiConnected = true;
                break;
            }
            success = WiFi.Connect(SSID, Password);
            if (success)
            {
                WriteLn("WiFi connected");
                wifiZone = "House";
                wifiIP = WiFi.IP;
                WriteLn("IP: " + wifiIP);
                WriteLn("Zone: " + wifiZone);
                wifiConnected = true;
                break;
            }
            Write(".");
            attempts++;
            if (attempts >= 10)
            {
                WriteLn("Failed to connect to WiFi");
                break;
            }
        }
        Delay(500); // at least 0.5 seconds so that we can see it
        blueLED = false;  UpdateLEDs();
    }
    
    UpdateTime()
    {
        greenLED = true;  UpdateLEDs();
        loop
        {
            WiFiConnect();
            if (!wifiConnected)
            {
                break;
            }
            Write("Updating Time");
            if (updateTime)
            {
                string timejson;
                if (WebClient.GetRequest("worldtimeapi.org/api/timezone/pacific/auckland", ref timejson))
                {
                    uint iBrace;
                    if (!timejson.IndexOf('{', ref iBrace))
                    {
                        WriteLn(": no JSON in response");
                        break;
                    }
                    timejson = timejson.Substring(iBrace);
                    if (!Serialize.TryFromJSON(timejson, ref time))
                    {
                        WriteLn(": deserialize failed");    
                        break;
                    }
                }
                else
                {
                    WriteLn(": GET request failed");    
                    break;
                }
                mcuTimeAtUpdateMinutes = Millis;
                mcuTimeAtUpdateMinutes /= 60000;
                timeRetrieved = true;
                WriteLn(": success");
                foreach(var kv in time)
                {
                    switch (kv.key)
                    {
                        case "week_number": { weekOfYear = kv.value; }
                        case "dst":         
                        { 
                            daylightSavings = kv.value; 
                        }
                        case "dst_offset":  
                        { 
                            dstOffsetMinutes = Variant.ToLong(kv.value); 
                            dstOffsetMinutes /= 60; 
                        }
                        case "raw_offset":  
                        { 
                            utcOffsetMinutes = Variant.ToLong(kv.value); 
                            utcOffsetMinutes /= 60; 
                        }
                        case "utc_datetime":    
                        { 
                            string utcDateTime = kv.value;
                            uint minutes = TimeToMinutes(utcDateTime);
                            utcTimeAtUpdate = minutes; 
                        }
                    }
                }
                localTimeAtUpdateMinutes = NormalizeMinutes(utcTimeAtUpdate + utcOffsetMinutes + dstOffsetMinutes);
            } // updateTime
            else
            {
                WriteLn(": skipped"); 
            }
            break;
        }
        Delay(500); // at least 0.5 seconds so that we can see it
        greenLED = false;  UpdateLEDs();
    }
    
    {
        WriteLn();
        
        NeoPixel.BuiltIn();
        MCU.PinMode(RelayPin, MCU.PinModeOption.Output);
        MCU.DigitalWrite(RelayPin, false);
        
        UpdateLEDs();
        
        InitializeTimes();
        
        //DisplayDriver.IsPortrait = true;
        //DisplayDriver.FlipX = true;
        //DisplayDriver.FlipY = true;
     
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        long start = Millis / 60000;
        WriteLn("Start: " + start.ToString() + " minutes");
        uint lap = 0;
        long lastUpdateMinutes;
        long lastRefreshMinutes;
        bool refreshEPaper = true;
        loop
        {
            long nowMinutes     = Millis / 60000;
            long deltaMinutes   = nowMinutes - mcuTimeAtUpdateMinutes;
            long currentMinutes = NormalizeMinutes(localTimeAtUpdateMinutes + deltaMinutes);
            
            uint elapsedMinutes = nowMinutes - start;
            lap++;
            WriteLn("Elapsed: " + elapsedMinutes.ToString() + " minutes, " + lap.ToString());
            WriteLn("  CurrentTime: " + MinutesToTime(currentMinutes));
            
            // Reboots?
            if (elapsedMinutes >= minutesPerCycle)
            {
                WriteLn("Rebooting");
                MCU.Reboot();
            }
            
            // Update the time from the web?
            if ((lap == 1) || (nowMinutes - lastUpdateMinutes >= minutesPerUpdate))
            {
                UpdateTime();
                lastUpdateMinutes = nowMinutes;
                refreshEPaper = true;
                continue;
            }
            bool oldState = lightsOn;
            if (timeRetrieved)
            {
                <string> times = GetWeekTimes(weekOfYear);
                long onTime  = TimeToMinutes(times[0]);
                long offTime = TimeToMinutes(times[1]);
                
                lightsOn = (currentMinutes >= onTime) && (currentMinutes <= offTime);
            }
            else
            {
                lightsOn = false;
            }
            MCU.DigitalWrite(RelayPin, lightsOn);
            
            // Refresh the ePaper display?
            if (refreshEPaper || (oldState != lightsOn) || (nowMinutes - lastRefreshMinutes >= minutesPerRefresh))
            {
                RefreshEPaper();
                lastRefreshMinutes = nowMinutes;
                refreshEPaper = false;
            }
            
            DelaySeconds(secondsPerLap);
            LED = true; Delay(500); LED = false; // pulse the built-in LED so we can see the cycles
        }
        
    }
}
