program SunsetLights
{   
    #define CHALLENGER_RP2040_WIFI
    
    const byte RelayPin = 10;
    
    uses "/Source/Library/Devices/AdafruitThinkInk213TriColor"
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Samples/MCU/Secrets2/Connect" // WiFi password
    uses "/Source/System/Serialize"             // to parse the JSON returned by the time webservice
    
    uses "/Source/Samples/Projects/DateTime"
    
    
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
                             // cyan (green+blue) while processing a valid response from WebClient.GetRequest(..)
    bool builtInLED = false; // 'alive' pulse on each lap in the cycle
    
    bool   wifiConnected = false;
    bool   lightsOn = false;
    string wifiIP;
    string wifiZone;
    byte   lastError;
    
    bool timeRetrieved = false;
    uint weekOfYear; // 1..52?
    long utcTimeAtUpdate;
    long mcuTimeAtUpdateMinutes;
    long localTimeAtUpdateMinutes;
    bool daylightSavings;                  // good defaults for NZ in case the time server doesn't provide 
    long dstOffsetMinutes           = 60;  //  3600 seconds
    long utcOffsetMinutes           = 720; // 43200 seconds
    
    <byte, <string> > mySunsetTimes;
    InitializeTimes()
    {
        mySunsetTimes.Clear();
        <string> times;
        times.Clear(); times.Append("20:00"); times.Append("22:30");
        for (byte i = 0; i <= 6; i++)   { mySunsetTimes[i] = times; }
        for (byte i = 47; i <= 53; i++) { mySunsetTimes[i] = times; }
        times.Clear(); times.Append("20:00"); times.Append("22:00");
        for (byte i = 7; i <= 9; i++)   { mySunsetTimes[i] = times; }
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
            long currentMinutes = DateTime.NormalizeMinutes(localTimeAtUpdateMinutes + deltaMinutes);
            
            // Data:
            Screen.SetCursor(11,4);
            Screen.Print(wifiIP + " (" + wifiZone + ")", Colour.Red, Colour.White);
            
            Screen.SetCursor(18,6);
            if (timeRetrieved)
            {
                Screen.Print(DateTime.MinutesToTime(localTimeAtUpdateMinutes), Colour.Red, Colour.White);
                Screen.SetCursor(18,7);
                Screen.Print(DateTime.MinutesToTime(currentMinutes) + " (" + (daylightSavings ? "DST" : "No DST") + ")", Colour.Red, Colour.White);
            }
            else
            {
                Screen.Print("Failed: 0x" + lastError.ToHexString(2));
            }
            
            Screen.SetCursor(18,9);
            <string> times = GetSunsetTimes(weekOfYear);
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
                wifiIP = WiFi.IP;
                WriteLn("IP: " + wifiIP);
                if (wifiIP.Contains('.')) // has IP
                {
                    wifiZone = "Garage";
                    WriteLn("Zone: " + wifiZone);
                    wifiConnected = true;
                    break;
                }
            }
            success = WiFi.Connect(SSID, Password);
            if (success)
            {
                WriteLn("WiFi connected");
                wifiIP = WiFi.IP;
                WriteLn("IP: " + wifiIP);
                if (wifiIP.Contains('.')) // has IP
                {
                    wifiZone = "House";
                    WriteLn("Zone: " + wifiZone);
                    wifiConnected = true;
                    break;
                }
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
            lastError = 0;      
            WiFiConnect();
            if (!wifiConnected)
            {
                lastError = 0x01;
                break;
            }
            Write("Updating Time");
            if (updateTime)
            {
                // get GMT from any http response header
                string timetext;
                if (WebClient.GetRequest("arduino.tips/asciilogo.txt", ref timetext))
                {
                    blueLED = true;  UpdateLEDs();
                    
                    uint iDate;
                    if (timetext.IndexOf("Date:", ref iDate))
                    {
                        timetext = timetext.Substring(iDate+5);
                    }
                    else
                    {
                        WriteLn(": GET no 'Date:'");
                        lastError = 0x03;
                        break;
                    }
                    uint iGMT;
                    if (timetext.IndexOf(" GMT", ref iGMT))
                    {
                        timetext = timetext.Substring(0, iGMT);
                    }
                    else
                    {
                        WriteLn(": GET no ' GMT'");
                        lastError = 0x04;
                        break;
                    }
                    timetext = timetext.Trim();
                    WriteLn("'" + timetext + "'");
                }
                else
                {
                    WriteLn(": GET request failed");
                    lastError = 0x02;
                    break;
                }
                
                mcuTimeAtUpdateMinutes = Millis;
                mcuTimeAtUpdateMinutes /= 60000;
                timeRetrieved = true;
                WriteLn(": success");
                
                // 'Tue, 15 Nov 1994 08:12:31'
                string timestring = timetext.Substring(timetext.Length-8);
                string datestring = timetext.Substring(0, timetext.Length-9).Trim();
                
                uint minutes;
                _ = DateTime.TryTimeToMinutes(timestring, ref minutes);
                utcTimeAtUpdate = minutes; 
                
                // 'Tue, 15 Nov 1994'
                uint days;
                _ = DateTime.TryDateToDays(datestring, ref days);
                _ = DateTime.TryDSTFromDay(days, ref daylightSavings);
                weekOfYear = (days / 7) + 1;
                
                localTimeAtUpdateMinutes = DateTime.NormalizeMinutes(utcTimeAtUpdate + utcOffsetMinutes + (daylightSavings ? dstOffsetMinutes : 0));
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
    
    /*
    UpdateTime()
    {
        greenLED = true;  UpdateLEDs();
        loop
        {   
            lastError = 0;
            WiFiConnect();
            if (!wifiConnected)
            {
                lastError = 0x01;
                break;
            }
            Write("Updating Time");
            if (updateTime)
            {
                <string, variant> time;
                string timejson;
                if (WebClient.GetRequest("time.jsontest.com", ref timejson))
                {
                    blueLED = true;  UpdateLEDs();
                    uint iBrace;
                    if (!timejson.IndexOf('{', ref iBrace))
                    {
                        WriteLn(": no JSON in response");
                        lastError = 0x03;
                        break;
                    }
                    timejson = timejson.Substring(iBrace);
                    if (!Serialize.TryFromJSON(timejson, ref time))
                    {
                        WriteLn(": deserialize failed");    
                        lastError = 0x04;
                        break;
                    }
                }
                else
                {
                    WriteLn(": GET request failed");    
                    lastError = 0x02;
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
                        case "date": // "02-11-2024"
                        {
                            string utcDate = kv.value;
                            uint days;
                            _ = DateTime.TryDateToDays(utcDate, ref days);
                            _ = DateTime.TryDSTFromDay(days, ref daylightSavings);
                            weekOfYear = (days / 7) + 1;
                        }
                        case "time": // "08:31:56 PM"
                        { 
                            string utcTime = kv.value;
                            uint minutes;
                            _ = DateTime.TryTimeToMinutes(utcTime, ref minutes);
                            utcTimeAtUpdate = minutes; 
                        }
                    }
                }
                localTimeAtUpdateMinutes = DateTime.NormalizeMinutes(utcTimeAtUpdate + utcOffsetMinutes + (daylightSavings ? dstOffsetMinutes : 0));
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
    */
    
    /*
    UpdateTime()
    {
        greenLED = true;  UpdateLEDs();
        loop
        {
            lastError = 0;
            WiFiConnect();
            if (!wifiConnected)
            {
                lastError = 1;
                break;
            }
            Write("Updating Time");
            if (updateTime)
            {
                <string, variant> time;
                string timejson;
                if (WebClient.GetRequest("worldtimeapi.org/api/timezone/pacific/auckland", ref timejson))
                {
                    blueLED = true;  UpdateLEDs();
                    uint iBrace;
                    if (!timejson.IndexOf('{', ref iBrace))
                    {
                        lastError = 0x03;
                        WriteLn(": no JSON in response");
                        break;
                    }
                    timejson = timejson.Substring(iBrace);
                    if (!Serialize.TryFromJSON(timejson, ref time))
                    {
                        lastError = 0x04;
                        WriteLn(": deserialize failed");    
                        break;
                    }
                }
                else
                {
                    lastError = 0x02;
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
                            uint minutes;
                            _ = DateTime.TryTimeToMinutes(utcDateTime, ref minutes);
                            utcTimeAtUpdate = minutes; 
                        }
                    }
                }
                localTimeAtUpdateMinutes = DateTime.NormalizeMinutes(utcTimeAtUpdate + utcOffsetMinutes + (daylightSavings ? dstOffsetMinutes : 0));            } // updateTime
            else
            {
                WriteLn(": skipped"); 
            }
            break;
        }
        Delay(500); // at least 0.5 seconds so that we can see it
        greenLED = false;  UpdateLEDs();
    }
    */
    {
        WriteLn();
        
        NeoPixel.BuiltIn();
        MCU.PinMode(RelayPin, MCU.PinModeOption.Output);
        MCU.DigitalWrite(RelayPin, false);
        
        UpdateLEDs();
        
        // allowing debugger to break-in before time call
        for (uint i=0; i < 20; i++)
        {
            Delay(250);
            Write('-');
        }
        WriteLn();
        
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
            long currentMinutes = DateTime.NormalizeMinutes(localTimeAtUpdateMinutes + deltaMinutes);
            
            uint elapsedMinutes = nowMinutes - start;
            lap++;
            WriteLn("Elapsed: " + elapsedMinutes.ToString() + " minutes, " + lap.ToString());
            WriteLn("  CurrentTime: " + DateTime.MinutesToTime(currentMinutes));
            
            // Reboots?
            if (elapsedMinutes >= minutesPerCycle)
            {
                WriteLn("Rebooting");
                MCU.Reboot(false);
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
                <string> times = GetSunsetTimes(weekOfYear);
                uint onTime;  
                uint offTime; 
                _ = DateTime.TryTimeToMinutes(times[0], ref onTime);
                _ = DateTime.TryTimeToMinutes(times[1], ref offTime);
                
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
