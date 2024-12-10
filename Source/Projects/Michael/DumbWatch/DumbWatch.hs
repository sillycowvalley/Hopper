program DumbWatch
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    uses "/Source/Library/GPS"
    
    // good defaults for NZ
    const uint dstStartDay  = 267;
    const uint dstEndDay    = 97;
    const string nzTimeZone = "+12:00";
    
    const string logPathA = "/SD/ActivityA.gpx";
    const string logPathB = "/SD/ActivityB.gpx";
    
    record Point
    {
        string latitude;
        string longitude;
        string elevation;
        string time;
    }
    
    string resetDate;    // date of last reset
    string powerDate;    // date of last power cycle
    
    bool logging;
    bool doFlush;
    uint points;
    <Point> log;
    
    SaveLog()
    {
        if (!SD.Mount())
        {
            IO.WriteLn("Failed to initialize SD");
            return;
        }
        LED = true;
        IO.WriteLn("SD card detected.");
        long start = Millis;
        
        string sourcePath      = logPathA;
        string destinationPath = logPathB;
        if (File.Exists(logPathB))
        {
            sourcePath      = logPathB;
            destinationPath = logPathA;
        }
        
        
        file sourceFile      = File.Open  (sourcePath);
        file destinationFile = File.Create(destinationPath);
        
        destinationFile.Append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + Char.EOL);
        destinationFile.Append("<gpx xmlns=\"http://www.topografix.com/GPX/1/1\">" + Char.EOL);
        destinationFile.Append("<trk><name>Hopper Log</name><type>running</type><trkseg>" + Char.EOL);
        
        string line;
        loop
        {
            line = sourceFile.ReadLine();
            if (!sourceFile.IsValid()) { break; }
            if (line.Contains("<trkpt"))
            {
                destinationFile.Append(line + Char.EOL);
            }
        }
        foreach (var pt in log)
        {
            line = "  <trkpt lat=\"" + pt.latitude + '"' +
                           " lon=\"" + pt.longitude + "\">" +
                           "<ele>" + pt.elevation + "</ele>" + 
                          "<time>" + pt.time + "</time></trkpt>";
            destinationFile.Append(line + Char.EOL);
        }
        destinationFile.Append("</trkseg></trk></gpx>" + Char.EOL);
        
        destinationFile.Flush();
        if (destinationFile.IsValid())
        {
            IO.WriteLn("Flushed '" + destinationPath + "'");
            File.Delete(sourcePath);
            SD.Eject();
        }
        else
        {
            IO.WriteLn("Flush Invalid.");
        }
        log.Clear();
        long elapsed = Millis - start;
        IO.WriteLn("Elapsed: " + elapsed.ToString() + "ms (" + (float(elapsed) / points).ToString() + "ms per point)");
        LED = false;
    }
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        switch (PinToButton(pin))
        {
            case "A":
            {
                logging = true;
            }
            case "B":
            {
                logging = false;
            }
            case "C":
            {
                doFlush = true;
            }
        }
    }  
    
    string AdjustTimeFromDST(string time, bool dst)
    {
        string dstTime = time;
        if (dst)
        {
            // if we are currently on DST, normalize the time in the RTC to standard time (no DST)
            uint currentMinutes;
            _ = DateTime.TryTimeToMinutes(time, ref currentMinutes);
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
            dstTime = (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
            if (time.Length > 5)
            {
                dstTime = dstTime + ":" + time.Substring(6,2);
            }       
        }
        return dstTime;
    }
    string AdjustTimeToDST(string time, bool dst)
    {
        string dstTime = time;
        if (dst)
        {
            // if we are currently on DST, normalize the time in the RTC to standard time (no DST)
            uint currentMinutes;
            _ = DateTime.TryTimeToMinutes(time, ref currentMinutes);
            currentMinutes += 60;
            if (currentMinutes >= 1440) // 24 x 60
            {
                currentMinutes -= 1440;
            }
            uint   hours = currentMinutes / 60;
            uint minutes = currentMinutes % 60;
            dstTime = (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
            if (time.Length > 5)
            {
                dstTime = dstTime + ":" + time.Substring(6,2);
            }
        }
        return dstTime;
    }
    
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
    
    InitializeRTC()
    {
        
        uint dayOfYear;      // current day of year
        bool dst;            // are we currently in DST?
        string currentDate;
        string currentTime;
    
        currentDate = RTC.Date;
        if (RTC.SetFromDebugger())
        {
            // this will only update the time if we are running in debugger
            currentTime = RTC.Time;
            _ = TryDateToDays(currentDate, ref dayOfYear);
            _ = TryDSTFromDay(dayOfYear, ref dst);  
            currentTime = AdjustTimeFromDST(currentTime, dst);
            IO.WriteLn("  Set RTC Standard Time: " + currentTime + (dst ? " (+1 hour for DST)" : "")); 
            RTC.Time = currentTime;
            resetDate = currentDate; 
            file rtcFile = File.Create("RTC"); // store the date we last initialized the RTC (in case there is a power cycle)
            rtcFile.Append(resetDate);
            rtcFile.Flush();
        }
        else
        {
            _ = File.TryReadAllText("RTC", ref resetDate); // restore the date we last initialized the RTC (in the event of a power cycle)
        }
        powerDate = currentDate;
    }   
    
    UpdateDisplay(string time, bool dst, string latitude, string longitude, string elevation)
    {
        string state = (logging? "On" : "Off");
        Display.Suspend();
        Screen.Clear();
        Screen.PrintLn("Time:  " + AdjustTimeToDST(time, dst));
        Screen.PrintLn("Lat:   " + latitude);
        Screen.PrintLn("Lon:   " + longitude);
        Screen.PrintLn("Elev:  " + elevation);
        Screen.PrintLn("Log:   " + points.ToString() + " pts (" + state + ")");
        Screen.PrintLn("Set:   " + resetDate);
        Screen.PrintLn("Start: " + powerDate);
        Display.Resume();
    }
    Hopper()
    {
        if (!GPS.Begin())
        {
            IO.WriteLn("Failed to initialize Mini GPS");
            return;
        }
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        InitializeRTC();
        
        File.Delete(logPathA);
        File.Delete(logPathB);
        
        DisplayDriver.FlipX = true;
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize OLED display");
            return;
        }
        Display.Clear();
        
        bool   dst;            // are we currently in DST?
        string lastDate;
        string lastTime;
        string lastLatitude;
        string lastLongitude;
        string lastElevation;
        bool   lastLogging;
        
        
        loop
        {
            string sentence;
            string time = (RTC.Time).Substring(0,5);
            if (lastDate != RTC.Date)
            {
                // if date changes, update DST
                uint dayOfYear;      
                lastDate = RTC.Date;
                _ = TryDateToDays(lastDate, ref dayOfYear);
                _ = TryDSTFromDay(dayOfYear, ref dst);  
            }
            if (time != lastTime)
            {
                lastTime      = time;
                UpdateDisplay(lastTime, dst, lastLatitude, lastLongitude, lastElevation);
            }
            if (lastLogging != logging)
            {
                lastLogging = logging;
                UpdateDisplay(lastTime, dst, lastLatitude, lastLongitude, lastElevation);
            }    
            
            if (GPS.NextSentence(ref sentence))
            {
                if (GPS.Consume(sentence))
                {
                    IO.WriteLn(sentence);  
                    if ((GPS.Latitude  != lastLatitude) 
                     || (GPS.Longitude != lastLongitude) 
                     || (GPS.Elevation != lastElevation)
                       )
                    {
                        lastLatitude  = GPS.Latitude;
                        lastLongitude = GPS.Longitude;
                        lastElevation = GPS.Elevation;
                        if (logging && (lastElevation != "M"))
                        {
                            Point pt;
                            pt.latitude  = GPS.DecimalLatitude;
                            pt.longitude = GPS.DecimalLongitude;
                            pt.elevation = lastElevation;
                            pt.time      = lastDate + "T" + (RTC.Time) + nzTimeZone;
                            points++;
                            log.Append(pt);
                            if ((log.Count) % 10 == 0)
                            {
                                SaveLog(); 
                            }
                        }
                        UpdateDisplay(lastTime, dst, lastLatitude, lastLongitude, lastElevation + "m");
                    }
                }
            }
            if (doFlush)
            {
                if (log.Count != 0)
                {
                    SaveLog();
                }
                doFlush = false;
            }
        }
    }
}
 
 

