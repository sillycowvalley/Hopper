program DumbWatch
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    uses "/Source/Library/GPS"
    uses "/Source/System/DateTime"
    
    // good defaults for NZ
    const uint dstStartDay  = 267;
    const uint dstEndDay    = 97;
    const int  nzTimeZone   = 12;
    
    const string logPathA = "/SD/ActivityA.gpx";
    const string logPathB = "/SD/ActivityB.gpx";
    
    record Point
    {
        string latitude;
        string longitude;
        string elevation;
        string time;
    }
    
    bool logging;
    bool doFlush;
    uint points;
    <Point> log;
    
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
    string AdjustTimeZone(string time, int timeZone)
    {
        string adjustedTime;
        
        uint currentMinutes;
        _ = DateTime.TryTimeToMinutes(time, ref currentMinutes);
        int iMinutes = int(currentMinutes);
        iMinutes += timeZone * 60;
        while (iMinutes >= 1440) // 24 x 60
        {
            iMinutes -= 1440;
        }
        while (iMinutes <= 0)
        {
            iMinutes += 1440;
        }   
        currentMinutes = uint(iMinutes);
        uint hours = currentMinutes / 60;
        uint minutes = currentMinutes % 60;
        adjustedTime = (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
        if (time.Length > 5)
        {
            adjustedTime = adjustedTime + ":" + time.Substring(6,2);
        }
        return adjustedTime;
    }
    
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
        
    UpdateDisplay(string time, string date, bool dst, string latitude, string longitude, string elevation)
    {
        string state = (logging? "On" : "Off");
        Display.Suspend();
        Screen.Clear();
        Screen.PrintLn("Time:  " + AdjustTimeToDST(time, dst));
        Screen.PrintLn("Date:  " + date);
        Screen.PrintLn("Lat:   " + latitude);
        Screen.PrintLn("Lon:   " + longitude);
        Screen.PrintLn("Elev:  " + elevation);
        Screen.PrintLn("Log:   " + points.ToString() + " pts (" + state + ")");
        Display.Resume();
    }
    Hopper()
    {
        // SD card on the AdafruitAdaloggerRTCSDFeatherwing:
        SD.SPIController = 0;
        SD.ClkPin = Board.SPI0SCK;
        SD.TxPin  = Board.SPI0Tx;
        SD.RxPin  = Board.SPI0Rx;
        
        SD.CSPin  = Board.GP10;
        
        if (SD.Mount())
        {
            File.Delete(logPathA);
            File.Delete(logPathB);
            SD.Eject();
            IO.WriteLn("Log reset");
        }
        
        
        if (!GPS.Begin())
        {
            IO.WriteLn("Failed to initialize Mini GPS");
            return;
        }
        
        
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
            string time = (GPS.UTC).Substring(0,5);
            time = AdjustTimeZone(time, nzTimeZone);
            if (lastDate != GPS.Date)
            {
                // if date changes, update DST
                uint dayOfYear;      
                lastDate = GPS.Date;
                _ = TryDateToDays(lastDate, ref dayOfYear);
                _ = TryDSTFromDay(dayOfYear, ref dst);  
            }
            if (time != lastTime)
            {
                lastTime      = time;
                UpdateDisplay(lastTime, lastDate, dst, lastLatitude, lastLongitude, lastElevation);
            }
            if (lastLogging != logging)
            {
                lastLogging = logging;
                UpdateDisplay(lastTime, lastDate, dst, lastLatitude, lastLongitude, lastElevation);
            }    
            
            if (GPS.NextSentence(ref sentence))
            {
                if (GPS.Consume(sentence))
                {
                    //IO.WriteLn(sentence);
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
                            string timeZoneString = (nzTimeZone).ToString();
                            timeZoneString = timeZoneString.LeftPad('0', 2);
                            if (nzTimeZone >= 0)
                            {
                                timeZoneString = "+" + timeZoneString;
                            }
                            pt.time      = lastDate + "T" + time + timeZoneString + ":00"; // "+12:00"
                            points++;
                            log.Append(pt);
                            if ((log.Count) % 10 == 0)
                            {
                                SaveLog(); 
                            }
                        }
                        UpdateDisplay(lastTime, lastDate, dst, lastLatitude, lastLongitude, lastElevation + "m");
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
 
 

