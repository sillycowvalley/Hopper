unit WebTime
{
    uses "/Source/Samples/MCU/Secrets2/Connect" // WiFi password
    uses "/Source/System/Serialize"             // to parse the JSON returned by the time webservice
    
    uses "DateTime"
    
    string wifiIP;
    byte   lastError;
    long   startTime;

    bool wifiConnected = false;    
    bool timeRetrieved = false;
    uint weekOfYear; // 1..52?
    long utcTimeAtUpdate;
    long mcuStartMinutes;
    long mcuTimeAtUpdateMinutes;
    long localTimeAtUpdateMinutes;
    bool daylightSavings;                  // good defaults for NZ in case the time server doesn't provide 
    long dstOffsetMinutes           = 60;  //  3600 seconds
    long utcOffsetMinutes           = 720; // 43200 seconds
    
    
    byte LastError              { get { return lastError; } set { lastError = value; } }
    bool IsValid                { get { return timeRetrieved;   } }
    bool DST                    { get { return daylightSavings; } }
    
    long MCUTimeAtStart         { get { return mcuStartMinutes;          } }
    long MCUTimeNow             { get { return Millis / 60000;           } }
    long MCUTimeAtUpdate        { get { return mcuTimeAtUpdateMinutes;   } }
    long LocalTimeAtUpdate      { get { return localTimeAtUpdateMinutes; } }
    long LocalTimeNow           { get { return DateTime.NormalizeMinutes(LocalTimeAtUpdate + MCUTimeNow - MCUTimeAtUpdate); } }
    
    long MinutesSinceLastUpdate { get { return MCUTimeNow - mcuTimeAtUpdateMinutes; } }
    
    uint WeekOfYear             { get { return weekOfYear;               } }
    
    Start()
    {
        mcuStartMinutes = MCUTimeNow;
    }
            
    string StateMessage
    {
        get
        {
            string ipMessage = wifiIP;
            if (!ipMessage.Contains('.'))
            {
                ipMessage += ": 0x" + lastError.ToHexString(2);
            }
            string wifiStatus = WiFi.ToString(WiFi.Status);
            ipMessage += " (" + wifiStatus + ")";    
            return ipMessage;
        }
    }
    bool Connect()
    {
        wifiConnected = false;
        
        BlueLED = true; UpdateLEDs();
        
        uint attempts = 0;
        WiFiStatus wifiStatus;
        wifiIP   = " . . . ";
        loop
        {
            bool success;
            string ssdi = SSID2;
            string password = Password2;
            success = WiFi.Connect(ssdi, password);
            wifiStatus = WiFi.Status;
            WriteLn("Using: " + ssdi + " (" + WiFi.ToString(wifiStatus) + ")");
            if (success)
            {
                WriteLn("WiFi " + WiFi.ToString(wifiStatus));
                wifiIP = WiFi.IP;
                WriteLn("IP: " + wifiIP);
                if (wifiIP.Contains('.')) // has IP
                {
                    wifiConnected = true;
                    break;
                }
            }
            Write(".");
            attempts++;
            if (attempts >= 10)
            {
                WriteLn("Failed to connect to WiFi: '" + WiFi.ToString(wifiStatus) + "'");
                break;
            }
        }
        Delay(500); // at least 0.5 seconds so that we can see it
        
        BlueLED = false;
        UpdateLEDs();
        
        return wifiConnected;
    }
    Disconnect()
    {
        if (wifiConnected)
        {
            WiFi.Disconnect();
            wifiConnected = false;
        }
    }

#ifdef HTTP_HEADER_TIME
    UpdateTime()
    {
        loop
        {   
            lastError = 0;      
            if (!WebTime.Connect())
            {
                lastError = 0x01;
                break;
            }
            Write("Updating Time");
        
            // get GMT from any http response header
            string timetext;
            if (WebClient.GetRequest("arduino.tips/asciilogo.txt", ref timetext))
            {
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
                WriteLn(" '" + timetext + "'");
            }
            else
            {
                WriteLn(": GET request failed");
                lastError = 0x02;
                break;
            }
            
            mcuTimeAtUpdateMinutes = WebTime.MCUTimeNow;
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
            break;
        }
    }
#endif

#ifdef UTC_TIME 
    UpdateTime()
    {
        loop
        {   
            lastError = 0;
            if (!Connect())
            {
                lastError = 0x01;
                break;
            }
            Write("Updating Time");
            <string, variant> time;
            string timejson;
            if (WebClient.GetRequest("time.jsontest.com", ref timejson))
            {
                uint iBrace;
                if (!timejson.IndexOf('{', ref iBrace))
                {
                    WriteLn(": no JSON in response");
                    lastError = 0x03;
                    break;
                }
                timejson = timejson.Substring(iBrace);
                if (!Serialize.TryFromJSON(timejson, time))
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
            
            mcuTimeAtUpdateMinutes = WebTime.MCUTimeNow;
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
            break;
        }
    }
#endif

#ifdef COMPLETE_TIME
    UpdateTime()
    {
        loop
        {
            lastError = 0;
            if (!Connect())
            {
                lastError = 1;
                break;
            }
            Write("Updating Time");
            <string, variant> time;
            string timejson;
            if (WebClient.GetRequest("worldtimeapi.org/api/timezone/pacific/auckland", ref timejson))
            {
                uint iBrace;
                if (!timejson.IndexOf('{', ref iBrace))
                {
                    lastError = 0x03;
                    WriteLn(": no JSON in response");
                    break;
                }
                timejson = timejson.Substring(iBrace);
                if (!Serialize.TryFromJSON(timejson, time))
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
            mcuTimeAtUpdateMinutes = WebTime.MCUTimeNow;
            timeRetrieved = true;
            WriteLn(": success");
            foreach(var kv in time)
            {
                switch (kv.key)
                {
                    case "week_number": 
                    { 
                        weekOfYear = kv.value; 
                    }
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
                } // switch
            } // foreach
            localTimeAtUpdateMinutes = DateTime.NormalizeMinutes(utcTimeAtUpdate + utcOffsetMinutes + (daylightSavings ? dstOffsetMinutes : 0));           
            break;
        } // loop
    }
    
#endif
    
}
