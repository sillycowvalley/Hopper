program GarageBox
{
    //#define DIAGNOSTICS
    
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"   
    
    uses "/Source/Library/RTCs/DS3231Driver" 
    uses "/Source/Library/Devices/GenericRTC"
    
    uses "/Source/System/DateTime"
    
    const byte sensorPin = GP25; // A4
    const byte relayPin  = GP5;
    
    // good defaults for NZ
    const uint dstStartDay  = 267;
    const uint dstEndDay    = 97;
    
    const uint onDarkMinutes    = 3;
    const uint onOpenMinutes    = 10;
    
    const uint sixPMMinutes = 1080;
    const uint tenPMMinutes = 1320;
    
    string resetDate;    // date of last reset
    string powerDate;    // date of last power cycle


    uint currentMinutes;    
    uint lightTill;
    
    long initialHeapFree;
    long initialStackFree;
    
    
    Hopper()
    {
        byte counter;
        bool doorWasOpen;
        string command;
        
        
        MCU.PinMode(sensorPin, PinModeOption.Input);
        MCU.PinMode(relayPin,  PinModeOption.Output);
        MCU.PinMode(GP24,  PinModeOption.Output);
        MCU.PinMode(GP23,  PinModeOption.Output);
        MCU.DigitalWrite(relayPin, false);
        
        UART.Setup(9600);
        
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        // on reset, set time from debugger (if running in debugger)
        InitializeRTC();
        
        initialHeapFree = MCU.HeapFree();
        initialStackFree = MCU.StackFree();
        
        loop
        {
            uint light  = GPIO.A0;     
            bool doorOpen = MCU.DigitalRead(sensorPin);
#ifdef DIAGNOSTICS
            bool doorShut;
#endif            
            MCU.DigitalWrite(GP23, doorOpen);
            
            if (doorOpen)
            {
                if (!doorWasOpen)
                {
                    counter = 0;
                }
                if (counter % 10 == 0)
                {
                    UART.WriteString("OPEN" + Char.EOL);
                }
                doorWasOpen = true;
            }
            else if (doorWasOpen)
            {
                UART.WriteString("SHUT" + Char.EOL);
                doorWasOpen = false;
#ifdef DIAGNOSTICS
                doorShut = true;
#endif
            }
            
#ifdef DIAGNOSTICS
            loop
            {
                string output = counter.ToString() + ":" + RTC.Date + ", " + RTC.Time;
                output += ", " + light.ToString();
                if (doorOpen)
                {
                    output += ", OPEN"; 
                    if (counter % 10 == 0)
                    {
                        output += " [pinged]";
                    }
                }
                if (doorShut)
                {
                    output += ", SHUT [pinged]"; 
                    doorShut = false;
                }
                IO.WriteLn(output);
                break;
            }
#endif
            MCU.DigitalWrite(GP24, light <= 20);
            CheckLights(doorOpen, light <= 20);
            
            Time.Delay(1000);
            
            if (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
#ifdef DIAGNOSTICS                
                Serial.WriteChar(ch);
#endif
                if (ch == Char.EOL)
                {   
                    // return the content on Char.EOL
                    switch (command)
                    {
                        case "INFO":
                        {
                            SendInfo();
                        }
                        default:
                        {
#ifdef DIAGNOSTICS                            
                            IO.WriteLn("Command: " + command);
#endif
                        }
                    }
                    command = "";
                }
                else
                {
                    command += ch;
                }
            }
            counter++;
            
            long currentHeapFree = MCU.HeapFree();            
            long currentStackFree = MCU.StackFree();
            if (initialHeapFree != currentHeapFree)
            {
                UART.WriteString("HEAP " + (currentHeapFree-initialHeapFree).ToString() + Char.EOL);
#ifdef DIAGNOSTICS
                IO.WriteLn("Heap difference = " + (currentHeapFree-initialHeapFree).ToString());
#endif
            }
            if (initialStackFree != currentStackFree)
            {
                UART.WriteString("STACK " + (currentStackFree-initialStackFree).ToString() + Char.EOL);
#ifdef DIAGNOSTICS
                IO.WriteLn("Stack difference = " + (currentStackFree-initialStackFree).ToString());
#endif
            }
        }
    }
    
    CheckLights(bool doorOpen, bool dark)
    {
        string currentTime = RTC.Time;
        
        _ = DateTime.TryTimeToMinutes(currentTime, ref currentMinutes);
#ifdef DIAGNOSTICS
        IO.WriteLn("Now: " + currentMinutes.ToString() + ", Light Until: " + lightTill.ToString());
#endif
        if ((lightTill != 0) && (lightTill >= currentMinutes))
        {
#ifdef DIAGNOSTICS
            IO.WriteLn("LIGHT");
#endif
            MCU.DigitalWrite(relayPin, true);
        }
        else
        {
            lightTill = 0;
            if (dark)
            {
                if (doorOpen)
                {
                    lightTill = currentMinutes + onOpenMinutes;
                    UART.WriteString("ON" + Char.EOL);
                }
                else if ((currentMinutes >= sixPMMinutes) && (currentMinutes <= tenPMMinutes + 60))
                {     
                    string currentDate = RTC.Date;
                    uint dayOfYear;
                    bool dst;
                    _ = TryDateToDays(currentDate, ref dayOfYear);
                    _ = TryDSTFromDay(dayOfYear, ref dst);
                    uint dstOffset = dst ? 60 : 0;
                    if (currentMinutes + dstOffset >= sixPMMinutes)
                    {
                        if (currentMinutes + dstOffset <= tenPMMinutes)
                        {
                            lightTill = currentMinutes + onDarkMinutes;
                            UART.WriteString("ON" + Char.EOL);
                        }
                    }
                }
            }
            if (lightTill == 0)
            {
                MCU.DigitalWrite(relayPin, false);
            }
        }
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
        if (RTC.SetFromDebugger())
        {
            // this will only update the time if we are running in debugger
            string currentDate = RTC.Date;
            uint dayOfYear;
            bool dst;
            _ = TryDateToDays(currentDate, ref dayOfYear);
            _ = TryDSTFromDay(dayOfYear, ref dst);  
            if (dst)
            {        
                // if we are currently on DST, normalize the time in the RTC to standard time (no DST)
                string currentTime = RTC.Time;
                uint currentMinutes;
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
            IO.WriteLn("  Set RTC Standard Time: " + RTC.Time + (dst ? " (+1 hour for DST)" : "")); 
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
    
    SendInfo()
    {
        string currentDate = RTC.Date;
        uint dayOfYear;
        bool dst;
        _ = TryDateToDays(currentDate, ref dayOfYear);
        _ = TryDSTFromDay(dayOfYear, ref dst);  
        
        string info = RTC.Time + "," + (dst ? "DST" : "") + "," + RTC.Date;
        info += "," + resetDate;
        info += "," + powerDate;
        info += "," + currentMinutes.ToString();
        info += "," + lightTill.ToString();
        UART.WriteString("INFO " + info + Char.EOL);
#ifdef DIAGNOSTICS
        IO.WriteLn("INFO " + info);
#endif        
    }
}
