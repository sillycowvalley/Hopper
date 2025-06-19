unit RTCDriver
{
    
    #define RTC_HAS_RAM
    #define RTC_HAS_LOSTPOWER
    
    uses "/Source/System/DateTime"
    uses "/Source/Library/RTC"
    
    friend RTC, RTCDevice, BoardDevice;
 
    const byte RTC_CONTROL    = 0x07;
    const byte RTC_LOCATION   = 0x00;  
    const byte RAM_LOCATION   = 0x20;
    
    const byte RAM_SIZE       = 64;
      
    byte     iControllerRTC;
    byte     addressRTC;
    bool     initialized;
    byte[19] registersRTC;
    
    bool begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {        
        
        bool success = Wire.Initialize(i2cController, sdaPin, sclPin);
        addressRTC = address;
        iControllerRTC = i2cController;
        
        controlBits(1, false);
        controlBits(2, false);
        
        initialized = success;
        return success;
    }
    controlBits(byte iEnableAlarm, bool enable)
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, RTC_CONTROL);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte control = Wire.Read(iControllerRTC);
        
        control &= ~0b10111111; // disable square wave 
        if (iEnableAlarm == 1)
        {
            if (enable)
            {
                control |= 0b00010000;
            }
            else
            {
                control &= ~0b00010000;
            }
        }
        if (iEnableAlarm == 2)
        {
            if (enable)
            {
                control |= 0b00100000;
            }
            else
            {
                control &= ~0b00100000;
            }
        }
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, RTC_CONTROL);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
    }
    
    Dump()
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, RTC_CONTROL);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte control = Wire.Read(iControllerRTC);
        Write(control.ToBinaryString());
        
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, 0x0D);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        WriteLn(" " + status.ToBinaryString());
        
    }
    
    resetStatus()
    {
    }
    
    clearLostPower()
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, 0x03);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        bool powerFailed = 0 != (0b00010000 & status);
        if (powerFailed)
        {
            // clear the flag
            status &= ~0b00010000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, 0x03);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
        lastPowerFailure = "";
    }
    
    string lastPowerFailure;
    bool lostPower 
    { 
        get
        { 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, 0x03);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            byte status = Wire.Read(iControllerRTC);
            bool powerFailed = 0 != (0b00010000 & status);
            if (powerFailed)
            {
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, 0x18);
                _ = Wire.EndTx(iControllerRTC);
                _ = Wire.RequestFrom(iControllerRTC, addressRTC, 4);
                byte minutes = fromBCD(Wire.Read(iControllerRTC));
                byte hours   = fromBCD(Wire.Read(iControllerRTC));
                byte day     = fromBCD(Wire.Read(iControllerRTC));
                byte month   = fromBCD(Wire.Read(iControllerRTC) & 0b00011111);
                lastPowerFailure  = (month.ToString()).LeftPad('0', 2) + "-" + (day.ToString()).LeftPad('0', 2) +
                                    (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
            }
            return powerFailed; 
        }
    }
    string lastLostPower { get { return lastPowerFailure; } }
    
    
    byte toBCD(uint decimal)
    {
        uint bcdHex = ((decimal / 10) << 4) + (decimal % 10);
        return byte(bcdHex);
    }

    byte fromBCD(byte bcdHex)
    {
        return ((bcdHex >> 4) * 10) + (bcdHex & 0x0F);
    }
    
    // "YYYY-MM-DD HH:MM:SS"
    bool setRTC(string dateTime)
    {
        bool success;
        loop
        {
            if (!initialized)
            {
                WriteLn("No call to RTCDevice.Begin()?");
                break;
            }
            
            // "YYYY-MM-DD"
            string date = dateTime.Substring(0, 10);
            // "HH:MM:SS"
            string time = dateTime.Substring(11);
            
            uint year;
            uint month;
            uint day;
            if (!UInt.TryParse(date.Substring(0, 4), ref year))
            {
                break;
            }
            year -= 2000;
            if (!UInt.TryParse(date.Substring(5, 2), ref month))
            {
                break;
            }
            if (!UInt.TryParse(date.Substring(8, 2), ref day))
            {
                break;
            }
            
            uint hours;
            uint minutes;
            uint seconds;
            DayOfWeek dayOfWeek;
            if (!UInt.TryParse(time.Substring(0, 2), ref hours))
            {
                break;
            }
            if (!UInt.TryParse(time.Substring(3, 2), ref minutes))
            {
                break;
            }
            if (!UInt.TryParse(time.Substring(6, 2), ref seconds))
            {
                break;
            }
            if (!DateTime.TryDateToDayOfWeek(date, ref dayOfWeek)) // Monday = 1
            {
                break;
            }
            byte iDayOfWeek = byte(dayOfWeek);
            iDayOfWeek++; // 0..6 -> 1..7 : Sunday is 1, not 0
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RTC_LOCATION);   // start location
            Wire.Write(iControllerRTC, 0b00000000 | toBCD(seconds)); // stop oscillator
            Wire.Write(iControllerRTC, toBCD(minutes));
            Wire.Write(iControllerRTC, toBCD(hours));
            Wire.Write(iControllerRTC, 0b10000000 | toBCD(iDayOfWeek)); // enable battery backup
            Wire.Write(iControllerRTC, toBCD(day));
            Wire.Write(iControllerRTC, toBCD(month));
            Wire.Write(iControllerRTC, toBCD(year));
            _ = Wire.EndTx(iControllerRTC);
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RTC_LOCATION); // start location
            Wire.Write(iControllerRTC, 0b10000000 | toBCD(seconds)); // start oscillator
            _ = Wire.EndTx(iControllerRTC);
            
            success = true;
            break;
        }
        return success;
    }
    bool disableAlarm(byte iAlarm)
    {
        bool success;
        loop
        {
            if ((iAlarm != 1) && (iAlarm != 2))
            {
                // bad alarm
                break;
            }
            controlBits(iAlarm, false);
            success = true;
            break;
        }
        return success;
    }
    
    byte[8] lastAlarm;
    AlarmMatch lastMatch1;
    AlarmMatch lastMatch2;
    
    bool alarmWasTriggered(byte iAlarm)
    {
        bool triggered;
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, (iAlarm == 1) ? 0x0D : 0x14);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        triggered = 0 != (0b00001000 & status);
        if (triggered)
        {
            // reset the alarm : only way I could think of to make alarms work the same as other RTC's
            _ = setAlarm(iAlarm, 
                    lastAlarm[iAlarm*4 + 0], lastAlarm[iAlarm*4 + 1], lastAlarm[iAlarm*4 + 2], lastAlarm[iAlarm*4 + 3], 
                    (iAlarm == 1) ? lastMatch1 : lastMatch2, false);
            
            // clear the flag
            status &= ~0b00001000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, (iAlarm == 1) ? 0x0D : 0x14);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
        return triggered;
    }
    clearInterrupts()
    {
        _= alarmWasTriggered(1);
        _= alarmWasTriggered(2);
    }
    bool setAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, bool alarmInterrupts)
    {
        bool success;
        loop
        {
            if ((iAlarm != 1) && (iAlarm != 2))
            {
                break; // bad alarm
            }
            if (!disableAlarm(iAlarm))
            {
                break;
            }
            if (!readRTC())
            {
                break;
            }
            
            if (match != AlarmMatch.None)
            {
                byte alarmMask = 0b01110000;
                byte month        = fromBCD(registersRTC[0x05] & 0b00011111);
                byte dayOfWeek    = day;
                
                byte secondNow    = fromBCD(registersRTC[0x00] & 0b01111111);
                byte minuteNow    = fromBCD(registersRTC[0x01] & 0b01111111);
                byte hourNow      = fromBCD(registersRTC[0x02] & 0b01111111);
                byte dayOfWeekNow = fromBCD(registersRTC[0x03] & 0b01111111);
                byte dayNow       = fromBCD(registersRTC[0x04] & 0b00111111);
                
                lastAlarm[iAlarm*4 + 0] = second;
                lastAlarm[iAlarm*4 + 1] = minute;
                lastAlarm[iAlarm*4 + 2] = hour;
                lastAlarm[iAlarm*4 + 3] = day; 
                if (iAlarm == 1) { lastMatch1 = match; }
                if (iAlarm == 2) { lastMatch2 = match; }
                
                switch (match)
                {
                    case AlarmMatch.SecondsMatch:
                    {
                        if ((minute == 0) && (hour == 0) && (day == 0))
                        {
                            minute    = minuteNow;
                            hour      = hourNow;
                            day       = dayNow;
                            dayOfWeek = dayOfWeekNow;
                            if (secondNow >= second)
                            {
                                minute++;
                            }
                        }
                    }
                    case AlarmMatch.MinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (hour == 0) && (day == 0))
                        {
                            hour      = hourNow;
                            day       = dayNow;
                            dayOfWeek = dayOfWeekNow;
                        }
                    }                        
                    case AlarmMatch.HoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (day == 0))
                        {
                            day       = dayNow;
                            dayOfWeek = dayOfWeekNow;                      
                        }
                    }
                    case AlarmMatch.DayHoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (hour == 0))
                        {
                            //alarmMask    = 0b00110000;
                        }
                    }
                    case AlarmMatch.DateHoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (hour == 0))
                        {
                            //alarmMask    = 0b01000000;
                        }
                    }
                }
                if (minute == 60)
                {
                    minute = 0;
                    hour++;
                }
                if (hour == 24)
                {
                    hour = 0;
                    day++;
                }
                
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, (iAlarm == 1) ? 0x0A : 0x11);              
                Wire.Write(iControllerRTC, toBCD(second));
                Wire.Write(iControllerRTC, toBCD(minute)); 
                Wire.Write(iControllerRTC, toBCD(hour));
                Wire.Write(iControllerRTC, alarmMask | toBCD(dayOfWeek));
                Wire.Write(iControllerRTC, toBCD(day));
                Wire.Write(iControllerRTC, toBCD(month));
                _ = Wire.EndTx(iControllerRTC);
                
                controlBits(iAlarm, true);
            }
            success = true;
            break;
        }
        return success;
    }
    bool setAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match)
    {
        return setAlarm(iAlarm, second, minute, hour, day, match, false);
    }
    bool setAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    {
        bool success;
        loop
        {
            if (!setAlarm(iAlarm, second, minute, hour, day, match, true))
            {
                break;
            }
            MCU.PinMode(pin, PinModeOption.InputPullup);
            success = MCU.AttachToPin(pin, alarmDelegate, PinStatus.Rising);
            success = true;
            break;
        }
        return success;
    }
       
    bool readRTC()
    {   
        bool success;
        loop
        {
            if (!initialized)
            {
                WriteLn("No call to RTCDevice.Begin()?");
                break;
            }
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RTC_LOCATION);
            _ = Wire.EndTx(iControllerRTC);
            byte bytesReceived = Wire.RequestFrom(iControllerRTC, addressRTC, 7);
            if (bytesReceived < 7)
            {
                break;
            }
            for (byte i = 0; i < 7; i++)
            {
                registersRTC[i] = Wire.Read(iControllerRTC);
            }
            success = true;
            break;
        } // loop
        return success;
    }
    
    string date 
    { 
        get
        { 
            string date;
            loop
            {
                if (!initialized)
                {
                    break; // was RTCDevice.Begin(..) called?
                }
                if (!readRTC())
                {
                    break;
                }
                
                byte day         = fromBCD(registersRTC[0x04] & 0b00111111);
                byte month       = fromBCD(registersRTC[0x05] & 0b00011111);
                uint year        = 2000 + fromBCD(registersRTC[0x06]);
                             
                date =         (year.ToString()).LeftPad('0', 4) 
                       + "-" + (month.ToString()).LeftPad('0', 2) 
                       + "-" + (day.ToString()).LeftPad('0', 2);
                break;
            }
            return date;
        }
        set
        { 
            string time = Time; // preserve current time
            _ = setRTC(value + " " + time);
        }
    }
    string time
    { 
        get
        { 
            string time;
            loop
            {
                if (!initialized)
                {
                    break; // was RTCDevice.Begin(..) called?
                }
                if (!readRTC())
                {
                    break;
                }
                
                
                byte second      = fromBCD(registersRTC[0x00] & 0b01111111);
                byte minute      = fromBCD(registersRTC[0x01] & 0b01111111);
                byte hour        = fromBCD(registersRTC[0x02] & 0b01111111);
                            
                time =        (hour.ToString()).LeftPad('0', 2) 
                      + ":" + (minute.ToString()).LeftPad('0', 2) 
                      + ":" + (second.ToString()).LeftPad('0', 2);               
                break;
            }
            return time;
        }
        set
        { 
            string date = Date; // preserve current date
            _ = setRTC(date + " " + value);            
        }
    }
    
    byte ramCount { get { return RAM_SIZE; } }
    byte[] ram
    { 
        get 
        { 
            byte[RAM_SIZE] ram; 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RAM_LOCATION);
            _ = Wire.EndTx(iControllerRTC);
            byte bytesReceived = Wire.RequestFrom(iControllerRTC, addressRTC, RAM_SIZE);
            if (bytesReceived == RAM_SIZE)
            {
                for (byte i = 0; i < RAM_SIZE; i++)
                {
                    ram[i] = Wire.Read(iControllerRTC);
                }
            }
            byte[] flexram = ram;
            return flexram; 
        } 
        set 
        { 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RAM_LOCATION);
            for (byte i = 0; i < RAM_SIZE; i++)
            {
                Wire.Write(iControllerRTC, value[i]);
            }
            _ = Wire.EndTx(iControllerRTC);
        } 
    }
}
