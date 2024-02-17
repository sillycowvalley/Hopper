unit RTCDriver
{
    // The DS3231 is a low-cost, extremely accurate, I2Creal-time clock (RTC).
    
    uses "/Source/System/DateTime"
    uses "/Source/Library/RTC"
    
    
    const byte DS3231_TIME        = 0x00;
    const byte DS3231_ALARM1      = 0x07;
    const byte DS3231_ALARM2      = 0x0B;
    const byte DS3231_CONTROL     = 0x0E;
    const byte DS3231_STATUS      = 0x0F;
    const byte DS3231_TEMPERATURE = 0x11; // (high byte - low byte is at 0x12), 10-bittemperature value                        
    
    byte     iControllerRTC;
    byte     addressRTC;
    bool     initialized;
    byte[19] registersRTC;
    
    bool Begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {
        bool success = Wire.Initialize(i2cController, sdaPin, sclPin);
        addressRTC = address;
        iControllerRTC = i2cController;
        
        // good default : square wave off, alarms disabled
        byte control = 0b00011100;
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, DS3231_CONTROL);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
        
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, DS3231_STATUS);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        bool triggered = 0 != (status & 0b00000011);
        if (triggered)
        {
            // clear the flags
            status &= ~0b00000011;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
        
        initialized = success;
        return success;
    }
    RawClearLostPower()
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, DS3231_STATUS);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        bool lastPower = 0 != (status & 0b10000000);
        if (lastPower)
        {
            // clear the flags
            status &= ~0b10000000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
    }
    
    bool RawLostPower 
    { 
        get
        { 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            byte status = Wire.Read(iControllerRTC);
            
            return 0 != (status & 0b10000000); 
        }
    }
    
    
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
            Wire.Write(iControllerRTC, DS3231_TIME); // set register pointer to 0
            
            Wire.Write(iControllerRTC, toBCD(seconds));
            Wire.Write(iControllerRTC, toBCD(minutes));
            Wire.Write(iControllerRTC, toBCD(hours));
            
            Wire.Write(iControllerRTC, toBCD(iDayOfWeek));
            
            Wire.Write(iControllerRTC, toBCD(day));
            Wire.Write(iControllerRTC, 0b10000000 | toBCD(month)); // hardcoded to mean 2000 +
            Wire.Write(iControllerRTC, toBCD(year));
            
            _ = Wire.EndTx(iControllerRTC);
            
            // clear OSF bit
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            byte status = Wire.Read(iControllerRTC);
            status &= ~0b10000000; 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            Wire.Write(iControllerRTC, status);
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
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_CONTROL);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            byte control = Wire.Read(iControllerRTC);
            byte mask    = (iAlarm == 1) ? 0b11111110 : 0b11111101;
            
            // clear the bit
            control &= mask;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_CONTROL);
            Wire.Write(iControllerRTC, control);
            _ = Wire.EndTx(iControllerRTC);
        
            success = true;
            break;
        }
        return success;
    }
    bool AlarmWasTriggered(byte iAlarm)
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, DS3231_STATUS);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        byte mask = (iAlarm == 1) ? 0b00000001 : 0b00000010;
        bool triggered = 0 != (mask & status);
        if (triggered)
        {
            // clear the flag
            status &= ~mask;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, DS3231_STATUS);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
        return triggered;
    }
    bool setAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, bool alarmInterrupts)
    {
        bool success;
        loop
        {
            if ((iAlarm != 1) && (iAlarm != 2))
            {
                // bad alarm
                break;
            }
            if (iAlarm == 2)
            {
                if ((match == AlarmMatch.SecondsMatch) || (second != 0))
                {
                    // invalid options
                    break;
                }
            }
            if (!disableAlarm(iAlarm))
            {
                break;
            }
            
            if (match != AlarmMatch.None)
            {
                byte alarmOn;
                byte secondsMask;
                byte minutesMatch;
                byte hoursMatch;
                byte daysMatch;
                byte dateMatch;
                
                if (iAlarm == 1)
                {
                    switch (match)
                    {
                        case AlarmMatch.SecondsMatch:
                        {
                            minutesMatch = 0b10000000;
                            hoursMatch   = 0b10000000;
                            daysMatch    = 0b10000000;
                        }
                        case AlarmMatch.MinutesAndSecondsMatch:
                        {
                            hoursMatch   = 0b10000000;
                            daysMatch    = 0b10000000;
                        }
                        case AlarmMatch.HoursMinutesAndSecondsMatch:
                        {
                            daysMatch    = 0b10000000;
                        }
                        case AlarmMatch.DateHoursMinutesAndSecondsMatch:
                        {
                        }
                        case AlarmMatch.DayHoursMinutesAndSecondsMatch:
                        {
                            dateMatch    = 0b01000000;
                        }
                    }
                    Wire.BeginTx(iControllerRTC, addressRTC);
                    Wire.Write(iControllerRTC, 0x07);              
                    Wire.Write(iControllerRTC, (toBCD(second) & 0b01111111) | secondsMask);
                    Wire.Write(iControllerRTC, (toBCD(minute) & 0b01111111) | minutesMatch); 
                    Wire.Write(iControllerRTC, (toBCD(hour)   & 0b00111111) | hoursMatch);
                    Wire.Write(iControllerRTC, (toBCD(day)    & 0b00111111) | (dateMatch | daysMatch));
                    _ = Wire.EndTx(iControllerRTC);
                    alarmOn = 0b00000001;
                }
                else // iAlarm = 2
                {
                    switch (match)
                    {
                        case AlarmMatch.MinutesAndSecondsMatch:
                        {
                            hoursMatch   = 0b10000000;
                            daysMatch    = 0b10000000;
                        }
                        case AlarmMatch.HoursMinutesAndSecondsMatch:
                        {
                            daysMatch    = 0b10000000;
                        }
                        case AlarmMatch.DateHoursMinutesAndSecondsMatch:
                        {
                        }
                        case AlarmMatch.DayHoursMinutesAndSecondsMatch:
                        {
                            dateMatch    = 0b10000000;
                        }
                    }
                    Wire.BeginTx(iControllerRTC, addressRTC);
                    Wire.Write(iControllerRTC, 0x0B);              
                    Wire.Write(iControllerRTC, (toBCD(minute) & 0b01111111) | minutesMatch); 
                    Wire.Write(iControllerRTC, (toBCD(hour)   & 0b00111111) | hoursMatch);
                    Wire.Write(iControllerRTC, (toBCD(day)    & 0b00111111) | (dateMatch | daysMatch));
                    _= Wire.EndTx(iControllerRTC);
                    alarmOn = 0b00000010;
                }
                
                // enable
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, DS3231_CONTROL);
                _ = Wire.EndTx(iControllerRTC);
                _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
                byte control = Wire.Read(iControllerRTC);
                if (alarmInterrupts)
                {
                    control |= (0b00000100 | alarmOn);
                }
                else
                {
                    control &= ~alarmOn;
                }
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, DS3231_CONTROL);
                Wire.Write(iControllerRTC, control);
                _ = Wire.EndTx(iControllerRTC);
            }
            success = true;
            break;
        }
        return success;
    }
    bool RawSetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match)
    {
        return setAlarm(iAlarm, second, minute, hour, day, match, false);
    }
    bool RawSetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
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
            break;
        }
        return success;
    }
    
    float RawTemperature
    {
        get
        {
            float temperature;
            loop
            {
                if (!initialized)
                {
                    WriteLn("No call to RTCDevice.Begin()?");
                    break;
                }
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, DS3231_TEMPERATURE);
                _ = Wire.EndTx(iControllerRTC);
                byte bytesReceived = Wire.RequestFrom(iControllerRTC, addressRTC, 2);
                if (bytesReceived < 2)
                {
                    break;
                }
                byte msb = Wire.Read(iControllerRTC);
                byte lsb = (Wire.Read(iControllerRTC) >> 6);
                
                temperature = msb + lsb * 0.25;
                break;
            }
            return temperature;
        }
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
            Wire.Write(iControllerRTC, DS3231_TIME); // set register pointer to 0
            _ = Wire.EndTx(iControllerRTC);
            byte bytesReceived = Wire.RequestFrom(iControllerRTC, addressRTC, 19);
            if (bytesReceived < 19)
            {
                break;
            }
            for (byte i = 0; i < 19; i++)
            {
                registersRTC[i] = Wire.Read(iControllerRTC);
            }
            /*
            WriteLn("Control: " + (registersRTC[0x0E]).ToHexString(2) 
                + ", Status: "  + (registersRTC[0x0F]).ToHexString(2)
                + ", DofW: "    + (registersRTC[0x03]).ToHexString(2));
            */
            success = true;
            break;
        } // loop
        return success;
    }
    
    string RawDate 
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
                
                byte day         = fromBCD(registersRTC[0x04]);
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
            // TODO 
            string time = Time; // preserve current time
            _ = setRTC(value + " " + time);
        }
    }
    string RawTime
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

}
