unit RTCDriver
{
    uses "/Source/System/DateTime"
    uses "/Source/Library/RTC"
    
    // https://sensorian.github.io/c_docs/_example1_2_m_c_p79410_8c_source.html
    // https://sensorian.github.io/c_docs/_m_c_p79410___r_t_c_c_2_example4_2main_8c_source.html
    
    #define RTC_HAS_RAM
    
    const byte RTC_LOCATION   = 0x00;  
    const byte RAM_LOCATION   = 0x20;
      
    byte     iControllerRTC;
    byte     addressRTC;
    bool     initialized;
    byte[19] registersRTC;
    
    bool Begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {        
        
        bool success = Wire.Initialize(i2cController, sdaPin, sclPin);
        addressRTC = address;
        iControllerRTC = i2cController;
        
        initialized = success;
        return success;
    }
    RawClearLostPower()
    {
        // TODO ?!
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, 0x03);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        bool lastPower = 0 != (status & 0b00010000);
        if (lastPower)
        {
            // clear the flags
            status &= ~0b00010000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, 0x03);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
    }
    
    bool RawLostPower 
    { 
        get
        { 
            // TODO ?!
            byte status;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, 0x03);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            status = Wire.Read(iControllerRTC);
            return 0 != (status & 0b00010000); 
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
            Wire.Write(iControllerRTC, RTC_LOCATION); // start location
            Wire.Write(iControllerRTC, toBCD(seconds));
            Wire.Write(iControllerRTC, toBCD(minutes));
            Wire.Write(iControllerRTC, toBCD(hours));
            Wire.Write(iControllerRTC, 0b10000000 | toBCD(iDayOfWeek)); // enable battery backup
            Wire.Write(iControllerRTC, toBCD(day));
            Wire.Write(iControllerRTC, toBCD(month));
            Wire.Write(iControllerRTC, toBCD(year));
            _ = Wire.EndTx(iControllerRTC);
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RTC_LOCATION); // start location
            Wire.Write(iControllerRTC, 0b10000000 | toBCD(seconds)); // start clock
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
            // TODO
            /*
            if (iAlarm != 1)
            {
                // bad alarm
                break;
            }
            
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_CONTROL_1);
            _ = Wire.EndTx(iControllerRTC);
            _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
            byte control = Wire.Read(iControllerRTC);
            
            // clear the bit
            control &= 0b11111101;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_CONTROL_1);
            Wire.Write(iControllerRTC, control);
            _ = Wire.EndTx(iControllerRTC);
        
            success = true;
            */
            break;
        }
        return success;
    }
    bool AlarmWasTriggered(byte iAlarm)
    {
        bool triggered;
        // TODO
        /*
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_CONTROL_2);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        triggered = 0 != (0b00001000 & status);
        if (triggered)
        {
            // clear the flag
            status &= ~0b00001000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_CONTROL_2);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
        */
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
            if (!disableAlarm(iAlarm))
            {
                break;
            }
            
            if (match != AlarmMatch.None)
            {
                byte alarmOn;
                byte alarmMask    = 0b11100000;
                switch (match)
                {
                    case AlarmMatch.SecondsMatch:
                    {
                        if ((minute == 0) && (hour == 0) && (day == 0) && (month == 0))
                        {
                            alarmMask    = 0b00000000;
                        }
                    }
                    case AlarmMatch.MinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (hour == 0) && (day == 0) && (month == 0))
                        {
                            alarmMask    = 0b00100000;
                        }
                    }                        
                    case AlarmMatch.HoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (day == 0) && (month == 0))
                        {
                            alarmMask    = 0b01000000;
                        }
                    }
                    case AlarmMatch.DayHoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (hour == 0) && (month == 0))
                        {
                            alarmMask    = 0b01100000;
                        }
                    }
                    case AlarmMatch.DateHoursMinutesAndSecondsMatch:
                    {
                        if ((second == 0) && (minute == 0) && (hour == 0) && (month == 0))
                        {
                            alarmMask    = 0b10000000;
                        }
                    }
                    
                }
                if (iAlarm == 1)
                {
                    Wire.BeginTx(iControllerRTC, addressRTC);
                    Wire.Write(iControllerRTC, 0x0A);              
                    Wire.Write(iControllerRTC, toBCD(second));
                    Wire.Write(iControllerRTC, toBCD(minute)); 
                    Wire.Write(iControllerRTC, toBCD(hour));
                    Wire.Write(iControllerRTC, alarmMask | toBCD(day)); // weekDay
                    Wire.Write(iControllerRTC, toBCD(day));
                    Wire.Write(iControllerRTC, 0); // month
                    _ = Wire.EndTx(iControllerRTC);
                    alarmOn = 0b00000001;
                }
                else // iAlarm = 2
                {
                    Wire.BeginTx(iControllerRTC, addressRTC);
                    Wire.Write(iControllerRTC, 0x11);              
                    Wire.Write(iControllerRTC, toBCD(second));
                    Wire.Write(iControllerRTC, toBCD(minute)); 
                    Wire.Write(iControllerRTC, toBCD(hour));
                    Wire.Write(iControllerRTC, alarmMask | toBCD(day)); // weekDay
                    Wire.Write(iControllerRTC, toBCD(day));
                    Wire.Write(iControllerRTC, 0); // month
                    _ = Wire.EndTx(iControllerRTC);
                    alarmOn = 0b00000010;
                }
                // TODO
                /*
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
                */
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
                
                byte day         = fromBCD(registersRTC[0x04] & 0b00011111);
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
    
    byte RawRAMCount { get { return 64; } }
    byte[] RawRAM    
    { 
        get 
        { 
            byte[64] ram; 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, RAM_LOCATION);
            _ = Wire.EndTx(iControllerRTC);
            byte bytesReceived = Wire.RequestFrom(iControllerRTC, addressRTC, 64);
            if (bytesReceived == 64)
            {
                for (byte i = 0; i < 64; i++)
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
            for (byte i = 0; i < 64; i++)
            {
                Wire.Write(iControllerRTC, value[i]);
            }
            _ = Wire.EndTx(iControllerRTC);
        } 
    }

}
