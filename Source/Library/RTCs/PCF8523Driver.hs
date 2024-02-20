unit RTCDriver
{
    uses "/Source/System/DateTime"
    uses "/Source/Library/RTC"
    
    
    const byte PCF8523_CONTROL_1  = 0x00;
    const byte PCF8523_CONTROL_2  = 0x01;
    const byte PCF8523_CONTROL_3  = 0x02;
    const byte PCF8523_STATUS     = 0x03; // bit 7 of seconds is clock integrity status flag
    const byte PCF8523_CLKCONTROL = 0x0F;
     
    
    
    byte     iControllerRTC;
    byte     addressRTC;
    bool     initialized;
    byte[19] registersRTC;
    
    bool Begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {        
        
        bool success = Wire.Initialize(i2cController, sdaPin, sclPin);
        addressRTC = address;
        iControllerRTC = i2cController;
        
        // initialization: mostly about disabling timers and interrupts
        byte control = 0b00000000;
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_CONTROL_1);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
        
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_CONTROL_2);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
        
        control = 0b10000000; // disable low battery interrupt
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_CONTROL_3);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
        
        control = 0b00111000; // disable CLKOUT frequency generator on INT pin
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_CLKCONTROL);
        Wire.Write(iControllerRTC, control);
        _ = Wire.EndTx(iControllerRTC);
        
        
        
        initialized = success;
        return success;
    }
    RawClearLostPower()
    {
        Wire.BeginTx(iControllerRTC, addressRTC);
        Wire.Write(iControllerRTC, PCF8523_STATUS);
        _ = Wire.EndTx(iControllerRTC);
        _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
        byte status = Wire.Read(iControllerRTC);
        bool lastPower = 0 != (status & 0b10000000);
        if (lastPower)
        {
            // clear the flags
            status &= ~0b10000000;
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_STATUS);
            Wire.Write(iControllerRTC, status);
            _ = Wire.EndTx(iControllerRTC);
        }
    }
    
    bool RawLostPower 
    { 
        get
        { 
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_STATUS);
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
            Wire.Write(iControllerRTC, 3); // start location
            
            Wire.Write(iControllerRTC, toBCD(seconds));
            Wire.Write(iControllerRTC, toBCD(minutes));
            Wire.Write(iControllerRTC, toBCD(hours));
            Wire.Write(iControllerRTC, toBCD(day));
            Wire.Write(iControllerRTC, toBCD(iDayOfWeek));
            Wire.Write(iControllerRTC, toBCD(month));
            Wire.Write(iControllerRTC, toBCD(year));
            
            _ = Wire.EndTx(iControllerRTC);
            
            // set to battery switchover mode
            Wire.BeginTx(iControllerRTC, addressRTC);
            Wire.Write(iControllerRTC, PCF8523_CONTROL_3);
            Wire.Write(iControllerRTC, 0);
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
            break;
        }
        return success;
    }
    bool AlarmWasTriggered(byte iAlarm)
    {
        bool triggered;
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
        return triggered;
    }
    bool setAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, bool alarmInterrupts)
    {
        bool success;
        loop
        {
            if (iAlarm != 1)
            {
                // bad alarm
                break;
            }
            if ((match == AlarmMatch.SecondsMatch) || (second != 0))
            {
                // invalid options
                break;
            }
            if (!disableAlarm(iAlarm))
            {
                break;
            }
            
            if (match != AlarmMatch.None)
            {
                byte minutesMatch;
                byte hoursMatch;
                byte daysMatch;
                byte dateMatch;
                
                switch (match)
                {
                    case AlarmMatch.MinutesAndSecondsMatch:
                    {
                        hoursMatch   = 0b10000000;
                        dateMatch    = 0b10000000;
                        daysMatch    = 0b10000000;
                    }
                    case AlarmMatch.HoursMinutesAndSecondsMatch:
                    {
                        dateMatch    = 0b10000000;
                        daysMatch    = 0b10000000;
                    }
                    case AlarmMatch.DateHoursMinutesAndSecondsMatch:
                    {
                        dateMatch    = 0b10000000;
                    }
                    case AlarmMatch.DayHoursMinutesAndSecondsMatch:
                    {
                        daysMatch    = 0b10000000;
                    }
                }
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, 0x0A);              
                Wire.Write(iControllerRTC, (toBCD(minute) & 0b01111111) | minutesMatch); 
                Wire.Write(iControllerRTC, (toBCD(hour)   & 0b00111111) | hoursMatch);
                Wire.Write(iControllerRTC, (toBCD(day)    & 0b00111111) | dateMatch); // day
                Wire.Write(iControllerRTC, (toBCD(day)    & 0b00111111) | daysMatch); // weekday
                _ = Wire.EndTx(iControllerRTC);
          
                // enable      
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, PCF8523_CONTROL_1);
                _ = Wire.EndTx(iControllerRTC);
                _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
                byte control = Wire.Read(iControllerRTC);
                if (alarmInterrupts)
                {
                    control |= 0b00000010;
                }
                else
                {
                    control &= ~0b00000010;
                }
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, PCF8523_CONTROL_1);
                Wire.Write(iControllerRTC, control);
                _ = Wire.EndTx(iControllerRTC);
                
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, PCF8523_CONTROL_2);
                _ = Wire.EndTx(iControllerRTC);
                _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
                control = Wire.Read(iControllerRTC);
                Write(" " + control.ToHexString(2));
                
                Wire.BeginTx(iControllerRTC, addressRTC);
                Wire.Write(iControllerRTC, PCF8523_CONTROL_3);
                _ = Wire.EndTx(iControllerRTC);
                _ = Wire.RequestFrom(iControllerRTC, addressRTC, 1);
                control = Wire.Read(iControllerRTC);
                WriteLn(" " + control.ToHexString(2));
                
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
            Wire.Write(iControllerRTC, 3);
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
                
                byte day         = fromBCD(registersRTC[0x03]);
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
