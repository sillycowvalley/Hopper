unit RTCDriver
{
    #define RTC_HAS_RAM
    
    uses "/Source/System/DateTime"
    uses "/Source/Library/RTC"
    
    friend RTC, RTCDevice;
    
    byte     iControllerRTC;
    byte     addressRTC;
    bool     initialized;
    byte[7] registersRTC;
    
    bool begin(byte i2cController, byte sdaPin, byte sclPin, byte address)
    {        
        
        bool success = Wire.Initialize(i2cController, sdaPin, sclPin);
        addressRTC = address;
        iControllerRTC = i2cController;
        
        initialized = success;
        return success;
    }
    
    resetStatus()
    {
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
            Wire.Write(iControllerRTC, 0);
            
            Wire.Write(iControllerRTC, toBCD(seconds));
            Wire.Write(iControllerRTC, toBCD(minutes));
            Wire.Write(iControllerRTC, toBCD(hours));
            
            Wire.Write(iControllerRTC, 0);
            Wire.Write(iControllerRTC, toBCD(day));
            Wire.Write(iControllerRTC, toBCD(month));
            Wire.Write(iControllerRTC, toBCD(year));
            
            _ = Wire.EndTx(iControllerRTC);
            
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
            Wire.Write(iControllerRTC, 0); // set register pointer to 0
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
                
                byte day         = fromBCD(registersRTC[0x04]);
                byte month       = fromBCD(registersRTC[0x05]);
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
                
                
                byte second      = fromBCD(registersRTC[0x00]);
                byte minute      = fromBCD(registersRTC[0x01]);
                byte hour        = fromBCD(registersRTC[0x02]);
                            
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
