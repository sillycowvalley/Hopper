unit RTC
{
    enum AlarmMatch
    {
        None,
        SecondsMatch,
        MinutesAndSecondsMatch,
        HoursMinutesAndSecondsMatch,
        DayHoursMinutesAndSecondsMatch,
        DateHoursMinutesAndSecondsMatch
    }
    
    bool SetFromDebugger()
    {
        bool success;
        loop
        {
            if (!Runtime.InDebugger)
            {
                break;
            }
            string dateTime = Runtime.DateTime;
            WriteLn("  From Debugger: " + dateTime);
            // "YYYY-MM-DD HH:MM:SS    "
            RTC.Date = dateTime.Substring(0, 10);
            RTC.Time = dateTime.Substring(11);
            RTCDriver.RawClearLostPower();
            success = true;
            break;
        }
        return false;
    }
    bool LostPower 
    { 
        get
        { 
            return RTCDriver.RawLostPower; 
        }
    }
    
    string Date 
    { 
        get
        { 
            return RTCDriver.RawDate; 
        }
        set
        { 
            RTCDriver.RawDate = value;
        }
    }
    string Time
    { 
        get
        { 
            return RTCDriver.RawTime; 
        }
        set
        { 
            RTCDriver.RawTime = value;
        }
    }
    
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match)
    {
        return RTCDriver.RawSetAlarm(iAlarm, second, minute, hour, day, match);
    }
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    {
        return RTCDriver.RawSetAlarm(iAlarm, second, minute, hour, day, match, alarmDelegate, pin);
    }
    
    
    // also clears the trigger
    bool AlarmWasTriggered(byte iAlarm) 
    { 
        return RTCDriver.RawAlarmWasTriggered(iAlarm);
    }
    
    // several RTC chips include a temperature sensor
    float Temperature
    {
        get
        {
            return RTCDriver.RawTemperature;
        }
    }
    
    
}
