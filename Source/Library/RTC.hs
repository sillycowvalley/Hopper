unit RTC
{
    enum AlarmMatch
    {
        None,
        SecondsMatch,
        MinutesAndSecondsMatch = 2,
        MinutesMatch = 2,
        HoursMinutesAndSecondsMatch = 3,
        HoursAndMinutesMatch = 3,
        DayHoursMinutesAndSecondsMatch = 4,
        DayHoursAndMinutesMatch = 4,
        DateHoursMinutesAndSecondsMatch = 5,
        DateHoursAndMinutesMatch = 5,
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
            // "YYYY-MM-DD HH:MM:SS"
            RTC.Date = dateTime.Substring(0, 10);
            RTC.Time = dateTime.Substring(11);
            RTCDriver.RawClearLostPower();
            WriteLn("Set From Debugger");
            success = true;
            break;
        }
        return false;
    }
    bool LostPower { get { return RTCDriver.RawLostPower;  } }
    string Date    { get { return RTCDriver.RawDate;       } set { RTCDriver.RawDate = value; } }
    string Time    { get { return RTCDriver.RawTime;       } set { RTCDriver.RawTime = value; } }
    
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match)
    {
        bool success;
        if ((AlarmMatch.MinutesMatch == match) || (AlarmMatch.HoursAndMinutesMatch == match))
        {
            success = RTCDriver.RawSetAlarm(iAlarm, 0, minute, hour, 0, match);
        }
        return success;
    }
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    {
        bool success;
        if ((AlarmMatch.MinutesMatch == match) || (AlarmMatch.HoursAndMinutesMatch == match))
        {
            success = RTCDriver.RawSetAlarm(iAlarm, 0, minute, hour, 0, match, alarmDelegate, pin);
        }
        return success;
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
    bool AlarmWasTriggered(byte iAlarm) { return RTCDriver.RawAlarmWasTriggered(iAlarm); }
 
#if defined(RTC_HAS_TEMPERATURE)       
    // several RTC chips include a temperature sensor
    float Temperature { get { return RTCDriver.RawTemperature;  } }
#endif
 
#if defined(RTC_HAS_RAM)   
    // several RTCs have some convenient non-volatile RAM    
    byte RAMCount { get { return RTCDriver.RawRAMCount; } }
    byte[] RAM    { get { return RTCDriver.RawRAM; } set { RTCDriver.RawRAM = value; } }
#endif
}
