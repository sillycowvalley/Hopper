unit RTC
{
    // Features Implemented    DS3231       PCF8523      MCP79410
    //
    // Time and Date           Yes          Yes          Yes
    //
    // Alarms                  2            1            2
    //    Resolution           Seconds      Minutes      Minutes
    //
    // Countdown Timers        No           2            No  
    //
    // Battery-backed SRAM     No           No           64 bytes  
    //
    // Temperature Sensor      Yes          No           No
    //             
    
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

#if defined(RTC_HAS_COUNTDOWN)        
    enum TimerTickLength
    {
        Hz4096,
        Hz64,
        Second,
        Minute,
        Hour
    }
#endif
    
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
            RTCDriver.RawResetStatus();
            WriteLn("Set From Debugger");
            success = true;
            break;
        }
        return false;
    }
    
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
    DisableAlarm(byte iAlarm)
    {
        _ = RTCDriver.RawSetAlarm(iAlarm, 0, 0, 0, 0, AlarmMatch.None);
    }
    
    // also clears the trigger
    bool AlarmWasTriggered(byte iAlarm) { return RTCDriver.RawAlarmWasTriggered(iAlarm); }
    
#if defined(RTC_HAS_COUNTDOWN)
    bool SetTimer(byte iTimer, byte ticks, TimerTickLength tickLength)
    {
        return RTCDriver.RawSetTimer(iTimer, ticks, tickLength);
    }
    bool SetTimer(byte iTimer, byte ticks, TimerTickLength tickLength, PinISRDelegate timerDelegate, byte pin)
    {
        return RTCDriver.RawSetTimer(iTimer, ticks, tickLength, timerDelegate, pin);
    }
    StopTimer(byte iAlarm)
    {
        RTCDriver.RawStopTimer(iTimer);
    }
    bool TimerWasTriggered(byte iTimer) { return RTCDriver.RawTimerWasTriggered(iTimer); }
#endif
 
#if defined(RTC_HAS_LOSTPOWER)    
    ClearLostPower()     { RTCDriver.RawClearLostPower(); }
    bool   LostPower     { get { return RTCDriver.RawLostPower;  } }
    string LastLostPower { get { return RTCDriver.RawLastLostPower;  } }
#endif

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
