unit RTC
{
    // Features Implemented    DS1307       DS1337       DS3231       PCF8523      MCP79410
    //
    // Time and Date           Yes          Yes          Yes          Yes          Yes
    //
    // Alarms                  No           2            2            1            2
    //    Resolution                        Seconds      Seconds      Minutes      Minutes
    //
    // Countdown Timers        No           No           No           2            No  
    //
    // Battery-backed SRAM     No           No           No           No           64 bytes  
    //
    // Temperature Sensor      No           No           Yes          No           No
    //    
    
    // Options:
    //     RTC_HAS_ALARM
    //     RTC_HAS_COUNTDOWN   
    //     RTC_HAS_LOSTPOWER            
    //     RTC_HAS_TEMPERATURE
    //     RTC_HAS_RAM
    
    
#ifdef RTC_HAS_ALARM    
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
#endif

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
            RTCDriver.resetStatus();
            IO.WriteLn("Set From Debugger: " + RTC.Date + " " + RTC.Time);
            
            success = true;
            break;
        }
        return success;
    }
    
    string Date    { get { return RTCDriver.date;       } set { RTCDriver.date = value; } }
    string Time    { get { return RTCDriver.time;       } set { RTCDriver.time = value; } }
    
#ifdef RTC_HAS_ALARM    
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match)
    {
        bool success;
        if ((AlarmMatch.MinutesMatch == match) || (AlarmMatch.HoursAndMinutesMatch == match))
        {
            success = RTCDriver.setAlarm(iAlarm, 0, minute, hour, 0, match);
        }
        return success;
    }
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    {
        bool success;
        if ((AlarmMatch.MinutesMatch == match) || (AlarmMatch.HoursAndMinutesMatch == match))
        {
            success = RTCDriver.setAlarm(iAlarm, 0, minute, hour, 0, match, alarmDelegate, pin);
        }
        return success;
    }
    
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match)
    {
        return RTCDriver.setAlarm(iAlarm, second, minute, hour, day, match);
    }
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    {
        return RTCDriver.setAlarm(iAlarm, second, minute, hour, day, match, alarmDelegate, pin);
    }
    DisableAlarm(byte iAlarm)
    {
        _ = RTCDriver.setAlarm(iAlarm, 0, 0, 0, 0, AlarmMatch.None);
    }
    
    // also clears the trigger
    bool AlarmWasTriggered(byte iAlarm) { return RTCDriver.alarmWasTriggered(iAlarm); }
    
    ClearInterrupts() { RTCDriver.clearInterrupts(); }
#endif
    
#if defined(RTC_HAS_COUNTDOWN)
    bool SetTimer(byte iTimer, byte ticks, TimerTickLength tickLength)
    {
        return RTCDriver.setTimer(iTimer, ticks, tickLength);
    }
    bool SetTimer(byte iTimer, byte ticks, TimerTickLength tickLength, PinISRDelegate timerDelegate, byte pin)
    {
        return RTCDriver.setTimer(iTimer, ticks, tickLength, timerDelegate, pin);
    }
    StopTimer(byte iAlarm)
    {
        RTCDriver.stopTimer(iTimer);
    }
    bool TimerWasTriggered(byte iTimer) { return RTCDriver.timerWasTriggered(iTimer); }
#endif
 
#if defined(RTC_HAS_LOSTPOWER)    
    ClearLostPower()     { RTCDriver.clearLostPower(); }
    bool   LostPower     { get { return RTCDriver.lostPower;  } }
    string LastLostPower { get { return RTCDriver.lastLostPower;  } }
#endif

#if defined(RTC_HAS_TEMPERATURE)       
    // several RTC chips include a temperature sensor
    float Temperature { get { return RTCDriver.temperature;  } }
#endif
 
#if defined(RTC_HAS_RAM)   
    // several RTCs have some convenient non-volatile RAM    
    byte RAMCount { get { return RTCDriver.ramCount; } }
    byte[] RAM    { get { return RTCDriver.ram; } set { RTCDriver.ram = value; } }
#endif
}
