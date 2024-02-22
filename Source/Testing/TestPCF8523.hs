program Test
{
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    
    AlarmPin(byte pin, PinStatus status)
    {
        WriteLn("    #### Pin: " + pin.ToString() + " #### at " + RTC.Time);
        RTC.ClearInterrupts();
    }
    
    
    {
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        _ = RTC.SetFromDebugger();
        
        
        
        PinISRDelegate alarmPin = AlarmPin;
        if (!RTC.SetAlarm(1, 31, 0, AlarmMatch.MinutesMatch, alarmPin, Board.GP6))
        {
            IO.WriteLn("Failed to set RTC alarm 1");
            return;
        }
        if (!RTC.SetTimer(1, 13, TimerTickLength.Second, alarmPin, Board.GP6))
        {
            IO.WriteLn("Failed to set RTC timer 1");
            return;
        }
        /*
        if (!RTC.SetTimer(2, 15, TimerTickLength.Second, alarmPin, Board.GP6))
        {
            IO.WriteLn("Failed to set RTC timer 2");
            return;
        }
        */
        
        byte laps = 0;
        loop
        {
            string time = RTC.Time;
            string date = RTC.Date;
            WriteLn(date + " " + time + " ");
            
            if (AlarmWasTriggered(1))
            {
                WriteLn();
                WriteLn("   Alarm 1 Triggered at " + time);
            }
            
            /*
            if (TimerWasTriggered(1))
            {
                WriteLn("   Timer 1 Triggered at " + time);
            }
            */
            /*
            if (TimerWasTriggered(2))
            {
                WriteLn("   Timer 2 Triggered at " + time);
            }
            */
                        
            DelaySeconds(5);
            laps++;
            if (IO.IsBreak())
            {
                break;
            }
        }
    }
}
