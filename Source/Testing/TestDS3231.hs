program Test
{
    uses "/Source/Library/Devices/AdafruitDS3231PrecisionRTCFeatherwing"
    
    AlarmPin(byte pin, PinStatus status)
    {
        IO.WriteLn();
        WriteLn("    #### AlarmPin: " + pin.ToString() + " #### at " + RTC.Time);
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
        
        if (!RTC.SetAlarm(1, 10, 0, 0, 0, AlarmMatch.SecondsMatch, alarmPin, Board.GP6))
        {
            IO.WriteLn("Failed to set RTC alarm 1");
            return;
        }
        
        byte laps = 0;
        loop
        {
            string time = RTC.Time;
            string date = RTC.Date;
            WriteLn(date + " " + time + " ");
            Dump();
            /*     
            if (AlarmWasTriggered(1))
            {
                WriteLn();
                WriteLn("   Alarm 1 Triggered at " + time);
            }
            */
            /*
            if (AlarmWasTriggered(2))
            {
                WriteLn();
                WriteLn("   Alarm 2 Triggered at " + time);
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
