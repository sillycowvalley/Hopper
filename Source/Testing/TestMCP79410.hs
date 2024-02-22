program Test
{
    //uses "/Source/Library/Devices/AdafruitDS3231PrecisionRTCFeatherwing"
    //uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    uses "/Source/Library/Devices/Challenger2040SDRTC"
    
    AlarmPin(byte pin, PinStatus status)
    {
        IO.WriteLn();
        WriteLn("    #### AlarmPin: " + pin.ToString() + " #### at " + RTC.Time);
    }
    
    {
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        _ = RTC.SetFromDebugger();
        
        PinISRDelegate alarmPin = AlarmPin;
        //if (!RTC.SetAlarm(1, 15, 0, 0, 0, AlarmMatch.SecondsMatch))
        //if (!RTC.SetAlarm(1, 55, 0, AlarmMatch.MinutesMatch))
        //{
        //    IO.WriteLn("Failed to set RTC alarm 1");
        //    return;
        //}
        if (!RTC.SetAlarm(1, 10, 0, 0, 0, AlarmMatch.SecondsMatch, alarmPin, Board.GP25))
        //if (!RTC.SetAlarm(1, 07, 0, AlarmMatch.MinutesMatch, alarmPin, Board.GP6))
        {
            IO.WriteLn("Failed to set RTC alarm 1");
            return;
        }
        
        byte[] ram = RTC.RAM;
        for (byte i=0; i < ram.Count; i++)
        {
            ram[i] = i;
        }
        RTC.RAM = ram;
        byte[] ram2 = RTC.RAM;
        
        byte laps = 0;
        loop
        {
            string time = RTC.Time;
            string date = RTC.Date;
            Write(date + " " + time + " ");
            Dump();
            
            if (RTC.LostPower)
            {
                WriteLn("Lost Power: " + RTC.LastLostPower);
                //RTC.ClearLostPower();
            }
            
            if (AlarmWasTriggered(1))
            {
                WriteLn();
                WriteLn("   Alarm 1 Triggered at " + time);
            }
            
            //RTCDriver.Dump();
            
            DelaySeconds(5);
            laps++;
            if (IO.IsBreak())
            {
                break;
            }
        }
    }
}
