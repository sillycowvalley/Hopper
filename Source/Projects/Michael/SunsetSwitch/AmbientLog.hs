program AmbientLog
{
    #define DIAGNOSTICS
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    
    uses "/Source/System/DateTime"
    
    const string logPath = "/SD/Logs/Data.csv";
    const uint   logFrequency = 2; // log every n-th minute
    
    record Log
    {
        uint Day;
        uint Minute;
        uint Light;
    }
    <Log> data;
    
    Hopper()
    {
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to AdaLogger");
            return;
        }
        
        // on reset, set time from debugger (if running in debugger)
        _ = RTC.SetFromDebugger();
        
        uint ticks;
        long accumulator;
        uint samples;
        loop
        {
            uint light  = MCU.AnalogRead(A0);
            accumulator  += light;
            samples++;
#ifdef DIAGNOSTICS
            IO.WriteLn(ticks.ToString() + ": " + light.ToString());
#else
            IO.Write(".");
#endif
            Time.Delay(1000);
            ticks++;
            if (ticks == 60)
            {
                string time = RTC.Time;
                
#ifdef DIAGNOSTICS
                IO.WriteLn("Time: " + time);
#else
                IO.WriteLn(".");
#endif
                ticks = 0;
                uint totalMinutes;
                if (!DateTime.TryTimeToMinutes(time, ref totalMinutes))
                {
                    IO.WriteLn("Failed to parse '" + time + "'");
                }
                
                if (totalMinutes % logFrequency == 0)
                {
                    uint totalDays;
                    string date = RTC.Date;
                    if (!DateTime.TryDateToDays(date, ref totalDays))
                    {
                        IO.WriteLn("Failed to parse '" + date + "'");
                    }
                    
                    accumulator /= samples;
                    
                    IO.WriteLn("Day: "     + totalDays.ToString() 
                           + ", Time: "    + totalMinutes.ToString()
                           + ", Average: " + accumulator.ToString()
                           );
                    Log log;
                    log.Day     = totalDays;
                    log.Minute  = totalMinutes;
                    log.Light   = uint(accumulator);
                    data.Append(log);
                    attemptSave();
                           
                    accumulator = 0;
                    samples = 0;
                }
            }
        }
    }
    
    attemptSave()
    {
        if (!SD.Mount())
        {
            IO.WriteLn("Failed to initialize SD");
        }
        else
        {
            IO.WriteLn("SD card detected.");
            file f = File.Create(logPath);
            foreach (var log in data)
            {
                string line = (log.Day).ToString() 
                      + "," + (log.Minute).ToString() 
                      + "," + (log.Light).ToString();
                f.Append(line + Char.EOL);
#ifdef DIAGNOSTICS
                if (!f.IsValid())
                {
                    IO.WriteLn("Append Invalid.");
                }
                IO.WriteLn(line); // debugging
#endif
            }
            f.Flush();
            if (f.IsValid())
            {
                IO.WriteLn("Flushed.");
                SD.Eject();
            }
            else
            {
                IO.WriteLn("Flush Invalid.");
            }
        }
    }
}
