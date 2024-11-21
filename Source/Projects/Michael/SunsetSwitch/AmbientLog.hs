program AmbiantLog
{
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    
    uses "/Source/System/DateTime"
    
    uint totalMinutes;
    uint totalDays;
    
    record Log
    {
        uint Day;
        uint Minute;
        uint Light;
    }
    <Log> data;
    
    Save()
    {
        file f = File.Create("Data.csv");
        foreach (var log in data)
        {
            string line = (log.Day).ToString() + "," + (log.Minute).ToString() + "," + (log.Light).ToString();
            f.Append(line + Char.EOL);
            IO.WriteLn(line); // debugging
        }
        f.Flush();
    }
    Hopper()
    {
        // on reset, set time from debugger
        if (Runtime.InDebugger)
        {
            string dateTime = Runtime.DateTime;  
            string date = dateTime.Substring(0, 10);
            string time = dateTime.Substring(11);
            _ = DateTime.TryTimeToMinutes(time, ref totalMinutes);
            _ = DateTime.TryDateToDays(date, ref totalDays);
            IO.WriteLn("Time set to " + totalMinutes.ToString() + " minutes");
            IO.WriteLn("Day set to " + totalDays.ToString());
        }
        
        
        uint ticks;
        long accumulator;
        uint samples;
        loop
        {
            uint light = AnalogRead(A0);
            accumulator += light;
            samples++;
            //IO.WriteLn(ticks.ToString() + ": " + light.ToString());
            IO.Write(".");
            Delay(1000);
            ticks++;
            if (ticks == 60)
            {
                IO.WriteLn(".");
                ticks = 0;
                totalMinutes++;
                
                if (totalMinutes % 15 == 0)
                {
                    accumulator /= samples;
                    IO.WriteLn("Day: " + totalDays.ToString() 
                           + ", Time: " + totalMinutes.ToString()
                           + ", Average: " + accumulator.ToString());
                    Log log;
                    log.Day     = totalDays;
                    log.Minute  = totalMinutes;
                    log.Light   = uint(accumulator);
                    data.Append(log);
                    Save();
                           
                    accumulator = 0;
                    samples = 0;
                }
                
                if (totalMinutes >= 1440)
                {
                    totalMinutes = 0;
                    totalDays++;
                }
            }
        }
    }
}
