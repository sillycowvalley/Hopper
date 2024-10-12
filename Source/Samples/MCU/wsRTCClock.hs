program RTCClock
{
    uses "/Source/Library/Boards/PiPico2"
    //uses "/Source/Library/Boards/PiPico"
    uses "/Source/Library/Devices/WSPico8SEGLED"
    uses "/Source/Library/Devices/WSPicoRTCDS3231"
    
    Hopper()
    {
        // This initializes I2C (Wire) to the pins required by the Pico-RTC-DS3231:
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        if (!Pico8SEGLED.Begin())
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-8SEG-LED");
            return;
        }
        
        Pico8SEGLED.DecimalPosition = 3;
        Pico8SEGLED.LeadingZeroes = true;
        
        // this will only update the time if we are running in debugger
        _ = RTC.SetFromDebugger();
        
        string timeString;
        uint hours;
        uint minutes;
        uint time;
        
        uint count;
            
        loop
        {
            timeString = RTC.Time;
            _ = UInt.TryParse(timeString.Substring(0,2), ref hours);
            _ = UInt.TryParse(timeString.Substring(3,2), ref minutes);
            time = hours * 100 + minutes;
            if (count > 40)
            {
                Pico8SEGLED.DecimalPosition = 0;
            }
            else
            {
                Pico8SEGLED.DecimalPosition = 3;
            }
            Pico8SEGLED.Show(time);  
            count++;
            if (count > 99)
            {
                count = 0;
            }
        }
    }
}
