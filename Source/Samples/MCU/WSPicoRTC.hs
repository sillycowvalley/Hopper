program Test
{
    uses "/Source/Library/Devices/WSPicoRTCDS3231"
    uses "/Source/Library/Devices/WSPico8SEGLED"
    
    {
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        if (!Pico8SEGLED.Begin())
        {
            IO.WriteLn("Failed to initialize 8 SEG Display");
            return;
        }
        Pico8SEGLED.DecimalPosition = 3;
        Pico8SEGLED.LeadingZeroes   = true;
        
        _ = RTC.SetFromDebugger();
        
        uint hours;
        uint minutes;
        loop
        {
            string time = RTC.Time; // "10:20:40"
            <string> parts = time.Split(':');
            _ = UInt.TryParse(parts[0], ref hours);
            _ = UInt.TryParse(parts[1], ref minutes);
            Pico8SEGLED.Show(hours * 100 + minutes);
        }
    }
}
