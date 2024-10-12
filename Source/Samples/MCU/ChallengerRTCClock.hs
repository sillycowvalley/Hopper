program RTCClock
{
    uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    
    uses "/Source/Library/Devices/Adafruit14Segment"
    uses "/Source/Library/Devices/AdafruitDS3231PrecisionRTCFeatherwing"
    
    Hopper()
    {
        // This initializes I2C (Wire) to the pins required by the Pico-RTC-DS3231:
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        
        Display.Add(0x70);
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        //Display.Brightness = 5;
        
        // this will only update the time if we are running in debugger
        _ = RTC.SetFromDebugger();
        
        string timeString;
        byte count;
            
        loop
        {
            timeString = (RTC.Time).Substring(0,5);  
            if (count > 4)
            {
                timeString = timeString.Replace(":", "");
            }
            else
            {
                timeString = timeString.Replace(':', '.');
            }
            Display.Write(timeString);
            Time.Delay(100);
            count++;
            if (count == 10)
            {
                count = 0;
            }
        }
        
    }
}
