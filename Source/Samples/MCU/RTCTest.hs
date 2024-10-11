program RTCFeather
{
    //uses "/Source/Library/Boards/AdafruitFeather"
    uses "/Source/Library/Boards/Hopper6502"
    
    //uses "/Source/Library/Devices/AdafruitDS3231PrecisionRTCFeatherwing"
    uses "/Source/Library/Devices/HW111RTC"
    
    uses "/Source/Library/Devices/Adafruit14Segment"
    
       
    Hopper()
    {
        _ = Wire.Initialize();
        
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
        
        
        // this will only update the time if we are running in debugger
        _ = RTC.SetFromDebugger();
        
        string lastUpdate;
        loop
        {
            string time = RTC.Time;
            if (lastUpdate != time)
            {
                lastUpdate = time;
                
                string date = RTC.Date;
                
#ifdef RTC_HAS_TEMPERATURE                
                // this RTC board also has a temperature sensor
                float temperature = RTC.Temperature;
                string temp = temperature.ToString() + "C"; 
                
                // diagnostics to serial console
                IO.WriteLn(date + " " + time + " " + temp); 
#else
                IO.WriteLn(date + " " + time); 
#endif                
                
                time = time.Substring(0,5);
                Display.Write(time);
                
            }
            Time.Delay(250);
            
            // heartbeat using built-in LED
            LED = !LED; 
        }
    }
}
