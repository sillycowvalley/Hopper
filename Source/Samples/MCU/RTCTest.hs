program RTCFeather
{
    uses "/Source/Library/Boards/PiPico2"
    
    uses "/Source/Library/RTCs/DS3232Driver"
    uses "/Source/Library/Devices/GenericRTC" // can be used with any 'uses "<driver path>" 
    
    Hopper()
    {
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
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
            }
            Time.Delay(250);
            
            // heartbeat using built-in LED
            LED = !LED; 
        }
    }
}
