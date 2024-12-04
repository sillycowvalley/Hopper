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
            if (lastUpdate != time) // seconds changed ..
            {
                lastUpdate = time;
                string diagnosticOutput = RTC.Date + ", " + time;
                
#ifdef RTC_HAS_TEMPERATURE                
                // this RTC board also has a temperature sensor
                diagnosticOutput += ", " + (RTC.Temperature).ToString() + "C"; 
#endif
                IO.WriteLn(diagnosticOutput); 
            }
            
            // heartbeat using built-in LED
            Time.Delay(250);
            LED = !LED;
        }
    }
}
