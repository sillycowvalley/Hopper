program RTCDemo
{
    uses "/Source/Library/Boards/AdafruitFeather"
    uses "/Source/Library/Devices/AdafruitDS3231PrecisionRTCFeatherwing"
    uses "/Source/Library/Devices/AdafruitThinkInk213TriColor"
    uses "/Source/Library/Fonts/Arduino6x8"
    
    const byte fontScale = 3;
    drawText(byte col, byte row, string text, uint foreColour, uint backColour)
    {
        foreach (var c in text)
        {
            Screen.DrawChar(col, row, c, foreColour, backColour, fontScale, int(0), int(12));
            col++;
        }
    }
    
    Hopper()
    {
        DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        if (!RTCDevice.Begin())
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        // this will only update the time if we are running in debugger
        _ = RTC.SetFromDebugger();
        
        string lastUpdate;
        loop
        {
            string time = (RTC.Time).Substring(0,5); // lop off the seconds
            if (lastUpdate != time)
            {
                string date = RTC.Date;
                
                // this RTC board also has a temperature sensor
                float temperature = RTC.Temperature;
                string temp = temperature.ToString() + "C"; 
                
                // diagnostics to serial console
                WriteLn(date + " " + time + " " + temp); 
                
                Display.Suspend();
                Screen.Clear();
                uint cx = (Screen.Columns/fontScale) / 2;
                uint cy = (Screen.Rows/fontScale) / 2 - 1;
                drawText(byte(cx - time.Length/2) + 0, byte(cy - 1), time, Colour.Black, Colour.White);
                drawText(byte(cx - date.Length/2) + 1, byte(cy + 0), date, Colour.Black, Colour.White);
                drawText(byte(cx - temp.Length/2) + 1, byte(cy + 2), temp, Colour.Red,   Colour.White);
                Display.Resume();
                lastUpdate = time;
            }
            Time.DelaySeconds(5);
            
            // heartbeat using built-in LED
            LED = !LED; 
        }
    }
}
