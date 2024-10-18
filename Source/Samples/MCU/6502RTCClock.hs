program RTC6502
{
    uses "/Source/Library/Boards/Hopper6502"
    uses "/Source/Library/RTCs/DS1307Driver"
    uses "/Source/Library/Devices/Adafruit14Segment"
       
    Hopper()
    {
        _ = Wire.Initialize();
        
        bool displayExists;
        bool rtcExists;
        
        for (byte i2cAddress = 8; i2cAddress < 120; i2cAddress++)
        {
            Wire.BeginTx(i2cAddress);
            if (0 == Wire.EndTx())
            {
                if (i2cAddress == 0x50)
                {
                    WriteLn(i2cAddress.ToHexString(2) + " exists (serial EEPROM: program)");
                }
                else if (i2cAddress == 0x54)
                {
                    WriteLn(i2cAddress.ToHexString(2) + " exists (serial EEPROM: files)");
                }
                else if (i2cAddress == 0x70)
                {
                    displayExists = true;
                    WriteLn(i2cAddress.ToHexString(2) + " exists (14 Segment Display)");
                }
                else if (i2cAddress == 0x68)
                {
                    rtcExists = true;
                    WriteLn(i2cAddress.ToHexString(2) + " exists (RTC)");
                }
                else
                {
                    WriteLn(i2cAddress.ToHexString(2) + " exists");
                }
            }
        }

        if (!rtcExists)
        {
            IO.WriteLn("RTC not found on I2C bus");
            return;
        }
        if (!RTCDriver.Begin(Wire.DefaultI2CController, Wire.DefaultI2CSDAPin, Wire.DefaultI2CSCLPin, 0x68))
        {
            IO.WriteLn("Failed to initialize RTC");
            return;
        }
        if (displayExists)
        {
            Display.Add(0x70);
            if (!Display.Begin())
            {
                IO.WriteLn("Failed to initialize display");
                return;
            }
        }
        
        
        // this will only update the time if we are running in debugger
        _ = RTC.SetFromDebugger();
        
        string lastUpdate;
        uint count;
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
                
                if (displayExists)
                {
                    time = time.Substring(0,2) + time.Substring(3,2);
                    Display.Write(time);
                }
                
            }
            Time.Delay(100);
            
            // heartbeat using built-in LED
            count++;
            if (count == 10)
            {
                count = 0;
            }
            LED = (count <= 2); 
        }
    }
}
