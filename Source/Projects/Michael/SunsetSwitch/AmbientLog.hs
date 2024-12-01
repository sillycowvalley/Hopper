program AmbiantLog
{
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Boards/PiPicoW"
    
    uses "/Source/System/DateTime"
    
    const byte bh1750Address    = 0x23;
    const byte bh1750Controller = 0;
    
    const string logPath = "/SD/Logs/Data.csv";
    const uint   logFrequency = 2; // log every nth minute
    uint totalMinutes;
    uint totalDays;
    
    record Log
    {
        uint Day;
        uint Minute;
        uint Light;
        uint Light2;
    }
    <Log> data;
    
    Save()
    {
        bool cardDetected = MCU.DigitalRead(GP0);
        if (cardDetected)
        {
            if (!SD.Mount()) // let SD library initialize SPI before call to SPI.Begin() in DisplayDriver.begin()
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
                          + "," + (log.Light).ToString() 
                          + "," + (log.Light).ToString();
                    f.Append(line + Char.EOL);
                    if (!f.IsValid())
                    {
                        IO.WriteLn("Append Invalid.");
                    }
                    IO.WriteLn(line); // debugging
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
        else
        {
            IO.WriteLn("No card detected");
        }
    }
    uint BH1750Read()
    {
        uint lux;
        
        Wire.BeginTx(bh1750Controller, bh1750Address);
        Wire.Write(bh1750Controller, 0x10); //1 lux resolution 120ms
        byte result = Wire.EndTx(bh1750Controller);
        
        Time.Delay(200);
        
        Wire.BeginTx(bh1750Controller, bh1750Address);
        result = Wire.RequestFrom(bh1750Controller, bh1750Address, 2);
        lux = (Wire.Read(bh1750Controller) << 8) + Wire.Read(bh1750Controller);
        // /= 1.2?
        result = Wire.EndTx(bh1750Controller);
        return lux;
    }
    Hopper()
    {
        // Settings for Hopper SD unit:
        SD.SPIController = 0;
        SD.ClkPin = SPI0SCK;
        SD.TxPin  = SPI0Tx;
        SD.RxPin  = SPI0Rx;
        SD.CSPin  = SPI0SS; 
        MCU.PinMode(GP0, PinModeOption.Input); // Card Detect
        
        if (!Wire.Initialize(/*bh1750Controller, Board.I2CSDA1, Board.I2CSCL1*/) || !Wire.Begin(bh1750Controller))
        {
            IO.WriteLn("Failed to initialize I2C");
        }
        
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
        else
        {
            return; // failed : restarted not in debugger (don't override data)
        }
        
        
        uint ticks;
        long accumulator;
        long accumulator2;
        uint samples;
        loop
        {
            uint light  = MCU.AnalogRead(A0);
            uint light2 = BH1750Read();
            accumulator  += light;
            accumulator2 += light2;
            samples++;
            IO.WriteLn(ticks.ToString() + ": " + light.ToString() + "," + light2.ToString());
            //IO.Write(".");
            Delay(1000);
            ticks++;
            if (ticks == 60)
            {
                IO.WriteLn(".");
                ticks = 0;
                totalMinutes++;
                
                if (totalMinutes % logFrequency == 0)
                {
                    accumulator /= samples;
                    accumulator2 /= samples;
                    IO.WriteLn("Day: " + totalDays.ToString() 
                           + ", Time: " + totalMinutes.ToString()
                           + ", Average: " + accumulator.ToString()
                           + ", Average2: " + accumulator2.ToString()
                           );
                    Log log;
                    log.Day     = totalDays;
                    log.Minute  = totalMinutes;
                    log.Light   = uint(accumulator);
                    log.Light2  = uint(accumulator2);
                    data.Append(log);
                    Save();
                           
                    accumulator = 0;
                    accumulator2 = 0;
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
