unit LibCalls
{
    <string,byte> libcalls;

    addEntry(string libCallName)
    {
        libcalls[libCallName] = byte(libcalls.Count);
    }
    
    New()
    {
        libcalls.Clear(); // in case called a 2nd time
    
        addEntry("Timer.Start");
        addEntry("Timer.Stop");
        addEntry("Timer.Alarm");
        addEntry("Timer.Cancel");
        
        addEntry("Wire.Begin");
        addEntry("Wire.BeginTx");
        addEntry("Wire.EndTx");
        addEntry("Wire.Write");
        addEntry("Wire.Configure");
        addEntry("Wire.Read");
        addEntry("Wire.RequestFrom");
        
        addEntry("MCU.PinMode");
        addEntry("MCU.DigitalRead");
        addEntry("MCU.DigitalWrite");
        addEntry("MCU.AnalogRead");
        addEntry("MCU.AnalogWrite");
        addEntry("MCU.AnalogWriteResolution");
        addEntry("MCU.Tone");
        addEntry("MCU.NoTone");
        addEntry("MCU.AttachToPin");
        addEntry("MCU.InterruptsEnabled_Get");
        addEntry("MCU.InterruptsEnabled_Set");
        addEntry("MCU.Reboot");
        
        addEntry("MCU.HeapFree");
        addEntry("MCU.StackFree");
        addEntry("MCU.ClockSpeed_Get");
        addEntry("MCU.ClockSpeed_Set");
                
        addEntry("SPI.Settings");
        addEntry("SPI.Begin");
        addEntry("SPI.BeginTransaction");
        addEntry("SPI.EndTransaction");
        addEntry("SPI.ReadByte");
        addEntry("SPI.ReadWord");
        addEntry("SPI.ReadBuffer");
        addEntry("SPI.WriteByte");
        addEntry("SPI.WriteBytes");
        addEntry("SPI.WriteWord");
        addEntry("SPI.WriteWords");
        addEntry("SPI.WriteBuffer");
        addEntry("SPI.SetCSPin");
        addEntry("SPI.GetCSPin");
        addEntry("SPI.SetClkPin");
        addEntry("SPI.SetTxPin");
        addEntry("SPI.SetRxPin");
        
        addEntry("SPI.CSPin_Get");
        addEntry("SPI.CSPin_Set");
        addEntry("SPI.ClkPin_Set");
        addEntry("SPI.TxPin_Set");
        addEntry("SPI.RxPin_Set");
        
        addEntry("NeoPixel.Begin");
        addEntry("NeoPixel.Brightness_Set");
        addEntry("NeoPixel.Brightness_Get");
        addEntry("NeoPixel.SetColor");
        addEntry("NeoPixel.Show");
        addEntry("NeoPixel.Length_Get");
        
        addEntry("WebClient.GetRequest");
        
        addEntry("WebServer.Begin");
        addEntry("WebServer.On");
        addEntry("WebServer.OnNotFound");
        addEntry("WebServer.Events");
        addEntry("WebServer.Close");
        addEntry("WebServer.Send");
        
        addEntry("SD.SPIController_Get");
        addEntry("SD.SPIController_Set");
        addEntry("SD.CSPin_Get");
        addEntry("SD.CSPin_Set");
        addEntry("SD.ClkPin_Get");
        addEntry("SD.ClkPin_Set");
        addEntry("SD.TxPin_Get");
        addEntry("SD.TxPin_Set");
        addEntry("SD.RxPin_Get");
        addEntry("SD.RxPin_Set");
        addEntry("SD.Mount");
        addEntry("SD.Eject");
        
        addEntry("StorageMedia.Initialize");
        addEntry("StorageMedia.Mount");
        addEntry("StorageMedia.Unmount");
        addEntry("StorageMedia.ReadSector");
        addEntry("StorageMedia.WriteSector");
        
    }
    
    bool TryParseLibCall(string name, ref byte index)
    {
        bool success = false;
        if (libcalls.Contains(name))
        {
            index = libcalls[name];
            success = true;
        }
        return success;
    }
    
    string GetLibCallName(byte iLibCall)
    {
        string name;
        foreach (var kv in libcalls)
        {
            if (kv.value == iLibCall)
            {
                name = kv.key;
                break;
            }
        }
        return name;
    }
    <string,byte> GetLibCalls()
    {
        return libcalls;
    }
    
}
