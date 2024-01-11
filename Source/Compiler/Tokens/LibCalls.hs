unit LibCalls
{
    <string,byte> libcalls;

    addEntry(string unitName, string methodName)
    {
        libcalls[unitName + '.' + methodName] = byte(libcalls.Count);
    }
    
    New()
    {
        libcalls.Clear(); // in case called a 2nd time
        
        addEntry("Wire", "Begin");
        addEntry("Wire", "BeginTx");
        addEntry("Wire", "EndTx");
        addEntry("Wire", "Write");
        addEntry("Wire", "Configure");
        
        addEntry("MCU", "PinMode");
        addEntry("MCU", "DigitalRead");
        addEntry("MCU", "DigitalWrite");
        addEntry("MCU", "AnalogRead");
        addEntry("MCU", "AnalogWrite");
        addEntry("MCU", "AnalogWriteResolution");
        addEntry("MCU", "AttachToPin");
        
        addEntry("SPI", "Settings");
        addEntry("SPI", "Begin");
        addEntry("SPI", "BeginTransaction");
        addEntry("SPI", "EndTransaction");
        addEntry("SPI", "ReadByte");
        addEntry("SPI", "ReadWord");
        addEntry("SPI", "ReadBuffer");
        addEntry("SPI", "WriteByte");
        addEntry("SPI", "WriteWord");
        addEntry("SPI", "WriteBuffer");
        addEntry("SPI", "SetCSPin");
        addEntry("SPI", "GetCSPin");
        addEntry("SPI", "SetClkPin");
        addEntry("SPI", "SetTxPin");
        addEntry("SPI", "SetRxPin");
        
        addEntry("SPI", "CSPin_Get");
        addEntry("SPI", "CSPin_Set");
        addEntry("SPI", "ClkPin_Set");
        addEntry("SPI", "TxPin_Set");
        addEntry("SPI", "RxPin_Set");
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
