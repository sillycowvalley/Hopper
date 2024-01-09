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
