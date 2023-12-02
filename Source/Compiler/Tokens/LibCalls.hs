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
        
        addEntry("MCU", "PinMode");
        addEntry("MCU", "DigitalRead");
        addEntry("MCU", "DigitalWrite");
        addEntry("MCU", "AnalogRead");
        addEntry("MCU", "AnalogWrite");
        addEntry("MCU", "AnalogWriteResolution");
        
        addEntry("Graphics", "ConfigureDisplay");
        addEntry("Graphics", "ConfigureSPI");
        addEntry("Graphics", "ConfigureReset");
        addEntry("Graphics", "ConfigureI2C");
        addEntry("Graphics", "ConfigureMatrix");
        addEntry("Graphics", "Begin");
        addEntry("Graphics", "End");
        
        addEntry("Graphics", "InvertDisplay");
        addEntry("Graphics", "FlipDisplay");
        
        addEntry("Graphics", "Clear");
        addEntry("Graphics", "Width_Get");
        addEntry("Graphics", "Height_Get");
        addEntry("Graphics", "SetPixel");
        
        addEntry("Graphics", "Line");
        addEntry("Graphics", "HorizontalLine");
        addEntry("Graphics", "VerticalLine");
        addEntry("Graphics", "Rectangle");
        addEntry("Graphics", "FilledRectangle");
        
        addEntry("Graphics", "Show");
        addEntry("Graphics", "DrawChar");
        
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
