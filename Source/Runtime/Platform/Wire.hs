unit HRWire
{
    // I2C APIs
    bool Begin(byte controller)
    {
        ErrorDump(133); Error = 0x0A; // not implemented on Windows
        return false;
    }
    
    BeginTx(byte controller, byte address)
    {
        ErrorDump(134); Error = 0x0A; // not implemented on Windows
    }
    
    byte EndTx(byte controller)
    {
        ErrorDump(135); Error = 0x0A; // not implemented on Windows
        return 0;
    }
    
    Write(byte controller, byte data)
    {
        ErrorDump(136); Error = 0x0A; // not implemented on Windows
    }
    Write(byte controller, uint hrarray, uint startIndex, uint length)
    {
        ErrorDump(136); Error = 0x0A; // not implemented on Windows
    }
    Configure(byte controller, byte sdaPin, byte sclPin)
    {
        ErrorDump(136); Error = 0x0A; // not implemented on Windows
    }
    
}
