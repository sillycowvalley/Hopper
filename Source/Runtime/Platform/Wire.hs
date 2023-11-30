unit HRWire
{
    // I2C APIs
    Begin()
    {
        ErrorDump(133); Error = 0x0A; // not implemented on Windows
    }
    
    BeginTx(byte address)
    {
        ErrorDump(134); Error = 0x0A; // not implemented on Windows
    }
    
    EndTx()
    {
        ErrorDump(135); Error = 0x0A; // not implemented on Windows
    }
    
    Write(byte data)
    {
        ErrorDump(136); Error = 0x0A; // not implemented on Windows
    }
    
}
