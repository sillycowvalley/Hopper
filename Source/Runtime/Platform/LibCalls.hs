unit LibCalls
{
    enum LibCall
    {
        WireBegin,
        WireBeginTx,
        WireEndTx,
        WireWrite,
        WireConfigure,
        
        MCUPinMode,
        MCUDigitalRead,
        MCUDigitalWrite,
        MCUAnalogRead,
        MCUAnalogWrite,
        MCUAnalogWriteResolution,
        MCUAttachToPin,
    }
}
