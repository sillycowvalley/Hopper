unit LibCalls
{
    enum LibCall
    {
        WireBegin,
        WireBeginTx,
        WireEndTx,
        WireWrite,
        WireConfigure,
        WireRead,
        WireRequestFrom,
        
        MCUPinMode,
        MCUDigitalRead,
        MCUDigitalWrite,
        MCUAnalogRead,
        MCUAnalogWrite,
        MCUAnalogWriteResolution,
        MCUAttachToPin,
        MCUInterruptsEnabledGet,
        MCUInterruptsEnabledSet,
        MCUReboot,
        
        MCUHeapFree,
        MCUStackFree,
        
        TimerStart,
        TimerStop,
        TimerAlarm,
        TimerCancel,
        
        SPISettings,
        SPIBegin,
        SPIBeginTransaction,
        SPIEndTransaction,
        SPIReadByte,
        SPIReadWord,
        SPIReadBuffer,
        SPIWriteByte,
        SPIWriteWord,
        SPIWriteBuffer,
        SPISetCSPin,
        SPIGetCSPin,
        SPISetClkPin,
        SPISetTxPin,
        SPISetRxPin,
        
        SPICSPinGet,
        SPICSPinSet,
        SPIClkPinSet,
        SPITxPinSet,
        SPIRxPinSet,
        
        NeoPixelBegin,
        NeoPixelBrightnessSet,
        NeoPixelBrightnessGet,
        NeoPixelSetColor,
        NeoPixelShow,
        NeoPixelLengthGet,
        
        WebClientGetRequest,
        
        WebServerBegin,
        WebServerOn,
        WebServerOnNotFound,
        WebServerEvents,
        WebServerClose,
        WebServerSend,
    }
}
