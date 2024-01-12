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
        
    }
}
