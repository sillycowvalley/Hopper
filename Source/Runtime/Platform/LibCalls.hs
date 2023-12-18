unit LibCalls
{
    enum LibCall
    {
        WireBegin,
        WireBeginTx,
        WireEndTx,
        WireWrite,
        
        MCUPinMode,
        MCUDigitalRead,
        MCUDigitalWrite,
        MCUAnalogRead,
        MCUAnalogWrite,
        MCUAnalogWriteResolution,
        MCUAttachToPin,
        
        GraphicsConfigureDisplay,
        GraphicsConfigureSPI,
        GraphicsConfigureSPIPort,
        GraphicsConfigureReset,
        GraphicsConfigureI2C,
        GraphicsConfigureMatrix,
        GraphicsBegin,
        GraphicsEnd,
        
        GraphicsInvertDisplay,
        GraphicsFlipDisplay,
        
        GraphicsClear,
        GraphicsWidthGet,
        GraphicsHeightGet,
        GraphicsSetPixel,
        
        GraphicsLine,
        GraphicsHorizontalLine,
        GraphicsVerticalLine,
        GraphicsRectangle,
        GraphicsFilledRectangle,
        GraphicsCircle,
        GraphicsFilledCircle,
        
        GraphicsShow,
        GraphicsDrawChar,
    }
}
