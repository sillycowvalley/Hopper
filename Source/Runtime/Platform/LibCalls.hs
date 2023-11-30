unit LibCalls
{
    enum LibCall
    {
        WireBegin,
        WireBeginTx,
        WireEndTx,
        WireWrite,
        
        GraphicsConfigureDisplay,
        GraphicsConfigureSPI,
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
        
        GraphicsShow,
        GraphicsDrawChar,
        
        MemoryIncWord,
    }
}
