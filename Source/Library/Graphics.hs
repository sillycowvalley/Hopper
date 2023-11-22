unit Graphics
{
    uses "/Source/System/Color"
    
    enum Display
    {
        NoDisplay,
        ILI9341,
        ST7796,
        SSD1306,
        LEDMatrix,
    }
    enum DisplayState
    {
        OK,
        DisplayNotSet,
        BadWidth,
        BadHeight,
        SPIPinsNotSet,
        I2CAddressNotSet,
        MatrixNotConfigured,
    }
    
    ConfigureDisplay(Display display, uint width, uint height) library;
    ConfigureSPI(byte chipSelectPin, byte dataCommandPin) library;
    ConfigureReset(byte resetPin) library;
    ConfigureI2C(byte i2cAddress) library;
    ConfigureMatrix(byte clockPin, byte dataPin, byte intensity) library;
    
    DisplayState Begin() library;
    End() library;
    
    InvertDisplay(bool invertColours) library;
    FlipDisplay(bool flipVertical) library;
    
    Clear(uint colour) library;
    SetPixel(uint x, uint y, uint colour) library;

    uint Width    { get library; }
    uint Height   { get library; }

    Line(uint x1, uint y1, uint x2, uint y2, uint colour) library;
    HorizontalLine(uint x1, uint y1, uint x2, uint y2, uint colour) library;
    VerticalLine(uint x1, uint y1, uint x2, uint y2, uint colour) library;
    Rectangle(uint x1, uint y1, uint x2, uint y2, uint colour) library;
    FilledRectangle(uint x1, uint y1, uint x2, uint y2, uint colour) library;
    
    Show(bool on) library;
    DrawChar(uint x, uint y, char c, uint foreColour, uint backColour, byte scale, bool antiAlias) library;
}
