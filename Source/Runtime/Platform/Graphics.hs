unit HRGraphics
{
    enum Display
    {
        NoDisplay,
        ILI9341,
        ST7796,
        SSD1306,
        LedMatrix,
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
    
    ConfigureDisplay(Display display, uint width, uint height) {  }
    ConfigureSPI(byte chipSelectPin, byte dataCommandPin) {  }
    ConfigureReset(byte resetPin) {  }
    ConfigureI2C(byte i2cAddress) {  }
    ConfigureMatrix(byte clockPin, byte dataPin, byte intensity) { }
    DisplayState Begin() { return DisplayState.OK; }
    InvertDisplay(bool invertColours) {}
    FlipDisplay(bool flipVertical) {}
    
    Clear(uint colour) {}
    SetPixel(uint x, uint y, uint colour) {}
    uint Width   { get { return 0; }  }
    uint Height  { get { return 0; }  }
    
    Line(uint x1, uint y1, uint x2, uint y2, uint colour) {}
    HorizontalLine(uint x1, uint y1, uint x2, uint y2, uint colour) {}
    VerticalLine(uint x1, uint y1, uint x2, uint y2, uint colour) {}
    Rectangle(uint x, uint y, uint w, uint h, uint colour) {}
    FilledRectangle(uint x, uint y, uint w, uint h, uint colour) {}
    Show(bool on) {}
    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour, byte scale, bool antiAlias) {}
    
}
