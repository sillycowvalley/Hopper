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
    
    ConfigureDisplay(Display display, uint width, uint height) { ErrorDump(137); Error = 0x0A; }
    ConfigureSPI(byte chipSelectPin, byte dataCommandPin) { ErrorDump(138); Error = 0x0A; }
    ConfigureReset(byte resetPin) { ErrorDump(139); Error = 0x0A; }
    ConfigureI2C(byte i2cAddress) { ErrorDump(140); Error = 0x0A; }
    ConfigureMatrix(byte clockPin, byte dataPin, byte intensity) { ErrorDump(141); Error = 0x0A; }
    DisplayState Begin() { ErrorDump(142); Error = 0x0A; return DisplayState.OK; }
    End() { }
    InvertDisplay(bool invertColours) { ErrorDump(143); Error = 0x0A; }
    FlipDisplay(bool flipVertical) { ErrorDump(144); Error = 0x0A; }
    
    Clear(uint colour) {ErrorDump(145); Error = 0x0A; }
    SetPixel(uint x, uint y, uint colour) { ErrorDump(146); Error = 0x0A; }
    uint Width   { get { ErrorDump(147); Error = 0x0A; return 0; }  }
    uint Height  { get { ErrorDump(148); Error = 0x0A; return 0; }  }
    
    Line(uint x1, uint y1, uint x2, uint y2, uint colour) { ErrorDump(149); Error = 0x0A; }
    HorizontalLine(uint x1, uint y1, uint x2, uint y2, uint colour) { ErrorDump(150); Error = 0x0A; }
    VerticalLine(uint x1, uint y1, uint x2, uint y2, uint colour) {ErrorDump(151); Error = 0x0A; }
    Rectangle(uint x, uint y, uint w, uint h, uint colour) { ErrorDump(152); Error = 0x0A; }
    FilledRectangle(uint x, uint y, uint w, uint h, uint colour) { ErrorDump(153); Error = 0x0A; }
    Show(bool on) { ErrorDump(154); Error = 0x0A; }
    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour, byte scale, bool antiAlias) { ErrorDump(155); Error = 0x0A; }
    
}
