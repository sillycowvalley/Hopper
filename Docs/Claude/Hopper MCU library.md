# Hopper Microcontroller Library Reference

## Display and Graphics

### Display
```hopper
unit Display
{
    int PixelWidth { get; set }
    int PixelHeight { get; set }
    uint ForeColour { get; set }
    uint BackColour { get; set }

    bool Begin()
    bool Visible { set }
    Clear()
    Clear(uint colour)
    
    SetPixel(int x, int y, uint colour)
    Rectangle(int x, int y, int w, int h, uint colour)
    FilledRectangle(int x, int y, int w, int h, uint colour)
    Triangle(int x0, int y0, int x1, int y1, int x2, int y2, uint colour)
    FilledTriangle(int x0, int y0, int x1, int y1, int x2, int y2, uint colour)
    Circle(int x0, int y0, int r, uint colour)
    FilledCircle(int x0, int y0, int r, uint colour)
    RoundedRectangle(int x, int y, int w, int h, int r, uint colour)
    FilledRoundedRectangle(int x, int y, int w, int h, int r, uint colour)
    Line(int x0, int y0, int x1, int y1, uint colour)
    HorizontalLine(int x1, int y, int x2, uint colour)
    VerticalLine(int x, int y1, int y2, uint colour)
    ScrollUp(uint lines)
}
```

### Screen
```hopper
unit Screen
{
    byte CursorX { get; set }
    byte CursorY { get; set }
    byte Columns { get }
    byte Rows { get }
    uint ForeColour { get; set }
    uint BackColour { get; set }
    byte TextScale { get; set }
    
    Clear()
    SetCursor(byte col, byte row)
    DrawChar(byte col, byte row, char c, uint foreColour, uint backColour)
    Print(char c, uint foreColour, uint backColour)
    Print(string s, uint foreColour, uint backColour)
    PrintLn()
    DrawText(int x, int y, string text, uint foreColour, uint backColour, byte scale)
}
```

## Hardware Control

### GPIO
```hopper
unit GPIO
{
#if defined(BOARD_HAS_RGBLED)
    bool LEDR { get; set }
    bool LEDG { get; set }
    bool LEDB { get; set }
#endif

#if defined(BOARD_HAS_LED)
    bool LED { get; set }
#endif

#if defined(BOARD_HAS_A0)
    uint A0 { get }
#endif
}
```

### MCU
```hopper
unit MCU
{
    flags PinModeOption
    {
        Input = 0x00,
        Output = 0x01,
        InputPullup = 0x02,
        InputPulldown = 0x03
    }
    
    enum PinStatus
    {
        Low = 0,
        High = 1,
        Change = 2,
        Falling = 3,
        Rising = 4
    }
    
    delegate PinISRDelegate(byte pin, PinStatus status)
    
    PinMode(byte pin, PinModeOption pinMode)
    bool DigitalRead(byte pin)
    DigitalWrite(byte pin, bool value)
    uint AnalogRead(byte pin)
    AnalogWrite(byte pin, uint value)
    
    Tone(byte pin, uint frequency, uint duration)
    NoTone(byte pin)
    
    bool AttachToPin(byte pin, PinISRDelegate gpioISR, PinStatus status)
    bool InterruptsEnabled { get; set }
}
```

## Communication Interfaces

### SPI
```hopper
unit SPI
{
    enum DataOrder { LSBFirst = 0, MSBFirst = 1 }
    enum DataMode { Mode0 = 0, Mode1 = 1, Mode2 = 2, Mode3 = 3 }
    
    byte CSPin { set; get }
    byte ClkPin { set }
    byte TxPin { set }
    byte RxPin { set }
    
    Settings(long speedMaximum, DataOrder dataOrder, DataMode dataMode)
    bool Begin()
    BeginTransaction()
    EndTransaction()
    
    byte ReadByte()
    uint ReadWord()
    ReadBuffer(byte[] data, uint startIndex, uint length)
    WriteByte(byte data)
    WriteWord(uint data)
    WriteBuffer(byte[] data, uint startIndex, uint length)
}
```

### Wire (I2C)
```hopper
unit Wire
{
    bool Initialize()
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin)
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin, uint freqkHz)
    
    bool Begin()
    BeginTx(byte address)
    byte EndTx()
    Write(byte data)
    byte RequestFrom(byte address, byte bytes)
    byte Read()
    
    Configure(byte i2cController, byte sdaPin, byte sclPin)
}
```

### UART
```hopper
unit UART
{
    Setup(uint baud, byte txPin, byte rxPin)
    Setup()
    Setup(uint baud)
    
    bool IsAvailable { get }
    char ReadChar()
    WriteChar(char ch)
    WriteString(string str)
}
```

## Storage

### SD
```hopper
unit SD
{
    byte SPIController { get; set }
    byte CSPin { get; set }
    byte ClkPin { get; set }
    byte TxPin { get; set }
    byte RxPin { get; set }
    
    bool Mount()
    Eject()
}
```

### StorageMedia
```hopper
unit StorageMedia
{
    const uint SectorSize = 512
    const uint TotalSectors = 1024
    
    bool Initialize()
    bool Mount()
    bool Unmount()
    bool ReadSector(uint index, ref byte[SectorSize] data)
    bool WriteSector(uint index, byte[SectorSize] data)
    bool Format()
}
```

## Network

### WebClient
```hopper
unit WebClient
{
    bool GetRequest(string url, ref string response)
}
```

### WebServer
```hopper
unit WebServer
{
    delegate HandlerDelegate(string uri, string method, <string,string> arguments)
    
    Begin()
    Begin(uint port)
    Close()
    Events()
    
    Send(uint httpCode, <string,string> headerContent, string content)
    Send(uint httpCode, string contentType, string content)
    Send(string content)
    
    On(string uri, HandlerDelegate handler)
    OnNotFound(HandlerDelegate handler)
}
```

## Special Hardware

### NeoPixel
```hopper
unit NeoPixel
{
    flags PixelType
    {
        KHz800 = 0x0000,
        KHz400 = 0x0100,
        RGB = ((0 << 6) | (0 << 4) | (1 << 2) | (2))
        GRB = ((1 << 6) | (1 << 4) | (0 << 2) | (2))
    }
    
    Begin(byte pin)
    Begin(uint length, byte pin)
    Begin(uint length, byte pin, PixelType pixelType)
    byte Brightness { get; set }
    
    SetColor(uint pixel, byte r, byte g, byte b)
    SetColor(uint pixel, byte r, byte g, byte b, byte w)
    Show()
    Clear()
    Fill(uint first, uint count, byte r, byte g, byte b)
    SetHue(uint pixel, byte hue)
}
```

### Timer
```hopper
unit Timer
{
    delegate TimerISRDelegate(uint timerID)
    
    uint Start(uint msInterval, TimerISRDelegate timerISR)
    Stop(uint timerID)
    uint Alarm(uint msInterval, TimerISRDelegate timerISR)
    Cancel(uint alarmID)
}
```

## Board Definitions
```hopper
// Standard board unit pattern:
unit Board
{
    // Board type definition
    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_TYPE
    
    // Hardware capabilities
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    
    // Pin definitions
    const byte BuiltInLED = pin
    const byte A0 = pin        // If BOARD_HAS_A0
    const byte I2CSDA0 = pin   // If BOARD_HAS_I2C
    const byte I2CSCL0 = pin   // If BOARD_HAS_I2C
    const byte SPI0Tx = pin    // If BOARD_HAS_SPI
    
    // GPIO mapping
    const byte GP0 = 0
    ...
    const byte GP29 = 29
    
    string BoardName { get }
}
```
