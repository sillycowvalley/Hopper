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
    Reset()
    Resume()
    Suspend()
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
    byte TextScale { get; set }
    uint ForeColour { get; set }
    uint BackColour { get; set }

    Clear()
    SetCursor(byte col, byte row)
    DrawChar(byte col, byte row, char c, uint foreColour, uint backColour)
    DrawChar(byte col, byte row, char c, uint foreColour, uint backColour, byte scale, int dx, int dy)
    DrawText(int x, int y, string text, uint foreColour, uint backColour, byte scale)
    
    Print(char c)
    Print(string s)
    Print(char c, uint foreColour, uint backColour)
    Print(string s, uint foreColour, uint backColour)
    PrintLn()
    PrintLn(char c)
    PrintLn(string s)
    PrintLn(char c, uint foreColour, uint backColour)
    PrintLn(string s, uint foreColour, uint backColour)
}
```

## Communication Interfaces

### SPI
```hopper
unit SPI
{
    enum DataOrder { LSBFirst, MSBFirst }
    enum DataMode { Mode0, Mode1, Mode2, Mode3 }
    
    byte CSPin { set; get }
    byte ClkPin { set }
    byte TxPin { set }
    byte RxPin { set }
    
    Settings(long speedMaximum, DataOrder dataOrder, DataMode dataMode)
    Settings(byte spiController, long speedMaximum, DataOrder dataOrder, DataMode dataMode)
    
    bool Begin()
    bool Begin(byte spiController)
    BeginTransaction()
    BeginTransaction(byte spiController)
    EndTransaction()
    EndTransaction(byte spiController)
    
    byte ReadByte()
    byte ReadByte(byte spiController)
    uint ReadWord()
    uint ReadWord(byte spiController)
    ReadBuffer(byte[] data, uint startIndex, uint length)
    ReadBuffer(byte spiController, byte[] data, uint startIndex, uint length)
    
    WriteByte(byte data)
    WriteByte(byte spiController, byte data)
    WriteWord(uint data)
    WriteWord(byte spiController, uint data)
    WriteBytes(byte data, uint count)
    WriteBytes(byte spiController, byte data, uint count)
    WriteBuffer(byte[] data, uint startIndex, uint length)
    WriteBuffer(byte spiController, byte[] data, uint startIndex, uint length)
    WriteBuffer(uint[] data, uint startIndex, uint length)
    WriteBuffer(byte spiController, uint[] data, uint startIndex, uint length)
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
    bool Begin(byte i2cController)
    BeginTx(byte address)
    BeginTx(byte i2cController, byte address)
    byte EndTx()
    byte EndTx(byte i2cController)
    
    Write(byte data)
    Write(byte i2cController, byte data)
    Write(byte i2cController, byte[] data, uint startIndex, uint length)
    Write(byte i2cController, bool[] data, uint startIndex, uint length)
    
    byte RequestFrom(byte address, byte bytes)
    byte RequestFrom(byte i2cController, byte address, byte bytes)
    byte Read()
    byte Read(byte i2cController)
    
    Configure(byte i2cController, byte sdaPin, byte sclPin)
    Configure(byte i2cController, byte sdaPin, byte sclPin, uint freqkHz)
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
    bool Format()
    bool ReadSector(uint index, ref byte[SectorSize] data)
    bool WriteSector(uint index, byte[SectorSize] data)
}
```

## Hardware Control

### GPIO
```hopper
unit GPIO
{
    bool LEDR { get; set }
    bool LEDG { get; set }
    bool LEDB { get; set }
    bool LED { get; set }
    uint A0 { get }
    uint A1 { get }
    uint A2 { get }
    uint A3 { get }
    uint A4 { get }
    uint A5 { get }
}
```

### Timer
```hopper
unit Timer
{
    delegate TimerISRDelegate(uint timerID)
    
    uint Start(uint msInterval, TimerISRDelegate timerISR)
    uint Start(long msInterval, TimerISRDelegate timerISR)
    Stop(uint timerID)
    
    uint Alarm(uint msInterval, TimerISRDelegate timerISR)
    uint Alarm(long msInterval, TimerISRDelegate timerISR)
    Cancel(uint alarmID)
}
```

### NeoPixel
```hopper
unit NeoPixel
{
    flags PixelType { KHz800, KHz400, RGB, RBG, GRB, GBR, BRG, BGR, WRGB }
    
    Begin(byte pin)
    Begin(uint length, byte pin)
    Begin(uint length, byte pin, PixelType pixelType)
    bool Begin()
    BuiltIn()
    
    byte Brightness { get; set }
    uint Length { get }
    
    SetColor(uint pixel, byte r, byte g, byte b)
    SetColor(uint pixel, byte r, byte g, byte b, byte w)
    Show()
    Clear()
    Fill(uint first, uint count, byte r, byte g, byte b)
    Fill(uint first, uint count, byte r, byte g, byte b, byte w)
    SetHue(uint pixel, byte hue)
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

## Real Time Clock

### RTC
```hopper
unit RTC
{
    enum AlarmMatch { None, SecondsMatch, MinutesMatch, HoursAndMinutesMatch, 
                      DayHoursAndMinutesMatch, DateHoursAndMinutesMatch }
    enum TimerTickLength { Hz4096, Hz64, Second, Minute, Hour }
    
    string Date { get; set }
    string Time { get; set }
    bool SetFromDebugger()
    
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match)
    bool SetAlarm(byte iAlarm, byte minute, byte hour, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match)
    bool SetAlarm(byte iAlarm, byte second, byte minute, byte hour, byte day, AlarmMatch match, PinISRDelegate alarmDelegate, byte pin)
    DisableAlarm(byte iAlarm)
    bool AlarmWasTriggered(byte iAlarm)
    ClearInterrupts()
    
    float Temperature { get }
    byte RAMCount { get }
    byte[] RAM { get; set }
}
```
