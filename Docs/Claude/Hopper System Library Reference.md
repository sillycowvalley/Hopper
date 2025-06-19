# Hopper System Library Reference

## Core Types

### Array
```hopper
unit Array
{
    uint Count { get }
    SetItem(V[] this, uint index, V value)
    V GetItem(V[] this, uint index) 
    V[] Slice(V[] this, uint start)
    V[] Slice(V[] this, uint start, uint length)
    type ItemType { get }
}
```

### Bool
```hopper
unit Bool
{
    string ToString(bool this)
}
```

### Byte
```hopper
unit Byte
{
    string ToString(byte this)
    string ToHexString(byte this, byte digits)
    string ToBinaryString(byte this)
    char ToHex(byte this)
    char ToDigit(byte this)
}
```

### Char
```hopper
unit Char
{
    const char EOL = char(0x0A)
    const char Escape = char(0x1B)
    const char Slash = char(0x5C)
    const char Formfeed = char(0x0C)
    const char Backspace = char(0x08)
    const char Break = char(0x03)
    const char Tab = char(0x09)

    char ToUpper(char this)
    bool IsDigit(char this)
    bool IsHexDigit(char this)
    bool IsLetterOrDigit(char this)
    bool IsLower(char this)
    bool IsUpper(char this)
    char ToLower(char this)
    string ToString(char this)
    bool IsLetter(char this)
    bool IsWhitespace(char this)
}
```

### DateTime
```hopper
unit DateTime
{
    enum DayOfWeek { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday }
    
    bool IsLeapYear(uint year)
    bool TryTimeToMinutes(string time, ref uint totalMinutes)
    bool TryDateToDays(string date, ref uint totalDays)
    bool TryDateToDaysSince2000(string date, ref uint totalDays)
    bool TryDateToDayOfWeek(string date, ref DayOfWeek dayOfWeek)
}
```

### Dictionary
```hopper
unit Dictionary
{
    uint Count { get }
    Set(<K,V> this, K key, V value)
    bool Contains(<K,V> this, K key)
    V Get(<K,V> this, K key)
    Clear(<K,V> this)
    string ToString(<variant,variant> this)
}
```

### Directory
```hopper
unit Directory
{
    bool Exists(string path)
    Delete(string path)
    bool IsValid(directory this)
    directory Open(string fullpath)
    Create(string path)
    uint GetDirectoryCount(directory this)
    uint GetDirectoryCount(directory this, ref uint skipped)
    uint GetFileCount(directory this)
    uint GetFileCount(directory this, ref uint skipped)
    string GetDirectory(directory this, uint index)
    string GetFile(directory this, uint index)
    string GetTime(string path)
    string GetDate(string path)
}
```

### File
```hopper
unit File
{
    bool Exists(string path)
    Delete(string path)
    long GetSize(string path)
    string GetTime(string path)
    string GetDate(string path)
    long GetTimeStamp(string path)
    bool IsValid(file this)
    
    file Open(string fullpath)
    string ReadLine(file this)
    byte Read(file this)
    byte Read(file this, long seekPosition)
    uint Read(file this, byte[] data, uint bufferSize)
    
    file Create(string fullpath)
    Append(file this, byte content)
    Append(file this, string content)
    Flush(file this)
}
```

### Float
```hopper
unit Float
{
    bool TryParse(string content, ref float returnValue)
    string ToString(float this)
    byte GetByte(float this, byte index)
    float FromBytes(byte b0, byte b1, byte b2, byte b3)
    
    const float Pi = 3.1415926535
    
    float Sin(float angle)
    float Cos(float angle)
    float ATan2(float y, float x)
    float Sqrt(float value)
    float Radians(float angle)
    float Degrees(float angle)
    float Abs(float value)
    float Min(float a, float b)
    float Max(float a, float b)
}
```

### Int
```hopper
unit Int
{
    bool TryParse(string input, ref int returnValue)
    string ToString(int this)
    float ToFloat(int this)
    long ToLong(int this)
    string ToHexString(int this, byte digits)
    byte GetByte(int this, byte index)
    int FromBytes(byte b0, byte b1)
    int Abs(int value)
    int Min(int a, int b)
    int Max(int a, int b)
    Swap(ref int a, ref int b)
}
```

### IO
```hopper
unit IO
{
    bool EchoToLCD { set; get }
    uint LineMax { get }
    
    Clear()
    WriteInt(int this)
    WriteUInt(uint this)
    WriteHex(byte b)
    WriteHex(uint u)
    Write(char c)
    Write(string s)
    WriteLn()
    WriteLn(string s)
    char Read()
    bool ReadLn(ref string str)
    bool IsAvailable { get }
    bool IsBreak()
}
```

### Keyboard
```hopper
unit Keyboard
{
    flags Key
    {
        NoKey = 0x0000, Alt = 0x0100, Control = 0x0200, Shift = 0x0400,
        Mask = 0xF0FF, Click = 0xE0FF, Scroll = 0xE0FE, ClickRight = 0xE0FD
        // ... additional key definitions
    }
    
    uint ClickX { get }
    uint ClickY { get }
    bool ClickUp { get }
    bool ClickDouble { get }
    int ScrollDelta { get }
    
    bool IsAvailable { get }
    Key ReadKey()
    Key ToKey(char c)
    char ToChar(Key this)
}
```

### List
```hopper
unit List
{
    delegate int ListCompareDelegate(<V> this, uint ai, uint bi)
    
    uint Count { get }
    Append(<V> this, V value)
    Remove(<V> this, uint index)
    V GetItem(<V> this, uint index)
    SetItem(<V> this, uint index, V value)
    Insert(<V> this, uint index, V value)
    Clear(<V> this)
    bool Contains(<V> this, V value)
    string ToString(<variant> this)
    Sort(<V> this, ListCompareDelegate comparer)
}
```

### Long
```hopper
unit Long
{
    bool TryParse(string input, ref long returnValue)
    string ToString(long this)
    byte GetByte(long this, byte index)
    long FromBytes(byte b0, byte b1, byte b2, byte b3)
    float ToFloat(long this)
    string ToHexString(long this, byte digits)
    string ToBinaryString(long this, byte digits)
    long Abs(long value)
    long Min(long a, long b)
    long Max(long a, long b)
}
```

### Pair
```hopper
unit Pair
{
    V Value(<K,V> this)
    K Key(<K,V> this)
}
```

### Path
```hopper
unit Path
{
    bool IsValidPathCharacter(char c)
    string GetFileName(string path)
    string GetDirectoryName(string fullPath)
    string Combine(string partOne, string partTwo)
    string GetExtension(string path)
    string GetFullPath(string path)
    string GetCorrectCase(string path)
}
```

### Runtime
```hopper
unit Runtime
{
    bool InDebugger { get }
    string DateTime { get }
    uint Execute(string programPath, <string> arguments)
    uint UserCode { get }
    uint Inline(byte[] code, uint startIndex)
    uint PC { get }
}
```

### Screen
```hopper
unit Screen
{
    byte CursorX { get }
    byte CursorY { get }
    byte Columns { get }
    byte Rows { get }
    uint ForeColour { get; set }
    uint BackColour { get; set }
    
    Suspend()
    Resume(bool isInteractive)
    bool ShowCursor { set }
    Clear()
    SetCursor(uint col, uint row)
    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour)
    Print(char c, uint foreColour, uint backColour)
    Print(string s, uint foreColour, uint backColour)
    PrintLn()
}
```

### Serial
```hopper
unit Serial
{
    Connect()
    Connect(uint port)
    Connect(uint port, string baud)
    Close()
    bool IsValid()
    bool IsAvailable { get }
    char ReadChar()
    WriteChar(char ch)
    WriteString(string str)
    <string> Ports { get }
}
```

### String
```hopper
unit String
{
    string Append(string this, string append)
    string Append(string this, char append)
    Build(ref string build, string append)
    Build(ref string build, char append)
    Build(ref string build)
    BuildFront(ref string build, char insert)
    char GetChar(string this, uint index)
    bool IsEmpty { get }
    string InsertChar(string this, uint index, char append)
    uint Length { get }
    bool IndexOf(string this, char pattern, ref uint index)
    bool Contains(string this, char needle)
    bool StartsWith(string this, char pattern)
    string Replace(string this, string pattern, string replace)
    string Replace(string this, char pattern, char replace)
    bool EndsWith(string this, char pattern)
    string Substring(string this, uint start)
    string Substring(string this, uint start, uint length)
    string Trim(string this)
    string ToUpper(string this)
    string ToLower(string this)
    int Compare(string left, string right)
}
```

### System
```hopper
unit System
{
    <string> Arguments { get }
    string CurrentDirectory { get; set }
    Beep()
}
```

### Time
```hopper
unit Time
{
    // Returns timestamp in millisecond-sized ticks since startup
    long Millis { get }
    
    // Returns timestamp in second-sized ticks since startup
    uint Seconds { get }
    
    // Number of microseconds per tick of Millis
    // Default: 1000 (1ms)
    // Minimum on RP2040: 22
    // Setting a lower value increases timer resolution but Millis ticks faster
    uint SampleMicros { set; get }
    
    // Pause execution for specified number of milliseconds
    Delay(uint ms)
    
    // Pause execution for specified number of seconds
    DelaySeconds(uint s)
}
```

### Type
```hopper
unit Type
{
    string ToString(type this)
}
```

### UInt
```hopper
unit UInt
{
    bool TryParse(string content, ref uint returnValue)
    string ToString(uint this)
    ToString(uint this, ref string result)
    long ToLong(uint this)
    float ToFloat(uint this)
    byte GetByte(uint this, byte index)
    uint FromBytes(byte b0, byte b1)
    string ToHexString(uint this, byte digits)
    string ToBinaryString(uint this)
    uint Min(uint a, uint b)
    uint Max(uint a, uint b)
    Swap(ref uint a, ref uint b)
    uint Random()
    Seed(uint seed)
}
```

### Variant
```hopper
unit Variant
{
    string ToString(variant vr)
    long ToLong(variant vr)
}
```

### WiFi
```hopper
unit WiFi
{
    enum WiFiStatus
    {
        Idle, Connected, ConnectionFailed, ConnectionLost,
        Disconnected, AccesspointListening, AccesspointConnected,
        AccessPointFailed, NoModule = 255
    }
    
    bool Connect(string ssid, string password)
    bool BeginAP(string ssid, string password)
    bool BeginAP()
    Disconnect()
    string IP { get }
    WiFiStatus Status { get }
}
```
