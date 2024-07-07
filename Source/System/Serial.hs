unit Serial
{
    Connect() system;
    Connect(uint port) system;
    Connect(uint port, string baud) system;
    Close() system;
    bool IsValid() system;
    bool IsAvailable { get system; }
    char ReadChar() system;
    WriteChar(char ch) system;
    WriteString(string str) system;
    
    <string> Ports { get system ; }
}
