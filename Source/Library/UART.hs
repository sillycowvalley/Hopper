unit UART
{
    Setup (uint baud, byte txPin, byte rxPin) library;
    Setup()
    {
        Setup(9600, UART0Tx, UART0Rx);
    }
    Setup(uint baud)
    {
        Setup(baud, UART0Tx, UART0Rx);
    }
    
    bool IsAvailable { get library; }
    char ReadChar() library;
    WriteChar(char ch) library;
    
    WriteString(string str)
    {
        foreach (var ch in str)
        {
            UART.WriteChar(ch);
            Time.Delay(10);
        }
    }
}
