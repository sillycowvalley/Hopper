unit Serial
{
#if !defined(MINIMAL_RUNTIME) && !defined(CPU_Z80)
    Connect() system;
#endif    
    WriteChar(char ch) system;
    char ReadChar(char ch) system;
    bool IsAvailable { get { system; } }
}
