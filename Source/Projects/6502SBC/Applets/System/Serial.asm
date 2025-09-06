unit Serial
{
    // Write character to serial port
    // Input:  A = character to send
    // Output: None (blocks until sent)
    // Note:   Handles XON/XOFF flow control automatically
    WriteChar()
    {
        LDX # SysCall.SerialWriteChar
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Wait for and read character from serial port
    // Input:  None
    // Output: A = character received
    // Note:   Blocks until character available
    //         Returns Ctrl+C (0x03) if break detected
    WaitForChar()
    {
        LDX # SysCall.SerialWaitForChar
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Check if serial input available
    // Input:  None
    // Output: C set if character available, clear if buffer empty
    // Note:   Non-blocking, use before WaitForChar() to avoid blocking
    IsAvailable()
    {
        LDX # SysCall.SerialIsAvailable
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Check for break condition (Ctrl+C or NMI)
    // Input:  None
    // Output: C set if break detected, clear otherwise
    // Note:   Clears break flag when detected
    IsBreak()
    {
        LDX # SysCall.IsBreak
        JMP [ZP.BIOSDISPATCH]
    }
}