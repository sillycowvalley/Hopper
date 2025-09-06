unit Print
{
    // Print null-terminated string
    // Input:  ZP.STR = pointer to string
    // Output: None (string printed to serial)
    // Note:   String must be null-terminated
    String()
    {
        LDX # SysCall.PrintString
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print single character
    // Input:  A = character to print
    // Output: None (character printed to serial)
    Char()
    {
        LDX # SysCall.PrintChar
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print byte as two hex digits
    // Input:  A = byte value (0x00-0xFF)
    // Output: None (prints two hex digits to serial)
    // Example: A=0x3F prints "3F"
    Hex()
    {
        LDX # SysCall.PrintHex
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print newline character
    // Input:  None
    // Output: None (newline printed to serial)
    NewLine()
    {
        LDX # SysCall.PrintNewLine
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print single space character
    // Input:  None
    // Output: None (space printed to serial)
    Space()
    {
        LDX # SysCall.PrintSpace
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print multiple spaces
    // Input:  Y = number of spaces to print
    // Output: None (Y spaces printed to serial)
    // Note:   Y=0 prints no spaces
    Spaces()
    {
        LDX # SysCall.PrintSpaces
        JMP [ZP.BIOSDISPATCH]
    }
}