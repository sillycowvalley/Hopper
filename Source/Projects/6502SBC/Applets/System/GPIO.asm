unit GPIO
{
    // Pin mode constants
    enum PINMODE
    {
        INPUT  = 0x00,  // High impedance input
        OUTPUT = 0x01,  // Push-pull output
    }
    
    // Configure pin direction
    // Input:  A = pin number (0-15)
    //         Y = mode (PinMode.INPUT or PinMode.OUTPUT)
    // Output: None
    // Note:   Pins 0-7 = Port A, Pins 8-15 = Port B
    //         VIA 65C22 DDR registers configured
    PinMode()
    {
        LDX # SysCall.PinMode
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Read digital pin state
    // Input:  A = pin number (0-15)
    // Output: A = pin value (0 or 1)
    //         Z flag set if LOW, clear if HIGH
    // Note:   Reads actual pin state regardless of DDR setting
    PinRead()
    {
        LDX # SysCall.PinRead
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Write digital pin state
    // Input:  A = pin number (0-15)
    //         Y = value (0 for LOW, non-zero for HIGH)
    // Output: None
    // Note:   Pin should be configured as OUTPUT first
    //         Writing to INPUT pin sets internal latch only
    PinWrite()
    {
        LDX # SysCall.PinWrite
        JMP [ZP.BIOSDISPATCH]
    }
}
