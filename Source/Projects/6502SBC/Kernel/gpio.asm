unit GPIO
{
    const byte[] BitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
     
    // Pin mode constants
    enum PINMODE
    {
        INPUT  = 0x00,
        OUTPUT = 0x01,
    }
    
    // Configure pin mode (INPUT or OUTPUT)
    // Input: A = pin number (0-15), Y = mode (PinMode.INPUT or PinMode.OUTPUT)
    // Output: None
    PinMode()
    {
        LDX #0 // port A
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use DDRB
            SEC
            SBC #8                        // Convert to 0-7 range
            INX  // port B
        }
        PHY
        TAY                          // Use as index
        LDA BitMasks, Y   // Load mask directly from table
            
        PLY // restore value
        // Set or clear the DDR bit based on mode
        // CPY # PINMODE.INPUT = 0
        if (Z)
        {
            // INPUT mode: Clear bit in DDR
            EOR #0xFF        // Invert mask
            AND ZP.DDRA, X
        }
        else
        {
            // OUTPUT mode: Set bit in DDR
            ORA ZP.DDRA, X
        }
        STA ZP.DDRA, X
    }
    
    // Write digital value to pin
    // Input: A = pin number (0-15), Y = value (0 or 1)
    // Output: None
    // Modifies: A, X, Y
    PinWrite()
    {
        LDX #0 // port A
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use PORTB
            SEC
            SBC #8                        // Convert to 0-7 range
            INX // port B
        }
        PHY
        TAY                          // Use as index
        LDA BitMasks, Y   // Load mask directly from table
        
        PLY // restore the value
        // Set or clear the port bit based on value
        if (Z)
        {
            // Write LOW: Clear bit
            EOR #0xFF        // Invert mask
            AND ZP.PORTA, X
        }
        else
        {
            // Write HIGH: Set bit
            ORA ZP.PORTA, X
        }
        STA ZP.PORTA, X
    }
    
    // Read digital value from pin
    // Input:    A = pin number (0-15)
    // Output:   A = pin value (0 or 1), Z flag set if LOW
    // Modifies: A, X, Y
    PinRead()
    {
        LDX #0 // port A
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use PORTB
            SEC
            SBC #8                        // Convert to 0-7 range
            INX // port B
        }
        TAY                           // Use as index
        LDA BitMasks, Y   // Load mask directly from table
            
        // Read the port and mask the bit
        AND ZP.PORTA, X
        if (Z)
        {
            LDA #0       // Return 0 for LOW
        }
        else
        {
            LDA #1       // Return 1 for HIGH
        }
        // A contains result, Z flag set appropriately
    }
}
