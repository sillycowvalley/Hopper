unit GPIO
{
    // Pin mode constants
    enum PINMODE
    {
        INPUT  = 0x00,
        OUTPUT = 0x01,
    }
    
    // Configure pin mode (INPUT or OUTPUT)
    // Input: A = pin number (0-15), X = mode (PinMode.INPUT or PinMode.OUTPUT)
    // Output: None
    PinMode()
    {
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use DDRB
            SEC
            SBC #8                        // Convert to 0-7 range
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Set or clear the DDR bit based on mode
            CPX #PINMODE.INPUT
            if (Z)
            {
                // INPUT mode: Clear bit in DDR
                EOR #0xFF        // Invert mask
                AND ZP.DDRB
            }
            else
            {
                // OUTPUT mode: Set bit in DDR
                ORA ZP.DDRB
            }
            STA ZP.DDRB
        }
        else
        {
            // Pin 0-7: Use DDRA
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Set or clear the DDR bit based on mode
            CPX #PINMODE.INPUT
            if (Z)
            {
                // INPUT mode: Clear bit in DDR
                EOR #0xFF        // Invert mask
                AND ZP.DDRA
            }
            else
            {
                // OUTPUT mode: Set bit in DDR
                ORA ZP.DDRA
            }
            STA ZP.DDRA
        }
    }
    
    // Write digital value to pin
    // Input: A = pin number (0-15), X = value (0 or 1)
    // Output: None
    // Modifies: A, Y
    PinWrite()
    {
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use PORTB
            SEC
            SBC #8                        // Convert to 0-7 range
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Set or clear the port bit based on value
            CPX #0
            if (Z)
            {
                // Write LOW: Clear bit
                EOR #0xFF        // Invert mask
                AND ZP.PORTB
            }
            else
            {
                // Write HIGH: Set bit
                ORA ZP.PORTB
            }
            STA ZP.PORTB
        }
        else
        {
            // Pin 0-7: Use PORTA
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Set or clear the port bit based on value
            CPX #0
            if (Z)
            {
                // Write LOW: Clear bit
                EOR #0xFF        // Invert mask
                AND ZP.PORTA
            }
            else
            {
                // Write HIGH: Set bit
                ORA ZP.PORTA
            }
            STA ZP.PORTA
        }
    }
    
    // Read digital value from pin
    // Input: A = pin number (0-15)
    // Output: A = pin value (0 or 1), Z flag set if LOW
    // Modifies: A, Y
    PinRead()
    {
        // Determine which port and create bit mask
        CMP #8
        if (C)
        {
            // Pin 8-15: Use PORTB
            SEC
            SBC #8                        // Convert to 0-7 range
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Read the port and mask the bit
            AND ZP.PORTB
            if (Z)
            {
                LDA #0       // Return 0 for LOW
            }
            else
            {
                LDA #1       // Return 1 for HIGH
            }
        }
        else
        {
            // Pin 0-7: Use PORTA
            TAY                           // Use as index
            LDA BASICArray.BitMasks, Y   // Load mask directly from table
            
            // Read the port and mask the bit
            AND ZP.PORTA
            if (Z)
            {
                LDA #0       // Return 0 for LOW
            }
            else
            {
                LDA #1       // Return 1 for HIGH
            }
        }
        // A contains result, Z flag set appropriately
    }
}
