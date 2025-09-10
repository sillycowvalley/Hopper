unit Print
{
    // Print null-terminated string
    // Input: ZP.STR = pointer to string
    // Output: String printed to serial
    String()
    {
        PHY
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.STR], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
    }
    
    // Write single character to serial output
    // Input: A = character to write
    // Output: Character sent to serial port
    // Preserves: X, Y, A
    // Munts: Flags
    Char()
    {
        Serial.WriteChar();
    }
    
    // Print byte value as two hex characters
    // Input: A = byte value to print
    // Output: Two hex characters printed to serial
    // Preserves: A
    // Munts: None (preserves via stack)
    Hex()
    {
        Serial.HexOut();
    }
    
    // Print newline character
    // Input: None
    // Output: Newline printed to serial
    // Munts: A
    NewLine()
    {
        LDA #'\n' 
        Serial.WriteChar();
    }
    
    // Print space character
    // Input: None
    // Output: Newline printed to serial
    // Munts: A
    Space()
    {
        LDA #' ' 
        Serial.WriteChar();
    }
    
    // Print specified number of space characters
    // Input: Y = number of spaces to print
    // Output: Y space characters printed to serial
    // Munts: Y
    Spaces()
    {
        PHY
        CPY #0
        if (NZ)
        {
            loop
            {
                LDA #' '
                Serial.WriteChar();
                DEY
                if (Z) // Set Z - counter reached 0
                { 
                    break; 
                }
            }
        }
        PLY
    }
    
    // Print 16-bit decimal number with no leading zeros
    // Input:  ZP.LTOP0-3 = 32 bit signed LONG (if ZP.TOPT == BASICType.LONG)
    // Output: Decimal number printed to serial
    // Preserves: X, Y, A
    // Munts: Flags
    Decimal()
    {
        // allowed types: zero, INT, LONG
        PHA
        Long.Print();
        PLA
    }
}
