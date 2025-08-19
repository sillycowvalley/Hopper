unit Print
{
    // Print null-terminated string to serial output
    // Input: ZP.STR = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    // Munts: Flags, A, Y
    String()
    {
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.STR], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
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
    // Write single byte to serial output as two hex digits
    // Input: A = byte to write
    // Output: two hex characters sent to serial port
    // Preserves: X, Y, A
    // Munts: Flags
    Hex()
    {
        Serial.HexOut();
    }
    
    // Write '\n'
    // Output: '\n' printed to serial
    // Preserves: X, Y, A
    // Munts: A, Flags
    NewLine()
    {
        LDA #'\n' 
        Serial.WriteChar();
    }
    
    // number of spaces to print in X
    // Munts: A, Flags
    Spaces()
    {
        loop
        {
            LDA #' '
            Serial.WriteChar();
            DEX
            if (Z) // Set Z - counter reached 0
            { 
                break; 
            }
        }
    }
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOPT for type (typically BASICType.WORD or BASICType.INT for signed)
    //        ZP.TOP0-1  = 16-bit number to print (0-65535)
    //        ZP.LTOP0-3 = 32 bit signed LONG (if ZP.TOPT == BASICType.LONG)
    // Output: Decimal number printed to serial
    // Preserves: X, Y, A
    // Munts: Flags
    Decimal()
    {
        PHA
        LDA ZP.TOPT
        PHA
        
        loop
        {
            if (BBR3, ZP.TOPT) // LONG = bit 3
            {
                Long.TopToLong(); // ZP.TOPL, ZP.TOPH, ZP.TOPT -> ZP.LTOP0-3, ZP.TOPT
                if (NC)
                {
                    break;
                }
            }
            Long.Print();
            break;
        } // single exit
        
        PLA
        STA ZP.TOPT
        PLA
    }
}
