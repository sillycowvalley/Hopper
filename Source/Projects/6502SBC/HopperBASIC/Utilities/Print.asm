unit Print
{
    // Print null-terminated string
    // Input: ZP.STR = pointer to string
    // Output: String printed to serial
    String()
    {
        PHA
        PHY
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.STR], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        SMB6 ZP.FLAGS // Bit 6 - output was produced
        
        PLY
        PLA
    }
    
    // Write single character to serial output
    // Input: A = character to write
    // Output: Character sent to serial port
    // Preserves: X, Y, A
    // Munts: Flags
    Char()
    {
        Serial.WriteChar();
        
        SMB6 ZP.FLAGS // Bit 6 - output was produced
    }
    
    // Print byte value as two hex characters
    // Input: A = byte value to print
    // Output: Two hex characters printed to serial
    // Preserves: A
    // Munts: None (preserves via stack)
    Hex()
    {
        Serial.HexOut();
        
        SMB6 ZP.FLAGS // Bit 6 - output was produced
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
    // Input: X = number of spaces to print
    // Output: X space characters printed to serial
    // Munts: A
    Spaces()
    {
        PHX
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
        PLX
    }
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOPT for type (typically BASICType.WORD or BASICType.INT for signed), 0 = BASICType.WORD
    //        ZP.TOP0-1  = 16-bit number to print (0-65535)
    //        ZP.LTOP0-3 = 32 bit signed LONG (if ZP.TOPT == BASICType.LONG)
    // Output: Decimal number printed to serial
    // Preserves: X, Y, A
    // Munts: Flags
    Decimal()
    {
        // allowed types: zero, INT, LONG
        PHA
        LDA ZP.TOPT
        PHA
        
        AND # BASICType.TYPEMASK
        STA ZP.TOPT
        BASICTypes.Promote(); // -> LONG
        Long.Print();
        
        PLA
        STA ZP.TOPT
        PLA
        
        SMB6 ZP.FLAGS // Bit 6 - output was produced
    }
}
