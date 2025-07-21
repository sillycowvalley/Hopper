unit Tools
{
    // Print null-terminated string to serial output
    // On entry: ZP.IDXL/ZP.IDXH = pointer to null-terminated string
    // On exit:  A,Y corrupted, IDX preserved
    // Uses:     Serial.WriteChar() for output
    PrintString()
    {
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.IDX], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
    }
    
    // Print 16-bit decimal number with no leading zeros
    // On entry: ZP.TOPL/ZP.TOPH = 16-bit number to print (0-65535)
    // On exit:  A,X,Y corrupted, TOP destroyed, ACCL modified
    // Uses:     Serial.WriteChar() for output
    
    // Original source: https://www.beebwiki.mdfs.net/Number_output_in_6502_machine_code
    
    // Powers of 10 table (little-endian format)
    const byte[] PrDec16Tens = { 
        0x01, 0x00,  // 1 (little-endian)
        0x0A, 0x00,  // 10
        0x64, 0x00,  // 100  
        0xE8, 0x03,  // 1000
        0x10, 0x27   // 10000
    };
    
    PrintDecimalWord()
    {
        STZ ZP.ACCL         // Initialize: no padding (suppress leading zeros)
        
        LDY #8              // Offset to powers of ten table
        
        loop                // Outer loop for each digit
        {
            LDX #0xFF       // Start with digit = -1
            SEC             // Prepare for subtraction
            
            loop            // Inner loop - subtract current power of 10
            {
                LDA ZP.TOPL
                SBC PrDec16Tens, Y
                STA ZP.TOPL
                LDA ZP.TOPH
                SBC PrDec16Tens+1, Y
                STA ZP.TOPH
                INX         // Count digits
                if (NC) { break; } // Loop until result < 0 (no carry)
            }
            
            // Add the power of 10 back (we subtracted one too many)
            LDA ZP.TOPL
            ADC PrDec16Tens, Y
            STA ZP.TOPL
            LDA ZP.TOPH
            ADC PrDec16Tens+1, Y
            STA ZP.TOPH
            
            TXA             // Get digit count
            if (NZ)         // Not zero, print it
            {
                LDX #'0'    // No more zero padding needed
                STX ZP.ACCL
                ORA #'0'    // Convert digit to ASCII
                Serial.WriteChar();
            }
            else
            {
                LDA ZP.ACCL // Check padding
                if (NZ)     // pad != 0, use it
                {
                    Serial.WriteChar();
                }
            }
            
            DEY             // Move to next power of 10
            DEY             // (table entries are 2 bytes each)
            if (MI) { break; } // Exit when Y goes negative
        }
    }

}
