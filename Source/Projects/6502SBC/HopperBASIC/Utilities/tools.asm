unit Tools // Tools.asm
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Production utilities only - debug functionality moved to Debug.asm
 
    // Print null-terminated string to serial output
    // Input: ZP.STR = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringSTR()
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
        
        PLY
        PLA
    } 
        
    // Print null-terminated string to serial output
    // Input: ZP.ACC = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringACC()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.ACC], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 
    
    // Print null-terminated string to serial output
    // Input: ZP.TOP = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringTOP()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.TOP], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 
    
    // Print null-terminated string to serial output
    // Input: ZP.IDY = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringIDY()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.IDY], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 

    // Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    {
        PHP  // Push processor status (including carry flag)
        PHA
        LDA #'\n' 
        Serial.WriteChar();
        PLA
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Write character preserving carry flag
    // Input: A = character to output
    // Output: Character printed to serial
    // Preserves: Everything
    COut()
    {
        PHP  // Push processor status (including carry flag)
        Serial.WriteChar();
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Print variable value with proper type formatting
    // Input: ZP.TOP = value, ZP.TOPT = type
    // Output: Value printed to serial (TRUE/FALSE for BIT, numeric for others)
    // Preserves: Everything
    PrintVariableValue()
    {
        PHA
        
        // Special handling for BIT type - print TRUE/FALSE instead of 1/0
        LDX ZP.TOPT
        switch(X)
        {
            case BasicType.BIT:
            {
                LDA ZP.TOPL
                CMP #0
                if (Z)
                {
                    LDA #Token.FALSE
                    Tokenizer.PrintKeyword();
                }
                else
                {
                    LDA #Token.TRUE
                    Tokenizer.PrintKeyword();
                }
            }
            case BasicType.STRING:
            {
                PrintStringTOP();  // Print the actual string content
            }
            default:
            {
                PrintDecimalWord(); // Numeric types
            }
        }
        
        PLA
    }
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOP = 16-bit number to print (0-65535)
    //        ZP.TOPT = type (for signed/unsigned determination)
    // Output: Decimal number printed to serial
    // Preserves: Everything
    PrintDecimalWord()
    {
        PHA
        PHX
        PHY
        
        // Save ZP.ACC since we'll use it as working space
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Save ZP.TOP since we'll modify it during conversion
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
        
        // Check if this is a signed type that's negative
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (Z)  // INT type
        {
            BIT ZP.TOPH  // Test sign bit
            if (MI)      // Negative
            {
                // Print minus sign
                LDA #'-'
                Serial.WriteChar();
                
                // Negate the value: TOP = 0 - TOP
                SEC
                LDA #0
                SBC ZP.TOPL
                STA ZP.TOPL
                LDA #0
                SBC ZP.TOPH
                STA ZP.TOPH
            }
        }
        
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
        
        // If we never printed anything (ACCL is still 0), the number was 0
        LDA ZP.ACCL
        if (Z)  // Never set padding, so number was 0
        {
            LDA #'0'
            Serial.WriteChar();
        }
        
        // Restore ZP.TOP
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        // Restore ZP.ACC
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }
    
    // Powers of 10 table for PrintDecimalWord (little-endian format)
    const byte[] PrDec16Tens = { 
        0x01, 0x00,  // 1 (little-endian)
        0x0A, 0x00,  // 10
        0x64, 0x00,  // 100  
        0xE8, 0x03,  // 1000
        0x10, 0x27   // 10000
    };
    
    // Copy bytes from source to destination
    // Input: ZP.FSOURCEADDRESS = source pointer
    //        ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to copy (16-bit)
    // Output: Data copied from source to destination
    // Preserves: Input parameters (saves/restores ZP.F* variables)
    CopyBytes()
    {
        PHA
        PHX
        PHY
        
        // Save the input parameters that we'll modify during the copy
        LDA ZP.FSOURCEADDRESSL
        PHA
        LDA ZP.FSOURCEADDRESSH
        PHA
        LDA ZP.FDESTINATIONADDRESSL
        PHA
        LDA ZP.FDESTINATIONADDRESSH
        PHA
        LDA ZP.FLENGTHL
        PHA
        LDA ZP.FLENGTHH
        PHA
        
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to copy
            
            // Copy one byte: *FDESTINATIONADDRESS = *FSOURCEADDRESS
            LDY #0
            LDA [ZP.FSOURCEADDRESS], Y
            STA [ZP.FDESTINATIONADDRESS], Y
            
            // Increment FSOURCEADDRESS
            INC ZP.FSOURCEADDRESSL
            if (Z)
            {
                INC ZP.FSOURCEADDRESSH
            }
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
        
        // Restore the input parameters in reverse order
        PLA
        STA ZP.FLENGTHH
        PLA
        STA ZP.FLENGTHL
        PLA
        STA ZP.FDESTINATIONADDRESSH
        PLA
        STA ZP.FDESTINATIONADDRESSL
        PLA
        STA ZP.FSOURCEADDRESSH
        PLA
        STA ZP.FSOURCEADDRESSL
        
        PLY
        PLX
        PLA
    }
    
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything (saves/restores ZP.TOP)
    StringLength()
    {
        PHX
        PHY
        
        // Use stack to preserve caller's ZP.TOP instead of corrupting it
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH  
        PHA
        
        STX ZP.TOPL
        STY ZP.TOPH
        
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y
            if (Z) { break; }
            INY
#ifdef DEBUG
            if (Z) // wrapped around from 0xFF to 0x00
            {
                LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
            }
#endif
        }
        
        // Restore caller's ZP.TOP
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        TYA  // Length in A
        
        PLY
        PLX
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    StringCompare()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.TOP], Y
            CMP [ZP.NEXT], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.TOP], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            
            INY
        }
        
        PLY
        PLA
    } 
    
    // Compare two strings using throwaway registers
    // Input: ZP.STR = first string, ZP.STR2 = second string
    // Output: C set if match, NC if different
    // Preserves: Everything
    StringCompareSTR()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.STRL
            CMP ZP.STR2L
            if (Z)
            {
                LDA ZP.STRH
                CMP ZP.STR2H
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.STR], Y
            CMP [ZP.STR2], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.STR], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            INY
        }
        PLY
        PLA
    } 
}
