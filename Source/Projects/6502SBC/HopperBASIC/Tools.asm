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
    

#ifdef DEBUG    
    // Debug function to dump key zero page variables
    DumpVariables()
    {
       // Store registers in zero page temporarily so we can display them
       STA ZP.U2  // Temporarily store A
       STX ZP.U0  // Temporarily store X
       STY ZP.U1  // Temporarily store Y
       
       LDA #'\n'
       Serial.WriteChar();
       LDA #'='
       Serial.WriteChar();
       LDA #'='
       Serial.WriteChar();
       LDA #' '
       Serial.WriteChar();
       LDA #'V'
       Serial.WriteChar();
       LDA #'A'
       Serial.WriteChar();
       LDA #'R'
       Serial.WriteChar();
       LDA #'S'
       Serial.WriteChar();
       LDA #' '
       Serial.WriteChar();
       LDA #'='
       Serial.WriteChar();
       LDA #'='
       Serial.WriteChar();
       LDA #'\n'
       Serial.WriteChar();
       
       // A register
       LDA #'A'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U2
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // X register
       LDA #'X'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U0
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // Y register
       LDA #'Y'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U1
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // TOP
       LDA #'T'
       Serial.WriteChar();
       LDA #'O'
       Serial.WriteChar();
       LDA #'P'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.TOPH
       Serial.HexOut();
       LDA ZP.TOPL
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // NEXT
       LDA #'N'
       Serial.WriteChar();
       LDA #'X'
       Serial.WriteChar();
       LDA #'T'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.NEXTH
       Serial.HexOut();
       LDA ZP.NEXTL
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // ACC
       LDA #'A'
       Serial.WriteChar();
       LDA #'C'
       Serial.WriteChar();
       LDA #'C'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.ACCH
       Serial.HexOut();
       LDA ZP.ACCL
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // IDX
       LDA #'I'
       Serial.WriteChar();
       LDA #'D'
       Serial.WriteChar();
       LDA #'X'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.IDXH
       Serial.HexOut();
       LDA ZP.IDXL
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // IDY
       LDA #'I'
       Serial.WriteChar();
       LDA #'D'
       Serial.WriteChar();
       LDA #'Y'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.IDYH
       Serial.HexOut();
       LDA ZP.IDYL
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // SP
       LDA #'S'
       Serial.WriteChar();
       LDA #'P'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.SP
       Serial.HexOut();
       
       LDA #'\n'
       Serial.WriteChar();
       
       // Restore registers
       LDY ZP.U1  // Restore Y
       LDX ZP.U0  // Restore X
       LDA ZP.U2  // Restore A
    }

    // Debug function to dump the value stack
    DumpStack()
    {
        PHA  // Save A
        PHX  // Save X  
        PHY  // Save Y
        
        LDA #'\n'
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        LDA #'S'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #'A'
        Serial.WriteChar();
        LDA #'C'
        Serial.WriteChar();
        LDA #'K'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'\n'
        Serial.WriteChar();
        
        // SP and BP
        LDA #'S'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.SP
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        LDA #'B'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.BP
        Serial.HexOut();
        LDA #'\n'
        Serial.WriteChar();
        
        // Dump stack contents (last 8 entries)
        LDX ZP.SP
        LDY #8  // Show last 8 entries
        
        loop
        {
            CPY #0
            if (Z) { break; }
            
            CPX #0
            if (Z) { break; }
            
            DEX
            
            // Print index
            TXA
            Serial.HexOut();
            LDA #':'
            Serial.WriteChar();
            
            // Print type
            LDA Address.TypeStackLSB, X
            Serial.HexOut();
            LDA #'/'
            Serial.WriteChar();
            
            // Print value (LSB/MSB)
            LDA Address.ValueStackMSB, X
            Serial.HexOut();
            LDA Address.ValueStackLSB, X
            Serial.HexOut();
            LDA #' '
            Serial.WriteChar();
            
            DEY
        }
        
        LDA #'\n'
        Serial.WriteChar();
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
    }
    
    // Debug function to dump the heap free list
    DumpHeap()
    {
        PHA  // Save A
        PHX  // Save X  
        PHY  // Save Y
        
        LDA #'\n'
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        LDA #'H'
        Serial.WriteChar();
        LDA #'E'
        Serial.WriteChar();
        LDA #'A'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'='
        Serial.WriteChar();
        LDA #'\n'
        Serial.WriteChar();
        
        // Show heap start and size
        LDA #'S'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #'A'
        Serial.WriteChar();
        LDA #'R'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.HEAPSTART
        Serial.HexOut();
        LDA #'0'
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        LDA #'S'
        Serial.WriteChar();
        LDA #'I'
        Serial.WriteChar();
        LDA #'Z'
        Serial.WriteChar();
        LDA #'E'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.HEAPSIZE
        Serial.HexOut();
        LDA #'0'
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #'\n'
        Serial.WriteChar();
        
        // Walk the free list
        LDA ZP.FREELISTL
        STA ZP.IDXL
        LDA ZP.FREELISTH
        STA ZP.IDXH
        
        LDX #0  // Block counter
        
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }  // End of free list
            
            // Print block number
            LDA #'['
            Serial.WriteChar();
            TXA
            Serial.HexOut();
            LDA #']'
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print block address
            LDA ZP.IDXH
            Serial.HexOut();
            LDA ZP.IDXL
            Serial.HexOut();
            LDA #':'
            Serial.WriteChar();
            
            // Print block size
            LDY #0
            LDA [ZP.IDX], Y
            Serial.HexOut();
            INY
            LDA [ZP.IDX], Y
            Serial.HexOut();
            LDA #' '
            Serial.WriteChar();
            
            // Print next pointer
            LDA #'>'
            Serial.WriteChar();
            INY
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            Serial.HexOut();
            PLA
            Serial.HexOut();
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next block
            DEY
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            INX
            CPX #8  // Limit to 8 blocks to avoid infinite loops
            if (Z) 
            { 
                LDA #'.'
                Serial.WriteChar();
                LDA #'.'
                Serial.WriteChar();
                LDA #'.'
                Serial.WriteChar();
                LDA #'\n'
                Serial.WriteChar();
                break; 
            }
        }
        
        LDA #'\n'
        Serial.WriteChar();
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
    }
#endif

}
