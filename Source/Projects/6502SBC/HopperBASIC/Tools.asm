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
    
    // Debug function to dump all heap blocks (allocated and free)
    // Walks through heap sequentially using block size headers
    // On entry: None
    // On exit:  A,X,Y preserved
    // Munts:    ZP.M0, ZP.M1, ZP.M2, ZP.M3, ZP.U0, ZP.U2, ZP.U3, ZP.IDX, ZP.IDY
    // Uses:     Serial.WriteChar(), Serial.HexOut() for output
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
        LDA #'D'
        Serial.WriteChar();
        LDA #'U'
        Serial.WriteChar();
        LDA #'M'
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
        
        // Start at heap beginning: ZP.HEAPSTART is the page number
        LDA ZP.HEAPSTART
        STA ZP.IDXH
        LDA #0
        STA ZP.IDXL
        
        LDX #0  // Block counter
        
        loop
        {
            // Check if we're past the end of heap
            // Calculate current page: IDX high byte - HEAPSTART
            LDA ZP.IDXH
            SEC
            SBC ZP.HEAPSTART
            CMP ZP.HEAPSIZE
            if (C) { break; }  // Past end of heap
            
            // Check for zero block size (corrupted heap)
            LDY #0
            LDA [ZP.IDX], Y     // Low byte of size
            STA ZP.M0
            INY
            LDA [ZP.IDX], Y     // High byte of size
            STA ZP.M1
            ORA ZP.M0           // Check if size is zero
            if (Z) 
            { 
                LDA #'Z'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'O'
                Serial.WriteChar();
                LDA #'\n'
                Serial.WriteChar();
                break; 
            }
            
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
            
            // Print block size (from header)
            LDA ZP.M1  // High byte
            Serial.HexOut();
            LDA ZP.M0  // Low byte
            Serial.HexOut();
            
            // Check if this block is on the free list
            LDA #' '
            Serial.WriteChar();
            LDA #'('
            Serial.WriteChar();
            
            // Save current position
            LDA ZP.IDXL
            STA ZP.M2
            LDA ZP.IDXH
            STA ZP.M3
            
            // Walk free list to see if this block is free
            LDA ZP.FREELISTL
            STA ZP.IDYL
            LDA ZP.FREELISTH
            STA ZP.IDYH
            
            STZ ZP.U0  // Flag: 0 = not found, 1 = found on free list
            
            loop
            {
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z) { break; }  // End of free list
                
                // Compare addresses - current block vs free list entry
                LDA ZP.IDYL
                CMP ZP.M2
                if (NZ) 
                { 
                    // Move to next free block
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    
                    // Get next pointer
                    LDY #2
                    LDA [ZP.IDX], Y
                    STA ZP.IDYL
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDYH
                    continue; 
                }
                
                LDA ZP.IDYH
                CMP ZP.M3
                if (NZ) 
                { 
                    // Move to next free block
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    
                    // Get next pointer
                    LDY #2
                    LDA [ZP.IDX], Y
                    STA ZP.IDYL
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDYH
                    continue; 
                }
                
                // Found it - this block is free
                LDA #1
                STA ZP.U0
                break;
            }
            
            // Print status based on flag
            LDA ZP.U0
            if (NZ)
            {
                LDA #'F'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
            }
            else
            {
                LDA #'U'
                Serial.WriteChar();
                LDA #'S'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
                LDA #'D'
                Serial.WriteChar();
            }
            
            LDA #')'
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Show first 16 bytes of block content (after 2-byte header)
            
            // Restore current position for content dump
            LDA ZP.M2
            STA ZP.IDXL
            LDA ZP.M3
            STA ZP.IDXH
            
            // Skip the 2-byte header to get to content
            CLC
            LDA ZP.IDXL
            ADC #2
            STA ZP.IDXL
            if (C)
            {
                INC ZP.IDXH
            }
            
            // Calculate effective content size (block size - 2 for header)
            // Block size is in ZP.M0 (low) and ZP.M1 (high)
            SEC
            LDA ZP.M0
            SBC #2
            STA ZP.U2  // Content size low byte
            LDA ZP.M1
            SBC #0
            STA ZP.U3  // Content size high byte
            
            // Limit to max 16 bytes for display
            LDA ZP.U3
            if (NZ)  // Size > 255, so limit to 16
            {
                LDA #16
                STA ZP.U2
                STZ ZP.U3
            }
            else
            {
                LDA ZP.U2
                CMP #16
                if (C)  // Size >= 16, limit to 16
                {
                    LDA #16
                    STA ZP.U2
                }
            }
            
            // First pass: dump hex bytes with spaces
            LDY #0
            loop
            {
                CPY ZP.U2
                if (Z) { break; }
                
                LDA [ZP.IDX], Y
                Serial.HexOut();
                LDA #' '
                Serial.WriteChar();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    CPY ZP.U2  // Don't add space if we're at the end
                    if (NZ)
                    {
                        LDA #' '
                        Serial.WriteChar();
                        LDA #' '
                        Serial.WriteChar();
                    }
                }
            }
            
            // Pad hex section to align ASCII (each byte takes 3 chars: "XX ")
            // Need to reach 16*3 + 2 = 50 characters for full alignment
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA #' '
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                
                INY
                
                // Add extra space after 8 bytes for alignment
                CPY #8
                if (Z)
                {
                    LDA #' '
                    Serial.WriteChar();
                    LDA #' '
                    Serial.WriteChar();
                }
            }
            
            // Add spacing before ASCII dump
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Second pass: dump same bytes as ASCII
            LDY #0
            loop
            {
                CPY ZP.U2
                if (Z) { break; }
                
                LDA [ZP.IDX], Y
                
                // Check if printable (32-127)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        Serial.WriteChar();  // Print the character
                        INY
                        
                        // Add space after 8 characters
                        CPY #8
                        if (Z)
                        {
                            CPY ZP.U2  // Don't add space if we're at the end
                            if (NZ)
                            {
                                LDA #' '
                                Serial.WriteChar();
                            }
                        }
                        continue;
                    }
                }
                
                // Not printable, print dot
                LDA #'.'
                Serial.WriteChar();
                INY
                
                // Add space after 8 characters
                CPY #8
                if (Z)
                {
                    CPY ZP.U2  // Don't add space if we're at the end
                    if (NZ)
                    {
                        LDA #' '
                        Serial.WriteChar();
                    }
                }
            }
            
            LDA #'\n'
            Serial.WriteChar();
            
            // Restore current position
            LDA ZP.M2
            STA ZP.IDXL
            LDA ZP.M3
            STA ZP.IDXH
            
            // Move to next block: current address + block size
            CLC
            LDA ZP.IDXL
            ADC ZP.M0  // Add low byte of size
            STA ZP.IDXL
            LDA ZP.IDXH
            ADC ZP.M1  // Add high byte of size
            STA ZP.IDXH
            
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
