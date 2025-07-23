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
        
        // If we never printed anything (ACCL is still 0), the number was 0
        LDA ZP.ACCL
        if (Z)  // Never set padding, so number was 0
        {
            LDA #'0'
            Serial.WriteChar();
        }
    }
    
    // Add this to Tools.asm

    // Copy FLENGTH bytes from FSOURCEADDRESS to FDESTINATIONADDRESS
    // On entry: FSOURCEADDRESS = source pointer
    //           FDESTINATIONADDRESS = destination pointer  
    //           FLENGTH = number of bytes to copy (16-bit)
    // On exit:  All registers preserved, FSOURCEADDRESS/FDESTINATIONADDRESS/FLENGTH munted
    // Uses:     Hardware stack for register preservation
    CopyBytes()
    {
        // Save registers we need to preserve
        PHA          // Save A
        PHX          // Save X  
        PHY          // Save Y
        
        // Main copy loop
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
        
        // Restore registers
        PLY          // Restore Y from stack
        PLX          // Restore X from stack
        PLA          // Restore A from stack
    }
    

#ifdef DEBUG    

    // Debug strings
    const string debugVarsHeader = "\n== VARS ==\n";
    const string debugStackHeader = "\n== STACK ==\n";
    const string debugHeapHeader = "\n== HEAP DUMP ==\n";
    const string debugBasicHeader = "\n== BASIC BUFFERS ==\n";
    const string debugZeroBlock = "ZERO\n";
    const string debugEllipsis = "...\n";
    
    // Debug function to dump key zero page variables
    DumpVariables()
    {
       // Store registers in zero page temporarily so we can display them
       STA ZP.U0  // Temporarily store A
       STX ZP.U1  // Temporarily store X
       STY ZP.U2  // Temporarily store Y
       
       LDA #(debugVarsHeader % 256)
       STA ZP.IDXL
       LDA #(debugVarsHeader / 256)
       STA ZP.IDXH
       PrintString();
       
       // A register
       LDA #'A'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U0
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // X register
       LDA #'X'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U1
       Serial.HexOut();
       LDA #' '
       Serial.WriteChar();
       
       // Y register
       LDA #'Y'
       Serial.WriteChar();
       LDA #':'
       Serial.WriteChar();
       LDA ZP.U2
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
       LDY ZP.U2  // Restore Y
       LDX ZP.U1  // Restore X
       LDA ZP.U0  // Restore A
    }

    // Debug function to dump the value stack
    DumpStack()
    {
        PHA  // Save A
        PHX  // Save X  
        PHY  // Save Y
        
        LDA #(debugStackHeader % 256)
        STA ZP.IDXL
        LDA #(debugStackHeader / 256)
        STA ZP.IDXH
        PrintString();
        
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
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        LDA #(debugHeapHeader % 256)
        STA ZP.IDXL
        LDA #(debugHeapHeader / 256)
        STA ZP.IDXH
        PrintString();
        
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
                LDA #(debugZeroBlock % 256)
                STA ZP.IDXL
                LDA #(debugZeroBlock / 256)
                STA ZP.IDXH
                PrintString();
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
            
            CLC
            LDA ZP.IDXL
            ADC # 2
            STA ZP.M2
            LDA ZP.IDXH
            ADC # 0
            
            // Print Alloc address (block address + 2)
            Serial.HexOut();
            LDA ZP.M2
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
                LDA #(debugEllipsis % 256)
                STA ZP.IDXL
                LDA #(debugEllipsis / 256)
                STA ZP.IDXH
                PrintString();
                break; 
            }
        }
        
        LDA #'\n'
        Serial.WriteChar();
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
    }
    
    // Generic function to dump a 256-byte page in hex+ASCII format
    // On entry: A = page number (high byte of address)
    // Uses:     Serial.WriteChar(), Serial.HexOut() for output, ZP.M0/M1 for addressing
    DumpPage()
    {
        PHA  // Save A
        PHX  // Save X  
        PHY  // Save Y
        
        // Set up M0/M1 to point to the page (preserve IDX)
        STA ZP.M1   // Page number in high byte
        STZ ZP.M0   // Start at offset 0
        
        // Print 16 lines of 16 bytes each
        LDX # 0  // Line counter (0-15)
        
        loop
        {
            // Print address for this line (page:offset)
            LDA ZP.M1
            
            Serial.HexOut();
            
            TXA
            ASL ASL ASL ASL
            NOP
            Serial.HexOut();
            LDA # ':'
            Serial.WriteChar();
            LDA # ' '
            
            Serial.WriteChar();
            
            // Print 16 hex bytes with space after each
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.M0], Y
                Serial.HexOut();
                LDA #' '
                Serial.WriteChar();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    LDA #' '
                    Serial.WriteChar();
                }
            }
            
            // Add spacing before ASCII dump
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.M0], Y
                
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
                            LDA #' '
                            Serial.WriteChar();
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
                    LDA #' '
                    Serial.WriteChar();
                }
            }
            
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next line (add 16 to M0/M1)
            CLC
            LDA ZP.M0
            ADC #16
            STA ZP.M0
            if (C)
            {
                INC ZP.M1  // This shouldn't happen within a page
            }
            
            INX
            CPX #16
            if (Z) { break; }  // Done with all 16 lines
        }
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
    }

    // Debug strings for BASIC buffers
    const string basicBuffersHeader = "\n== BASIC BUFFERS ==\n";
    const string basicInputInfo = "InLen:";
    const string basicTokPosInfo = " TokPos:";
    const string basicCurTokInfo = " CurTok:";
    const string basicExprTypeInfo = " ExprType:";
    const string basicTokPtrInfo = "\nTokPtr:";
    const string basicErrorInfo = " Err:";
    const string basicInputBufferLabel = "\nInputBuffer (0x0900):\n";
    const string basicWorkBufferLabel = "\nWorkBuffer (0x0980) - First 64 bytes:\n";

    // Dump the BASIC input and work buffers
    DumpBasicBuffers()
    {
        PHA  // Save A
        
        LDA #(basicBuffersHeader % 256)
        STA ZP.IDXL
        LDA #(basicBuffersHeader / 256)
        STA ZP.IDXH
        PrintString();
        
        // Show current BASIC zero page variables (0x30-0x37)
        LDA #(basicInputInfo % 256)
        STA ZP.IDXL
        LDA #(basicInputInfo / 256)
        STA ZP.IDXH
        PrintString();
        LDA ZP.BasicInputLength  // 0x30
        Serial.HexOut();
        
        LDA #(basicTokPosInfo % 256)
        STA ZP.IDXL
        LDA #(basicTokPosInfo / 256)
        STA ZP.IDXH
        PrintString();
        LDA ZP.TokenizerPos      // 0x31
        Serial.HexOut();
        
        LDA #(basicCurTokInfo % 256)
        STA ZP.IDXL
        LDA #(basicCurTokInfo / 256)
        STA ZP.IDXH
        PrintString();
        LDA ZP.CurrentToken      // 0x32
        Serial.HexOut();
        
        // Show Error pointers if set
        LDA #(basicErrorInfo % 256)
        STA ZP.IDXL
        LDA #(basicErrorInfo / 256)
        STA ZP.IDXH
        PrintString();
        LDA ZP.LastErrorH
        Serial.HexOut();
        LDA ZP.LastErrorL
        Serial.HexOut();
        
        // BasicInputBuffer (0x0900, 128 bytes)
        LDA #(basicInputBufferLabel % 256)
        STA ZP.IDXL
        LDA #(basicInputBufferLabel / 256)
        STA ZP.IDXH
        PrintString();
        
        // Dump the entire page containing BasicInputBuffer
        LDA #(Address.BasicInputBuffer >> 8)  // Page 0x09
        DumpPage();
        
        // BasicTokenizerBuffer (0x0980, 256 bytes) - show first 64 bytes
        LDA #(basicWorkBufferLabel % 256)
        STA ZP.IDXL
        LDA #(basicWorkBufferLabel / 256)
        STA ZP.IDXH
        PrintString();
        
        // Show first 64 bytes of work buffer using DumpPage logic
        PHX
        PHY
        
        // Set up M0/M1 to point to BasicTokenizerBuffer (0x0980)
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        STA ZP.M0
        LDA #(Address.BasicTokenizerBuffer >> 8)
        STA ZP.M1
        
        // Print 4 lines of 16 bytes each (64 bytes total)
        LDX #0  // Line counter (0-3)
        
        loop
        {
            // Print address for this line
            LDA ZP.M1
            Serial.HexOut();
            TXA
            ASL
            ASL
            ASL
            ASL
            CLC
            ADC ZP.M0
            Serial.HexOut();
            LDA #':'
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print 16 hex bytes
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.M0], Y
                Serial.HexOut();
                LDA #' '
                Serial.WriteChar();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    LDA #' '
                    Serial.WriteChar();
                }
            }
            
            // Add spacing before ASCII dump
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.M0], Y
                
                // Check if printable (32-127)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        Serial.WriteChar();
                        INY
                        
                        // Add space after 8 characters
                        CPY #8
                        if (Z)
                        {
                            LDA #' '
                            Serial.WriteChar();
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
                    LDA #' '
                    Serial.WriteChar();
                }
            }
            
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next line (add 16 to M0/M1)
            CLC
            LDA ZP.M0
            ADC #16
            STA ZP.M0
            if (C)
            {
                INC ZP.M1
            }
            
            INX
            CPX #4
            if (Z) { break; }  // Done with 4 lines
        }
        
        PLY
        PLX
        
        LDA #'\n'
        Serial.WriteChar();
        
        PLA  // Restore A
    }
    
#endif

}
