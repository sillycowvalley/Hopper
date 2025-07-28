unit Tools
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
 
    uses "BasicTypes"

#ifdef DEBUG       
    // Print BasicType enum value as readable string
    // Input: A = BasicType enum value
    // Output: Type name printed to serial
    // Preserves: Everything
    PrintType()
    {
        PHP  // Save flags
        PHA
        PHX
        PHY
        
        switch (A)
        {
            case BasicType.INT:     // 0x02
            {
                LDA #'I'
                Serial.WriteChar();
                LDA #'N'
                Serial.WriteChar();
                LDA #'T'
                Serial.WriteChar();
            }
            case BasicType.BYTE:    // 0x03
            {
                LDA #'B'
                Serial.WriteChar();
                LDA #'Y'
                Serial.WriteChar();
                LDA #'T'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
            }
            case BasicType.WORD:    // 0x04
            {
                LDA #'W'
                Serial.WriteChar();
                LDA #'O'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'D'
                Serial.WriteChar();
            }
            case BasicType.BIT:     // 0x06
            {
                LDA #'B'
                Serial.WriteChar();
                LDA #'I'
                Serial.WriteChar();
                LDA #'T'
                Serial.WriteChar();
            }
            case BasicType.STRING:  // 0x0F
            {
                LDA #'S'
                Serial.WriteChar();
                LDA #'T'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'I'
                Serial.WriteChar();
                LDA #'N'
                Serial.WriteChar();
                LDA #'G'
                Serial.WriteChar();
            }
            case BasicType.ARRAY:   // 0x12
            {
                LDA #'A'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'R'
                Serial.WriteChar();
                LDA #'A'
                Serial.WriteChar();
                LDA #'Y'
                Serial.WriteChar();
            }
            default:
            {
                PHA
                // Unknown type - print as hex
                LDA #'?'
                Serial.WriteChar();
                PLA
                Serial.HexOut();
            }
        }
        
        PLY
        PLX
        PLA
        PLP  // Restore flags
    }
#endif
    
    // Print null-terminated string to serial output
    // Input: ZP.IDX = pointer to null-terminated string
    // Output: String printed to serial
    printStringIDX()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.IDX], Y // Load character from string
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
    
    // Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    {
        PHP  // Push processor status (including carry flag)
        LDA #'\n' 
        Serial.WriteChar();
        PLP  // Pull processor status (restore carry flag)
    }
    
    
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOP = 16-bit number to print (0-65535)
    //        ZP.TOPT = type (for signed/unsigned determination)
    // Output: Decimal number printed to serial
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
        }
        
        TYA  // Length in A
        
        // Restore caller's ZP.TOP
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        PLY
        PLX
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    StringCompare()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
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

#ifdef DEBUG    

    // Debug strings
    const string debugVarsHeader = "\n== VARS ==\n";
    const string debugStackHeader = "\n== STACK ==\n";
    const string debugHeapHeader = "\n== HEAP DUMP ==\n";
    const string debugBasicHeader = "\n== BASIC BUFFERS ==\n";
    const string debugZeroBlock = "ZERO\n";
    const string debugEllipsis = "...\n";
    
    // Dump key zero page variables for debugging
    // Input: None
    // Output: Variables printed to serial
    // Preserves: Everything
    DumpVariables()
    {
       PHP  // Save flags
       
       // Store registers in zero page temporarily so we can display them
       STA ZP.U0  // Temporarily store A
       STX ZP.U1  // Temporarily store X
       STY ZP.U2  // Temporarily store Y
       
       LDA ZP.IDXL
       STA ZP.U3
       LDA ZP.IDXH
       STA ZP.U4
       
       LDA #(debugVarsHeader % 256)
       STA ZP.IDXL
       LDA #(debugVarsHeader / 256)
       STA ZP.IDXH
       printStringIDX();
       
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
       LDA ZP.TOPT
       PrintType();
       LDA #'-'
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
       LDA ZP.NEXTT
       PrintType();
       LDA #'-'
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
       LDA ZP.U4
       Serial.HexOut();
       LDA ZP.U3
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
       
       LDA ZP.U4
       STA ZP.IDXH
       LDA ZP.U3
       STA ZP.IDXL
       
       // Restore registers
       LDY ZP.U2  // Restore Y
       LDX ZP.U1  // Restore X
       LDA ZP.U0  // Restore A
       
       PLP  // Restore flags
    }

    // Dump the value stack for debugging
    // Input: None
    // Output: Stack contents printed to serial
    // Preserves: Everything
    DumpStack()
    {
        PHP  // Save flags
        PHA
        PHX
        PHY
        
        LDA #(debugStackHeader % 256)
        STA ZP.IDXL
        LDA #(debugStackHeader / 256)
        STA ZP.IDXH
        printStringIDX();
        
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
            LDA #'\n'
            Serial.WriteChar();
            
            DEY
        }
        
        LDA #'\n'
        Serial.WriteChar();
        
        PLY
        PLX
        PLA
        PLP  // Restore flags
    }
    
    // Lightweight heap summary for use during iteration
    // Input: None
    // Output: List head pointers printed to serial
    // Preserves: Everything
    DumpHeapSummary()
    {
        PHP  // Save flags
        PHA
        
        // Just show the list heads
        LDA #'V'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.VariablesListH
        Serial.HexOut();
        LDA ZP.VariablesListL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        LDA #'F'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.FunctionsListH
        Serial.HexOut();
        LDA ZP.FunctionsListL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA
        PLP  // Restore flags
    }

    // Debug output for iteration state
    // Input: None
    // Output: Current IDX pointer printed to serial
    // Preserves: Everything
    DumpIterationState()
    {
        PHP  // Save flags
        PHA
        
        LDA #'I'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.IDXH
        Serial.HexOut();
        LDA ZP.IDXL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA
        PLP  // Restore flags
    }
    
    // Dump heap with state preservation for debugging
    // Input: None
    // Output: Heap contents printed to serial
    // Modifies: ZP.M* scratch space (internal to heap analysis operations)
    DumpHeap()
    {
        PHP  // Save flags
        PHA
        PHX
        PHY
        
        // Save iteration-critical state
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Save any other state that might be important
        LDA ZP.LCURRENTL
        PHA
        LDA ZP.LCURRENTH
        PHA
        
        // Call the internal dumpHeap
        dumpHeap();
        
        // Restore saved state
        PLA
        STA ZP.LCURRENTH
        PLA
        STA ZP.LCURRENTL
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
        PLP  // Restore flags
    }
    
    // Internal heap dump implementation
    // Input: None
    // Output: Heap contents printed to serial
    // Modifies: ZP.M0-M3, ZP.U0, ZP.U2, ZP.U3, ZP.IDX, ZP.IDY, A, X, Y (internal operations)
    dumpHeap()
    {
        PHA
        PHX
        PHY
        
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
        printStringIDX();
        
        LDA #'V'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.VariablesListH
        Serial.HexOut();
        LDA ZP.VariablesListL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        LDA #'F'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.FunctionsListH
        Serial.HexOut();
        LDA ZP.FunctionsListL
        Serial.HexOut();
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
                LDA #(debugZeroBlock % 256)
                STA ZP.IDXL
                LDA #(debugZeroBlock / 256)
                STA ZP.IDXH
                printStringIDX();
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
            CPX #20  // Limit to 20 blocks to avoid infinite loops
            if (Z) 
            { 
                LDA #(debugEllipsis % 256)
                STA ZP.IDXL
                LDA #(debugEllipsis / 256)
                STA ZP.IDXH
                printStringIDX();
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
        
        PLY
        PLX
        PLA
    }
    
    // Dump a 256-byte page in hex+ASCII format for debugging
    // Input: A = page number (high byte of address)
    // Output: Page contents printed to serial
    // Modifies: ZP.M0, ZP.M1 (internal operations)
    DumpPage()
    {
        PHA
        PHX
        PHY
        
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
        
        PLY
        PLX
        PLA
    }

    // Debug strings for BASIC buffers
    const string basicBuffersHeader = "\n== BASIC BUFFERS ==\n";
    const string basicInputInfo = "InBufLen:";
    const string basicTokPosInfo = " TokPos:";
    const string basicTokLenInfo = " TokBufLen:";
    const string basicCurTokInfo = " CurTok:";
    const string basicErrorInfo = " Err:";
    const string basicCurrentCharInfo = " Char:";
    const string basicInputBufferLabel = "\nInputBuffer - First 64 bytes:\n";
    const string basicTokenizerBufferLabel = "\nTokenizerBuffer - First 64 bytes:\n";
    const string basicTokenStringLabel = "\nToken string: '";

    // Dump the BASIC input and tokenizer buffers for debugging
    // Input: None
    // Output: Buffer contents printed to serial
    // Modifies: ZP.M0, ZP.M1, ZP.IDX, A, X, Y (internal operations)
    DumpBasicBuffers()
    {
        PHP  // Save flags
        PHA
        
        LDA #(basicBuffersHeader % 256)
        STA ZP.IDXL
        LDA #(basicBuffersHeader / 256)
        STA ZP.IDXH
        printStringIDX();
        
        // Show current BASIC zero page variables (0x30-0x37)
        LDA #(basicInputInfo % 256)
        STA ZP.IDXL
        LDA #(basicInputInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDA ZP.BasicInputLength  // 0x30
        Serial.HexOut();
        
        LDA #(basicTokPosInfo % 256)
        STA ZP.IDXL
        LDA #(basicTokPosInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDA ZP.TokenizerPosH     // 0x34
        Serial.HexOut();
        LDA ZP.TokenizerPosL     // 0x33
        Serial.HexOut();
        
        LDA #(basicTokLenInfo % 256)
        STA ZP.IDXL
        LDA #(basicTokLenInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDA ZP.TokenBufferLengthH
        Serial.HexOut();
        LDA ZP.TokenBufferLengthL
        Serial.HexOut();
        
        
        LDA #(basicCurTokInfo % 256)
        STA ZP.IDXL
        LDA #(basicCurTokInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDA ZP.CurrentToken      // 0x37
        Serial.HexOut();
        
        // Show Error pointers if set
        LDA #(basicErrorInfo % 256)
        STA ZP.IDXL
        LDA #(basicErrorInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDA ZP.LastErrorH
        Serial.HexOut();
        LDA ZP.LastErrorL
        Serial.HexOut();
        
        // Show current character at tokenizer position in INPUT buffer
        LDA #(basicCurrentCharInfo % 256)
        STA ZP.IDXL
        LDA #(basicCurrentCharInfo / 256)
        STA ZP.IDXH
        printStringIDX();
        LDX ZP.TokenizerPosL     // Use low byte only for input buffer index
        CPX ZP.BasicInputLength
        if (C)  // TokPos < InputLength
        {
            LDA Address.BasicInputBuffer, X
            Serial.WriteChar();
            LDA Address.BasicInputBuffer, X
            Serial.WriteChar(); // writes [xx] if not printable
        }
        else
        {
            LDA #'E'
            Serial.WriteChar();
            LDA #'O'
            Serial.WriteChar();
            LDA #'L'
            Serial.WriteChar();
        }
        
        // Show the current token string from TokenizerBuffer
        LDA #(basicTokenStringLabel % 256)
        STA ZP.IDXL
        LDA #(basicTokenStringLabel / 256)
        STA ZP.IDXH
        printStringIDX();
        
        // Print token string from TokenizerBuffer until null terminator
        LDX #0
        loop
        {
            LDA Address.BasicTokenizerBuffer, X
            if (Z) { break; }  // Hit null terminator
            
            // Check if printable
            CMP #32
            if (C)
            {
                CMP #127
                if (NC)
                {
                    Serial.WriteChar();
                }
                else
                {
                    LDA #'?'
                    Serial.WriteChar();
                }
            }
            else
            {
                LDA #'?'
                Serial.WriteChar();
            }
            
            INX
            CPX #64  // Safety limit
            if (Z) { break; }
        }
        LDA #'\''
        Serial.WriteChar();
        
        // BasicInputBuffer - First 64 bytes using constants
        LDA #(basicInputBufferLabel % 256)
        STA ZP.IDXL
        LDA #(basicInputBufferLabel / 256)
        STA ZP.IDXH
        printStringIDX();
        
        PHX
        PHY
        
        // Set up M0/M1 to point to BasicInputBuffer using constants
        LDA #(Address.BasicInputBuffer & 0xFF)
        STA ZP.M0
        LDA #(Address.BasicInputBuffer >> 8)
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
        
        // BasicTokenizerBuffer - show first 64 bytes using constants
        LDA #(basicTokenizerBufferLabel % 256)
        STA ZP.IDXL
        LDA #(basicTokenizerBufferLabel / 256)
        STA ZP.IDXH
        printStringIDX();
        
        PHX
        PHY
        
        // Set up M0/M1 to point to BasicTokenizerBuffer using constants
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
        
        PLA
        PLP  // Restore flags
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
    
    // Output hex byte preserving carry flag  
    // Input: A = byte to output as hex
    // Output: Hex byte printed to serial
    // Preserves: Everything
    HOut()
    {
        PHP  // Push processor status (including carry flag)
        Serial.HexOut();
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output IDX register as "IDX:hhll "
    // Input: None (uses ZP.IDX)
    // Output: IDX value printed to serial
    // Preserves: Everything
    XOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
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
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output IDY register as "IDY:hhll "
    // Input: None (uses ZP.IDY)
    // Output: IDY value printed to serial
    // Preserves: Everything
    YOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
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
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ZP.SymbolIteratorFilter as "I:ll "
    // Input: None (uses ZP.SymbolIteratorFilter)
    // Output: ZP.SymbolIteratorFilter value printed to serial
    // Preserves: Everything
    IOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA #'I'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.SymbolIteratorFilter
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ACC register as "ACC:hhll "
    // Input: None (uses ZP.ACC)
    // Output: ACC value printed to serial
    // Preserves: Everything
    AOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
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
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output NEXT register as "NEXT:type-hhll "
    // Input: None (uses ZP.NEXT)
    // Output: NEXT value printed to serial
    // Preserves: Everything
    NOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA #'N'
        Serial.WriteChar();
        LDA #'E'
        Serial.WriteChar();
        LDA #'X'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.NEXTT
        PrintType();
        LDA #'-'
        Serial.WriteChar();
        LDA ZP.NEXTH
        Serial.HexOut();
        LDA ZP.NEXTL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output TOP register as "TOP:type-hhll "
    // Input: None (uses ZP.TOP)
    // Output: TOP value printed to serial
    // Preserves: Everything
    TOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA #'T'
        Serial.WriteChar();
        LDA #'O'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.TOPT
        PrintType();
        LDA #'-'
        Serial.WriteChar();
        LDA ZP.TOPH
        Serial.HexOut();
        LDA ZP.TOPL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ACCL register as "ACCL:ll "
    // Input: None (uses ZP.ACCL)
    // Output: ACCL value printed to serial
    // Preserves: Everything
    ALOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA #'A'
        Serial.WriteChar();
        LDA #'C'
        Serial.WriteChar();
        LDA #'C'
        Serial.WriteChar();
        LDA #'L'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA ZP.ACCL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
#endif

}
