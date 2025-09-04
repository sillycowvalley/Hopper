unit Debug // Debug.asm
{
    // Debug strings - main headers
    const string debugHeapHeader = "\n== HEAP DUMP ==\n";
    const string debugCrashHeader = "\n== CRASH ==\n";
    const string debugZeroPageHeader = "\n== ZERO PAGE ==\n";
    
    const string debugZeroBlock = "ZERO\n";
    const string debugEllipsis = "...\n";
    
    // Register and status labels (3+ characters only)
    const string regTOP = "TOP:";
    const string regNXT = "NEXT:";
    const string regRES = "RESULT:";
    const string regACC = "ACC:";
    const string regIDX = "IDX:";
    const string regIDY = "IDY:";
    const string regACCL = "ACCL:";
    const string regACCT = "ACCT:";
    const string regSP = "SP:";
    
    // Status indicators (3+ characters only)
    const string statusFree = "FREE";
    const string statusUsed = "USED";
    

#if defined(DEBUG)

    // === Private helper methods (no state preservation) ===
    
    // Output single space
    space()
    {
        LDA #' '
        Serial.WriteChar();
    }
    
    // Output newline
    nL()
    {
        LDA #'\n'
        Serial.WriteChar();
    }
    
    // Output hex byte (A = byte to output)
    hOut()
    {
        Serial.HexOut();
    }
    
    // Output character (A = character)
    cOut()
    {
        Serial.WriteChar();
    }
    
    // Print multiple spaces (A = count)
    spaces()
    {
        TAX  // Save count in X
        CPX #0
        if (NZ)
        {
            loop
            {
                space();
                DEX
                if (Z) { break; }
            }
        }
    }
    
    // Print string from STR
    printString()
    {
        PHY
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }
            Serial.WriteChar();
            INY
        }
        PLY
    }
    
    
    // Print null-terminated string from IDX
    printStringIDX()
    {
        LDY #0
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }
            Serial.WriteChar();
            INY
        }
    }
    
    // === Public wrapper methods (full state preservation) ===
    
    // Public convenience wrappers for compatibility
    COut() 
    { 
        PHP
        Serial.WriteChar();
        PLP
    }
    
    NL() 
    { 
        PHP PHA
        nL();
        PLA PLP
    }
    
    Space()
    {
        PHP PHA
        space();
        PLA PLP
    }
    
    // Print character if printable, otherwise print '.'
    // Input: A = character to test and print
    // Output: Character or '.' printed to serial  
    // Preserves: A
    // Munts: None
    Printable()
    {
        Char.IsPrintable();
        if (C)
        {
            COut();
        }
        else
        {
            PHA LDA #'.' COut(); PLA
        }
    }
    
    // === Private output methods (use DB slots, no preservation) ===
    
    printString2()
    {
        PHY
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.STR2], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
    }
    
    xOut()  // Output IDX register
    {
        LDA #(regIDX % 256)
        STA ZP.STR2
        LDA #(regIDX / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.IDXH
        hOut();
        LDA ZP.IDXL
        hOut();
        space();
    }
    
    
    yOut()  // Output IDY register
    {
        LDA #(regIDY % 256)
        STA ZP.STR2
        LDA #(regIDY / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.IDYH
        hOut();
        LDA ZP.IDYL
        hOut();
        space();
    }
    
    aOut()  // Output ACC register
    {
        LDA #(regACC % 256)
        STA ZP.STR2
        LDA #(regACC / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.ACCT
        BASICTypes.PrintType();
        LDA #':'
        cOut();
        LDA ZP.ACCH
        hOut();
        LDA ZP.ACCL
        hOut();
        space();
    }
    
    atOut()  // Output ACCT register
    {
        LDA #(regACCT % 256)
        STA ZP.STR2
        LDA #(regACCT / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.ACCT
        hOut();
        space();
    }
    
    nOut()  // Output NEXT register with type
    {
        LDA #(regNXT % 256)
        STA ZP.STR2
        LDA #(regNXT / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.NEXTT
        BASICTypes.PrintType();
        LDA #':'
        cOut();
        LDA ZP.NEXTH
        hOut();
        LDA ZP.NEXTL
        hOut();
        space();
    }
    
    tOut()  // Output TOP register with type
    {
        LDA #(regTOP % 256)
        STA ZP.STR2
        LDA #(regTOP / 256)
        STA ZP.STR2H
        printString2();
        cOut();
        LDA ZP.TOPH
        hOut();
        LDA ZP.TOPL
        hOut();
        space();
    }
    
    nlOut()  // Output NEXT register with type
    {
        LDA #(regNXT % 256)
        STA ZP.STR2
        LDA #(regNXT / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.NEXT3
        hOut();
        LDA ZP.NEXT2
        hOut();
        LDA ZP.NEXT1
        hOut();
        LDA ZP.NEXT0
        hOut();
        space();
    }
    rlOut()  // Output RESULT register
    {
        LDA #(regRES % 256)
        STA ZP.STR2
        LDA #(regRES / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.RESULT3
        hOut();
        LDA ZP.RESULT2
        hOut();
        LDA ZP.RESULT1
        hOut();
        LDA ZP.RESULT0
        hOut();
        space();
    }
    
    tlOut()  // Output TOP register with type
    {
        LDA #(regTOP % 256)
        STA ZP.STR2
        LDA #(regTOP / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.TOP3
        hOut();
        LDA ZP.TOP2
        hOut();
        LDA ZP.TOP1
        hOut();
        LDA ZP.TOP0
        hOut();
        space();
    }
    
   
    
    alOut()  // Output ACCL register
    {
        LDA ZP.ACCL
        STA ZP.DB15  // Save value to print
        
        LDA #(regACCL % 256)
        STA ZP.STR2
        LDA #(regACCL / 256)
        STA ZP.STR2H
        printString2();
        
        LDA ZP.DB15
        hOut();
        space();
    }
    
    cfOut()  // Output carry flag status
    {
        if (C)
        {
            LDA #'C' cOut(); LDA #' ' cOut();
        }
        else
        {
            LDA #(statusNC % 256)
            STA ZP.STR2
            LDA #(statusNC / 256)
            STA ZP.STR2H
            printString2();
        }
    }
    
    zfOut()  // Output zero flag status
    {
        if (Z)
        {
            LDA #'Z' cOut(); LDA #' ' cOut();
        }
        else
        {
            LDA #(statusNZ % 256)
            STA ZP.STR2
            LDA #(statusNZ / 256)
            STA ZP.STR2H
            printString2();
        }
    }
    
    // === Public output methods (preserve state) ===
      
    XOut()
    {
        PHP PHA PHY
        xOut();
        PLY PLA PLP
    }
    
    
    YOut()
    {
        PHP PHA PHY
        yOut();
        PLY PLA PLP
    }
    
    AOut()
    {
        PHP PHA PHY
        aOut();
        PLY PLA PLP
    }
    
    ATOut()
    {
        PHP PHA PHY
        atOut();
        PLY PLA PLP
    }
    
    NOut()
    {
        PHP PHA PHY
        nOut();
        PLY PLA PLP
    }
    
    TOut()
    {
        PHP PHA PHY
        tOut();
        PLY PLA PLP
    }
    NLOut()
    {
        PHP PHA PHY
        nlOut();
        PLY PLA PLP
    }
    RLOut()
    {
        PHP PHA PHY
        rlOut();
        PLY PLA PLP
    }
    
    TLOut()
    {
        PHP PHA PHY
        tlOut();
        PLY PLA PLP
    }
    
    ALOut()
    {
        PHP PHA PHY
        alOut();
        PLY PLA PLP
    }
    
    CFOut()
    {
        PHP PHA PHY
        cfOut();
        PLY PLA PLP
    }
    
    ZFOut()
    {
        PHP PHA PHY
        zfOut();
        PLY PLA PLP
    }
    
    HOut()
    {
        PHP
        hOut();
        PLP
    }
    
   
    
    // === Private complex methods (use DB slots freely) ===
    
    dumpBlockAddress()
    {
        nL();
        LDA #5
        TAX
        CPX #0
        if (NZ)
        {
            loop
            {
                space();
                DEX
                if (Z) { break; }
            }
        }
        CLC
        TYA
        ADC ZP.DB0  // Using DB0/DB1 as block pointer
        PHA
        LDA #0
        ADC ZP.DB1
        hOut();
        PLA
        hOut();
        LDA #':' cOut();
        LDA #11
        TAX
        CPX #0
        if (NZ)
        {
            loop
            {
                space();
                DEX
                if (Z) { break; }
            }
        }
        STZ ZP.DB5  // Reset bytes on this row
    }
    
    dumpBlockAscii()
    {
        PHX
        PHY  // Save Y since caller needs it
        
        // For partial rows, we need to align to column 61 (where ASCII starts)
        // Full row: 5 (indent) + 4 (addr) + 1 (:) + 48 (16 hex bytes with spaces) + 1 (extra space at col 8) + 2 = 61
        // Current position after N bytes: 5 + 4 + 1 + (N*3) + (1 if N>8) + 2
        
        LDX ZP.DB5  // Number of chars on this row
        
        // Skip if no chars to print
        CPX #0
        if (Z) 
        { 
            PLY
            PLX
            RTS
        }
        
        // Calculate current position after hex output
        // Base: 5 + 4 + 1 + 2 = 12
        LDA #12
        STA ZP.DB13
        
        // Add 3 for each byte printed
        TXA
        ASL         // x2
        CLC
        ADC ZP.DB5  // x3
        CLC
        ADC ZP.DB13
        STA ZP.DB13
        
        // Add 1 if we printed more than 8 bytes (for column separator)
        CPX #9
        if (C)
        {
            INC ZP.DB13
        }
        
        // Now pad to column 61
        SEC
        LDA #61
        SBC ZP.DB13
        TAX
        
        // Print padding spaces
        loop
        {
            CPX #0
            if (Z) { break; }
            space();
            DEX
        }
        
        // Add the standard 2-space gap before ASCII
        space();
        space();
        
        // Print the ASCII characters
        LDY ZP.DB4  // Start of row position
        LDX ZP.DB5  // Number of chars to print
        loop
        {
            CPX #0
            if (Z) { break; }
            
            LDA [ZP.DB0], Y
            Printable();
            INY
            DEX
        }
        
        PLY  // Restore Y
        PLX
    }
    
    // DB0/DB1 - points to content
    // DB2 - size LSB
    // DB3 - size MSB
    dumpBlockContent()
    {
        STZ ZP.DB4  // Start of current row
        STZ ZP.DB5  // Bytes printed on this row
        LDY #0      // Current position
        LDX #255    // Max bytes to output
        
        loop
        {
            // Check if we've hit the 255 byte limit
            CPX #0
            if (Z) 
            { 
                // Hit 255 byte limit, check if partial row
                LDA ZP.DB5
                if (NZ)  // Bytes on current row
                {
                    dumpBlockAscii();
                }
                break;
            }
            
            // Check if we've printed all content bytes
            LDA ZP.DB2
            ORA ZP.DB3
            if (Z) 
            { 
                // No more content, dump any partial row
                LDA ZP.DB5
                if (NZ)  // Bytes on current row
                {
                    dumpBlockAscii();
                }
                break; 
            }
            
            // Check if starting a new row (not first byte and Y is multiple of 16)
            TYA
            if (NZ)  // Not first byte
            {
                AND #0x0F
                if (Z)  // Starting new row
                {
                    // First dump ASCII for previous row
                    dumpBlockAscii();
                    // Then start new row
                    dumpBlockAddress();
                    STY ZP.DB4  // Save row start
                    STZ ZP.DB5  // Reset byte count
                }
            }
            
            // Add column spacing
            TYA
            AND #0x07
            if (Z)
            {
                space();  // Column space every 8 bytes
            }
            
            // Print hex byte
            space();
            LDA [ZP.DB0], Y
            hOut();
            INC ZP.DB5  // Count bytes on this row
            
            // Check if Y is about to wrap before incrementing
            INY
            if (Z)  // Y just wrapped from 255 to 0
            {
                // We've hit the Y register limit, finish up
                LDA ZP.DB5
                if (NZ)  // Bytes on current row
                {
                    dumpBlockAscii();
                }
                break;  // Exit before using wrapped Y value
            }
            
            DEX
            
            // Decrement content size
            LDA ZP.DB2
            if (Z)
            {
                DEC ZP.DB3  // MSB
            }
            DEC ZP.DB2  // LSB
        }
    }
    
    // Internal heap dump using only DB slots
    dumpHeap()
    {
        LDA #(debugHeapHeader % 256)
        STA ZP.STR
        LDA #(debugHeapHeader / 256)
        STA ZP.STRH
        printString();
        
        // Start at heap beginning
        LDA ZP.HEAPSTART
        STA ZP.DB1
        LDA #0
        STA ZP.DB0
        
        LDX #0  // Block counter
        STX ZP.DB12  // Use DB12 for block counter since X wraps at 255
        
        loop
        {
            // Check if past end of heap
            LDA ZP.DB1
            SEC
            SBC ZP.HEAPSTART
            CMP ZP.HEAPSIZE
            if (C) { break; }
            
            // Get block size
            LDY #0
            LDA [ZP.DB0], Y
            STA ZP.DB2  // Size low
            STA ZP.DB14 // Save original size low
            INY
            LDA [ZP.DB0], Y
            STA ZP.DB3  // Size high
            STA ZP.DB15 // Save original size high
            ORA ZP.DB2
            if (Z)
            {
                LDA #(debugZeroBlock % 256)
                STA ZP.STR
                LDA #(debugZeroBlock / 256)
                STA ZP.STRH
                printString();
                break;
            }
            
            // Print block number
            LDA #'[' cOut();
            LDA ZP.DB12  // Use DB12 instead of X
            hOut();
            LDA #']' cOut();
            space();
            
            // Print Alloc address (block + 2)
            CLC
            LDA ZP.DB0
            ADC #2
            PHA
            LDA ZP.DB1
            ADC #0
            hOut();
            PLA
            hOut();
            LDA #':' cOut();
            
            // Print block size
            LDA ZP.DB3
            hOut();
            LDA ZP.DB2
            hOut();
            
            // Check if on free list
            space();
            LDA #'(' cOut();
            
            // Save current block address
            LDA ZP.DB0
            STA ZP.DB10
            LDA ZP.DB1
            STA ZP.DB11
            
            // Walk free list
            LDA ZP.FREELISTL
            STA ZP.DB6
            LDA ZP.FREELISTH
            STA ZP.DB7
            
            STZ ZP.DB9  // Free flag
            
            loop
            {
                LDA ZP.DB6
                ORA ZP.DB7
                if (Z) { break; }  // End of free list
                
                // Compare addresses
                LDA ZP.DB10
                CMP ZP.DB6
                if (Z)
                {
                    LDA ZP.DB11
                    CMP ZP.DB7
                    if (Z)
                    {
                        // Found on free list
                        LDA #1
                        STA ZP.DB9
                        break;
                    }
                }
                
                // Get next free block
                LDY #2
                LDA [ZP.DB6], Y
                PHA
                INY
                LDA [ZP.DB6], Y
                STA ZP.DB7
                PLA
                STA ZP.DB6
            }
            
            // Restore block address
            LDA ZP.DB10
            STA ZP.DB0
            LDA ZP.DB11
            STA ZP.DB1
            
            // Print status
            LDA ZP.DB9
            if (NZ)
            {
                LDA #(statusFree % 256)
                STA ZP.STR
                LDA #(statusFree / 256)
                STA ZP.STRH
                printString();
            }
            else
            {
                LDA #(statusUsed % 256)
                STA ZP.STR
                LDA #(statusUsed / 256)
                STA ZP.STRH
                printString();
            }
            
            LDA #')' cOut();
            
            LDA ZP.DB9
            if (Z)  // USED
            {
                // Skip header to get to content
                CLC
                LDA ZP.DB0
                ADC #2
                STA ZP.DB0
                if (C)
                {
                    INC ZP.DB1
                }
                
                // Calculate content size (block size - 2)
                SEC
                LDA ZP.DB14  // Use saved original size
                SBC #2
                STA ZP.DB2
                LDA ZP.DB15
                SBC #0
                STA ZP.DB3
                
                dumpBlockContent();
                
                // Restore header position
                SEC
                LDA ZP.DB0
                SBC #2
                STA ZP.DB0
                if (NC)
                {
                    DEC ZP.DB1
                }
            }
            
            nL();
            
            // Move to next block (using saved original size)
            CLC
            LDA ZP.DB0
            ADC ZP.DB14  // Use saved size low
            STA ZP.DB0
            LDA ZP.DB1
            ADC ZP.DB15  // Use saved size high
            STA ZP.DB1
            
            INC ZP.DB12  // Increment block counter
            LDA ZP.DB12
            CMP #20
            if (Z)
            {
                LDA #(debugEllipsis % 256)
                STA ZP.STR
                LDA #(debugEllipsis / 256)
                STA ZP.STRH
                printString();
                break;
            }
        }
        
        nL();
    }
    
    dumpPage()  // A = page number
    {
        // Set up DB0/DB1 to point to the page
        STA ZP.DB1
        STZ ZP.DB0
        
        // Print 16 lines of 16 bytes each
        LDX #0  // Line counter
        
        loop
        {
            // Print address
            LDA ZP.DB1
            hOut();
            TXA
            ASL A ASL A ASL A ASL A
            hOut();
            LDA #':' cOut();
            space();
            
            // Print 16 hex bytes
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                hOut();
                space();
                
                INY
                
                // Extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    space();
                }
            }
            
            // Spacing before ASCII
            space();
            space();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                Printable();
                INY
            }
            
            nL();
            
            // Next line
            CLC
            LDA ZP.DB0
            ADC #16
            STA ZP.DB0
            if (C)
            {
                INC ZP.DB1
            }
            
            INX
            CPX #16
            if (Z) { break; }
        }
    }
    
    
    
    // Find the last row containing non-zero data in a memory block
    // Input: X = max number of rows (1-16)
    //        ZP.DB1:ZP.DB0 = pointer to start of block
    // Output: X = last row containing non-zero data (0-based)
    //         or 0xFF if all rows are zero
    // Preserves: ZP.DB0, ZP.DB1
    // Munts: A, Y
    findLastNonZeroRow()
    {
        DEX                      // Convert to 0-based index (0-15)
        loop
        {
            // Check if we've gone below row 0
            CPX #0xFF            // Check for underflow
            if (Z)               // All rows were zero
            {
                INX // zero rows
                break;
            }
            
            // Calculate starting offset for row X
            TXA                      // Row number to A
            ASL A ASL A ASL A ASL A  // Multiply by 16
            TAY                      // Y = starting offset for this row
            
            // Save row number on stack
            PHX
            
            // Check 16 bytes in this row
            LDX # 16
            loop
            {
                LDA [ZP.DB0], Y  // Get byte at offset Y
                if (NZ)          // Found non-zero byte
                {
                    PLX          // Restore row number
                    INX          // 1.. 16 (convert to 1-based row count (1-16)
                    return;
                }
                INY              // Next byte in row
                DEX              // Decrement byte counter
                if (Z)           // Checked all 16 bytes
                {
                    INX // 1.. 16 (convert to 1-based row count (1-16)
                    break;
                }
            }
            
            // This row was all zeros, try previous row
            PLX                  // Restore row number
            DEX                  // Previous row
        }
        // X contains 0 if all rows were zero
    }
    
    dumpMemoryBlock()  // address: DB1 = MSB, DB0 = LSB, X = maximum number of rows
    {
        findLastNonZeroRow();
        CPX #0
        if (Z) { INX } // at least one row
        
        LDA #(firstNRowsPrefix % 256)
        STA ZP.STR
        LDA #(firstNRowsPrefix / 256)
        STA ZP.STRH
        printString();
        
        TXA hOut();
        
        LDA #(firstNRowsSuffix % 256)
        STA ZP.STR
        LDA #(firstNRowsSuffix / 256)
        STA ZP.STRH
        printString();
        dumpMemoryBlockX();
    }
    
    dumpMemoryBlockX() // address: DB1 = MSB, DB0 = LSB, X = number of rows
    {    
        // Print X rows of 16 bytes each (256 bytes total)
        INX
        
        loop
        {
            DEX
            if (Z) { break; }
            
            // Print address
            LDA ZP.DB1
            hOut();
            LDA ZP.DB0
            hOut();
            LDA #':' cOut();
            space();
            
            // Print 16 hex bytes
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                hOut();
                space();
                
                INY
                
                // Extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    space();
                }
            }
            
            // Spacing before ASCII
            space();
            space();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                Printable();
                INY
            }
            
            nL();
            
            // Next row
            CLC
            LDA ZP.DB0
            ADC #16
            STA ZP.DB0
            if (C)
            {
                INC ZP.DB1
            }
        }
    }
    
    dumpMemoryBlockL() // address: DB1 = MSB, DB0 = LSB, length: FLENGTHH = MSB, FLENGTHL = LSB
    {
        Debug.NL();
        
        loop
        {
            // Check if we're done (FLENGTH == 0)
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Set Z - no more bytes to dump
            
            // Print address
            LDA ZP.DB1
            hOut();
            LDA ZP.DB0
            hOut();
            LDA #':' cOut();
            space();
            
            // Calculate bytes for this row (min(16, FLENGTH))
            LDA ZP.FLENGTHL
            STA ZP.DB4       // Save bytes for this row
            LDA ZP.FLENGTHH
            if (NZ)           // Set NZ - more than 255 bytes left
            {
                LDA #16       // Full row
                STA ZP.DB4 
            }
            else
            {
                LDA ZP.FLENGTHL
                CMP #17
                if (C)        // Set C - 17 or more bytes left
                {
                    LDA #16   // Full row
                    STA ZP.DB4 
                }
                // else TEMP0 already has the remaining count
            }
            
            // Print hex bytes
            LDY #0
            loop
            {
                CPY ZP.DB4 
                if (Z) { break; }  // Set Z - printed all bytes for this row
                
                LDA [ZP.DB0], Y
                hOut();
                space();
                
                INY
                
                // Extra space after 8 bytes (if we have that many)
                CPY #8
                if (Z)            // Set Z - at byte 8
                {
                    space();
                }
            }
            
            // Pad with spaces if less than 16 bytes
            loop
            {
                CPY #16
                if (Z) { break; }  // Set Z - reached 16
                
                // 3 spaces per missing byte (2 hex chars + 1 space)
                space();
                space();
                space();
                
                INY
                
                // Extra space after position 8
                CPY #8
                if (Z)            // Set Z - at position 8
                {
                    space();
                }
            }
            
            // Spacing before ASCII
            space();
            space();
            
            // Print ASCII characters
            LDY #0
            loop
            {
                CPY ZP.DB4 
                if (Z) { break; }  // Set Z - printed all ASCII for this row
                
                LDA [ZP.DB0], Y
                Printable();
                INY
            }
            
            nL();
            
            // Update address pointer
            CLC
            LDA ZP.DB0
            ADC ZP.DB4       // Add actual bytes printed
            STA ZP.DB0
            if (C)            // Set C - carry from addition
            {
                INC ZP.DB1
            }
            
            // Decrement FLENGTH by bytes printed
            SEC
            LDA ZP.FLENGTHL
            SBC ZP.DB4 
            STA ZP.FLENGTHL
            if (NC)           // Set NC - borrow occurred
            {
                DEC ZP.FLENGTHH
            }
        }
    }
    
    
    
    dumpZeroPage()
    {
        LDA #(debugZeroPageHeader % 256)
        STA ZP.STR
        LDA #(debugZeroPageHeader / 256)
        STA ZP.STRH
        printString();
        nL();
        LDA #0
        dumpPage();
    }
    
    
    DumpHeap()
    {
        PHP PHA PHX PHY
        
        // Save state that dumpHeap uses
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        dumpHeap();
        
        // Restore state
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY PLX PLA PLP
    }
    
    DumpPage()
    {
        PHA PHX PHY
        dumpPage();
        PLY PLX PLA
    }
    
    
    
    // Print single hex nibble (low 4 bits of A)
    hNibbleOut()
    {
        PHA
        AND #0x0F
        CMP #0x0A
        if (C)
        {
            ADC #6  // add 7 (6+C=1)
        }
        ADC #'0'
        cOut();
        PLA
    }
    
    
    DumpZeroPage()
    {
        PHP PHA PHX PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        dumpZeroPage();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY PLX PLA PLP
    }
    
#endif // DEBUG

    
    
    // Walk heap blocks for validation
    // only called from TRACE in normal circumstances (so removed from production - no need for #ifdef DEBUG)
    ValidateHeap()
    {
        PHA PHX PHY

        // Save state
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        loop // Single exit pattern
        {
            // First validate basic heap structure
            validateHeapBlocks();
            if (NC)
            {
                // corrupt heap blocks
                NL();
                LDA #'H' COut();
                LDA #'B' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            break;
        }
        
        if (NC)
        {
            Debug.DumpHeap();
            loop { } // hang rather than crashing
        }
        
        // Restore state
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY PLX PLA
    }

    // Validate basic heap block structure
    validateHeapBlocks()
    {
        // Start at heap beginning
        LDA ZP.HEAPSTART
        STA ZP.IDXH
        LDA #0
        STA ZP.IDXL
        
        LDX #0  // Block counter
        
        loop
        {
            // Check if past end of heap
            LDA ZP.IDXH
            SEC
            SBC ZP.HEAPSTART
            CMP ZP.HEAPSIZE
            if (C) 
            { 
                SEC // Success - reached end normally
                break; 
            }
            
            // Check for zero block size
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.DB0
            INY
            LDA [ZP.IDX], Y
            STA ZP.DB1
            ORA ZP.DB0
            if (Z)
            {
                Error.HeapCorruptError();
                CLC // Failed
                break;
            }
            
            // Move to next block
            CLC
            LDA ZP.IDXL
            ADC ZP.DB0
            STA ZP.IDXL
            LDA ZP.IDXH
            ADC ZP.DB1
            STA ZP.IDXH
            
            INX
            CPX #0xFF
            if (Z)
            {
                // Too many blocks
                Error.HeapCorruptError();
                CLC
                break;
            }
        }
    }

    // Check if address is a valid heap object
    // Input: ZP.IDY = address to check
    // Output: C set if valid heap object or null, NC if invalid
    isValidHeapPointer()
    {
        loop // Single exit pattern
        {
            
            // Check for null pointer (always valid)
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                SEC // Null is valid
                break;
            }
            
            // Check if address is in heap range
            LDA ZP.IDYH
            CMP ZP.HEAPSTART
            if (NC) // < HEAPSTART?
            {
                CLC // Not in heap range
                break;
            }
            
            // Check if < heap end
            SEC
            SBC ZP.HEAPSTART
            CMP ZP.HEAPSIZE
            if (C)
            {
                CLC // Past end of heap
                break;
            }
            
            // Address is in heap range - check if it's a valid allocation
            // Valid allocations are 2 bytes after block start (skip size field)
            SEC
            LDA ZP.IDYL
            SBC #2
            STA ZP.DB2
            LDA ZP.IDYH
            SBC #0
            STA ZP.DB3
            
            // Check if DB2/DB3 points to valid size field
            LDY #0
            LDA [ZP.DB2], Y
            STA ZP.DB4
            INY
            LDA [ZP.DB2], Y
            STA ZP.DB5
            
            // Size must be non-zero and reasonable
            LDA ZP.DB4
            ORA ZP.DB5
            if (Z)
            {
                CLC // Zero size = invalid
                break;
            }
            
            // Size must be < heap size
            LDA ZP.DB5
            CMP ZP.HEAPSIZE
            if (C)
            {
                CLC // Size too large
                break;
            }
            
            SEC // Valid heap pointer
            break;
        }
    }
 
    // Crash handler
    Crash()
    {
        Serial.HexOut();
        
        TSX PHX
        PHA
        
        Print.NewLine();
        
        LDA #(debugCrashHeader % 256)
        STA ZP.STR
        LDA #(debugCrashHeader / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        Serial.HexOut();
        Print.Space();
        LDA #(regSP % 256)
        STA ZP.STR
        LDA #(regSP / 256)
        STA ZP.STRH
        Print.String();
        PLA
        Serial.HexOut();
        
#if defined(DEBUG)
        Print.NewLine();
        DumpZeroPage();
        DumpHeap();
#endif
        loop {}
    }
}
