unit Debug // Debug.asm
{
    // API Status: Clean  
    // All public methods preserve caller state except for documented outputs
    // Debug-only functionality with conditional compilation guards
    // Optimized for size using public/private pattern and DB0-DB15 slots
    
    // Debug strings - main headers
    const string debugVarsHeader = "\n== VARS ==\n";
    const string debugStackHeader = "\n== STACK ==\n";
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
    const string regXID = "XID:";
    const string regSP = "SP:";
    const string regBP = "BP:";
    const string regPC = "PC:";
    const string regACCL = "ACCL:";
    const string regACCT = "ACCT:";
    const string regCSP = "CSP:";
    
    const string regOP = "OpCodes:";
    const string regTP = "Tokens:";
    
    // Stack dump strings
    const string callStackHeader = "Call Stack (";
    const string framesSuffix = " frames):\n";
    const string framePrefix = "  Frame ";
    const string framePC = ": PC=0x";
    const string frameBP = " BP:";
    const string currentFrameMarker = " (current)";
    const string valueStackHeader = "Value Stack (";
    const string entriesSuffix = " entries):\n";
    const string stackEllipsis = "  ... (truncated)\n";
    const string bpMarker = " <- BP";
    const string returnSlotMarker = " *** RETURN SLOT ***";
    //const string frameMarkerPrefix = "\n--- Frame ";
    //const string frameMarkerSuffix = " ---";
    const string argMarker = " (arg)";
    const string localMarker = " (local)";
    
    // Status indicators (3+ characters only)
    const string statusFree = "FREE";
    const string statusUsed = "USED";
    const string statusNC = "NC ";
    const string statusNZ = "NZ ";
    
    // Buffer labels
    const string basicInputBufferLabel = "\nLine Input Buffer (Size:";
    
    const string tokenizerBufferLabel = "\nTokenizer Buffer (Addr:";
    const string functionTokensBufferLabel = "\nActive Function Tokens Buffer (Addr:";
    
    const string basicTokPosLabel = " Pos:";
    const string basicTokBufSizeLabel = " Size:";
    const string basicCurTokLabel = " CurTok:";
    const string replOpCodeBufferLabel = "\nREPL Compile and Execute OpCodeBuffer (Addr:";
    const string functionOpCodeBufferLabel = "\nFunction Compile OpCodeBuffer (Addr:";
    const string functionExecuteOpCodeBufferLabel = "\nActive Function Execute OpCodeBuffer (Addr:";
    
    const string basicOpCodeSizeLabel = " Size:";
    const string basicPCLabel = " PC:";
    const string firstNRowsPrefix = ") - First ";
    const string firstNRowsSuffix = " rows:\n";
    const string basicErrorLabel = "\nLastError: ";
    //const string sectionSuffix = " ===\n";
    
    // Common formatting - only keep multi-use strings
    const string listVL = "VL:";
    const string listFL = "FL:";
    const string listIT = "IT:";

#if defined(DEBUG) || defined(TRACE)

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
    // Print string from STR2
    printString2()
    {
        PHY
        LDY #0
        loop
        {
            LDA [ZP.STR2], Y
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
        Tools.IsPrintable();
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
    
    tpOut()  // Output TokenBuffer register
    {
        LDA #(regTP % 256)
        STA ZP.STR2
        LDA #(regTP / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.TokenBufferH
        hOut();
        LDA ZP.TokenBufferL
        hOut();
        space();
    }
    
    opOut()  // Output OpCodeBuffer register
    {
        LDA #(regOP % 256)
        STA ZP.STR2
        LDA #(regOP / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.OpCodeBufferH
        hOut();
        LDA ZP.OpCodeBufferL
        hOut();
        space();
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
    xiOut()  // Output XID register
    {
        LDA #(regXID % 256)
        STA ZP.STR2
        LDA #(regXID / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.XIDH
        hOut();
        LDA ZP.XIDL
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
        LDA ZP.TOPT
        BASICTypes.PrintType();
        LDA #':'
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
        LDA ZP.NEXTT
        BASICTypes.PrintType();
        LDA #':'
        cOut();
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
        LDA ZP.TOPT
        BASICTypes.PrintType();
        LDA #':'
        cOut();
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
    
    pcOut()  // Output PC register
    {
        LDA #(regPC % 256)
        STA ZP.STR2
        LDA #(regPC / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.PCH
        hOut();
        LDA ZP.PCL
        hOut();
        space();
    }
    
    bpOut()  // Output BP register
    {
        LDA #(regBP % 256)
        STA ZP.STR2
        LDA #(regBP / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.BP
        hOut();
        space();
    }
    
    spOut()  // Output SP register
    {
        LDA #(regSP % 256)
        STA ZP.STR2
        LDA #(regSP / 256)
        STA ZP.STR2H
        printString2();
        LDA ZP.SP
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
    
    TPOut()
    {
        PHP PHA PHY
        tpOut();
        PLY PLA PLP
    }
    OPOut()
    {
        PHP PHA PHY
        opOut();
        PLY PLA PLP
    }
    
    XOut()
    {
        PHP PHA PHY
        xOut();
        PLY PLA PLP
    }
    XIOut()
    {
        PHP PHA PHY
        xiOut();
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
    
    PCOut()
    {
        PHP PHA PHY
        pcOut();
        PLY PLA PLP
    }
    
    BPOut()
    {
        PHP PHA PHY
        bpOut();
        PLY PLA PLP
    }
    
    SPOut()
    {
        PHP PHA PHY
        spOut();
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
    
    // Helper to checkpoint error state
    ChkErr()
    {
        PHP PHA
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (NZ)
        {
            LDA #'{' cOut(); LDA #'E' cOut();
            PLA
            hOut();  // Show checkpoint ID where error was first detected
            PHA
            LDA #'}' cOut();
        }
        PLA PLP
    }
    
    // === Private complex methods (use DB slots freely) ===
    
    dumpVariables()
    {
        // Store registers in DB slots for display
        STA ZP.DB7  // A
        STX ZP.DB8  // X
        STY ZP.DB9  // Y
        
        LDA #(debugVarsHeader % 256)
        STA ZP.STR
        LDA #(debugVarsHeader / 256)
        STA ZP.STRH
        printString();
        
        // A register
        LDA #'A' cOut(); LDA #':' cOut();
        LDA ZP.DB7
        hOut();
        space();
        
        // X register
        LDA #'X' cOut(); LDA #':' cOut();
        LDA ZP.DB8
        hOut();
        space();
        
        // Y register
        LDA #'Y' cOut(); LDA #':' cOut();
        LDA ZP.DB9
        hOut();
        space();
        
        // TOP with type
        tOut();
        
        // NEXT with type
        nOut();
        
        // ACC with type
        aOut();
        
        // IDX
        xOut();
        
        // IDY
        yOut();
        
        // SP
        spOut();
        
        nL();
    }
    
    dumpIterationState()
    {
        LDA #(listIT % 256)
        STA ZP.STR
        LDA #(listIT / 256)
        STA ZP.STRH
        printString();
        LDA ZP.IDXH
        hOut();
        LDA ZP.IDXL
        hOut();
        space();
    }
    
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
        
        LDA #(listVL % 256)
        STA ZP.STR
        LDA #(listVL / 256)
        STA ZP.STRH
        printString();
        LDA ZP.VariablesListH
        hOut();
        LDA ZP.VariablesListL
        hOut();
        space();
        
        LDA #(listFL % 256)
        STA ZP.STR
        LDA #(listFL / 256)
        STA ZP.STRH
        printString();
        LDA ZP.FunctionsListH
        hOut();
        LDA ZP.FunctionsListL
        hOut();
        nL();
        
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
    
    dumpBasicBuffers()
    {
        // InputBuffer (shared between REPL and BASIC)
        LDA #(basicInputBufferLabel % 256)
        STA ZP.STR
        LDA #(basicInputBufferLabel / 256)
        STA ZP.STRH
        printString();
        LDA ZP.BasicInputLength
        hOut();
        
        // Dump input buffer
        LDA #(Address.BasicInputBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicInputBuffer >> 8)
        STA ZP.DB1
        LDX #4 // 4 lines = 16 x 4 = 64 bytes
        dumpMemoryBlock(); // address: DB1 = MSB, DB0 = LSB
        
        // TokenizerBuffer
        LDA #(tokenizerBufferLabel % 256)
        STA ZP.STR
        LDA #(tokenizerBufferLabel / 256)
        STA ZP.STRH
        printString();
        
        // Print tokenizer buffer address
        LDA #(Address.TokenizerBuffer >> 8)
        hOut();
        LDA #(Address.TokenizerBuffer & 0xFF)
        hOut();
        
        // Print position label and value (if REPL is active)
        LDA ZP.TokenBufferH
        CMP #(Address.TokenizerBuffer >> 8)
        if (Z)
        {
            LDA #(basicTokPosLabel % 256)
            STA ZP.STR
            LDA #(basicTokPosLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.TokenizerPosH
            hOut();
            LDA ZP.TokenizerPosL
            hOut();
            
            LDA #(basicTokBufSizeLabel % 256)
            STA ZP.STR
            LDA #(basicTokBufSizeLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.TokenBufferContentLengthH
            hOut();
            LDA ZP.TokenBufferContentLengthL
            hOut();
            
            LDA #(basicCurTokLabel % 256)
            STA ZP.STR
            LDA #(basicCurTokLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.CurrentToken
            hOut();
        }
        
        // Dump REPL tokenizer buffer
        LDA #(Address.TokenizerBuffer >> 8)
        STA ZP.DB1
        LDA #(Address.TokenizerBuffer & 0xFF)
        STA ZP.DB0
        LDX #4 // 4 lines = 16 x 4 = 64 bytes
        dumpMemoryBlock();
        
        // REPL OpCodeBuffer
        LDA #(replOpCodeBufferLabel % 256)
        STA ZP.STR
        LDA #(replOpCodeBufferLabel / 256)
        STA ZP.STRH
        printString();
        
        // Print REPL opcode buffer address
        LDA #(Address.REPLOpCodeBuffer >> 8)
        hOut();
        LDA #(Address.REPLOpCodeBuffer & 0xFF)
        hOut();
        
        // Print size and PC (if REPL is active) - TEST
        LDA ZP.OpCodeBufferH
        CMP #(Address.REPLOpCodeBuffer >> 8)
        if (Z)
        {
            LDA #(basicOpCodeSizeLabel % 256)
            STA ZP.STR
            LDA #(basicOpCodeSizeLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.OpCodeBufferContentLengthH
            hOut();
            LDA ZP.OpCodeBufferContentLengthL
            hOut();
            
            LDA #(basicPCLabel % 256)
            STA ZP.STR
            LDA #(basicPCLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.PCH
            hOut();
            LDA ZP.PCL
            hOut();
        }
        
        // Dump REPL opcode buffer
        LDA #(Address.REPLOpCodeBuffer >> 8)
        STA ZP.DB1
        LDA #(Address.REPLOpCodeBuffer & 0xFF)
        STA ZP.DB0
        LDX #4 // 4 lines = 16 x 4 = 64 bytes
        dumpMemoryBlock();
        
        
        
        LDA ZP.TokenBufferH
        CMP #(Address.TokenizerBuffer >> 8)
        if (NZ)
        {
            // FUNCTION Token Buffer
            LDA #(functionTokensBufferLabel % 256)
            STA ZP.STR
            LDA #(functionTokensBufferLabel / 256)
            STA ZP.STRH
            printString();
            
            // Print Function tokenizer buffer address
            LDA ZP.TokenBufferH
            hOut();
            LDA ZP.TokenBufferL
            hOut();
            
            // Print position label and value
            
            LDA #(basicTokPosLabel % 256)
            STA ZP.STR
            LDA #(basicTokPosLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.TokenizerPosH
            hOut();
            LDA ZP.TokenizerPosL
            hOut();
            
            LDA #(basicTokBufSizeLabel % 256)
            STA ZP.STR
            LDA #(basicTokBufSizeLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.TokenBufferContentLengthH
            hOut();
            LDA ZP.TokenBufferContentLengthL
            hOut();
            
            LDA #(basicCurTokLabel % 256)
            STA ZP.STR
            LDA #(basicCurTokLabel / 256)
            STA ZP.STRH
            printString();
            
            LDA ZP.CurrentToken
            hOut();
            LDA #')' COut();
            
            LDY #1
            LDA [ZP.DB0]
            STA ZP.FLENGTHL
            LDA [ZP.DB0], Y
            STA ZP.FLENGTHH
            
            LDA ZP.TokenBufferContentLengthL
            STA ZP.FLENGTHL
            LDA ZP.TokenBufferContentLengthH
            STA ZP.FLENGTHH
            
            LDA ZP.TokenBufferH
            STA ZP.DB1
            LDA ZP.TokenBufferL
            STA ZP.DB0
            dumpMemoryBlockL();
        }
        
        LDA ZP.OpCodeBufferH
        CMP #(Address.FunctionOpCodeBuffer >> 8)
        if (Z)
        {
            LDA #(functionOpCodeBufferLabel % 256)
            STA ZP.STR
            LDA #(functionOpCodeBufferLabel / 256)
            STA ZP.STRH
            printString();
            
            // Print BASIC opcode buffer address
            LDA #(Address.FunctionOpCodeBuffer >> 8)
            hOut();
            LDA #(Address.FunctionOpCodeBuffer & 0xFF)
            hOut();
            
            // Dump BASIC opcode buffer
            LDA #(Address.FunctionOpCodeBuffer >> 8)
            STA ZP.DB1
            LDA #(Address.FunctionOpCodeBuffer & 0xFF)
            STA ZP.DB0
            LDX #4 // 4 lines = 16 x 4 = 64 bytes
            dumpMemoryBlock();
        }
        /*
        LDA ZP.OpCodeBufferH
        CMP #(Address.FunctionOpCodeBuffer >> 8)
        if (NZ)
        {
            CMP #(Address.REPLOpCodeBuffer >> 8)
            if (NZ)
            {
                LDA #(functionExecuteOpCodeBufferLabel % 256)
                STA ZP.STR
                LDA #(functionExecuteOpCodeBufferLabel / 256)
                STA ZP.STRH
                printString();
                
                // Print BASIC opcode buffer address
                LDA ZP.OpCodeBufferH
                hOut();
                LDA ZP.OpCodeBufferL
                hOut();
                
                // find heap allocation block size:
                SEC
                LDA ZP.OpCodeBufferL
                SBC #2
                STA ZP.DB0
                LDA ZP.OpCodeBufferH
                SBC #0
                STA ZP.DB1
                
                LDY #1
                LDA [ZP.DB0]
                STA ZP.FLENGTHL
                LDA [ZP.DB0], Y
                STA ZP.FLENGTHH
                
                // substract 2 (the block size word)
                SEC
                LDA ZP.FLENGTHL
                SBC #2
                STA ZP.FLENGTHL
                LDA ZP.FLENGTHH
                SBC #0
                STA ZP.FLENGTHH
                
                LDA ZP.OpCodeBufferH
                STA ZP.DB1
                LDA ZP.OpCodeBufferL
                STA ZP.DB0
                dumpMemoryBlockL();
            }
        }        
        */
        // Error pointers
        LDA #(basicErrorLabel % 256)
        STA ZP.STR
        LDA #(basicErrorLabel / 256)
        STA ZP.STRH
        printString();
        LDA ZP.LastError
        hOut();
        nL();
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
    
    dumpMemoryBlockX() // address: DB1 = MSB, DB0 = LSB, X = umber of rows
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
    
    findFunctionByAddressSTR()  // ACC = search address, returns STR = name, C if found
    {
        // Save state we'll modify
        PHX
        LDA ZP.IDXL
        STA ZP.DB10
        LDA ZP.IDXH
        STA ZP.DB11
        LDA ZP.NEXTL
        STA ZP.DB12
        LDA ZP.NEXTH
        STA ZP.DB13
        
        loop
        {
            // Start function iteration
            LDX #ZP.FunctionsList
            Table.GetFirst();
            if (NC)
            {
                CLC  // No functions
                break;
            }
            
            loop
            {
                // Get function's opcode stream
                Functions.GetOpCodes();
                if (NC)
                {
                    // Not compiled, try next
                    Table.GetNext();
                    if (NC) { CLC break; }
                    continue;
                }
                
                // Get block size
                SEC
                LDA ZP.IDYL
                SBC #2
                STA ZP.NEXTL
                LDA ZP.IDYH
                SBC #0
                STA ZP.NEXTH
                
                // Load block size
                LDY #0
                LDA [ZP.NEXT], Y
                PHA
                INY
                LDA [ZP.NEXT], Y
                STA ZP.NEXTH
                PLA
                STA ZP.NEXTL
                
                // Calculate end address
                SEC
                LDA ZP.NEXTL
                SBC #2
                STA ZP.NEXTL
                LDA ZP.NEXTH
                SBC #0
                STA ZP.NEXTH
                
                CLC
                LDA ZP.IDYL
                ADC ZP.NEXTL
                STA ZP.NEXTL
                LDA ZP.IDYH
                ADC ZP.NEXTH
                STA ZP.NEXTH
                
                // Check if searchPC >= start
                LDA ZP.ACCH
                CMP ZP.IDYH
                if (NZ)
                {
                    if (NC)
                    {
                        // searchPC < start
                        Table.GetNext();
                        if (NC) { CLC break; }
                        continue;
                    }
                }
                else
                {
                    LDA ZP.ACCL
                    CMP ZP.IDYL
                    if (NC)
                    {
                        // searchPC < start
                        Table.GetNext();
                        if (NC) { CLC break; }
                        continue;
                    }
                }
                
                // Check if searchPC < end
                LDA ZP.ACCH
                CMP ZP.NEXTH
                if (NZ)
                {
                    if (C)
                    {
                        // searchPC >= end
                        Table.GetNext();
                        if (NC) { CLC break; }
                        continue;
                    }
                }
                else
                {
                    LDA ZP.ACCL
                    CMP ZP.NEXTL
                    if (C)
                    {
                        // searchPC >= end
                        Table.GetNext();
                        if (NC) { CLC break; }
                        continue;
                    }
                }
                
                // Found it!
                Functions.GetName(); // -> STR
                SEC
                break;
            }
            break;
        }
        
        // Restore state
        LDA ZP.DB13
        STA ZP.NEXTH
        LDA ZP.DB12
        STA ZP.NEXTL
        LDA ZP.DB11
        STA ZP.IDXH
        LDA ZP.DB10
        STA ZP.IDXL
        PLX
    }
    
    dumpStack()
    {
        // Main header
        LDA #(debugStackHeader % 256)
        STA ZP.STR
        LDA #(debugStackHeader / 256)
        STA ZP.STRH
        printString();
        
        // Stack pointers
        LDA #(regCSP % 256)
        STA ZP.STR
        LDA #(regCSP / 256)
        STA ZP.STRH
        printString();
        LDA ZP.CSP
        hOut();
        space();
        
        spOut();
        bpOut();
        nL();
        
        // Call Stack
        LDA #(callStackHeader % 256)
        STA ZP.STR
        LDA #(callStackHeader / 256)
        STA ZP.STRH
        printString();
        LDA ZP.CSP
        hOut();
        LDA #(framesSuffix % 256)
        STA ZP.STR
        LDA #(framesSuffix / 256)
        STA ZP.STRH
        printString();
        
        // Current frame
        LDA #(framePrefix % 256)
        STA ZP.STR
        LDA #(framePrefix / 256)
        STA ZP.STRH
        printString();
        LDA #'0' cOut(); LDA #':' cOut();
        space();
        LDA #(framePC % 256)
        STA ZP.STR
        LDA #(framePC / 256)
        STA ZP.STRH
        printString();
        
        LDA ZP.PCH
        hOut();
        LDA ZP.PCL
        hOut();
        
        // Look up current function
        LDA ZP.PCL
        STA ZP.ACCL
        LDA ZP.PCH
        STA ZP.ACCH
        findFunctionByAddressSTR();
        if (C)
        {
            space();
            LDA #'(' cOut();
            Print.String();  // Keep this - STR already set by findFunctionByAddress
            LDA #')' cOut();
        }
        
        LDA #(frameBP % 256)
        STA ZP.STR
        LDA #(frameBP / 256)
        STA ZP.STRH
        printString();
        LDA ZP.BP
        hOut();
        LDA #(currentFrameMarker % 256)
        STA ZP.STR
        LDA #(currentFrameMarker / 256)
        STA ZP.STRH
        printString();
        nL();
        
        // Walk call stack backwards from CSP
        LDX ZP.CSP
        LDY #1  // Frame counter (start at 1 since we already showed frame 0)
        
        loop
        {
            // Need at least 2 entries (PC + BP pair) before current position
            // Check if we're at the beginning
            CPX #0
            if (Z) { break; }  // Can't go lower
            
            CPX #1  
            if (Z) { break; }  // Only 1 entry left, can't make a pair
            
            // CSP points to next free slot, so back up to previous frame
            DEX  // Move to BP of previous frame
            DEX  // Move to PC of previous frame
            
            // Print frame info
            LDA #(framePrefix % 256)
            STA ZP.STRL
            LDA #(framePrefix / 256)
            STA ZP.STRH
            printString();
            TYA
            hOut();
            LDA #(framePC % 256)
            STA ZP.STRL
            LDA #(framePC / 256)
            STA ZP.STRH
            printString();
            
            // Load return address from even index (PC)
            LDA Address.CallStackLSB, X
            STA ZP.ACCL
            LDA Address.CallStackMSB, X
            STA ZP.ACCH
            
            // Print address
            LDA ZP.ACCH
            hOut();
            LDA ZP.ACCL
            hOut();
            
            // Find function
            findFunctionByAddressSTR();
            if (C)
            {
                space();
                LDA #'(' cOut();
                Print.String();  // Keep this - STR already set by findFunctionByAddress
                LDA #')' cOut();
            }
            
            // Print BP from odd index (BP)
            LDA #(frameBP % 256)
            STA ZP.STR
            LDA #(frameBP / 256)
            STA ZP.STRH
            printString();
            INX  // Move to BP position (odd index)
            LDA Address.CallStackLSB, X
            hOut();
            DEX  // Back to PC position
            
            nL();
            INY  // Increment frame counter
        }
        
        nL();
        
        // Value Stack
        LDA #(valueStackHeader % 256)
        STA ZP.STRL
        LDA #(valueStackHeader / 256)
        STA ZP.STRH
        printString();
        LDA ZP.SP
        hOut();
        LDA #(entriesSuffix % 256)
        STA ZP.STRL
        LDA #(entriesSuffix / 256)
        STA ZP.STRH
        printString();
        
        LDX ZP.SP
        LDY #0  // Entry counter
        STZ ZP.DB7  // Frame counter
        
        loop
        {
            CPX #0
            if (Z) { break; }
            
            CPY #20
            if (Z)
            {
                LDA #(stackEllipsis % 256)
                STA ZP.STR
                LDA #(stackEllipsis / 256)
                STA ZP.STRH
                printString();
                break;
            }
            
            DEX  // Move to previous entry
            
            // Check for frame boundary
            /*
            TXA
            CMP ZP.BP
            if (Z)
            {
                // Frame boundary marker
                LDA #(frameMarkerPrefix % 256)
                STA ZP.STRL
                LDA #(frameMarkerPrefix / 256)
                STA ZP.STRH
                printString();
                LDA ZP.DB7
                hOut();
                LDA #(frameMarkerSuffix % 256)
                STA ZP.STRL
                LDA #(frameMarkerSuffix / 256)
                STA ZP.STRH
                printString();
                INC ZP.DB7
                nL();
            }
            */
            
            // Print entry
            TXA
            hOut();
            LDA #' ' cOut();
            space();
            
            // Type and value
            LDA Address.TypeStackLSB, X
            BASICTypes.PrintType();
            LDA #':' cOut();
            LDA Address.ValueStackMSB, X
            hOut();
            LDA Address.ValueStackLSB, X
            hOut();
            
            // Position markers
            TXA
            CMP ZP.BP
            if (Z)
            {
                LDA #(bpMarker % 256)
                STA ZP.STRL
                LDA #(bpMarker / 256)
                STA ZP.STRH
                printString();
            }
            else
            {
                STZ ZP.DB9
                
                LDA Address.TypeStackLSB, X
                CMP #Types.Undefined
                if (Z)
                {
                    INC ZP.DB9
                }
                else
                {
                    CMP #Types.Bool
                    if (Z)
                    {
                        LDA Address.ValueStackLSB, X
                        ORA Address.ValueStackMSB, X
                        if (Z)
                        {
                            INC ZP.DB9
                        }
                    }
                }
                
                LDA ZP.DB9
                if (NZ)
                {
                    LDA #(returnSlotMarker % 256)
                    STA ZP.STRL
                    LDA #(returnSlotMarker / 256)
                    STA ZP.STRH
                    printString();
                }
                else
                {
                    SEC
                    TXA
                    SBC ZP.BP
                    if (NC)
                    {
                        LDA #(argMarker % 256)
                        STA ZP.STRL
                        LDA #(argMarker / 256)
                        STA ZP.STRH
                        printString();
                    }
                    else
                    {
                        LDA #(localMarker % 256)
                        STA ZP.STRL
                        LDA #(localMarker / 256)
                        STA ZP.STRH
                        printString();
                    }
                }
            }
            
            nL();
            INY
        }
        
        nL();
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
    
    // === Public debug methods (preserve all state) ===
    
    DumpVariables()
    {
        PHP PHA PHX PHY
        dumpVariables();
        PLY PLX PLA PLP
    }
    
    DumpIterationState()
    {
        PHP PHA PHY
        dumpIterationState();
        PLY PLA PLP
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
        LDA ZP.LCURRENTL
        PHA
        LDA ZP.LCURRENTH
        PHA
        
        dumpHeap();
        
        // Restore state
        PLA
        STA ZP.LCURRENTH
        PLA
        STA ZP.LCURRENTL
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
    
    DumpXIDBuffer()
    {
        // XID
        PHP PHA PHX PHY
        
        NL();
        LDA ZP.XIDH HOut(); LDA ZP.XIDL HOut(); LDA #':' COut(); Space();
        
        LDA ZP.TokenBufferContentSizeL
        TAX
        LDY #0
        loop
        {
            LDA [ZP.XID], Y
            HOut(); Space();
            INY
            DEX
            if (Z) { break; }
        }
        Space();
        LDA ZP.TokenBufferContentSizeL
        TAX
        LDY #0
        loop
        {
            LDA [ZP.XID], Y
            Printable();
            INY
            DEX
            if (Z) { break; }
        }
        
        PLY PLX PLA PLP
    }
    
    DumpBuffers()
    {
        PHP PHA PHX PHY
        dumpBasicBuffers();
        PLY PLX PLA PLP
    }
    
    // munts A (context id)
    DumpRuntime()
    {
        PHP PHX PHY
        
        NL(); COut(); Space(); TPOut(); OPOut(); PCOut(); SPOut(); BPOut(); Space(); PrintState();
        
        IsFailure();
        if (C)
        {
            Space();
            LDA ZP.LastErrorL
            ORA ZP.LastErrorH
            if (NZ)
            {
                // Print the error message
                LDA #'?'
                Serial.WriteChar(); // '?' prefix
                LDA ZP.LastErrorL
                STA ZP.STRL
                LDA ZP.LastErrorH
                STA ZP.STRH
                Print.String();
                
                // 6502 PC
                Print.Space();
                LDA #'('
                Serial.WriteChar();
                LDA #'0'
                Serial.WriteChar();
                LDA #'x'
                Serial.WriteChar();
                LDA ZP.EmulatorPCH
                Serial.HexOut();
                LDA ZP.EmulatorPCL
                Serial.HexOut();
                LDA #')'
                Serial.WriteChar();
            }
        }
        
        PLY PLX PLP
    }
    
    // Public compact stack dump method
    // Input: Stack state (ZP.SP, ZP.BP, Address.*Stack arrays)
    // Output: Single line stack dump to serial, right-aligned at column 21
    //         Shows top 20 stack elements: newest on left, oldest on right
    //         Format: "     i:0000 b:0042|v:0000 v:0000" where | marks BP
    // Modifies: Serial output only
    // Preserves: All registers, flags, and ZP locations (including ZP.ACCL, ZP.STRL, ZP.STRH)
    // 
    // Displays stack elements with single-character type indicators:
    //   v=VOID, i=INT, b=BYTE, c=CHAR, w=WORD, B=BIT, s=STRING, a=ARRAY
    // VAR types shown in brackets: [i:0042]
    // Base Pointer position marked with '|' separator
    CompactStack()
    {
        PHP PHA PHX PHY
        
        // Save state that compactStack modifies
        LDA ZP.ACCL
        PHA
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        nL();
        
        // First call: count characters (A=0)
        LDA #0
        compactStack();  // Returns character count in ZP.ACCL
        
        // Calculate and print leading spaces
        LDA #120
        SEC
        SBC ZP.ACCL
        TAY  // Y = number of leading spaces
        
        loop
        {
            CPY #0
            if (Z) { break; }
            space();
            DEY
        }
        
        // Second call: print the stack (A=1)
        LDA #1
        compactStack();
        Debug.ValidateHeap();

//NL(); LDA #'S' COut(); LDA #'P' COut(); LDA #':' COut(); LDA ZP.SP HOut();      
        
        // Restore state
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        PLA
        STA ZP.ACCL
        
        PLY PLX PLA PLP
    }

    // Internal compact stack implementation with LONG support
    // Input: A = mode (0=count characters, 1=print)
    //        Stack state (ZP.SP, ZP.BP, Address.*Stack arrays)
    // Output: If counting (A=0): character count in ZP.ACCL
    //         If printing (A=1): formatted stack dump to serial
    // Modifies: ZP.ACCL (character counter when A=0), ZP.DB0 (flag bits: 0=VAR, 1=ARRAY, 2=LONG)
    compactStack()
    {
        PHA  // Save mode on stack
        
        // Check for empty stack
        LDX ZP.SP
        CPX #0
        if (Z) 
        {
            PLA  // Clean up mode from stack
            return; // empty stack
        }
        
        PLA
        PHA  // Peek at mode
        if (Z)
        {
            // Counting mode - initialize counter
            STZ ZP.ACCL
        }
        
        LDY #0  // Counter for elements processed
        
        loop  // Element loop
        {
            CPY #20
            if (Z) { break; }  // Max 20 elements
            
            DEX  // Point to actual stack element (SP-1 is top)
            CPX #0xFF  // Did we wrap around?
            if (Z) 
            { 
                // We've gone below position 0, stop
                break; 
            }
            
            // Check special cases using bit flags
            STZ ZP.DB0  // Clear all flags
            
            // Check VAR status  
            LDA Address.TypeStackLSB, X
            AND #BASICType.VAR
            if (NZ)
            {
                SMB0 ZP.DB0  // Set bit 0 for VAR
            }
            
            // Check ARRAY status
            LDA Address.TypeStackLSB, X  
            AND #BASICType.ARRAY
            if (NZ)
            {
                SMB1 ZP.DB0  // Set bit 1 for ARRAY
            }
            
            // Check LONG type
            LDA Address.TypeStackLSB, X
            AND #BASICType.TYPEMASK
            CMP #BASICType.LONG
            if (Z)
            {
                SMB2 ZP.DB0  // Set bit 2 for LONG
            }
            
            PLA
            PHA  // Peek at mode
            if (Z)
            {
                // === COUNTING MODE ===
                
                // Count opening bracket for VAR or ARRAY
                if (BBS0, ZP.DB0)  // VAR?
                {
                    INC ZP.ACCL  // '{'
                }
                else
                {
                    if (BBS1, ZP.DB0)  // ARRAY?
                    {
                        INC ZP.ACCL  // '['
                    }
                }
                
                INC ZP.ACCL      // Type character
                INC ZP.ACCL      // ':'
                
                // Count hex digits based on type
                if (BBS2, ZP.DB0)  // LONG?
                {
                    // Check if upper bytes are zero for compact display
                    LDA Address.ValueStackB3, X
                    ORA Address.ValueStackB2, X
                    if (NZ)  // Upper bytes are non-zero?
                    {
                        // Full LONG: 8 hex digits
                        LDA ZP.ACCL
                        CLC
                        ADC #8
                        STA ZP.ACCL
                    }
                    else
                    {
                        // Compact LONG: 4 hex digits (same as regular types)
                        LDA ZP.ACCL
                        CLC
                        ADC #4
                        STA ZP.ACCL
                    }
                }
                else
                {
                    // Regular: 4 hex digits (2 bytes: 0x1234)
                    LDA ZP.ACCL
                    CLC 
                    ADC #4
                    STA ZP.ACCL
                }
                
                // Count closing bracket for VAR or ARRAY
                if (BBS0, ZP.DB0)  // VAR?
                {
                    INC ZP.ACCL  // '}'
                }
                else
                {
                    if (BBS1, ZP.DB0)  // ARRAY?
                    {
                        INC ZP.ACCL  // ']'
                    }
                }
                
                INC ZP.ACCL      // Space or '|' separator
            }
            else
            {
                // === PRINTING MODE ===
                
                // Print opening bracket for VAR or ARRAY
                if (BBS0, ZP.DB0)  // VAR?
                {
                    LDA #'{'
                    cOut();
                }
                else
                {
                    if (BBS1, ZP.DB0)  // ARRAY?
                    {
                        LDA #'['
                        cOut();
                    }
                }
                
                // Print type character
                LDA Address.TypeStackLSB, X
                AND #SymbolType.MASK
                if (NZ)
                {
                    LDA #'!'
                    cOut();
                }
                else
                {
                    LDA Address.TypeStackLSB, X
                    AND #BASICType.TYPEMASK
                    switch (A)
                    {
                        case BASICType.VOID:
                        {
                            LDA #'v'
                            cOut();
                        }
                        case BASICType.CHAR:
                        {
                            LDA #'c'
                            cOut();
                        }
                        case BASICType.INT:
                        {
                            LDA #'i'
                            cOut();
                        }
                        case BASICType.BYTE:
                        {
                            LDA #'b'
                            cOut();
                        }
                        case BASICType.WORD:
                        {
                            LDA #'w'
                            cOut();
                        }
                        case BASICType.LONG:
                        {
                            LDA #'l'  // lowercase "l" for LONG
                            cOut();
                        }
                        case BASICType.BIT:
                        {
                            LDA #'B'
                            cOut();
                        }
                        case BASICType.STRING:
                        {
                            LDA #'s'
                            cOut();
                        }
                        default:
                        {
                            LDA #'?'
                            cOut();
                        }
                    }
                }
                
                // Print colon
                LDA #':'
                cOut();
                
                // Print value based on type
                if (BBS2, ZP.DB0)  // LONG?
                {
                    // Check if upper bytes are zero
                    LDA Address.ValueStackB3, X
                    ORA Address.ValueStackB2, X
                    if (NZ)  // Upper bytes are non-zero?
                    {
                        // Print all 4 hex digits for full LONG
                        LDA Address.ValueStackB3, X  // MSB first
                        Serial.HexOut();
                        LDA Address.ValueStackB2, X
                        Serial.HexOut();
                    }
                    // Always print lower 2 bytes (fall through)
                }

                // Print lower 2 bytes (common for all types and compact LONGs)
                LDA Address.ValueStackB1, X   // MSB first
                Serial.HexOut();
                LDA Address.ValueStackB0, X   // LSB last  
                Serial.HexOut();
                
                // Print closing bracket for VAR or ARRAY
                if (BBS0, ZP.DB0)  // VAR?
                {
                    LDA #'}'
                    cOut();
                }
                else
                {
                    if (BBS1, ZP.DB0)  // ARRAY?
                    {
                        LDA #']'
                        cOut();
                    }
                }
                
                // Print separator (space or BP marker)
                TXA  // Current stack position
                CMP ZP.BP
                if (Z)
                {
                    LDA #'|'  // BP marker
                    cOut();
                }
                else
                {
                    space();
                }
            }
            
            INY  // Increment element counter
        } // Element loop
        
        PLA  // Clean up mode from stack
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
    
    DumpStack()
    {
        PHP PHA PHX PHY
        
        // Save state that dumpStack modifies
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
        LDA ZP.NEXTL
        PHA
        LDA ZP.NEXTH
        PHA
        
        dumpStack();
        
        // Restore state
        PLA
        STA ZP.NEXTH
        PLA
        STA ZP.NEXTL
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY PLX PLA PLP
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
            
            // Now validate function list
            validateFunctionList();
            if (NC)
            {
                // corrupt function list
                NL();
                LDA #'F' COut();
                LDA #'L' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            
            // Finally validate variable list  
            validateVariableList();
            if (NC)
            {
                // corrupt variable list
                NL();
                LDA #'V' COut();
                LDA #'L' COut();
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

    // Validate function list integrity
    validateFunctionList()
    {
        // Get function list head
        LDA ZP.FunctionsList
        STA ZP.IDXL
        LDA ZP.FunctionsList+1
        STA ZP.IDXH
        
        LDX #0 // Safety counter
        
        loop
        {
            // Check if we're at end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                SEC // Success - reached end normally
                break;
            }
            
            // Check if current node is valid heap pointer
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid function node pointer
                NL();
                LDA #'F' COut();
                LDA #'L' COut();
                LDA #'!' COut();
                LDA ZP.IDXH HOut();
                LDA ZP.IDXL HOut();
                CLC // Failed
                break;
            }
            
            // Validate inline name
            validateInlineName();  // IDX already points to node
            if (NC)
            {
                // Invalid function name
                NL();
                LDA #'F' COut();
                LDA #'N' COut();
                LDA #'!' COut();
                CLC
                break;
            }
                        
            // Check tokens pointer at offset 3-4
            LDY #Objects.snTokens
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid tokens pointer
                NL();
                LDA #'F' COut();
                LDA #'T' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            // Check opcodes pointer at offset 7-8
            LDY #Objects.snOpCodes
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid opcodes pointer
                NL();
                LDA #'F' COut();
                LDA #'O' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            // Check arguments pointer at offset 5-6
            LDY #Objects.snValue // Arguments store in "value" field
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid arguments pointer
                NL();
                LDA #'F' COut();
                LDA #'A' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            validateArgumentsList();  // ZP.IDY = arguments list head
            if (NC)
            {
                // Invalid arguments list
                NL();
                LDA #'F' COut();
                LDA #'A' COut();
                LDA #'L' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            // Get next pointer at offset 0-1
            LDY #0
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Safety check - prevent infinite loops
            INX
            CPX #0x80
            if (Z)
            {
                // Too many nodes
                NL();
                LDA #'F' COut();
                LDA #'#' COut();
                CLC // Failed
                break;
            }
        }
    }
     
    // Validate inline name string within an argument/local node
    // Input: ZP.IDX = node address  
    // Output: C if valid, NC if invalid
    // Uses: DB2-DB5 for calculations
    validateArgumentName()
    {
        PHY
        
        loop // Single exit pattern
        {
            // Get the heap block size (2 bytes before node)
            SEC
            LDA ZP.IDXL
            SBC #2
            STA ZP.DB2
            LDA ZP.IDXH
            SBC #0
            STA ZP.DB3
            
            // Read block size
            LDY #0
            LDA [ZP.DB2], Y
            STA ZP.DB4  // Size low
            INY
            LDA [ZP.DB2], Y
            STA ZP.DB5  // Size high
            
            // Subtract 2 for the size header
            SEC
            LDA ZP.DB4
            SBC #2
            STA ZP.DB4
            LDA ZP.DB5
            SBC #0
            STA ZP.DB5
            
            // Check if node size is reasonable
            LDA ZP.DB5
            if (NZ)  // High byte non-zero
            {
                NL();
                LDA #'A' COut();
                LDA #'B' COut();
                LDA #'S' COut();  // ABS = Argument Block Size too large
                CLC
                break;
            }
            
            // Scan for null terminator starting at lnName
            LDY #Locals.lnName  // Start at offset 3
            
            loop
            {
                // Check if Y reached node size
                CPY ZP.DB4
                if (C)  // Y >= node size
                {
                    NL();
                    LDA #'A' COut();
                    LDA #'O' COut();
                    LDA #'V' COut();  // AOV = Argument Overflow
                    CLC
                    break;
                }
                
                // Check for null terminator
                LDA [ZP.IDX], Y
                if (Z)
                {
                    SEC  // Found null - valid name
                    break;
                }
                
                // Check for non-printable chars
                CMP #32
                if (NC)  // < 32
                {
                    NL();
                    LDA #'A' COut();
                    LDA #'B' COut();
                    LDA #'C' COut();  // ABC = Argument Bad Char
                    CLC
                    break;
                }
                CMP #127
                if (C)  // >= 127
                {
                    NL();
                    LDA #'A' COut();
                    LDA #'B' COut();
                    LDA #'C' COut();  // ABC = Argument Bad Char
                    CLC
                    break;
                }
                
                // Check reasonable name length
                INY
                TYA
                SEC
                SBC #Locals.lnName
                CMP #64
                if (C)  // >= 64
                {
                    NL();
                    LDA #'A' COut();
                    LDA #'T' COut();
                    LDA #'L' COut();  // ATL = Argument Too Long
                    CLC
                    break;
                }
            }
            
            break;  // Exit outer loop
        }
        
        PLY
    }
    
    // Deep validate arguments list
    // Arguments/locals have structure: [next_ptr(2)] + [type(1)] + [name_string(null-terminated)]
    validateArgumentsList()  // ZP.IDY = arguments list head
    {
        // Save current IDX
        LDA ZP.IDXL PHA
        LDA ZP.IDXH PHA
        
        LDX #0  // Safety counter
        
        loop
        {
            // Check if at end of arguments list
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z) 
            { 
                SEC  // Success - reached end
                break; 
            }
            
            // Validate this argument node is a valid heap pointer
            isValidHeapPointer();  // Checks ZP.IDY
            if (NC)
            {
                NL();
                LDA #'A' COut();
                LDA #'R' COut();
                LDA #'!' COut();
                CLC
                break;
            }
            
            // Move pointer to IDX for accessing node
            // IDY -> IDX
            LDA ZP.IDYL STA ZP.IDXL
            LDA ZP.IDYH STA ZP.IDXH
            
            // Get next pointer first (offset 0-1)
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.IDYL      // Next pointer low
            INY  
            LDA [ZP.IDX], Y
            STA ZP.IDYH      // Next pointer high
            
            // Now validate the argument name
            // NO ADJUSTMENT NEEDED - validateArgumentName already uses Locals.lnName
            validateArgumentName();  // IDX points to argument node
            if (NC)
            {
                NL();
                LDA #'A' COut();
                LDA #'N' COut();  // AN = Argument Name
                LDA #'!' COut();
                CLC
                break;
            }
            
            // Safety check to prevent infinite loops
            INX
            CPX #20  // Maximum 20 arguments
            if (Z)
            {
                NL();
                LDA #'A' COut();
                LDA #'#' COut();  // A# = Too many arguments
                CLC
                break;
            }
            
            // Continue with next node (IDY already has next pointer)
        }
        
        // Restore IDX
        PLA STA ZP.IDXH
        PLA STA ZP.IDXL
    }
    
    // Validate a heap-allocated string
    // Input: ZP.IDY = pointer to string
    // Output: C if valid string, NC if invalid
    validateString()
    {
        PHY
            
        loop // Single exit pattern
        {
            // First check if it's a valid heap pointer
            isValidHeapPointer();
            if (NC)
            {
                NL();
                LDA #'S' COut();
                LDA #'B' COut();
                LDA #'H' COut();  // SBH = String bad heap pointer
                
                Space(); XOut(); LDA #'-' COut(); LDA #'>' COut(); Space(); YOut();
                
                LDY # Objects.snType // load
                LDA [ZP.IDX], Y
                BASICTypes.PrintType();
                
                CLC  // Not a valid heap allocation
                break;
            }
            
            // Get the heap block size (2 bytes before string)
            SEC
            LDA ZP.IDYL
            SBC #2
            STA ZP.DB2
            LDA ZP.IDYH
            SBC #0
            STA ZP.DB3
            
            // Read block size
            LDY #0
            LDA [ZP.DB2], Y
            STA ZP.DB4  // Size low
            INY
            LDA [ZP.DB2], Y
            STA ZP.DB5  // Size high
            
            // Subtract 2 for the size header
            SEC
            LDA ZP.DB4
            SBC #2
            STA ZP.DB4
            LDA ZP.DB5
            SBC #0
            STA ZP.DB5
            
            // Check if size is reasonable for a string (< 256)
            LDA ZP.DB5
            if (NZ)
            {
                // String too large
                NL();
                LDA #'S' COut();
                LDA #'T' COut();
                LDA #'L' COut();  // STL = String Too Long
                CLC
                break;
            }
            
            // Scan for null terminator
            LDY #0
            loop
            {
                // Check if we've reached the allocation limit
                CPY ZP.DB4
                if (C)  // Y >= size
                {
                    // No null terminator found
                    NL();
                    LDA #'S' COut();
                    LDA #'N' COut();
                    LDA #'T' COut();  // SNT = String No Terminator
                    CLC
                    break;
                }
                
                // Check for null
                LDA [ZP.IDY], Y
                if (Z)
                {
                    SEC  // Found null - valid string
                    break;
                }
                
                // Optional: check printable
                CMP #32
                if (NC)
                {
                    CMP #127
                    if (C)
                    {
                        NL();
                        LDA #'S' COut();
                        LDA #'B' COut();
                        LDA #'C' COut();  // SBC = String Bad Char
                        CLC
                        break;
                    }
                }
                
                INY
            }
            
            SEC // all good
            break;
        }
        
        PLY
    }
    
    // Validate inline name string within a heap object
    // Input: ZP.IDX = node address
    // Output: C if valid, NC if name runs past node boundary or block too large
    // Uses: DB2-DB5 for calculations
    validateInlineName()
    {
        PHY
        
        loop // Single exit pattern
        {
            // Get the heap block size (2 bytes before node)
            SEC
            LDA ZP.IDXL
            SBC #2
            STA ZP.DB2
            LDA ZP.IDXH
            SBC #0
            STA ZP.DB3
            
            // Read block size
            LDY #0
            LDA [ZP.DB2], Y
            STA ZP.DB4  // Size low
            INY
            LDA [ZP.DB2], Y
            STA ZP.DB5  // Size high
            
            // Subtract 2 for the size header itself
            SEC
            LDA ZP.DB4
            SBC #2
            STA ZP.DB4
            LDA ZP.DB5
            SBC #0
            STA ZP.DB5
            
            // Now DB4/DB5 = actual node size
            // Check if node size > 256 + snName (265 bytes)
            // This would be unreasonable for a BASIC symbol
            LDA ZP.DB5
            if (NZ)  // High byte non-zero means size > 255
            {
                // Definitely corrupt - nodes shouldn't be this large
                NL();
                LDA #'N' COut();
                LDA #'B' COut();
                LDA #'S' COut();  // NBS = Node Block Size too large
                CLC  // Failed
                break;
            }
            
            // Also check if low byte > (256 - Objects.snName)
            // to ensure we can scan the name without Y wraparound
            LDA ZP.DB4
            CMP #(256 - Objects.snName)  // 247
            if (C)  // >= 247
            {
                // Block too large for safe name scanning
                NL();
                LDA #'N' COut();
                LDA #'B' COut();
                LDA #'S' COut();  // NBS = Node Block Size too large
                CLC  // Failed
                break;
            }
            
            // Now we can safely scan with Y register
            // Scan for null terminator starting at offset snName
            LDY #Objects.snName
            
            loop
            {
                // Check if Y reached node size
                CPY ZP.DB4  // Safe 8-bit comparison
                if (C)  // Y >= node size
                {
                    // Reached end without finding null
                    NL();
                    LDA #'N' COut();
                    LDA #'O' COut();
                    LDA #'V' COut();  // NOV = Name Overflow
                    CLC  // Failed
                    break;
                }
                
                // Check for null terminator
                LDA [ZP.IDX], Y
                if (Z)
                {
                    SEC  // Found null - valid name
                    break;
                }
                
                // Check for non-printable chars
                CMP #32
                if (NC)  // < 32
                {
                    NL();
                    LDA #'N' COut();
                    LDA #'B' COut();
                    LDA #'C' COut();  // NBC = Name Bad Char
                    CLC
                    break;
                }
                CMP #127
                if (C)  // >= 127
                {
                    NL();
                    LDA #'N' COut();
                    LDA #'B' COut();
                    LDA #'C' COut();  // NBC = Name Bad Char
                    CLC
                    break;
                }
                
                // Check reasonable name length (8-bit check)
                INY
                TYA
                SEC
                SBC #Objects.snName
                CMP #64  // Max 64 chars for name itself
                if (C)  // >= 64
                {
                    NL();
                    LDA #'N' COut();
                    LDA #'T' COut();
                    LDA #'L' COut();  // NTL = Name Too Long
                    CLC
                    break;
                }
            }
            
            break;  // Exit outer loop
        }
        
        PLY
    }   
    
    // Validate a heap-allocated array
    // Input: ZP.IDY = pointer to array
    // Output: C if valid array, NC if invalid
    validateArray()
    {
        PHY
        PHA
        
        // Save IDX since we'll use it
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop // Single exit pattern
        {
            // First check if it's a valid heap pointer
            isValidHeapPointer();
            if (NC)
            {
                NL();
                LDA #'A' COut();
                LDA #'B' COut();
                LDA #'H' COut();  // ABH = Array bad heap pointer
                
                Space(); XOut();
                
                LDY # Objects.snType // load
                LDA [ZP.IDX], Y
                BASICTypes.PrintType();
                
                CLC  // Not a valid heap allocation
                break;
            }
            
            // Validate element type at offset 2
            LDY # BASICArray.aiType
            LDA [ZP.IDY], Y
            
            // Check if it's a valid element type
            CMP #BASICType.INT
            if (Z) { SEC break; }
            CMP #BASICType.WORD
            if (Z) { SEC break; }
            CMP #BASICType.BYTE
            if (Z) { SEC break; }
            CMP #BASICType.CHAR
            if (Z) { SEC break; }
            CMP #BASICType.BIT
            if (Z) { SEC break; }
            
            // Invalid element type
            NL();
            LDA #'A' COut();
            LDA #'E' COut();
            LDA #'T' COut();  // AET = Array element type invalid
            Serial.HexOut();   // Show the invalid type
            CLC
            break;
        }
        
        // Restore IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLA
        PLY
    }
          
                
    // Validate variable list integrity
    validateVariableList()
    {
        // Get variable list head
        LDA ZP.VariablesList
        STA ZP.IDXL
        LDA ZP.VariablesList+1
        STA ZP.IDXH
        
        LDX #0 // Safety counter
        
        loop
        {
            // Check if we're at end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                SEC // Success - reached end normally
                break;
            }
            
            // Check if current node is valid heap pointer
            LDA ZP.IDXL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid variable node pointer
                NL();
                LDA #'V' COut();
                LDA #'L' COut();
                LDA #'!' COut();
                LDA ZP.IDXH HOut();
                LDA ZP.IDXL HOut();
                CLC // Failed
                break;
            }
            
            // Validate inline name
            validateInlineName();  // IDX already points to node
            if (NC)
            {
                // Invalid function name
                NL();
                LDA #'V' COut();
                LDA #'N' COut();
                LDA #'!' COut();
                CLC
                break;
            }
            
            // Check tokens pointer at offset 3-4
            LDY #Objects.snTokens
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            isValidHeapPointer();
            if (NC)
            {
                // Invalid tokens pointer
                NL();
                LDA #'V' COut();
                LDA #'T' COut();
                LDA #'!' COut();
                LDA ZP.IDYH HOut();
                LDA ZP.IDYL HOut();
                CLC // Failed
                break;
            }
            
            // Check if it's a STRING variable
            LDY # Objects.snType // load
            LDA [ZP.IDX], Y
            AND # BASICType.TYPEMASK
            CMP # BASICType.STRING
            if (Z)
            {
                // Get string pointer from value field
                LDY #Objects.snValue
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                // Check if null (which is legal)
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z)
                {
                    // Null string pointer - legal, skip validation
                }
                else
                {
                    // Validate as string
                    validateString();
                    if (NC)
                    {
                        NL();
                        LDA #'V' COut();
                        LDA #'S' COut();
                        LDA #'!' COut();  // VS! = Variable String
                        Space(); XOut();
                        CLC
                        break;
                    }
                }
            }
            
            
            // Check if it's an ARRAY variable  
            LDY # Objects.snType // load
            LDA [ZP.IDX], Y
            AND # BASICType.ARRAY
            if (NZ)
            {
                // Get array pointer from value field
                LDY #Objects.snValue
                LDA [ZP.IDX], Y
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                STA ZP.IDYH
                
                // Check if null
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z)
                {
                    // Null array pointer - not legal but could be intermediate stage
                }
                else
                {
                    // Validate as array
                    validateArray();
                    if (NC)
                    {
                        NL();
                        LDA #'V' COut();
                        LDA #'A' COut();
                        LDA #'!' COut();  // VA! = Variable Array invalid
                        LDA ZP.IDYH HOut();
                        LDA ZP.IDYL HOut();
                        CLC // Failed
                        break;
                    }
                }
            }
            
            
            
            // Get next pointer at offset 0-1
            LDY #0
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Safety check - prevent infinite loops
            INX
            CPX #0x80
            if (Z)
            {
                // Too many nodes
                NL();
                LDA #'V' COut();
                LDA #'#' COut();
                CLC // Failed
                break;
            }
        }
    }
    
    // Validate type byte for Variable/Constant nodes
    // Input: ZP.IDX = node address
    // Output: C if valid type, NC if invalid
    validateVariableType()
    {
        PHY
        PHA
        
        loop // Single exit pattern
        {
            // Get type byte
            LDY # Objects.snType // load
            LDA [ZP.IDX], Y
            
            // Check symbol type (high bits)
            AND #SymbolType.MASK
            CMP #SymbolType.VARIABLE
            if (Z)
            {
                // Valid VARIABLE type
            }
            else
            {
                CMP #SymbolType.CONSTANT
                if (NZ)
                {
                    // Not VARIABLE or CONSTANT
                    NL();
                    LDA #'V' COut();
                    LDA #'T' COut();
                    LDA #'!' COut();  // VT! = Variable Type invalid
                    CLC
                    break;
                }
            }
            
            // Check data type (low bits)
            LDY # Objects.snType // load
            LDA [ZP.IDX], Y
            AND #BASICType.MASK  // Gets type and VAR bit
            
            // Remove VAR bit to check base type
            PHA
            AND #BASICType.TYPEMASK
            
            loop
            {
                // Valid base types: INT, WORD, BYTE, CHAR, BIT, STRING
                CMP #BASICType.INT
                if (Z) { SEC break; }
                CMP #BASICType.WORD
                if (Z) { SEC break; }
                CMP #BASICType.BYTE
                if (Z) { SEC break; }
                CMP #BASICType.CHAR
                if (Z) { SEC break; }
                CMP #BASICType.BIT
                if (Z) { SEC break; }
                CMP #BASICType.STRING
                if (Z) { SEC break; }
                
                CLC // Invalid data type
                break;
            }
            PLA
            if (C)
            {
                break; // good type
            }
            
            // Invalid data type
            NL();
            LDA #'V' COut();
            LDA #'D' COut();
            LDA #'T' COut();  // VDT = Variable Data Type invalid
            CLC
            break;
        }
        
        PLA
        PLY
    }

    // Validate type byte for Argument/Local nodes
    // Input: ZP.IDX = node address
    // Output: C if valid type, NC if invalid
    validateArgumentType()
    {
        PHY
        PHA
        
        loop // Single exit pattern
        {
            // Get type byte
            LDY #Locals.lnType
            LDA [ZP.IDX], Y
            
            // Check symbol type (high bits)
            AND #SymbolType.MASK
            CMP #SymbolType.ARGUMENT
            if (Z)
            {
                // Valid ARGUMENT type
            }
            else
            {
                CMP #SymbolType.LOCAL
                if (NZ)
                {
                    // Not ARGUMENT or LOCAL
                    NL();
                    LDA #'A' COut();
                    LDA #'T' COut();
                    LDA #'!' COut();  // AT! = Argument Type invalid
                    CLC
                    break;
                }
            }
            
            // Check data type (low bits)
            LDY #Locals.lnType
            LDA [ZP.IDX], Y
            AND #BASICType.MASK  // Gets type and VAR bit
            
            // Remove VAR bit to check base type
            PHA
            AND #BASICType.TYPEMASK
            loop
            {
                // Valid base types: INT, WORD, BYTE, BIT, STRING
                CMP # BASICType.INT
                if (Z) { SEC break; }
                CMP # BASICType.WORD
                if (Z) { SEC break; }
                CMP # BASICType.BYTE
                if (Z) { SEC break; }
                CMP # BASICType.CHAR
                if (Z) { SEC break; }
                CMP # BASICType.BIT
                if (Z) { SEC break; }
                CMP # BASICType.STRING
                if (Z) { SEC break; }
                
                CLC // Invalid data type
                break;
            }
            PLA
            if (C)
            {
                break; // good type
            }
            // Invalid data type
            NL();
            LDA #'A' COut();
            LDA #'D' COut();
            LDA #'T' COut();  // ADT = Argument Data Type invalid
            CLC
            break;
        }
        
        PLA
        PLY
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
        
        Print.Space();
        CheckAndPrint();

#if defined(DEBUG) || defined(TRACE)
        Print.NewLine();
        DumpVariables();
        DumpZeroPage();
        DumpBuffers();
        DumpHeap();
#endif
        loop {}
    }
}
