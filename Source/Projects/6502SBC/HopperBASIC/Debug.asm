unit Debug
{
    // API Status: Clean  
    // All public methods preserve caller state except for documented outputs
    // Debug-only functionality with conditional compilation guards
    // Optimized for size using public/private pattern and DB0-DB15 slots
    
    uses "Tools"      // For convenience wrappers
    uses "BasicTypes" // For type printing
    
    // Debug strings - main headers
    const string debugVarsHeader = "\n== VARS ==\n";
    const string debugStackHeader = "\n== STACK ==\n";
    const string debugHeapHeader = "\n== HEAP DUMP ==\n";
    const string debugBasicHeader = "\n== BASIC BUFFERS ==\n";
    const string debugCrashHeader = "\n== CRASH ==\n";
    const string debugZeroPageHeader = "\n== ZERO PAGE ==\n";
    
    const string debugZeroBlock = "ZERO\n";
    const string debugEllipsis = "...\n";
    
    // Register and status labels
    const string regA = "A:";
    const string regX = "X:";
    const string regY = "Y:";
    const string regTOP = "TOP:";
    const string regNXT = "NEXT:";
    const string regACC = "ACC:";
    const string regIDX = "IDX:";
    const string regIDY = "IDY:";
    const string regSP = "SP:";
    const string regBP = "BP:";
    const string regPC = "PC:";
    const string regACCL = "ACCL:";
    const string regACCT = "ACCT:";
    
    const string regCSP = "CSP:";
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
    const string frameMarkerPrefix = "\n--- Frame ";
    const string frameMarkerSuffix = " ---";
    const string argMarker = " (arg)";
    const string localMarker = " (local)";
    
    // Status indicators
    const string statusFree = "FREE";
    const string statusUsed = "USED";
    const string statusC = "C ";
    const string statusNC = "NC ";
    const string statusZ = "Z ";
    const string statusNZ = "NZ ";
    
    // Buffer labels
    const string basicInputBufferLabel = "\nInputBuffer (InBufLen:";
    const string basicTokenizerBufferLabel = "\nTokenizerBuffer (TokPos:";
    const string basicTokBufLenLabel = " TokBufLen:";
    const string basicCurTokLabel = " CurTok:";
    const string basicOpCodeBufferLabel = "\nOpCodeBuffer (OpCodeLen:";
    const string basicPCLabel = " PC:";
    const string basicBufferSuffix = ") - First 64 bytes:\n";
    const string basicErrorLabel = "\nLastError: ";
    
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
    
    // Print label string from DB0/DB1
    printLabel()
    {
        LDY #0
        loop
        {
            LDA [ZP.DB0], Y
            if (Z) { break; }
            Serial.WriteChar();
            INY
        }
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
    
    // === Private output methods (use DB slots, no preservation) ===
    
    xOut()  // Output IDX register
    {
        LDA #(regIDX % 256)
        STA ZP.DB0
        LDA #(regIDX / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.IDXH
        hOut();
        LDA ZP.IDXL
        hOut();
        space();
    }
    
    yOut()  // Output IDY register
    {
        LDA #(regIDY % 256)
        STA ZP.DB0
        LDA #(regIDY / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.IDYH
        hOut();
        LDA ZP.IDYL
        hOut();
        space();
    }
    
    aOut()  // Output ACC register
    {
        LDA #(regACC % 256)
        STA ZP.DB0
        LDA #(regACC / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.ACCH
        hOut();
        LDA ZP.ACCL
        hOut();
        space();
    }
    
    atOut()  // Output ACCT register
    {
        LDA #(regACCT % 256)
        STA ZP.DB0
        LDA #(regACCT / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.ACCT
        hOut();
        space();
    }
    
    nOut()  // Output NEXT register with type
    {
        LDA #(regNXT % 256)
        STA ZP.DB0
        LDA #(regNXT / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.NEXTT
        Tools.PrintType();
        LDA #'-'
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
        STA ZP.DB0
        LDA #(regTOP / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.TOPT
        Tools.PrintType();
        LDA #'-'
        cOut();
        LDA ZP.TOPH
        hOut();
        LDA ZP.TOPL
        hOut();
        space();
    }
    
    pcOut()  // Output PC register
    {
        LDA #(regPC % 256)
        STA ZP.DB0
        LDA #(regPC / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.PCH
        hOut();
        LDA ZP.PCL
        hOut();
        space();
    }
    
    bpOut()  // Output BP register
    {
        LDA #(regBP % 256)
        STA ZP.DB0
        LDA #(regBP / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.BP
        hOut();
        space();
    }
    
    spOut()  // Output SP register
    {
        LDA #(regSP % 256)
        STA ZP.DB0
        LDA #(regSP / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.SP
        hOut();
        space();
    }
    
    alOut()  // Output ACCL register
    {
        LDA ZP.ACCL
        STA ZP.DB15  // Save value to print
        
        LDA #(regACCL % 256)
        STA ZP.DB0
        LDA #(regACCL / 256)
        STA ZP.DB1
        printLabel();
        
        LDA ZP.DB15
        hOut();
        space();
    }
    
    cfOut()  // Output carry flag status
    {
        if (C)
        {
            LDA #(statusC % 256)
            STA ZP.DB0
            LDA #(statusC / 256)
            STA ZP.DB1
        }
        else
        {
            LDA #(statusNC % 256)
            STA ZP.DB0
            LDA #(statusNC / 256)
            STA ZP.DB1
        }
        printLabel();
    }
    
    zfOut()  // Output zero flag status
    {
        if (Z)
        {
            LDA #(statusZ % 256)
            STA ZP.DB0
            LDA #(statusZ / 256)
            STA ZP.DB1
        }
        else
        {
            LDA #(statusNZ % 256)
            STA ZP.DB0
            LDA #(statusNZ / 256)
            STA ZP.DB1
        }
        printLabel();
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
            LDA #'{'
            cOut();
            LDA #'E'
            cOut();
            PLA
            hOut();  // Show checkpoint ID where error was first detected
            PHA
            LDA #'}'
            cOut();
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
        STA ZP.ACCL
        LDA #(debugVarsHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // A register
        LDA #(regA % 256)
        STA ZP.DB0
        LDA #(regA / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.DB7
        hOut();
        space();
        
        // X register
        LDA #(regX % 256)
        STA ZP.DB0
        LDA #(regX / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.DB8
        hOut();
        space();
        
        // Y register
        LDA #(regY % 256)
        STA ZP.DB0
        LDA #(regY / 256)
        STA ZP.DB1
        printLabel();
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
        STA ZP.DB0
        LDA #(listIT / 256)
        STA ZP.DB1
        printLabel();
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
        LDA #':'
        cOut();
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
        space();
        space();
        
        LDX ZP.DB5  // Number of chars on this row
        
        SEC
        LDA #16
        SBC ZP.DB5
        if (NZ)
        {
            CMP #8
            if (C)
            {
                space();
            }
            PHA
            TAX
            CPX #0
            if (NZ)
            {
                loop
                {
                    space();
                    space();
                    space();
                    DEX
                    if (Z) { break; }
                }
            }
            PLA
        }
        
        // Restore Y to start of row
        LDY ZP.DB4  // Start of row position
        LDX ZP.DB5  // Number of chars to print
        loop
        {
            CPX #0
            if (Z) { break; }
            
            LDA [ZP.DB0], Y
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (C)  // > 127
                {
                    LDA #'.'
                }
            }
            else
            {
                LDA #'.'
            }
            Serial.WriteChar();
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
        STZ ZP.DB4
        STZ ZP.DB5  // Bytes printed on this row
        LDY #0      // Current position
        LDX #64     // Max bytes to output
        
        loop
        {
            // Check if we've printed 64 bytes
            CPX #0
            if (Z) { break; }  // Max bytes limit
            
            // Check if we've printed all content bytes
            LDA ZP.DB2
            ORA ZP.DB3
            if (Z) 
            { 
                // No more content, dump any partial row
                TYA
                AND #0x0F
                if (NZ)  // Partial row exists
                {
                    dumpBlockAscii();
                }
                break; 
            }
            
            CPY #0
            if (NZ)  // Not first line
            {
                TYA
                AND #0x0F
                if (Z)
                {
                    dumpBlockAscii();
                    dumpBlockAddress();
                    STY ZP.DB4
                }
            }
            
            TYA
            AND #0x07
            if (Z)
            {
                space();  // Column space
            }
            
            space();
            LDA [ZP.DB0], Y
            hOut();
            INC ZP.DB5
            INY
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
        STA ZP.ACCL
        LDA #(debugHeapHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA #(listVL % 256)
        STA ZP.ACCL
        LDA #(listVL / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.VariablesListH
        hOut();
        LDA ZP.VariablesListL
        hOut();
        space();
        
        LDA #(listFL % 256)
        STA ZP.ACCL
        LDA #(listFL / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
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
                STA ZP.ACCL
                LDA #(debugZeroBlock / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                break;
            }
            
            // Print block number
            LDA #'['
            cOut();
            LDA ZP.DB12  // Use DB12 instead of X
            hOut();
            LDA #']'
            cOut();
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
            LDA #':'
            cOut();
            
            // Print block size
            LDA ZP.DB3
            hOut();
            LDA ZP.DB2
            hOut();
            
            // Check if on free list
            space();
            LDA #'('
            cOut();
            
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
                STA ZP.ACCL
                LDA #(statusFree / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
            }
            else
            {
                LDA #(statusUsed % 256)
                STA ZP.ACCL
                LDA #(statusUsed / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
            }
            
            LDA #')'
            cOut();
            
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
                STA ZP.ACCL
                LDA #(debugEllipsis / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
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
            ASL ASL ASL ASL
            NOP
            hOut();
            LDA #':'
            cOut();
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
                
                // Check if printable
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        cOut();
                        INY
                        continue;
                    }
                }
                
                // Not printable
                LDA #'.'
                cOut();
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
        LDA #(debugBasicHeader % 256)
        STA ZP.ACCL
        LDA #(debugBasicHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // InputBuffer
        LDA #(basicInputBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicInputBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.BasicInputLength
        hOut();
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump input buffer
        LDA #(Address.BasicInputBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicInputBuffer >> 8)
        STA ZP.DB1
        dumpMemoryBlock();
        
        // TokenizerBuffer
        LDA #(basicTokenizerBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicTokenizerBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.TokenizerPosH
        hOut();
        LDA ZP.TokenizerPosL
        hOut();
        
        LDA #(basicTokBufLenLabel % 256)
        STA ZP.ACCL
        LDA #(basicTokBufLenLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.TokenBufferLengthH
        hOut();
        LDA ZP.TokenBufferLengthL
        hOut();
        
        LDA #(basicCurTokLabel % 256)
        STA ZP.ACCL
        LDA #(basicCurTokLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.CurrentToken
        hOut();
        
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump tokenizer buffer
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicTokenizerBuffer >> 8)
        STA ZP.DB1
        dumpMemoryBlock();
        
        // OpCodeBuffer
        LDA #(basicOpCodeBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicOpCodeBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.OpCodeBufferLengthH
        hOut();
        LDA ZP.OpCodeBufferLengthL
        hOut();
        
        LDA #(basicPCLabel % 256)
        STA ZP.ACCL
        LDA #(basicPCLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.PCH
        hOut();
        LDA ZP.PCL
        hOut();
        
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump opcode buffer
        LDA #(Address.BasicOpCodeBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicOpCodeBuffer >> 8)
        STA ZP.DB1
        dumpMemoryBlock();
        
        // Error pointers
        LDA #(basicErrorLabel % 256)
        STA ZP.ACCL
        LDA #(basicErrorLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.LastErrorH
        hOut();
        LDA ZP.LastErrorL
        hOut();
        nL();
    }
    
    dumpMemoryBlock()  // DB0/DB1 = address
    {
        // Print 4 lines of 16 bytes each (64 bytes total)
        LDX #0  // Line counter
        
        loop
        {
            CPX #4
            if (Z) { break; }
            
            // Print address
            LDA ZP.DB1
            hOut();
            LDA ZP.DB0
            hOut();
            LDA #':'
            cOut();
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
                
                // Check if printable
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        cOut();
                        INY
                        continue;
                    }
                }
                
                // Not printable
                LDA #'.'
                cOut();
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
        }
    }
    
    findFunctionByAddress()  // ACC = search address, returns TOP = name, C if found
    {
        // Save state we'll modify
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
                Functions.GetName();
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
    }
    
    dumpStack()
    {
        // Main header
        LDA #(debugStackHeader % 256)
        STA ZP.ACCL
        LDA #(debugStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Stack pointers
        LDA #(regCSP % 256)
        STA ZP.DB0
        LDA #(regCSP / 256)
        STA ZP.DB1
        printLabel();
        LDA ZP.CSP
        hOut();
        space();
        
        spOut();
        bpOut();
        nL();
        
        // Call Stack
        LDA #(callStackHeader % 256)
        STA ZP.ACCL
        LDA #(callStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.CSP
        hOut();
        LDA #(framesSuffix % 256)
        STA ZP.ACCL
        LDA #(framesSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Current frame
        LDA #(framePrefix % 256)
        STA ZP.ACCL
        LDA #(framePrefix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA #'0'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        space();
        LDA #(framePC % 256)
        STA ZP.ACCL
        LDA #(framePC / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.PCH
        hOut();
        LDA ZP.PCL
        hOut();
        
        // Look up current function
        LDA ZP.PCL
        STA ZP.ACCL
        LDA ZP.PCH
        STA ZP.ACCH
        findFunctionByAddress();
        if (C)
        {
            space();
            LDA #'('
            Serial.WriteChar();
            Tools.PrintStringTOP();
            LDA #')'
            Serial.WriteChar();
        }
        
        LDA #(frameBP % 256)
        STA ZP.ACCL
        LDA #(frameBP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.BP
        hOut();
        LDA #(currentFrameMarker % 256)
        STA ZP.ACCL
        LDA #(currentFrameMarker / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        nL();
        
        // Walk call stack
        LDX ZP.CSP
        LDY #0  // Frame counter
        
        loop
        {
            CPX #2
            if (NC) { break; }
            
            DEX
            
            TXA
            AND #1
            if (NZ)  // Odd = return address
            {
                // Print frame info
                LDA #(framePrefix % 256)
                STA ZP.ACCL
                LDA #(framePrefix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                TYA
                hOut();
                LDA #(framePC % 256)
                STA ZP.ACCL
                LDA #(framePC / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                
                DEX  // Move to return address
                
                // Load return address
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
                findFunctionByAddress();
                if (C)
                {
                    space();
                    LDA #'('
                    Serial.WriteChar();
                    Tools.PrintStringTOP();
                    LDA #')'
                    Serial.WriteChar();
                }
                
                // Print BP
                LDA #(frameBP % 256)
                STA ZP.ACCL
                LDA #(frameBP / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                INX
                LDA Address.CallStackLSB, X
                hOut();
                
                nL();
                INY
            }
        }
        
        nL();
        
        // Value Stack
        LDA #(valueStackHeader % 256)
        STA ZP.ACCL
        LDA #(valueStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.SP
        hOut();
        LDA #(entriesSuffix % 256)
        STA ZP.ACCL
        LDA #(entriesSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
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
                STA ZP.ACCL
                LDA #(stackEllipsis / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                break;
            }
            
            DEX
            
            // Check for frame boundary
            TXA
            CMP ZP.BP
            if (Z)
            {
                // Frame boundary marker
                LDA #(frameMarkerPrefix % 256)
                STA ZP.ACCL
                LDA #(frameMarkerPrefix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                LDA ZP.DB7
                hOut();
                LDA #(frameMarkerSuffix % 256)
                STA ZP.ACCL
                LDA #(frameMarkerSuffix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                INC ZP.DB7
            }
            
            // Print entry
            TXA
            hOut();
            LDA #':'
            cOut();
            space();
            
            // Type and value
            LDA Address.TypeStackLSB, X
            Tools.PrintType();
            LDA #'-'
            cOut();
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
                STA ZP.ACCL
                LDA #(bpMarker / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
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
                    STA ZP.ACCL
                    LDA #(returnSlotMarker / 256)
                    STA ZP.ACCH
                    Tools.PrintStringACC();
                }
                else
                {
                    SEC
                    TXA
                    SBC ZP.BP
                    if (NC)
                    {
                        LDA #(argMarker % 256)
                        STA ZP.ACCL
                        LDA #(argMarker / 256)
                        STA ZP.ACCH
                        Tools.PrintStringACC();
                    }
                    else
                    {
                        LDA #(localMarker % 256)
                        STA ZP.ACCL
                        LDA #(localMarker / 256)
                        STA ZP.ACCH
                        Tools.PrintStringACC();
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
        STA ZP.ACCL
        LDA #(debugZeroPageHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
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
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        dumpIterationState();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
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
    
    DumpBasicBuffers()
    {
        PHP PHA PHX PHY
        
        // Save state
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        dumpBasicBuffers();
        
        // Restore state
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY PLX PLA PLP
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
            if (C) { break; }
            
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
                break;
            }
            
            // Calculate block address
            CLC
            LDA ZP.IDXL
            ADC #2
            STA ZP.DB2
            LDA ZP.IDXH
            ADC #0
            STA ZP.DB3
            
            // Check if on free list
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Walk free list
            LDA ZP.FREELISTL
            STA ZP.IDYL
            LDA ZP.FREELISTH
            STA ZP.IDYH
            
            STZ ZP.DB7  // Free flag
            
            loop
            {
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z) { break; }
                
                // Compare addresses
                LDA ZP.IDYL
                CMP ZP.DB2
                if (NZ)
                {
                    // Get next
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    
                    LDY #2
                    LDA [ZP.IDX], Y
                    STA ZP.IDYL
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDYH
                    continue;
                }
                
                LDA ZP.IDYH
                CMP ZP.DB3
                if (NZ)
                {
                    // Get next
                    LDA ZP.IDYL
                    STA ZP.IDXL
                    LDA ZP.IDYH
                    STA ZP.IDXH
                    
                    LDY #2
                    LDA [ZP.IDX], Y
                    STA ZP.IDYL
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDYH
                    continue;
                }
                
                // Found on free list
                LDA #1
                STA ZP.DB7
                break;
            }
            
            // Restore position
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // Move to next block
            CLC
            LDA ZP.IDXL
            ADC ZP.DB0
            STA ZP.IDXL
            LDA ZP.IDXH
            ADC ZP.DB1
            STA ZP.IDXH
            
            INX
            CPX #100
            if (Z) { break; }
        }
        
        CheckError();
        
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
    
    // Crash handler
    Crash()
    {
        Serial.HexOut();
        
        TSX PHX
        PHA
        
        Tools.NL();
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        LDA #(debugCrashHeader % 256)
        STA ZP.ACCL
        LDA #(debugCrashHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.ACCH
        
        PLA
        Serial.HexOut();
        LDA #' '
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA #' '
        Tools.COut();
        PLA
        Serial.HexOut();
        
        LDA #' '
        Tools.COut();
        CheckAndPrint();

#if defined(DEBUG) || defined(TRACE)
        Tools.NL();
        DumpVariables();
        DumpZeroPage();
        DumpBasicBuffers();
        DumpHeap();
#endif
        loop {}
    }
}