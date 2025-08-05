unit Debug
{
    // API Status: Clean  
    // All public methods preserve caller state except for documented outputs
    // Debug-only functionality with conditional compilation guards
    
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

    // Convenience wrappers - allow existing debug code to work unchanged
    // Input: A = character to output / None for newline
    // Output: Character/newline printed to serial
    // Preserves: Everything (delegates to Tools methods)
    COut() { Tools.COut(); }
    NL() { Tools.NL(); }
    
    // Common helper for single space
    // Input: None
    // Output: Single space printed to serial
    // Modifies: A, Z
    Space()
    {
        PHA LDA #' ' Tools.COut(); PLA
    }
    
    // Print null-terminated string to serial output
    // Input: ZP.IDX = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    printStringIDX()
    {
        PHP
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.IDX], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Tools.COut();   // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
        PLP
    }

    // Print string constant using ACC register - UNUSED, removing
    // Input: String constant address
    // Output: String printed to serial
    // Preserves: Everything
    // printString() - REMOVED, was unused

    // Dump key zero page variables for debugging
    // Input: None
    // Output: Key registers and variables printed to serial in formatted layout
    // Preserves: Everything (uses temporary ZP.DB7-U4 for state preservation)
    DumpVariables()
    {
       PHP  // Save flags
       
       // Store registers in zero page temporarily so we can display them
       STA ZP.DB7  // Temporarily store A
       STX ZP.DB9  // Temporarily store X
       STY ZP.DB10  // Temporarily store Y
       
       LDA ZP.IDXL
       STA ZP.DB11
       LDA ZP.IDXH
       STA ZP.DB12
       LDA ZP.ACCL
       STA ZP.DB13
       LDA ZP.ACCH
       STA ZP.DB14
       
       
       LDA #(debugVarsHeader % 256)
       STA ZP.ACCL
       LDA #(debugVarsHeader / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       
       // A register
       LDA #(regA % 256)
       STA ZP.ACCL
       LDA #(regA / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.DB7
       Serial.HexOut();
       Space();
       
       // X register
       LDA #(regX % 256)
       STA ZP.ACCL
       LDA #(regX / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.DB9
       Serial.HexOut();
       Space();
       
       // Y register
       LDA #(regY % 256)
       STA ZP.ACCL
       LDA #(regY / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.DB10
       Serial.HexOut();
       Space();
       
       // TOP with type
       LDA #(regTOP % 256)
       STA ZP.ACCL
       LDA #(regTOP / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.TOPT
       Tools.PrintType();
       LDA #'-'
       Tools.COut();
       LDA ZP.TOPH
       Serial.HexOut();
       LDA ZP.TOPL
       Serial.HexOut();
       Space();
       
       // NEXT with type
       LDA #(regNXT % 256)
       STA ZP.ACCL
       LDA #(regNXT / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.NEXTT
       Tools.PrintType();
       LDA #'-'
       Tools.COut();
       LDA ZP.NEXTH
       Serial.HexOut();
       LDA ZP.NEXTL
       Serial.HexOut();
       Space();
       
       // ACC with type
       LDA #(regACC % 256)
       STA ZP.ACCL
       LDA #(regACC / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.ACCT
       Tools.PrintType();
       LDA #'-'
       Tools.COut();
       LDA ZP.DB14
       Serial.HexOut();
       LDA ZP.DB13
       Serial.HexOut();
       Space();
       
       // IDX
       LDA #(regIDX % 256)
       STA ZP.ACCL
       LDA #(regIDX / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.DB12
       Serial.HexOut();
       LDA ZP.DB11
       Serial.HexOut();
       Space();
       
       // IDY
       LDA #(regIDY % 256)
       STA ZP.ACCL
       LDA #(regIDY / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.IDYH
       Serial.HexOut();
       LDA ZP.IDYL
       Serial.HexOut();
       Space();
       
       // Stack pointer
       LDA #(regSP % 256)
       STA ZP.ACCL
       LDA #(regSP / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.SP
       Serial.HexOut();
       
       NL();
       
       LDA ZP.DB14
       STA ZP.ACCL
       LDA ZP.DB13
       STA ZP.ACCH
       LDA ZP.DB12
       STA ZP.IDXH
       LDA ZP.DB11
       STA ZP.IDXL
       
       
       // Restore registers
       LDY ZP.DB10  // Restore Y
       LDX ZP.DB9  // Restore X
       LDA ZP.DB7  // Restore A
       
       PLP  // Restore flags
    }

    // Debug output for iteration state
    // Input: None
    // Output: Current IDX pointer printed to serial for iteration tracking
    // Preserves: Everything (minimal impact for safe use during iteration)
    DumpIterationState()
    {
        PHP  // Save flags
        PHA
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(listIT % 256)
        STA ZP.ACCL
        LDA #(listIT / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.IDXH
        Serial.HexOut();
        LDA ZP.IDXL
        Serial.HexOut();
        Space();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        PLP  // Restore flags
    }
    
    Spaces()
    {
        CMP # 0
        if (NZ)
        {
            loop
            {
                Space();
                DEC
                if (Z) { break; }
            }
        }
    }
    
    dumpBlockAscii()
    {
        PHX
        PHY
        Space();Space();
        
        LDX ZP.DB5  // number of chars on this row     
        
        SEC
        LDA #16
        SBC ZP.DB5
        if (NZ)
        {
            CMP #8
            if (C)
            {
                Space();
            }
            PHA Spaces(); PLA
            PHA Spaces(); PLA
            Spaces();
        }
        
        LDA ZP.DB4 // current index of start of row
        TAY 
        loop
        {
            LDA [ZP.IDX], Y
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
            if (Z) { break; }
        }
        PLY
        PLX
    }
    
    // Internal heap dump implementation
    // Input: None
    // Output: Detailed heap block analysis with hex/ASCII dump (up to 64 bytes per block)
    // Modifies: ZP.DB0-DB7, ZP.IDX, ZP.IDY, A, X, Y (internal operations)
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
        Serial.HexOut();
        LDA ZP.VariablesListL
        Serial.HexOut();
        Space();
        
        LDA #(listFL % 256)
        STA ZP.ACCL
        LDA #(listFL / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.FunctionsListH
        Serial.HexOut();
        LDA ZP.FunctionsListL
        Serial.HexOut();
        NL();
        
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
            STA ZP.DB0
            INY
            LDA [ZP.IDX], Y     // High byte of size
            STA ZP.DB1
            ORA ZP.DB0           // Check if size is zero
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
            Tools.COut();
            TXA
            Serial.HexOut();
            LDA #']'
            Tools.COut();
            Space();
            
            // Print Alloc address (block address + 2) - using A instead of DB2
            CLC
            LDA ZP.IDXL
            ADC #2
            PHA         // Save low byte on stack
            LDA ZP.IDXH
            ADC #0
            
            Serial.HexOut();
            PLA         // Get low byte from stack
            Serial.HexOut();
            LDA #':'
            Tools.COut();
            
            // Print block size (from header)
            LDA ZP.DB1  // High byte
            Serial.HexOut();
            LDA ZP.DB0  // Low byte
            Serial.HexOut();
            
            // Check if this block is on the free list
            Space();
            LDA #'('
            Tools.COut();
            
            // Save current position on stack (the header address)
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA

            // Walk free list to see if this block is free
            LDA ZP.FREELISTL
            STA ZP.IDYL
            LDA ZP.FREELISTH
            STA ZP.IDYH

            STZ ZP.DB7  // Flag: 0 = not found, 1 = found on free list

            loop
            {
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z) { break; }  // End of free list
                
                // Compare addresses - both are headers, no adjustment needed!
                // First compare low bytes
                TSX
                INX
                INX         // Adjust to point to saved IDXL (stack+2)
                LDA HardwareStack, X
                CMP ZP.IDYL
                if (Z)      // Low bytes match, now check high bytes
                {
                    TSX
                    INX     // Adjust to point to saved IDXH (stack+1) - NOT stack+3!
                    LDA HardwareStack, X
                    CMP ZP.IDYH
                    if (Z)  // High bytes also match - found it!
                    {
                        // Found it - this block is free
                        LDA #1
                        STA ZP.DB7
                        
                        // DEBUG: Found free block
                        break;
                    }
                }
                
                // No match - move to next free block
                // IDY points to the header, next pointer is at offset 2
                LDY #2
                LDA [ZP.IDY], Y
                PHA         // Save next low
                INY
                LDA [ZP.IDY], Y
                STA ZP.IDYH
                PLA
                STA ZP.IDYL
            }

            // Print status based on flag
            LDA ZP.DB7
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
            Tools.COut();
                                
            // Restore current position from stack
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
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
            SEC
            LDA ZP.DB0
            SBC #2
            STA ZP.DB2  // Content size low byte
            LDA ZP.DB1
            SBC #0
            STA ZP.DB3  // Content size high byte
            
            LDA ZP.DB7
            if (Z) // USED
            {
                // IDX - points to content (can be munted)
                // DB2 - size LSB
                // DB3 - size MSB 
                dumpBlockContent();
            }
                                                
            NL();  // End this block's output
            
            // Restore header position to move to next block
            SEC
            LDA ZP.IDXL
            SBC #2
            STA ZP.IDXL
            if (NC)
            {
                DEC ZP.IDXH
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
            CPX #20
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
        
        NL();
        
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

    dumpBlockAddress()
    {
        NL();
        LDA # 5 Spaces();
        CLC
        TYA
        ADC ZP.IDXL
        PHA         // Push low byte of address for later
        LDA #0
        ADC ZP.IDXH
        HOut();
        PLA         // Pull low byte of address
        HOut();
        LDA # ':' COut(); LDA # 11 Spaces();
        STZ ZP.DB5 // number of bytes on this row zero now
    }
    
    // IDX - points to content (can be munted)
    // DB2 - size LSB
    // DB3 - size MSB 
    dumpBlockContent()
    {
        PHX
        PHY
        
        STZ ZP.DB4
        STZ ZP.DB5 // number of bytes printed on this row
        LDY # 0   // current position
        LDX # 64  // max bytes to output
        loop
        {
            CPY #0
            if (NZ) // not first line
            {
                TYA
                AND # 0x0F
                if (Z)
                {
                    dumpBlockAscii();
                }
                CPX #0
                if (Z)
                {
                    break; // limit of max bytes
                }
                
                TYA
                AND # 0x0F
                if (Z)
                {
                    dumpBlockAddress(); 
                    STY ZP.DB4
                }
            }
            TYA
            AND # 0x07
            if (Z)
            {
                Space(); // column space   
            }
            Space(); LDA [ZP.IDX], Y Debug.HOut();
            INC ZP.DB5
            INY
            DEX
            LDA ZP.DB2 // LSB
            if (Z)
            {
                LDA ZP.DB3
                if (Z)
                {
                    dumpBlockAscii(); // partial row
                    
                    break; // ran out of bytes
                }
                DEC ZP.DB3 // MSB
            }
            DEC ZP.DB2 // LSB
        } // loop
        PLY
        PLX
    }

    
    // Dump heap with state preservation for debugging
    // Input: None
    // Output: Complete heap analysis with allocation status printed to serial
    // Modifies: ZP.M* scratch space (internal to heap analysis operations)
    // Preserves: All iteration-critical state (ZP.ACC, ZP.LCURRENT)
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
    
    
    
    
    
    
    // Dump a 256-byte page in hex+ASCII format for debugging
    // Input: A = page number (high byte of address)
    // Output: Complete page contents printed in 16 lines of 16 bytes each
    // Modifies: ZP.DB0, ZP.DB1 (internal operations)
    // Preserves: All other state
    DumpPage()
    {
        PHA
        PHX
        PHY
        
        // Set up M0/M1 to point to the page (preserve IDX)
        STA ZP.DB1   // Page number in high byte
        STZ ZP.DB0   // Start at offset 0
        
        // Print 16 lines of 16 bytes each
        LDX # 0  // Line counter (0-15)
        
        loop
        {
            // Print address for this line (page:offset)
            LDA ZP.DB1
            Serial.HexOut();
            
            TXA
            ASL ASL ASL ASL
            NOP
            Serial.HexOut();
            LDA #':'
            Tools.COut();
            Space();
            
            // Print 16 hex bytes with space after each
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                Serial.HexOut();
                Space();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    Space();
                }
            }
            
            // Add spacing before ASCII dump
            Space();
            Space();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                
                // Check if printable (32-127)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        Tools.COut();  // Print the character
                        INY
                        continue;
                    }
                }
                
                // Not printable, print dot
                LDA #'.'
                Tools.COut();
                INY
            }
            
            NL();
            
            // Move to next line (add 16 to M0/M1)
            CLC
            LDA ZP.DB0
            ADC #16
            STA ZP.DB0
            if (C)
            {
                INC ZP.DB1  // This shouldn't happen within a page
            }
            
            INX
            CPX #16
            if (Z) { break; }  // Done with all 16 lines
        }
        
        PLY
        PLX
        PLA
    }

    // Dump the BASIC input and tokenizer buffers for debugging
    // Input: None
    // Output: Complete buffer state and contents printed to serial
    // Preserves: ZP.IDX and all critical state (saves/restores modified registers)
    DumpBasicBuffers()
    {
        PHP  // Save flags
        PHA  // Save A
        PHX  // Save X  
        PHY  // Save Y
        
        // Save all ZP variables that we or called functions modify
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(debugBasicHeader % 256)
        STA ZP.ACCL
        LDA #(debugBasicHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // InputBuffer (InBufLen:XX) - First 64 bytes:
        LDA #(basicInputBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicInputBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.BasicInputLength  // 0x30
        Serial.HexOut();
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump input buffer (first 64 bytes)
        LDA #(Address.BasicInputBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicInputBuffer >> 8)
        STA ZP.DB1
        DumpMemoryBlock();
        
        // TokenizerBuffer (TokPos:XXXX TokBufLen:XXXX CurTok:XX) - First 64 bytes:
        LDA #(basicTokenizerBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicTokenizerBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.TokenizerPosH     // 0x34
        Serial.HexOut();
        LDA ZP.TokenizerPosL     // 0x33
        Serial.HexOut();
        
        LDA #(basicTokBufLenLabel % 256)
        STA ZP.ACCL
        LDA #(basicTokBufLenLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.TokenBufferLengthH
        Serial.HexOut();
        LDA ZP.TokenBufferLengthL
        Serial.HexOut();
        
        LDA #(basicCurTokLabel % 256)
        STA ZP.ACCL
        LDA #(basicCurTokLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.CurrentToken      // 0x37
        Serial.HexOut();
        
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump tokenizer buffer (first 64 bytes)
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicTokenizerBuffer >> 8)
        STA ZP.DB1
        DumpMemoryBlock();
        
        // OpCodeBuffer (OpCodeLen:XXXX PC:XXXX) - First 64 bytes:
        LDA #(basicOpCodeBufferLabel % 256)
        STA ZP.ACCL
        LDA #(basicOpCodeBufferLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.OpCodeBufferLengthH    // 0x3B
        Serial.HexOut();
        LDA ZP.OpCodeBufferLengthL    // 0x3A
        Serial.HexOut();
        
        LDA #(basicPCLabel % 256)
        STA ZP.ACCL
        LDA #(basicPCLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        LDA ZP.PCH    // 0x01
        Serial.HexOut();
        LDA ZP.PCL    // 0x00
        Serial.HexOut();
        
        LDA #(basicBufferSuffix % 256)
        STA ZP.ACCL
        LDA #(basicBufferSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Dump opcode buffer (first 64 bytes)
        LDA #(Address.BasicOpCodeBuffer & 0xFF)
        STA ZP.DB0
        LDA #(Address.BasicOpCodeBuffer >> 8)
        STA ZP.DB1
        DumpMemoryBlock();
        
        // Show Error pointers if set
        LDA #(basicErrorLabel % 256)
        STA ZP.ACCL
        LDA #(basicErrorLabel / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.LastErrorH
        Serial.HexOut();
        LDA ZP.LastErrorL
        Serial.HexOut();
        NL();
        
        // Restore all ZP variables in reverse order
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.ACCH

        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
        PLP  // Restore flags
    }
    
    // Dump a 64-byte memory block in hex+ASCII format for debugging
    // Input: ZP.DB0/ZP.DB1 = start address to dump
    // Output: 64 bytes (4 lines of 16 bytes) printed in hex+ASCII format
    // Modifies: A, X, Y (internal operations)
    // Preserves: ZP.DB0/ZP.DB1 (start address preserved)
    DumpMemoryBlock()
    {
        PHA
        PHX
        PHY
        
        // Save original address
        LDA ZP.DB0
        PHA
        LDA ZP.DB1
        PHA
        
        // Print 4 lines of 16 bytes each (64 bytes total)
        LDX #0  // Line counter (0-3)
        
        loop
        {
            CPX #4
            if (Z) { break; }  // Done with 4 lines
            
            // Print address for this line
            LDA ZP.DB1
            Serial.HexOut();
            LDA ZP.DB0
            Serial.HexOut();
            LDA #':'
            Tools.COut();
            Space();
            
            // Print 16 hex bytes with space after each
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                Serial.HexOut();
                Space();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    Space();
                }
            }
            
            // Add spacing before ASCII dump
            Space();
            Space();
            
            // Print 16 ASCII characters
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.DB0], Y
                
                // Check if printable (32-127)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #127
                    if (NC)  // <= 127
                    {
                        Tools.COut();  // Print the character
                        INY
                        continue;
                    }
                }
                
                // Not printable, print dot
                LDA #'.'
                Tools.COut();
                INY
            }
            
            NL();
            
            // Move to next line (add 16 to address)
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
        
        // Restore original address
        PLA
        STA ZP.DB1
        PLA
        STA ZP.DB0
        
        PLY
        PLX
        PLA
    }

    // Output hex byte preserving carry flag  
    // Input: A = byte to output as hex
    // Output: Hex byte printed to serial
    // Preserves: Everything (flags and registers)
    HOut()
    {
        PHP  // Push processor status (including carry flag)
        Serial.HexOut();
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output IDX register as "IDX:hhll "
    // Input: None (uses ZP.IDX)
    // Output: IDX value printed to serial with label
    // Preserves: Everything
    XOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regIDX % 256)
        STA ZP.ACCL
        LDA #(regIDX / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.IDXH
        Serial.HexOut();
        LDA ZP.IDXL
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output IDY register as "IDY:hhll "
    // Input: None (uses ZP.IDY)
    // Output: IDY value printed to serial with label
    // Preserves: Everything
    YOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regIDY % 256)
        STA ZP.ACCL
        LDA #(regIDY / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.IDYH
        Serial.HexOut();
        LDA ZP.IDYL
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
       
    // Output ACC register as "ACC:hhll "
    // Input: None (uses ZP.ACC)
    // Output: ACC value printed to serial with label
    // Preserves: Everything
    AOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regACC % 256)
        STA ZP.ACCL
        LDA #(regACC / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.ACCH
        Serial.HexOut();
        LDA ZP.ACCL
        Serial.HexOut();
        Space();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ACCT register as "ACCT:ll "
    // Input: None (uses ZP.ACCT)
    // Output: ACCT value printed to serial with label
    // Preserves: Everything
    ATOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regACCT % 256)
        STA ZP.ACCL
        LDA #(regACCT / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.ACCT
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output NEXT register as "NEXT:type-hhll "
    // Input: None (uses ZP.NEXT)
    // Output: NEXT value with type printed to serial with label
    // Preserves: Everything
    NOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regNXT % 256)
        STA ZP.ACCL
        LDA #(regNXT / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.NEXTT
        Tools.PrintType();
        LDA #'-'
        Tools.COut();
        LDA ZP.NEXTH
        Serial.HexOut();
        LDA ZP.NEXTL
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output PC register as "PC:hhll "
    // Input: None (uses ZP.PC)
    // Output: PC value with type printed to serial with label
    // Preserves: Everything
    PCOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regPC % 256)
        STA ZP.ACCL
        LDA #(regPC / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.PCH
        Serial.HexOut();
        LDA ZP.PCL
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output BP register as "BP:ll "
    // Input: None (uses ZP.BP)
    // Output: PC value with type printed to serial with label
    // Preserves: Everything
    BPOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regBP % 256)
        STA ZP.ACCL
        LDA #(regBP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.BP
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output SP register as "SP:ll "
    // Input: None (uses ZP.SP)
    // Output: PC value with type printed to serial with label
    // Preserves: Everything
    SPOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regSP % 256)
        STA ZP.ACCL
        LDA #(regSP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.SP
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output TOP register as "TOP:type-hhll "
    // Input: None (uses ZP.TOP)
    // Output: TOP value with type printed to serial with label
    // Preserves: Everything
    TOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(regTOP % 256)
        STA ZP.ACCL
        LDA #(regTOP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.TOPT
        Tools.PrintType();
        LDA #'-'
        Tools.COut();
        LDA ZP.TOPH
        Serial.HexOut();
        LDA ZP.TOPL
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ACCL register as "ACCL:ll "
    // Input: None (uses ZP.ACCL)
    // Output: ACCL value printed to serial with label
    // Preserves: Everything
    ALOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        // Save ACCL/ACCH (critical since we're printing ACCL itself!)
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Store the ACCL value we want to print
        LDA ZP.ACCL
        PHA  // Save the value to print
        
        LDA #(regACCL % 256)
        STA ZP.ACCL
        LDA #(regACCL / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        PLA  // Get the value we want to print
        Serial.HexOut();
        Space();
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output carry flag status as "C " or "NC "
    // Input: None (reads current processor flags)
    // Output: Carry flag status printed to serial
    // Preserves: Everything
    CFOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        if (C)
        {
            LDA #(statusC % 256)
            STA ZP.ACCL
            LDA #(statusC / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        else
        {
            LDA #(statusNC % 256)
            STA ZP.ACCL
            LDA #(statusNC / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output zero flag status as "Z " or "NZ "
    // Input: None (reads current processor flags)
    // Output: Zero flag status printed to serial
    // Preserves: Everything
    ZFOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA
        
        // Save ACCL/ACCH
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        if (Z)
        {
            LDA #(statusZ % 256)
            STA ZP.ACCL
            LDA #(statusZ / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        else
        {
            LDA #(statusNZ % 256)
            STA ZP.ACCL
            LDA #(statusNZ / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
        
        // Restore ACCL/ACCH
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Helper to checkpoint error state
    // Input: A = checkpoint ID  
    // Output: Shows checkpoint ID only if error occurred since last check
    // Preserves: Everything
    ChkErr()
    {
        PHP
        PHA
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (NZ)
        {
            LDA #'{'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            PLA
            Serial.HexOut();  // Show checkpoint ID where error was first detected
            PHA
            LDA #'}'
            Tools.COut();
        }
        PLA
        PLP
    }
    
    DumpZeroPage()
    {
        PHP
        PHA
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA #(debugZeroPageHeader % 256) STA ZP.ACCL LDA #(debugZeroPageHeader / 256) STA ZP.ACCH Tools.PrintStringACC();
        Tools.NL();
        LDA #0
        DumpPage(); // zero page
        
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.ACCH
        
        PLA
        PLP
    }
    
    // Find function containing given address
    // Input: ZP.ACC = address to search for
    // Output: ZP.TOP = function name pointer if found, C set if found, NC if not found
    // Preserves: ZP.ACC (search address)
    // Modifies: ZP.IDX, ZP.IDY, ZP.NEXT (during function iteration and range checks)
    findFunctionByAddress()
    {
       PHA
       PHX
       PHY
       
       // Save ZP.IDX since we'll be iterating functions
       LDA ZP.IDXL
       PHA
       LDA ZP.IDXH
       PHA
       
       // Save ZP.NEXT since we'll use it for range calculations
       LDA ZP.NEXTL
       PHA
       LDA ZP.NEXTH
       PHA
       
       loop // Single exit block
       {
           // Start function iteration
           LDX #ZP.FunctionsList
           Table.GetFirst(); // Result in ZP.IDX
           if (NC) 
           { 
               CLC // No functions found
               break; 
           }
           
           loop // Function iteration loop
           {
               // Get function's opcode stream
               Functions.GetOpCodes(); // Input: ZP.IDX, Output: ZP.IDY = opcodes start, C if compiled
               if (NC) 
               { 
                   // Function not compiled, skip it
                   Table.GetNext(); // Move to next function
                   if (NC) { CLC break; } // No more functions, not found
                   continue;
               }
               
               // Get block size from memory manager (2 bytes before the allocated block)
               SEC
               LDA ZP.IDYL
               SBC #2
               STA ZP.NEXTL
               LDA ZP.IDYH  
               SBC #0
               STA ZP.NEXTH
               
               // Load block size from memory manager header at [ZP.NEXT]
               LDY #0
               LDA [ZP.NEXT], Y    // LSB of block size
               PHA
               INY
               LDA [ZP.NEXT], Y    // MSB of block size
               STA ZP.NEXTH
               PLA
               STA ZP.NEXTL
               // ZP.NEXT now contains block size
               
               // Calculate end address = start + (blocksize - 2)
               // Opcode stream length <= blocksize - 2 bytes (memory manager overhead)
               SEC
               LDA ZP.NEXTL
               SBC #2
               STA ZP.NEXTL
               LDA ZP.NEXTH
               SBC #0
               STA ZP.NEXTH
               // ZP.NEXT now has opcode stream length
               
               CLC
               LDA ZP.IDYL
               ADC ZP.NEXTL
               STA ZP.NEXTL
               LDA ZP.IDYH
               ADC ZP.NEXTH
               STA ZP.NEXTH
               // ZP.NEXT now has end address (start + length)
               
               // Check if searchPC >= start
               LDA ZP.ACCH
               CMP ZP.IDYH
               if (NZ)
               {
                   if (NC) 
                   { 
                       // searchPC < start, try next function
                       Table.GetNext();
                       if (NC) { CLC break; } // No more functions
                       continue; 
                   }
               }
               else
               {
                   LDA ZP.ACCL
                   CMP ZP.IDYL
                   if (NC) 
                   { 
                       // searchPC < start, try next function
                       Table.GetNext();
                       if (NC) { CLC break; } // No more functions
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
                       // searchPC >= end, try next function
                       Table.GetNext();
                       if (NC) { CLC break; } // No more functions
                       continue; 
                   }
               }
               else
               {
                   LDA ZP.ACCL
                   CMP ZP.NEXTL
                   if (C) 
                   { 
                       // searchPC >= end, try next function
                       Table.GetNext();
                       if (NC) { CLC break; } // No more functions
                       continue; 
                   }
               }
               
               // Found it! searchPC is within [start, end)
               Functions.GetName(); // Get function name in ZP.TOP
               SEC // Found
               break;
           }
           break; // Exit outer loop
       }
       
       // Restore ZP.NEXT
       PLA
       STA ZP.NEXTH
       PLA
       STA ZP.NEXTL
       
       // Restore ZP.IDX
       PLA
       STA ZP.IDXH
       PLA
       STA ZP.IDXL
       
       PLY
       PLX
       PLA
    }
        
    // Enhanced DumpStack method for Debug.asm
    // Input: None  
    // Output: Comprehensive stack dump showing:
    //   - Current stack pointers (CSP, SP, BP)
    //   - Call stack with frame hierarchy and return addresses
    //   - Value stack with frame boundaries, return slots, and position markers
    // Preserves: Everything (saves/restores all modified registers and state)
    // Format: 
    //   CSP:05 SP:18 BP:12
    //   Call Stack (2 frames):
    //     Frame 0: PC=0x2C45 BP:12 (current)
    //     Frame 1: PC=0x1A23 BP:08
    //   Value Stack (18 entries):
    //     11: I-003C (local)
    //     10: W-1234 *** RETURN SLOT ***
    //     09: B-01 (arg) <- BP
    DumpStack()
    {
        PHP  // Save flags
        PHA
        PHX
        PHY
        
        // Save ZP state that we'll modify during analysis
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
        
        // Main header with current stack state
        LDA #(debugStackHeader % 256)
        STA ZP.ACCL
        LDA #(debugStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Current stack pointers
        LDA #(regCSP % 256)
        STA ZP.ACCL
        LDA #(regCSP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.CSP
        Serial.HexOut();
        Space();
        
        LDA #(regSP % 256)
        STA ZP.ACCL
        LDA #(regSP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.SP
        Serial.HexOut();
        Space();
        
        LDA #(regBP % 256)
        STA ZP.ACCL
        LDA #(regBP / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.BP
        Serial.HexOut();
        NL();
        
        // Call Stack Analysis
        LDA #(callStackHeader % 256)
        STA ZP.ACCL
        LDA #(callStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.CSP
        Serial.HexOut();
        LDA #(framesSuffix % 256)
        STA ZP.ACCL
        LDA #(framesSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        
        
        // Show current frame first (where we're executing now)
        LDA #(framePrefix % 256)
        STA ZP.ACCL
        LDA #(framePrefix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA #'0'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        LDA #(framePC % 256)
        STA ZP.ACCL
        LDA #(framePC / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Show current PC and try to resolve function name
        LDA ZP.PCH
        Serial.HexOut();
        LDA ZP.PCL
        Serial.HexOut();
        
        // Look up current function
        LDA ZP.PCL
        STA ZP.ACCL
        LDA ZP.PCH
        STA ZP.ACCH
        findFunctionByAddress();
        if (C)
        {
            LDA #' '
            Serial.WriteChar();
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
        Serial.HexOut();
        LDA #(currentFrameMarker % 256)
        STA ZP.ACCL
        LDA #(currentFrameMarker / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        NL();
        
        // Walk call stack backwards to show frame hierarchy
        // Call stack layout: [BP][PC][BP][PC]... where CSP points to next free slot
        LDX ZP.CSP
        LDY #0  // Frame counter
        
        loop
        {
            CPX #2  // Need at least 2 entries (PC + BP pair)
            if (NC) { break; }  // Not enough for a complete frame
            
            // Move to saved BP slot (most recent odd index)
            DEX
            
            // Check if this is a return address slot (odd index)
            TXA
            AND #1
            if (NZ)  // Odd index = return address
            {
                // Print frame info with function name lookup
                LDA #(framePrefix % 256)
                STA ZP.ACCL
                LDA #(framePrefix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                TYA
                Serial.HexOut();
                LDA #(framePC % 256)
                STA ZP.ACCL
                LDA #(framePC / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                
                DEX  // Move to return address (even index)
                
                // Load return address into ZP.ACC for function lookup
                LDA Address.CallStackLSB, X
                STA ZP.ACCL
                LDA Address.CallStackMSB, X
                STA ZP.ACCH
                
                // Print the address first
                LDA ZP.ACCH
                Serial.HexOut();
                LDA ZP.ACCL
                Serial.HexOut();
                
                // Try to find function containing this address
                findFunctionByAddress(); // Input: ZP.ACC, Output: ZP.TOP = name, C if found
                if (C)
                {
                    // Found function name - print it
                    LDA #' '
                    Serial.WriteChar();
                    LDA #'('
                    Serial.WriteChar();
                    Tools.PrintStringTOP(); // Print function name
                    LDA #')'
                    Serial.WriteChar();
                }
                
                // Print rest of frame info
                LDA #(frameBP % 256)
                STA ZP.ACCL
                LDA #(frameBP / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                INX  // Move to BP value (next slot)
                LDA Address.CallStackLSB, X
                Serial.HexOut();
                
                NL();
                INY  // Increment frame counter
            }
        }
        
        NL();
        
        // Value Stack Analysis
        LDA #(valueStackHeader % 256)
        STA ZP.ACCL
        LDA #(valueStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.SP
        Serial.HexOut();
        LDA #(entriesSuffix % 256)
        STA ZP.ACCL
        LDA #(entriesSuffix / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Value stack analysis with frame correlation
        LDX ZP.SP
        LDY #0  // Entry counter
        
        // Use ZP.DB7 to track current frame being analyzed
        STZ ZP.DB7  // Start with frame 0 (current)
        
        loop
        {
            CPX #0
            if (Z) { break; }  // Empty stack
            
            CPY #20  // Limit entries to prevent screen overflow
            if (Z) 
            { 
                LDA #(stackEllipsis % 256)
                STA ZP.ACCL
                LDA #(stackEllipsis / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                break; 
            }
            
            DEX  // Move to previous entry
            
            // Check if we've crossed into a new frame (BP boundary)
            TXA
            CMP ZP.BP
            if (Z)
            {
                // Print frame boundary marker
                LDA #(frameMarkerPrefix % 256)
                STA ZP.ACCL
                LDA #(frameMarkerPrefix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                LDA ZP.DB7
                Serial.HexOut();
                LDA #(frameMarkerSuffix % 256)
                STA ZP.ACCL
                LDA #(frameMarkerSuffix / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
                INC ZP.DB7  // Next frame
            }
            
            // Print stack entry with intelligent labeling
            TXA
            Serial.HexOut();
            LDA #':'
            Tools.COut();
            Space();
            
            // Print type and value
            LDA Address.TypeStackLSB, X
            Tools.PrintType();
            LDA #'-'
            Tools.COut();
            LDA Address.ValueStackMSB, X
            Serial.HexOut();
            LDA Address.ValueStackLSB, X
            Serial.HexOut();
            
            // Smart position identification
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
                // Use ZP.DB9 to check if this is likely a return slot
                // Return slots are typically VOID type or were initialized as BIT 0
                STZ ZP.DB9  // Assume not a return slot
                
                LDA Address.TypeStackLSB, X
                CMP #Types.Undefined
                if (Z)
                {
                    INC ZP.DB9  // Likely return slot
                }
                else
                {
                    CMP #Types.Bool
                    if (Z)
                    {
                        // Check if it's zero value (default return slot initialization)
                        LDA Address.ValueStackLSB, X
                        ORA Address.ValueStackMSB, X
                        if (Z)
                        {
                            INC ZP.DB9  // Likely return slot (BIT 0)
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
                    // Show position relative to BP for context
                    SEC
                    TXA
                    SBC ZP.BP
                    if (NC)  // Below BP (arguments)
                    {
                        LDA #(argMarker % 256)
                        STA ZP.ACCL
                        LDA #(argMarker / 256)
                        STA ZP.ACCH
                        Tools.PrintStringACC();
                    }
                    else  // Above BP (locals/temps)
                    {
                        LDA #(localMarker % 256)
                        STA ZP.ACCL
                        LDA #(localMarker / 256)
                        STA ZP.ACCH
                        Tools.PrintStringACC();
                    }
                }
            }
            
            NL();
            INY
        }
        
        NL();
        
        // Restore all saved state in reverse order
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
        
        PLY
        PLX
        PLA
        PLP  // Restore flags
    }

    
    
    
#endif // DEBUG

    // Walk heap blocks for validation : errors if corrupt
    // Input: None
    // Output: Calls block processing for each heap block
    // Modifies: ZP.DB0-M3, ZP.DB7, ZP.IDX, ZP.IDY, A, X, Y (internal operations)
    // Preserves: All iteration-critical state (ZP.ACC, ZP.LCURRENT)
    ValidateHeap()
    {
        PHA
        PHX
        PHY
        
        // Save iteration-critical state that we modify
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
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
            STA ZP.DB0
            INY
            LDA [ZP.IDX], Y     // High byte of size
            STA ZP.DB1
            ORA ZP.DB0           // Check if size is zero
            if (Z) 
            { 
                Error.HeapCorruptError();
                break;
            }
            
            // Calculate block address (block address + 2 for data)
            CLC
            LDA ZP.IDXL
            ADC #2
            STA ZP.DB2
            LDA ZP.IDXH
            ADC #0
            STA ZP.DB3
            
            // Check if this block is on the free list
            // Save current position
            LDA ZP.IDXL
            PHA
            LDA ZP.IDXH
            PHA
            
            // Walk free list to see if this block is free
            LDA ZP.FREELISTL
            STA ZP.IDYL
            LDA ZP.FREELISTH
            STA ZP.IDYH
            
            STZ ZP.DB7  // Flag: 0 = not found, 1 = found on free list
            
            loop
            {
                LDA ZP.IDYL
                ORA ZP.IDYH
                if (Z) { break; }  // End of free list
                
                // Compare addresses - current block vs free list entry
                LDA ZP.IDYL
                CMP ZP.DB2
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
                CMP ZP.DB3
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
                STA ZP.DB7
                break;
            }
            
            // Restore current position
            PLA
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
            
            // In case we want to do somthing with the block, at this point we have:
            // ZP.IDX = pointer to block header
            // ZP.DB0/M1 = block size (including 2-byte header)
            // ZP.DB2/M3 = pointer to block data (header + 2)
            // ZP.DB7 = 0 if allocated, 1 if free
            // X = block number
            
            // Block header at ZP.IDX
            // Block data at ZP.DB2/M3
            // Block size in ZP.DB0/M1 (total size including header)
            // Free status in ZP.DB7 (0=allocated, 1=free)
            // Block number in X
            
            // Move to next block: current address + block size
            CLC
            LDA ZP.IDXL
            ADC ZP.DB0  // Add low byte of size
            STA ZP.IDXL
            LDA ZP.IDXH
            ADC ZP.DB1  // Add high byte of size
            STA ZP.IDXH
            
            INX
            CPX # 100  // Limit to 100 blocks to avoid infinite loops
            if (Z) { break; }
        }
        CheckError(); // set C | NC
        
        // Restore saved state in reverse order
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


    // Crash instance in A
    Crash() // hard stop
    {
        Serial.HexOut();
        
        TSX PHX     // save SP
        PHA         // save crash instance
        
        Tools.NL();
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        LDA #(debugCrashHeader % 256) STA ZP.ACCL LDA #(debugCrashHeader / 256) STA ZP.ACCH Tools.PrintStringACC();
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.ACCH
        
        PLA
        Serial.HexOut(); // crash instance
        LDA #' ' Tools.COut(); LDA #'S' Tools.COut(); LDA #'P' Tools.COut(); LDA #':' Tools.COut(); LDA #' ' Tools.COut();
        PLA
        Serial.HexOut(); // SP
        
        LDA #' ' Tools.COut(); CheckAndPrint();

#if defined(DEBUG) || defined(TRACE)
        Tools.NL();
        DumpVariables();
        
        DumpZeroPage();
        
        DumpBasicBuffers();
        DumpHeap();
#endif
        loop {  }
    }

}
