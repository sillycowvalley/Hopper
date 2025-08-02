unit Debug
{
    // API Status: Clean  
    // All public methods preserve caller state except for documented outputs
    // Debug-only functionality with conditional compilation guards
    
    uses "Tools"      // For convenience wrappers
    uses "BasicTypes" // For type printing
    
#ifdef DEBUG
    // Debug strings - main headers
    const string debugVarsHeader = "\n== VARS ==\n";
    const string debugStackHeader = "\n== STACK ==\n";
    const string debugHeapHeader = "\n== HEAP DUMP ==\n";
    const string debugBasicHeader = "\n== BASIC BUFFERS ==\n";
    const string debugZeroBlock = "ZERO\n";
    const string debugEllipsis = "...\n";
    
    // Register and status labels
    const string regA = "A:";
    const string regX = "X:";
    const string regY = "Y:";
    const string regTOP = "TOP:";
    const string regNXT = "NXT:";
    const string regACC = "ACC:";
    const string regIDX = "IDX:";
    const string regIDY = "IDY:";
    const string regSP = "SP:";
    const string regBP = "BP:";
    const string regACCL = "ACCL:";
    const string regACCT = "ACCT:";
    const string regNEXT = "NEXT:";
    const string regI = "I:";
    
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
#endif

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
        LDA #' ' Tools.COut();
    }
    
#endif

#ifdef DEBUG
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
    // Preserves: Everything (uses temporary ZP.U0-U4 for state preservation)
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
       LDA ZP.U0
       Serial.HexOut();
       Space();
       
       // X register
       LDA #(regX % 256)
       STA ZP.ACCL
       LDA #(regX / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.U1
       Serial.HexOut();
       Space();
       
       // Y register
       LDA #(regY % 256)
       STA ZP.ACCL
       LDA #(regY / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.U2
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
       LDA ZP.ACCH
       Serial.HexOut();
       LDA ZP.ACCL
       Serial.HexOut();
       Space();
       
       // IDX
       LDA #(regIDX % 256)
       STA ZP.ACCL
       LDA #(regIDX / 256)
       STA ZP.ACCH
       Tools.PrintStringACC();
       LDA ZP.U4
       Serial.HexOut();
       LDA ZP.U3
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
    // Output: Stack pointer info and last 8 stack entries printed to serial
    // Preserves: Everything (saves/restores all modified registers)
    DumpStack()
    {
        PHP  // Save flags
        PHA
        PHX
        PHY
        
        LDA #(debugStackHeader % 256)
        STA ZP.ACCL
        LDA #(debugStackHeader / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        // Stack and base pointers
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
            
            // Print stack index
            TXA
            Serial.HexOut();
            LDA #':'
            Tools.COut();
            
            // Print type
            LDA Address.TypeStackLSB, X
            Serial.HexOut();
            LDA #'/'
            Tools.COut();
            
            // Print value (MSB/LSB)
            LDA Address.ValueStackMSB, X
            Serial.HexOut();
            LDA Address.ValueStackLSB, X
            Serial.HexOut();
            NL();
            
            DEY
        }
        
        NL();
        
        PLY
        PLX
        PLA
        PLP  // Restore flags
    }
    
    // Lightweight heap summary for use during iteration
    // Input: None
    // Output: Variable and function list head pointers printed to serial
    // Preserves: Everything (minimal register usage for iteration safety)
    DumpHeapSummary()
    {
        PHP  // Save flags
        PHA
        
        // Show the critical list head pointers
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
        Space();
        
        PLA
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
        PLP  // Restore flags
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
    
    // Internal heap dump implementation
    // Input: None
    // Output: Detailed heap block analysis with hex/ASCII dump
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
            STA ZP.M0
            INY
            LDA [ZP.IDX], Y     // High byte of size
            STA ZP.M1
            ORA ZP.M0           // Check if size is zero
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
            Tools.COut();
            
            // Print block size (from header)
            LDA ZP.M1  // High byte
            Serial.HexOut();
            LDA ZP.M0  // Low byte
            Serial.HexOut();
            
            // Check if this block is on the free list
            Space();
            LDA #'('
            Tools.COut();
            
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
            Space();
            Space();
            Space();
            Space();
            
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
                Space();
                
                INY
                
                // Add extra space after 8 bytes
                CPY #8
                if (Z)
                {
                    CPY ZP.U2  // Don't add space if we're at the end
                    if (NZ)
                    {
                        Space();
                        Space();
                    }
                }
            }
            
            // Pad hex section to align ASCII (each byte takes 3 chars: "XX ")
            // Need to reach 16*3 + 2 = 50 characters for full alignment
            loop
            {
                CPY #16
                if (Z) { break; }
                
                Space();
                Space();
                Space();
                
                INY
                
                // Add extra space after 8 bytes for alignment
                CPY #8
                if (Z)
                {
                    Space();
                    Space();
                }
            }
            
            // Add spacing before ASCII dump
            Space();
            Space();
            Space();
            
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
    
    // Dump a 256-byte page in hex+ASCII format for debugging
    // Input: A = page number (high byte of address)
    // Output: Complete page contents printed in 16 lines of 16 bytes each
    // Modifies: ZP.M0, ZP.M1 (internal operations)
    // Preserves: All other state
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
            LDA #':'
            Tools.COut();
            Space();
            
            // Print 16 hex bytes with space after each
            LDY #0
            loop
            {
                CPY #16
                if (Z) { break; }
                
                LDA [ZP.M0], Y
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
                
                LDA [ZP.M0], Y
                
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
        STA ZP.M0
        LDA #(Address.BasicInputBuffer >> 8)
        STA ZP.M1
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
        STA ZP.M0
        LDA #(Address.BasicTokenizerBuffer >> 8)
        STA ZP.M1
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
        STA ZP.M0
        LDA #(Address.BasicOpCodeBuffer >> 8)
        STA ZP.M1
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
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY  // Restore Y
        PLX  // Restore X
        PLA  // Restore A
        PLP  // Restore flags
    }
    
    // Dump a 64-byte memory block in hex+ASCII format for debugging
    // Input: ZP.M0/ZP.M1 = start address to dump
    // Output: 64 bytes (4 lines of 16 bytes) printed in hex+ASCII format
    // Modifies: A, X, Y (internal operations)
    // Preserves: ZP.M0/ZP.M1 (start address preserved)
    DumpMemoryBlock()
    {
        PHA
        PHX
        PHY
        
        // Save original address
        LDA ZP.M0
        PHA
        LDA ZP.M1
        PHA
        
        // Print 4 lines of 16 bytes each (64 bytes total)
        LDX #0  // Line counter (0-3)
        
        loop
        {
            CPX #4
            if (Z) { break; }  // Done with 4 lines
            
            // Print address for this line
            LDA ZP.M1
            Serial.HexOut();
            LDA ZP.M0
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
                
                LDA [ZP.M0], Y
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
                
                LDA [ZP.M0], Y
                
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
            LDA ZP.M0
            ADC #16
            STA ZP.M0
            if (C)
            {
                INC ZP.M1
            }
            
            INX
        }
        
        // Restore original address
        PLA
        STA ZP.M1
        PLA
        STA ZP.M0
        
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
        
        PLA  // Restore A register
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Output ZP.SymbolIteratorFilter as "I:ll "
    // Input: None (uses ZP.SymbolIteratorFilter)
    // Output: Iterator filter value printed to serial with label
    // Preserves: Everything
    IOut()
    {
        PHP  // Push processor status (including carry flag)
        PHA  // Save A register
        
        LDA #(regI % 256)
        STA ZP.ACCL
        LDA #(regI / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.SymbolIteratorFilter
        Serial.HexOut();
        Space();
        
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
        
        LDA #(regACCT % 256)
        STA ZP.ACCL
        LDA #(regACCT / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.ACCT
        Serial.HexOut();
        Space();
        
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
        
        LDA #(regNEXT % 256)
        STA ZP.ACCL
        LDA #(regNEXT / 256)
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
        
        LDA #(regACCL % 256)
        STA ZP.ACCL
        LDA #(regACCL / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        LDA ZP.ACCL
        Serial.HexOut();
        Space();
        
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
    
#endif
}
