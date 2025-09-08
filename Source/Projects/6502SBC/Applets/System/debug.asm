unit Debug
{
    uses "System/Definitions"
    uses "System/Screen"
    uses "System/Print"
    uses "System/Serial"
    
    // Zero page allocation - single base + offsets
    const byte debugSlots = 0x60;
    
    // Persistent state
    const byte debugRow     = debugSlots+0; // Current debug output row
    const byte debugCol     = debugSlots+1; // Current debug output row
    const byte debugEnabled = debugSlots+2; // Debug output enabled flag
    const byte debugEntries = debugSlots+3; // Nested calls to store and restore
    
    const uint dumpCount    = debugSlots+4;
    
    
    // Constants - use camelCase, not ALL_CAPS
    const byte cDebugColumn = 81;   // Right side of terminal (80..99 available because of GotoXY limits)
    const byte cMaxWidth    = 18;   // 81..99
    const byte cMaxRows     = 34;   // Screen height
    const byte cMaxColumn   = (cDebugColumn+cMaxWidth);
    
    // Store current cursor position (uses VT100 save cursor)
    storeState()
    {
        INC debugEntries
        LDA debugEntries
        CMP #1
        if (Z)
        {
            Screen.SaveCursor();  // VT100 ESC[s command
        }
    }
    
    // Restore saved cursor position (uses VT100 restore cursor)  
    restoreState()
    {
        DEC debugEntries
        LDA debugEntries
        if (Z)
        {
            Screen.RestoreCursor();  // VT100 ESC[u command
        }
    }
    
    // Initialize debug system
    Initialize()
    {
        STZ debugRow
        STZ debugCol
        LDA #1
        STA debugEnabled
        STZ debugEntries
    }
    
    // Disable debug output
    Disable()
    {
        STZ debugEnabled
    }
    
    // Enable debug output  
    Enable()
    {
        LDA #1
        STA debugEnabled
    }
    
    // Clear debug area
    Clear()
    {
        LDA debugEnabled
        if (Z) { return; }
        
        PHX
        PHY
        
        storeState();
        
        STZ debugRow
        STZ debugCol
        loop
        {
            LDA # cDebugColumn
            LDY debugRow
            Screen.GotoXY();
            
            // Output 15 spaces
            LDX # cMaxWidth
            loop
            {
                PHX
                LDA #' '
                Serial.WriteChar();
                PLX
                DEX
                if (Z) { break; }
            }
            
            INC debugRow
            LDA debugRow
            CMP # cMaxRows
            if (Z) { break; }
        }
        
        STZ debugRow
        
        restoreState();        
        
        PLY
        PLX
    }
    
    // Private helper - move to next debug row
    nextRow()
    {
        INC debugRow
        LDA debugRow
        CMP # cMaxRows
        if (C)
        {
            STZ debugRow
        }
    }
    
    // Private helper - output label at current debug row
    // Input: ZP.STR = label string
    // Stops when reaching cMaxColumn (99)
    printSTR()
    {
        PHY
        PHX
        
        // Start at cDebugColumn
        LDX #cDebugColumn
        
        // Output label
        LDY #0
        loop
        {
            // Check if we've reached the string end
            LDA [ZP.STR], Y
            if (Z) { break; }
            
            // Check if we've reached the column limit
            CPX #cMaxColumn
            if (C) { break; }     // >= column 99
            
            // Output the character
            Serial.WriteChar();
            
            // Advance to next character and column
            INX
            INY
        }
        
        PLX
        PLY
    }   
    // Output string at debug column
    // Input: ZP.STR = string pointer
    String()
    {
        PHP
        
        LDA debugEnabled
        if (Z) { PLP return; }
        
        PHX
        PHY
        
        storeState();
        
        LDA # cDebugColumn
        LDY debugRow
        Screen.GotoXY();
        
        printSTR();
        
        nextRow();
        
        restoreState();
        
        PLY
        PLX
        PLP
    }
    
    // Output byte in hex
    // Input: A = byte value
    Byte()
    {
        PHP
        PHX
        PHY
            
        PHA
        loop
        {
            LDA debugEnabled
            if (Z) 
            { 
                break; 
            }
            
            storeState();
            
            LDA # cDebugColumn
            LDY debugRow
            Screen.GotoXY();
            
            PLA
            PHA
            Print.Hex();
            
            nextRow();
            
            restoreState();
            
            break;
        } // single exit
        
        PLA
        PLY
        PLX
        PLP
    }
    
    // Output byte in hex with space, continuing from last position
    // Input: A = byte value
    ByteSpace()
    {
        PHP
        PHX
        PHY
        
        PHA
        loop
        {
            LDA debugEnabled
            if (Z) 
            { 
                break; 
            }
            
            storeState();
            
            // Position at current location
            CLC
            LDA # cDebugColumn
            ADC debugCol
            LDY debugRow
            Screen.GotoXY();
            
            // Print hex and space
            PLA
            PHA
            Print.Hex();
            Print.Space();
            
            // Update position
            CLC
            LDA debugCol
            ADC #3  // 2 hex + 1 space
            STA debugCol
            
            // Wrap if needed
            CMP # cMaxWidth  // >= 18?
            if (C)
            {
                STZ debugCol
                nextRow();
            }
            
            restoreState();
            
            break;
        } // single exit
        
        PLA
        PLY
        PLX
        PLP
    }
    
    // Output word (16-bit) in hex
    // Input: ZP.ACC = word value
    Word()
    {
        PHP
        LDA debugEnabled
        if (Z) { PLP return; }
        
        PHX
        PHY
        
        storeState();
        
        LDA # cDebugColumn
        LDY debugRow
        Screen.GotoXY();
        
        LDA ZP.ACCH
        Print.Hex();
        LDA ZP.ACCL
        Print.Hex();
        
        nextRow();
        
        restoreState();
        
        PLY
        PLX
        PLP
    }
    
    // Output labeled byte
    // Input: ZP.STR = label, then call with A = byte
    LabeledByte()
    {
        PHP
        PHX
        PHY
        PHA
        loop
        {
            LDA debugEnabled
            if (Z) 
            { 
                break; 
            }
            
            
            storeState();
            
            LDA # cDebugColumn
            LDY debugRow
            Screen.GotoXY();
            
            // Output label
            printSTR();
            
            // Output byte
            PLA
            PHA
            Print.Hex();
            
            nextRow();
            
            restoreState();
            
            break;
        } // single exit
        PLA
        PLY
        PLX
        PLP
    }
    
    // Output labeled word
    // Input: ZP.STR = label, ZP.ACC = word
    LabeledWord()
    {
        PHP
        LDA debugEnabled
        if (Z) { PLP return; }
        
        PHX
        PHY
        
        storeState();
        
        LDA # cDebugColumn
        LDY debugRow
        Screen.GotoXY();
        
        // Output label
        printSTR();
        
        // Output word
        LDA ZP.ACCH
        Print.Hex();
        LDA ZP.ACCL
        Print.Hex();
        
        nextRow();
        
        restoreState();
        
        PLY
        PLX
        PLP
    }
    
    // Dump memory bytes (leaf function - can use M0-M17)
    // Input: ZP.IDX = start address, A = byte count
    DumpMemory()
    {
        PHP
        STA dumpCount  // Save count
        LDA debugEnabled
        if (Z) { PLP return; }
        
        PHX
        PHY
        
        storeState();
        
        LDA # cDebugColumn
        LDY debugRow
        Screen.GotoXY();
        
        // Show address
        LDA ZP.IDXH
        Print.Hex();
        LDA ZP.IDXL
        Print.Hex();
        LDA #':'
        Serial.WriteChar();
        
        nextRow();
        
        // Dump bytes
        LDY #0
        loop
        {
            // Check if need new line every 4 bytes
            TYA
            AND #3
            if (Z)
            {
                LDA debugRow
                if (Z) // wrapped to top
                {
                    break;
                }
                
                PHY
                LDA # cDebugColumn
                LDY debugRow
                Screen.GotoXY();
                nextRow();
                LDA #' '
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                PLY
            }
            
            LDA [ZP.IDX], Y
            Print.Hex();
            LDA #' '
            Serial.WriteChar();
            
            INY
            CPY dumpCount
            if (Z) { break; }
        }
        
        restoreState();
        
        PLY
        PLX
        PLP
    }
}
