unit ScreenBuffer
{
    uses "Definitions"
    uses "Memory"
    uses "Serial"
    uses "Screen"
    
    // Public zero page properties
    const byte CursorX = 0x58;
    const byte CursorY = 0x59;
    const byte Foreground = 0x5A;      // Current foreground color (0-7)
    const byte Background = 0x5B;      // Current background color (0-7)
    const byte Attributes = 0x5C;      // Current attributes (bit 0 = bold, bit 1 = inverse)
    
    // Attribute bit flags
    enum Attr
    {
        Normal  = 0x00,
        Bold    = 0x01,
        Inverse = 0x02,
    }
    
    // Private workspace
    const byte sbWidth = 0x5D;
    const byte sbHeight = 0x5E;
    
    const uint sbCurrent  = 0x5F;       // Current buffer pointer (2 bytes)
    const uint sbCurrentL = 0x5F;
    const uint sbCurrentH = 0x60;
    
    const uint sbPrevious  = 0x61;      // Previous buffer pointer (2 bytes)
    const uint sbPreviousL = 0x61;
    const uint sbPreviousH = 0x62;
    
    const byte sbSuspendCount = 0x63;
    const byte sbCursorVisible = 0x64;
    const byte sbInitialized = 0x65;
    
    const uint sbSize = 0x66;          // Total buffer size (2 bytes)
    const uint sbSizeL = 0x66;
    const uint sbSizeH = 0x67;
    
    const byte sbTemp = 0x68;          // Temp workspace
    const byte sbTemp2 = 0x69;
    
    const uint sbOffset  = 0x6A;        // Current offset (2 bytes)
    const uint sbOffsetL = 0x6A;
    const uint sbOffsetH = 0x6B;
    
    // Constants
    const byte dirtyBit = 0x80;
    const byte charMask = 0x7F;
    
    // Initialize buffer system
    Initialize() // Input: A = width, Y = height
    {
        // Check not already initialized
        LDA sbInitialized
        if (NZ)
        {
            CLC  // Already initialized
            return;
        }
        
        // Save dimensions
        STA sbWidth
        STY sbHeight
        
        // Calculate total size = width * height * 2
        // First calculate width * height
        STZ ZP.ACCH
        LDA sbWidth
        STA ZP.ACCL
        STZ ZP.TOP0
        LDA sbHeight
        STA ZP.TOP1
        
        // Multiply (simple repeated addition for small values)
        STZ sbSizeL
        STZ sbSizeH
        LDY sbHeight
        loop
        {
            CLC
            LDA sbSizeL
            ADC sbWidth
            STA sbSizeL
            LDA sbSizeH
            ADC #0
            STA sbSizeH
            DEY
            if (Z) { break; }
        }
        
        // Double it (shift left for * 2)
        ASL sbSizeL
        ROL sbSizeH
        
        // Allocate current buffer
        LDA sbSizeL
        STA ZP.ACCL
        LDA sbSizeH
        STA ZP.ACCH
        Memory.Allocate();
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // Allocation failed
            return;
        }
        LDA ZP.IDXL
        STA sbCurrentL
        LDA ZP.IDXH
        STA sbCurrentH
        
        // Allocate previous buffer
        LDA sbSizeL
        STA ZP.ACCL
        LDA sbSizeH
        STA ZP.ACCH
        Memory.Allocate();
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            // Free first buffer and fail
            LDA sbCurrentL
            STA ZP.IDXL
            LDA sbCurrentH
            STA ZP.IDXH
            Memory.Free();
            CLC
            return;
        }
        LDA ZP.IDXL
        STA sbPreviousL
        LDA ZP.IDXH
        STA sbPreviousH
        
        // Set defaults
        LDA #Screen.Color.White
        STA Foreground
        LDA #Screen.Color.Black
        STA Background
        STZ Attributes
        STZ CursorX
        STZ CursorY
        STZ sbSuspendCount
        LDA #1
        STA sbCursorVisible
        STA sbInitialized
        
        // Clear both buffers
        Clear();
        clearPrevious();
        
        SEC  // Success
    }
    
    // Free allocated memory
    Dispose()
    {
        LDA sbInitialized
        if (Z) { return; }
        
        // Free current buffer
        LDA sbCurrentL
        STA ZP.IDXL
        LDA sbCurrentH
        STA ZP.IDXH
        Memory.Free();
        
        // Free previous buffer
        LDA sbPreviousL
        STA ZP.IDXL
        LDA sbPreviousH
        STA ZP.IDXH
        Memory.Free();
        
        STZ sbInitialized
    }
    
    // Helper: calculate buffer offset for CursorX, CursorY
    calculateOffset() // Returns offset in sbOffset
    {
        // offset = (CursorY * sbWidth + CursorX) * 2
        STZ sbOffsetL
        STZ sbOffsetH
        
        // Add sbWidth to offset CursorY times
        LDY CursorY
        if (NZ)
        {
            loop
            {
                CLC
                LDA sbOffsetL
                ADC sbWidth
                STA sbOffsetL
                LDA sbOffsetH
                ADC #0
                STA sbOffsetH
                DEY
                if (Z) { break; }
            }
        }
        
        // Add CursorX
        CLC
        LDA sbOffsetL
        ADC CursorX
        STA sbOffsetL
        LDA sbOffsetH
        ADC #0
        STA sbOffsetH
        
        // Double for 2 bytes per cell
        ASL sbOffsetL
        ROL sbOffsetH
    }
    
    // Helper: pack current attributes into byte
    packAttributes() // Returns packed byte in A
    {
        LDA Foreground      // Bits 0-2
        AND #0x07
        STA sbTemp
        
        LDA Background      // Bits 3-5
        AND #0x07
        ASL
        ASL
        ASL
        ORA sbTemp
        STA sbTemp
        
        LDA Attributes
        AND #Attr.Bold
        if (NZ)
        {
            LDA sbTemp
            ORA #0x40       // Bit 6
            STA sbTemp
        }
        
        LDA Attributes
        AND #Attr.Inverse
        if (NZ)
        {
            LDA sbTemp
            ORA #0x80       // Bit 7
            STA sbTemp
        }
        
        LDA sbTemp
    }
    
    // Set foreground color
    SetForeground() // Input: A = color (0-7)
    {
        AND #0x07
        STA Foreground
    }
    
    // Set background color
    SetBackground() // Input: A = color (0-7)
    {
        AND #0x07
        STA Background
    }
    
    // Set both colors
    SetColors() // Input: A = foreground, Y = background
    {
        AND #0x07
        STA Foreground
        TYA
        AND #0x07
        STA Background
    }
    
    // Enable bold
    SetBold()
    {
        LDA Attributes
        ORA #Attr.Bold
        STA Attributes
    }
    
    // Enable inverse
    SetInverse()
    {
        LDA Attributes
        ORA #Attr.Inverse
        STA Attributes
    }
    
    // Clear all special attributes
    SetNormal()
    {
        STZ Attributes
    }
    
    // Reset to white on black, normal
    Reset()
    {
        LDA #Screen.Color.White
        STA Foreground
        LDA #Screen.Color.Black
        STA Background
        STZ Attributes
    }
    
    // Write character at cursor position
    Char() // Input: A = character
    {
        PHY
        PHA
        
        calculateOffset();
        
        // Add base address to offset
        CLC
        LDA sbOffsetL
        ADC sbCurrentL
        STA ZP.IDXL
        LDA sbOffsetH
        ADC sbCurrentH
        STA ZP.IDXH
        
        // Store character with dirty bit
        PLA
        ORA #dirtyBit
        LDY #0
        STA [ZP.IDX], Y
        
        // Store packed attributes
        packAttributes();
        INY
        STA [ZP.IDX], Y
        
        // Advance cursor
        INC CursorX
        LDA CursorX
        CMP sbWidth
        if (Z)
        {
            STZ CursorX
            INC CursorY
        }
        
        PLY
    }
    
    // Write string at cursor position
    String() // Input: ZP.STR = string pointer
    {
        PHY
        
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // Null terminator
            
            Char();
            INY
        }
        
        PLY
    }
    
    // Move to start of next line
    NewLine()
    {
        STZ CursorX
        INC CursorY
        LDA CursorY
        CMP sbHeight
        if (Z)
        {
            DEC CursorY  // Stay at bottom
        }
    }
    
    // Write a space
    Space()
    {
        LDA #' '
        Char();
    }
    
    // Write multiple spaces
    Spaces() // Input: A = count
    {
        TAX
        if (Z) { return; }
        loop
        {
            LDA #' '
            PHX
            Char();
            PLX
            DEX
            if (Z) { break; }
        }
    }
    
    // Write hex byte
    Hex() // Input: A = byte value
    {
        PHA
        
        // High nibble
        LSR
        LSR
        LSR
        LSR
        CMP #10
        if (C)
        {
            CLC
            ADC #('A' - 10)
        }
        else
        {
            CLC
            ADC #'0'
        }
        Char();
        
        // Low nibble
        PLA
        AND #0x0F
        CMP #10
        if (C)
        {
            CLC
            ADC #('A' - 10)
        }
        else
        {
            CLC
            ADC #'0'
        }
        Char();
    }
    
    // Clear buffer with current background color
    Clear()
    {
        PHY
        PHX
        
        packAttributes();
        STA sbTemp2  // Save packed attributes
        
        // Fill current buffer
        LDY #0
        LDX sbHeight
        loop  // For each row
        {
            PHX
            LDX sbWidth
            loop  // For each column
            {
                // Calculate position and store space + attributes
                LDA #(' ' | dirtyBit)
                STA [sbCurrent], Y
                INY
                LDA sbTemp2
                STA [sbCurrent], Y
                INY
                
                DEX
                if (Z) { break; }
            }
            PLX
            DEX
            if (Z) { break; }
        }
        
        STZ CursorX
        STZ CursorY
        
        PLX
        PLY
    }
    
    // Helper: clear previous buffer
    clearPrevious()
    {
        PHY
        PHX
        
        // Fill previous buffer with zeros
        LDY #0
        LDX sbHeight
        loop  // For each row
        {
            PHX
            LDX sbWidth
            loop  // For each column
            {
                LDA #0
                STA [sbPrevious], Y
                INY
                STA [sbPrevious], Y
                INY
                
                DEX
                if (Z) { break; }
            }
            PLX
            DEX
            if (Z) { break; }
        }
        
        PLX
        PLY
    }
    
    // Draw a box
    Box() // Input: A = width, Y = height (at CursorX, CursorY)
    {
        PHX
        PHY
        PHA
        
        STA sbTemp   // Save width
        STY sbTemp2  // Save height
        
        // Top line
        LDA #'+'
        Char();
        LDA sbTemp
        SEC
        SBC #2
        TAX
        loop
        {
            LDA #'-'
            PHX
            Char();
            PLX
            DEX
            if (Z) { break; }
        }
        LDA #'+'
        Char();
        
        // Sides
        LDY sbTemp2
        DEY
        DEY
        loop
        {
            PHY
            
            // Position at left edge
            INC CursorY
            LDA CursorX
            PHA
            SEC
            SBC sbTemp
            STA CursorX
            
            // Left side
            LDA #'|'
            Char();
            
            // Spaces
            LDA sbTemp
            SEC
            SBC #2
            Spaces();
            
            // Right side
            LDA #'|'
            Char();
            
            PLA
            SEC
            SBC sbTemp
            STA CursorX
            
            PLY
            DEY
            if (Z) { break; }
        }
        
        // Bottom line
        INC CursorY
        LDA CursorX
        SEC
        SBC sbTemp
        STA CursorX
        
        LDA #'+'
        Char();
        LDA sbTemp
        SEC
        SBC #2
        TAX
        loop
        {
            LDA #'-'
            PHX
            Char();
            PLX
            DEX
            if (Z) { break; }
        }
        LDA #'+'
        Char();
        
        PLA
        PLY
        PLX
    }
    
    // Update screen (send only changes)
    Update()
    {
        // Check suspend count
        LDA sbSuspendCount
        if (NZ) { return; }
        
        PHY
        PHX
        
        // Hide cursor during update
        Screen.HideCursor();
        
        // Compare buffers and update changes
        LDY #0  // Offset in buffers
        LDX #0  // Current row
        
        loop // row loop
        {
            PHX
            LDX #0  // Current column
            
            loop // column loop
            {
                // Check dirty bit
                LDA [sbCurrent], Y
                AND #dirtyBit
                if (NZ)
                {
                    // Position cursor
                    TXA
                    PHX
                    LDY CursorY  // Save current position
                    PHY
                    LDY sbTemp   // Use temp for row
                    PLY
                    Screen.GotoXY();
                    PLY  // Restore offset
                    
                    // Get character (without dirty bit)
                    LDA [sbCurrent], Y
                    AND #charMask
                    PHA
                    
                    // Clear dirty bit
                    LDA [sbCurrent], Y
                    AND #charMask
                    STA [sbCurrent], Y
                    
                    // Copy to previous buffer
                    STA [sbPrevious], Y
                    
                    // Get attributes
                    INY
                    LDA [sbCurrent], Y
                    PHA
                    
                    // Copy attributes to previous
                    STA [sbPrevious], Y
                    
                    // Apply attributes
                    applyAttributes();
                    
                    // Send character
                    PLA
                    PLA
                    Serial.WriteChar();
                    
                    // Reset attributes
                    Screen.Reset();
                    
                    DEY  // Back to character position
                    PLX
                }
                else
                {
                    INY  // Skip attribute byte
                }
                
                INY  // Next cell
                INX
                CPX sbWidth
                if (Z) { break; }
            } // column loop
            
            PLX
            INX
            CPX sbHeight
            if (Z) { break; }
        } // row loop
        
        // Position hardware cursor
        LDA sbCursorVisible
        if (NZ)
        {
            LDA CursorX
            LDY CursorY
            Screen.GotoXY();
            Screen.ShowCursor();
        }
        
        PLX
        PLY
    }
    
    // Helper: apply packed attributes to screen
    applyAttributes() // Input: A = packed attributes
    {
        PHA
        
        // Extract and set foreground color
        AND #0x07
        Screen.Foreground();
        
        // Extract and set background color
        PLA
        PHA
        LSR
        LSR
        LSR
        AND #0x07
        Screen.Background();
        
        // Check bold
        PLA
        PHA
        AND #0x40
        if (NZ)
        {
            Screen.Bold();
        }
        
        // Check inverse
        PLA
        AND #0x80
        if (NZ)
        {
            Screen.Inverse();
        }
    }
    
    // Force redraw everything
    Redraw()
    {
        PHY
        
        // Mark everything dirty
        LDY #0
        LDX sbHeight
        loop
        {
            PHX
            LDX sbWidth
            loop
            {
                LDA [sbCurrent], Y
                ORA #dirtyBit
                STA [sbCurrent], Y
                INY
                INY  // Skip attribute byte
                
                DEX
                if (Z) { break; }
            }
            PLX
            DEX
            if (Z) { break; }
        }
        
        // Clear previous buffer to force full redraw
        clearPrevious();
        
        // Now update
        Update();
        
        PLY
    }
    
    // Suspend updates
    Suspend()
    {
        INC sbSuspendCount
    }
    
    // Resume updates
    Resume()
    {
        DEC sbSuspendCount
        if (Z)
        {
            Update();
        }
    }
    
    // Show cursor after updates
    ShowCursor()
    {
        LDA #1
        STA sbCursorVisible
    }
    
    // Hide cursor after updates
    HideCursor()
    {
        STZ sbCursorVisible
    }
    
    // Position cursor
    GotoXY() // Input: A = X, Y = Y
    {
        STA CursorX
        STY CursorY
    }
}
