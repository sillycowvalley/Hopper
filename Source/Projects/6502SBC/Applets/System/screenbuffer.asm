unit ScreenBuffer
{
    uses "Definitions"
    uses "Memory"
    uses "Serial"
    uses "Screen"
    uses "Shared"
    
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
    
    const byte sbRow        = 0x6C;
    const byte sbCol        = 0x6D;
    const byte sbScreenFg   = 0x6E;    // Current screen foreground
    const byte sbScreenBg   = 0x6F;    // Current screen background  
    const byte sbScreenAttr = 0x70;    // Current screen attributes
    const byte sbScreenX    = 0x71;    // Where cursor currently is
    const byte sbScreenY    = 0x72;    // Where cursor currently is
    
    
    
    // Constants
    const byte dirtyBit = 0x80;
    const byte charMask = 0x7F;
    
    // Initialize buffer system
    Initialize() // Input: A = width, Y = height
    {
        // Save dimensions
        STA sbWidth
        STY sbHeight
        
        // Check not already initialized
        LDA sbInitialized
        if (NZ)
        {
            CLC  // Already initialized
            return;
        }
        
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
        ASL A ASL A ASL A
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
            Char();
            DEX
            if (Z) { break; }
        }
    }
    
    // Write hex byte
    Hex() // Input: A = byte value
    {
        PHA
        
        // High nibble
        LSR A LSR A LSR A LSR A
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
        
        // Start at beginning of current buffer
        LDA sbCurrentL
        STA ZP.IDXL
        LDA sbCurrentH
        STA ZP.IDXH
        
        // Calculate total cells = width * height into IDY
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDY sbHeight
        loop
        {
            CLC
            LDA ZP.IDYL
            ADC sbWidth
            STA ZP.IDYL
            LDA ZP.IDYH
            ADC #0
            STA ZP.IDYH
            DEY
            if (Z) { break; }
        }
        
        // Loop through all cells
        loop
        {
            // Store space with dirty bit
            LDA #(' ' | dirtyBit)
            STA [ZP.IDX]
            
            // Store attributes
            LDY #1
            LDA sbTemp2
            STA [ZP.IDX], Y
            
            // Move to next cell (2 bytes forward)
            Shared.IncIDX();
            Shared.IncIDX();
            
            // Decrement cell counter
            Shared.DecIDY();
            
            // Check if done
            LDA ZP.IDYL
            ORA ZP.IDYH
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
            Char();
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
            Char();
            DEX
            if (Z) { break; }
        }
        LDA #'+'
        Char();
        
        PLA
        PLY
        PLX
    }
    
    
    
    Update()
    {
        
        // Check suspend count
        LDA sbSuspendCount
        if (NZ) { return; }
        
        // Hide cursor during update
        Screen.HideCursor();
        
        // Initialize screen state to "unknown" (0xFF = invalid/unknown)
        LDA #0xFF
        STA sbScreenX
        STA sbScreenY
        STA sbScreenAttr  // Packed attributes currently on screen
        
        // Loop through all cells
        STZ sbRow
        loop  // Row loop
        {
            STZ sbCol
            loop  // Column loop
            {
                // Calculate offset = (sbRow * sbWidth + sbCol) * 2
                STZ sbOffsetL
                STZ sbOffsetH
                
                LDY sbRow
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
                
                // Add column
                CLC
                LDA sbOffsetL
                ADC sbCol
                STA sbOffsetL
                LDA sbOffsetH
                ADC #0
                STA sbOffsetH
                
                // Double for 2 bytes per cell
                ASL sbOffsetL
                ROL sbOffsetH
                
                // Check dirty bit
                CLC
                LDA sbOffsetL
                ADC sbCurrentL
                STA ZP.IDXL
                LDA sbOffsetH
                ADC sbCurrentH
                STA ZP.IDXH
                
                LDY #0
                LDA [ZP.IDX], Y
                AND #dirtyBit
                if (NZ)
                {
                    // Check if cursor needs positioning
                    LDA sbCol
                    CMP sbScreenX
                    if (NZ)
                    {
                        // Column different, need to position
                        positionCursor:
                        LDA sbCol
                        LDY sbRow
                        Screen.GotoXY();
                        LDA sbCol
                        STA sbScreenX
                        LDA sbRow
                        STA sbScreenY
                    }
                    else
                    {
                        // Column matches, check row
                        LDA sbRow
                        CMP sbScreenY
                        if (NZ)
                        {
                            // Row different, need to position
                            BRA positionCursor
                        }
                        // Cursor already at right position!
                    }
                    
                    // Get attributes
                    INY
                    LDA [ZP.IDX], Y
                    STA sbTemp  // Save packed attributes
                    
                    // Compare entire attribute byte with what's on screen
                    CMP sbScreenAttr
                    if (NZ)
                    {
                        // Attributes changed - reset and apply them all
                        STA sbScreenAttr
                        
                        // Reset to normal first (clears any previous bold/inverse)
                        Screen.Reset();
                        
                        // Extract and set foreground color
                        LDA sbTemp
                        AND #0x07
                        Screen.Foreground();
                        
                        // Extract and set background color
                        LDA sbTemp
                        LSR
                        LSR
                        LSR
                        AND #0x07
                        Screen.Background();
                        
                        // Apply bold if needed
                        LDA sbTemp
                        AND #0x40
                        if (NZ)
                        {
                            Screen.Bold();
                        }
                        
                        // Apply inverse if needed
                        LDA sbTemp
                        AND #0x80
                        if (NZ)
                        {
                            Screen.Inverse();
                        }
                    }
                    
                    // Get character (without dirty bit)
                    DEY  // Back to character byte
                    LDA [ZP.IDX], Y
                    AND #charMask
                    PHA  // Save character
                    
                    // Clear dirty bit in current buffer
                    STA [ZP.IDX], Y
                    
                    // Copy to previous buffer
                    CLC
                    LDA sbOffsetL
                    ADC sbPreviousL
                    STA ZP.IDYL
                    LDA sbOffsetH
                    ADC sbPreviousH
                    STA ZP.IDYH
                    
                    PLA
                    PHA
                    STA [ZP.IDY], Y
                    
                    // Copy attributes to previous
                    INY
                    LDA sbTemp
                    STA [ZP.IDY], Y
                    
                    // Send character
                    PLA
                    Serial.WriteChar();
                    
                    // Update screen cursor position (cursor advanced)
                    INC sbScreenX
                    LDA sbScreenX
                    CMP sbWidth
                    if (Z)
                    {
                        // At end of buffer row - terminal cursor is NOT at column 0
                        // Mark position as unknown
                        LDA #0xFF
                        STA sbScreenX
                        STA sbScreenY
                    }
                }
                
                // Next column
                INC sbCol
                LDA sbCol
                CMP sbWidth
                if (Z) { break; }
            }
            
            // Next row
            INC sbRow
            LDA sbRow
            CMP sbHeight
            if (Z) { break; }
        }
        
        // Position hardware cursor if visible
        LDA sbCursorVisible
        if (NZ)
        {
            LDA CursorX
            LDY CursorY
            Screen.GotoXY();
            Screen.ShowCursor();
        }
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
