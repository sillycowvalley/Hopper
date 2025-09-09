unit ScreenBuffer
{
    uses "Definitions"
    uses "Memory"
    uses "Serial"
    uses "Screen"
    uses "Shared"
    
    // Attribute bit flags
    enum Attribute
    {
        Normal  = 0b00000000,
        Bold    = 0b01000000,
        Inverse = 0b10000000,
    }
    // Attributes are:
    // Bit 0-2 : foreground colour
    // Bit 3-5 : background colour
    // Bit 6-7 : Attr
    
        
    // Internal Constants:
    const byte dirtyBit = 0b10000000;
    const byte charMask = 0b01111111;
    
    // Public zero page properties
    const byte zeroPageSlots = 0x70;
    
    const byte CursorCol  = zeroPageSlots+0;
    const byte CursorRow  = zeroPageSlots+1;
    const byte Foreground = zeroPageSlots+2;      // Current foreground color (0-7)
    const byte Background = zeroPageSlots+3;      // Current background color (0-7)
    const byte Attributes = zeroPageSlots+4;      // Current attributes (bit 0 = bold, bit 1 = inverse)
    
    
    // Private Workspace
    const byte sbWidth  = zeroPageSlots+5;
    const byte sbHeight = zeroPageSlots+6;
    
    const uint sbBuffer  = zeroPageSlots+7; 
    const uint sbBufferL = zeroPageSlots+7;
    const uint sbBufferH = zeroPageSlots+8;
    
    const byte sbSuspendCount  = zeroPageSlots+9;
    const byte sbCursorVisible = zeroPageSlots+10;
    
    // Leaf node workspace slots
    const uint msbSize    = ZP.M0;        // Total buffer size (2 bytes)
    const uint msbSizeL   = ZP.M0;
    const uint msbSizeH   = ZP.M1;
    
    const byte msbRow     = ZP.M2;
    const byte msbCol     = ZP.M3;
    
    const byte msbOffset  = ZP.M4;
    const byte msbOffsetL = ZP.M4;
    const byte msbOffsetH = ZP.M5;
    
    const byte msbCharacter = ZP.M6;
    const byte msbAttribute = ZP.M7;
    
    const byte msbLastRow   = ZP.M8;
    const byte msbLastCol   = ZP.M9;
    
    
    // Helper: calculate buffer offset for A = col, Y = row
    calculateOffset() // Returns offset in sbOffset, munts X
    {
        PHX
        
        // save arguments
        STA msbCol
        STY msbRow
        
        // offset = (sbRow * sbWidth + sbCol) * 2
        STZ msbOffsetL
        STZ msbOffsetH
        
        // Add sbWidth to offset sbRow times
        LDX msbRow
        if (NZ)
        {
            loop
            {
                CLC
                LDA msbOffsetL
                ADC sbWidth
                STA msbOffsetL
                LDA msbOffsetH
                ADC #0
                STA msbOffsetH
                DEX
                if (Z) { break; }
            }
        }
        
        // Add sbCol
        CLC
        LDA msbOffsetL
        ADC msbCol
        STA msbOffsetL
        LDA msbOffsetH
        ADC #0
        STA msbOffsetH
        
        // Double for 2 bytes per cell
        ASL msbOffsetL
        ROL msbOffsetH
        
        PLX
    }
    
    calculateBufferSize()
    {
        LDA #0
        LDY sbHeight
        calculateOffset();
    }
    
    
    // Helper: pack current attributes into byte
    packAttributes() // Returns packed byte in A
    {
        LDA Background      // Bits 3-5
        ASL A ASL A ASL A
        
        ORA Foreground      // Bits 0-2
              
        ORA Attributes      // Bits 6-7
        // return A
    }
    
    
    // Initialize buffer system
    Initialize() // Input: A = width, Y = height
    {
        // Save arguments
        STA sbWidth
        STY sbHeight
        
        // Calculate total size = width (A) * height (Y) * 2
        calculateBufferSize();
        LDA msbOffsetL
        STA ZP.ACCL
        LDA msbOffsetH
        STA ZP.ACCH
        
        // Allocate current buffer
        Memory.Allocate();
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            STZ sbBufferL
            STZ sbBufferH
            CLC  // Allocation failed
            return;
        }
        LDA ZP.IDXL
        STA sbBufferL
        LDA ZP.IDXH
        STA sbBufferH
        
        // Set defaults
        ScreenBuffer.Reset();
        STZ sbSuspendCount
        
        // Clear screen
        Clear();
        
        SEC  // Success
    }
    
    // Free allocated memory
    Dispose()
    {
        // Free current buffer
        LDA sbBufferL
        ORA sbBufferH
        if (NZ)
        {
            LDA sbBufferL
            STA ZP.IDXL
            LDA sbBufferH
            STA ZP.IDXH
            Memory.Free();
            
            STZ sbBufferL
            STZ sbBufferH
        }
    }
    
    // Reset to white on black, normal, home cursor visible
    Reset()
    {
        // Set defaults
        LDA # Screen.Color.White
        STA Foreground
        LDA # Screen.Color.Black
        STA Background
        STZ Attributes
        STZ CursorCol
        STZ CursorRow
        ScreenBuffer.ShowCursor();
    }
    
       
    // Set foreground color
    SetForeground() inline // Input: A = color (0-7)
    {
        STA Foreground
    }
    
    // Set background color
    SetBackground() inline // Input: A = color (0-7)
    {
        STA Background
    }
       
    // Enable bold
    SetBold() inline
    {
        SMB6 Attributes
    }
    SetNotBold() inline
    {
        RMB6 Attributes
    }
    
    // Enable inverse
    SetInverse() inline
    {
        SMB7 Attributes
    }
    SetNotInverse() inline
    {
        RMB7 Attributes
    }
    
    // Clear all special attributes
    SetNormal() inline
    {
        STZ Attributes
    }
    
    // Suspend updates
    Suspend() inline
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
        SMB0 sbCursorVisible
        Screen.ShowCursor();
    }
    
    // Hide cursor after updates
    HideCursor() inline
    {
        STZ sbCursorVisible
        Screen.HideCursor();
    }
    
    // Position cursor
    GotoXY() // Input: A = col, Y = row
    {
        STA CursorCol
        STY CursorRow
    }
    
    // Clear buffer with current background color
    Clear()
    {
        Suspend();
        
        packAttributes(); // current background, foreground and attributes
        STA ZP.TEMP       // Save packed attributes
        
        // Start at beginning of buffer
        LDA sbBufferL
        STA ZP.IDXL
        LDA sbBufferH
        STA ZP.IDXH
        
        // Calculate total cells = width * height into IDY
        calculateBufferSize();
        LDA msbOffsetL
        STA ZP.IDYL
        LDA msbOffsetH
        STA ZP.IDYH
        // byte size / 2 -> cell size 
        LSR ZP.IDYH
        ROR ZP.IDYL
               
        // Loop through all cells
        loop
        {
            // Store space with dirty bit
            LDA #('_' | dirtyBit)
            STA [ZP.IDX]
            
            // Store attributes
            LDY #1
            LDA ZP.TEMP
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
        
        STZ CursorCol
        STZ CursorRow
        
        Resume();
    }    
    
    // Write character at cursor position
    Char() // Input: A = character
    {
        PHY
        Suspend();
        
        STA ZP.TEMP
        
        LDA CursorRow
        CMP sbHeight
        if (NC) // CursorRow < sbHeight
        {
            LDA CursorCol
            CMP sbWidth
            if (NC) // CursorCol < sbWidth
            {
                LDY CursorRow
                calculateOffset();
                
                // Add base address to offset
                CLC
                LDA msbOffsetL
                ADC sbBufferL
                STA ZP.IDXL
                LDA msbOffsetH
                ADC sbBufferH
                STA ZP.IDXH
                
                LDA ZP.TEMP
                CMP [ZP.IDX]
                if (NZ) // character has changed
                {
                    // Store character with dirty bit
                    ORA #dirtyBit
                    STA [ZP.IDX]
                    
                    // Store packed attributes
                    packAttributes();
                    LDY #1
                    STA [ZP.IDX], Y
                }
                else
                {
                    LDY #1
                    packAttributes();
                    CMP [ZP.IDX], Y
                    if (NZ) // attributes have changed
                    {
                        STA [ZP.IDX], Y
                        
                        // mark cell as dirty
                        LDA ZP.TEMP
                        ORA #dirtyBit
                        STA [ZP.IDX]
                    }
                }
            }
        }
        
        // Advance cursor
        INC CursorCol
        Resume();
        PLY
    }
    
    // Write string at cursor position
    String() // Input : STR = string
    {
        Suspend();
        LDY #0
        loop
        {
            LDA [STR], Y
            if (Z) { break; }
            Char();
            INY
        }   
        Resume();
    }
    
    // Force redraw everything
    Redraw()
    {
        // Start at beginning of buffer
        LDA sbBufferL
        STA ZP.IDXL
        LDA sbBufferH
        STA ZP.IDXH
        
        // Calculate total cells = width * height into IDY
        calculateBufferSize();
        LDA sbOffsetL
        STA ZP.IDYL
        LDA sbOffsetH
        STA ZP.IDYH
        // byte size / 2 -> cell size 
        LSR ZP.IDYH
        ROR ZP.IDYL
               
        // Loop through all cells
        loop
        {
            // Store space with dirty bit
            LDA [ZP.IDX]
            ORA # dirtyBit
            STA [ZP.IDX]
            
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
                
        // Now update
        Update();
    }
    
    Update()
    {
        // Check suspend count
        LDA sbSuspendCount
        if (NZ) { return; }
        
        LDA sbCursorVisible
        if (NZ)
        {
            // Hide cursor during update
            Screen.HideCursor();
        }
        
        
        // Start at beginning of buffer
        LDA sbBufferL
        STA ZP.IDXL
        LDA sbBufferH
        STA ZP.IDXH
        
        // Calculate total cells = width * height into IDY
        calculateBufferSize();
        LDA msbOffsetL
        STA ZP.IDYL
        LDA msbOffsetH
        STA ZP.IDYH
        // byte size / 2 -> cell size 
        LSR ZP.IDYH
        ROR ZP.IDYL

        // set attributes to normal
        Screen.Reset(); 
        
        LDA # Screen.Color.Black // Bits 3-5
        ASL A ASL A ASL A
        ORA # Screen.Color.White // Bits 0-2
        STA msbAttribute
                        
        STZ msbRow
        STZ msbCol     
        LDA #0xFF
        STA msbLastCol
        STA msbLastRow
        
        // Loop through all cells
        loop
        {
            LDA [ZP.IDX]
            if (MI) // dirty?
            {
                AND # charMask
                STA msbCharacter // current character
                STA [ZP.IDX] // clear dirty
                
                LDA msbAttribute
                STA ZP.TEMP // old attribute
                    
                LDY #1
                LDA [ZP.IDX], Y  
                CMP msbAttribute
                if (NZ)
                {
                    STA msbAttribute // new current attribute
                    
                    // Apply bold if needed
                    if (BBS6, msbAttribute)
                    {
                        if (BBR6, ZP.TEMP) // was not bold
                        {
                            Screen.Bold(); // SysCalls : munt A, X
                        }
                    }
                    else
                    {
                        if (BBS6, ZP.TEMP) // was bold
                        {
                            Screen.BoldOff(); // SysCalls : munt A, X
                        }
                    }
                    
                    // Apply inverse if needed
                    if (BBS7, msbAttribute)
                    {
                        if (BBR7, ZP.TEMP) // was not inverse
                        {
                            Screen.Inverse(); // SysCalls : munt A, X
                        }
                    }
                    else
                    {
                        if (BBS7, ZP.TEMP) // was inverse
                        {
                            Screen.InverseOff(); // SysCalls : munt A, X
                        }
                    }
                    LDA ZP.TEMP
                    PHA
                    AND #0b00000111
                    STA ZP.TEMP
                    LDA msbAttribute
                    AND #0b00000111
                    CMP ZP.TEMP
                    if (NZ)
                    {
                        // Extract and set foreground color
                        LDA msbAttribute
                        AND #0b00000111
                        Screen.Foreground(); // SysCalls : munt A, X
                    }
                    PLA
                    AND #0b00111000
                    STA ZP.TEMP
                    LDA msbAttribute
                    AND #0b00111000
                    CMP ZP.TEMP
                    if (NZ)
                    {
                        // Extract and set background color
                        LDA msbAttribute
                        LSR A LSR A LSR A
                        AND #0b00000111
                        Screen.Background(); // SysCalls : munt A, X
                    }
                }
                LDA msbLastCol
                CMP msbCol
                if (Z)
                {
                    LDA msbLastRow
                    CMP msbRow
                }
                if (NZ)
                {
                    LDA msbCol
                    STA msbLastCol
                    LDY msbRow
                    STY msbLastRow
                    Screen.GotoXY();
                }
                LDA msbCharacter
                Screen.Char(); // SysCalls : munt A, X
                
                INC msbLastCol // drawing Char advanced column
            }
            // Move to next cell (2 bytes forward)
            Shared.IncIDX();
            Shared.IncIDX();
            
            // Decrement cell counter
            Shared.DecIDY();
            
            // Check if done
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z) { break; }
            
            // Advance current position
            INC msbCol
            LDA msbCol
            CMP sbWidth
            if (Z)
            {
                STZ msbCol
                INC msbRow
            }
        }
        
        // Position hardware cursor if visible
        LDA sbCursorVisible
        if (NZ)
        {
            LDA CursorCol
            LDY CursorRow
            Screen.GotoXY();
            Screen.ShowCursor();
        }
    }
    
    // ScrollUp should only be called if there are no dirty bits in the buffer!
    // A = background colour for last row
    ScrollUp()
    {
        ASL A ASL A ASL A
        STA ZP.TEMP
        
        Suspend();
        
        // Start at beginning of buffer
        LDA sbBufferL
        STA ZP.IDXL
        LDA sbBufferH
        STA ZP.IDXH
        
        // Calculate total cells = width * height into IDY
        calculateBufferSize();
        LDA sbOffsetL
        STA ZP.IDYL
        LDA sbOffsetH
        STA ZP.IDYH
        // byte size / 2 -> cell size 
        LSR ZP.IDYH
        ROR ZP.IDYL
        
        // Substract last row
        SEC
        LDA ZP.IDYL
        SBC sbWidth
        STA ZP.IDYL
        LDA ZP.IDYH
        SBC #0
        STA ZP.IDYH
        
        loop
        {
            LDA sbWidth
            ASL // sbWidth x2
            TAY
            
            // source cell
            LDA [ZP.IDX], Y
            AND # charMask
            STA sbCharacter
            INY 
            LDA [ZP.IDX], Y
            STA sbAttribute
            
            // destination cell
            LDX #1
            LDA [ZP.IDX]
            AND # charMask
            CMP sbCharacter
            if (Z)
            {
                LDY #1
                LDA [IDX], Y
                CMP sbAttribute
                if (Z)
                {
                    // no change
                    LDX #0
                }
            }
            CPX #0
            if (NZ)
            {
                LDA sbCharacter
                ORA # dirtyBit
                STA [ZP.IDX]
                LDA sbAttribute
                LDY #1
                STA [ZP.IDX], Y
            }
            
            IncIDX();
            IncIDX();
            DecIDY();
            LDA ZP.IDYH
            ORA ZP.IDYL
            if (Z) { break; }
        } // loop
        
        // last row
        LDX sbWidth
        loop
        {
            LDA #' '
            ORA # dirtyBit
            STA [ZP.IDX]
            
            LDY #1
            LDA ZP.TEMP
            STA [ZP.IDX], Y
        
            
            IncIDX();
            IncIDX();
            DEX
            if (Z) { break; }
        }
        
        Resume();
    }
    
    // ScrollDown should only be called if there are no dirty bits in the buffer!
    // A = background colour for first row
    ScrollDown()
    {
        ASL A ASL A ASL A
        STA ZP.TEMP
        
        Suspend();
        
        LDA #0
        LDY sbHeight
        DEY
        calculateOffset();
        LDA sbOffsetL
        STA ZP.IDYL
        LDA sbOffsetH
        STA ZP.IDYH
        CLC
        LDA sbBufferL
        ADC sbOffsetL
        STA ZP.IDXL
        LDA sbBufferH
        ADC sbOffsetH
        STA ZP.IDXH
        
        // Back up by 2 bytes to point to last cell(on the 2nd last row)
        DecIDX();
        DecIDX();
        
        // Calculate cells to process = (height-1) * width
        // byte size / 2 -> cell size 
        LSR ZP.IDYH
        ROR ZP.IDYL
        
        loop
        {
            // source cell
            LDA [ZP.IDX]
            AND # charMask
            STA sbCharacter
            LDY #1 
            LDA [ZP.IDX], Y
            STA sbAttribute
            
            // destination cell (next row down)
            LDA sbWidth
            ASL // sbWidth x2
            TAY
            
            LDX #1
            LDA [ZP.IDX], Y
            AND # charMask
            CMP sbCharacter
            if (Z)
            {
                INY
                LDA [ZP.IDX], Y
                CMP sbAttribute
                if (Z)
                {
                    // no change
                    LDX #0
                }
            }
            CPX #0
            if (NZ)
            {
                LDA sbWidth
                ASL // sbWidth x2
                TAY
                LDA sbCharacter
                ORA # dirtyBit
                STA [ZP.IDX], Y
                LDA sbAttribute
                INY
                STA [ZP.IDX], Y
            }
            
            // Move backwards by one cell
            DecIDX();
            DecIDX();
            
            DecIDY();
            LDA ZP.IDYH
            ORA ZP.IDYL
            if (Z) { break; }
        } // loop
        
        // Clear first row
        LDX sbWidth
        LDY #0
        loop
        {
            LDA #' '
            ORA # dirtyBit
            STA [sbBuffer], Y
            
            INY
            LDA ZP.TEMP
            STA [sbBuffer], Y
            INY
            
            DEX
            if (Z) { break; }
        }
        
        Resume();
    }
    
    DumpBuffer()  // Debug routine - hex dump of buffer
    {
        // Save cursor position
        LDA CursorCol
        PHA
        LDA CursorRow
        PHA
        
        Screen.Reset();
        // Start at screen position 0,15
        LDA #0
        LDY #15
        Screen.GotoXY();
        
        // Start at beginning of buffer
        LDA sbBufferL
        STA ZP.IDXL
        LDA sbBufferH
        STA ZP.IDXH
        
        // Row counter
        STZ sbRow
        
        loop
        {
            // Print row number
            LDA sbRow
            Print.Hex();
            LDA #':'
            Print.Char();
            LDA ZP.IDXH
            Print.Hex();
            LDA ZP.IDXL
            Print.Hex();
            Print.Space();
            
            // Column counter
            STZ sbCol
            
            loop
            {
                // Read and print character byte (with dirty bit)
                LDA [ZP.IDX]
                Print.Hex();
                Print.Space();
                
                // Read and print attribute byte
                LDY #1
                LDA [ZP.IDX], Y
                STA ZP.TEMP
                AND #0b11000000
                LSR A LSR A LSR A LSR A LSR A LSR A 
                ADC #'0'
                Print.Char();                
                Print.Space();
                
                LDA ZP.TEMP
                AND #0b00111000
                LSR A LSR A LSR A
                ADC #'0'
                Print.Char();                
                Print.Space();
                
                LDA ZP.TEMP
                AND #0b00000111
                ADC #'0'
                Print.Char();
                Print.Space();                
                
                
                // Move to next cell
                Shared.IncIDX();
                Shared.IncIDX();
                
                // Next column
                INC sbCol
                LDA sbCol
                CMP sbWidth
                if (Z) { break; }
            }
            
            // New line
            Print.NewLine();
            
            // Next row
            INC sbRow
            LDA sbRow
            CMP sbHeight
            if (Z) { break; }
        }
        
        // Restore cursor
        PLY
        PLA
        Screen.GotoXY();
    }
}

