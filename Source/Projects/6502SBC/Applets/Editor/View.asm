unit View
{
    uses "System/Definitions"
    uses "System/Memory"
    uses "System/ScreenBuffer"
    uses "System/Print"
    uses "Editor/GapBuffer"
    
    friend Commands;
    
    // String constants for status line
    const string statusFilename = "TEST";
    const string statusLineLabel = " L:";
    const string statusColLabel = " C:";
    const string statusPadding = "                                        ";
    
    // Zero page allocation
    const byte viewSlots = 0x90;
    
    // Persistent state
    const uint vwLogicalCursor = viewSlots+0;   // Position in document
    const uint vwLogicalCursorL = viewSlots+0;
    const uint vwLogicalCursorH = viewSlots+1;
    
    const uint vwTopLine = viewSlots+2;         // First visible line offset
    const uint vwTopLineL = viewSlots+2;
    const uint vwTopLineH = viewSlots+3;
    
    const uint vwCurrentLine = viewSlots+4;     // Current line number
    const uint vwCurrentLineL = viewSlots+4;
    const uint vwCurrentLineH = viewSlots+5;
    
    const byte vwCurrentCol = viewSlots+6;      // Current column
    const byte vwScreenRows = viewSlots+7;      // Viewport height
    const byte vwScreenCols = viewSlots+8;      // Viewport width
    const byte vwModified = viewSlots+9;        // Document modified flag
    const byte vwDirty = viewSlots+10;          // Screen needs refresh
    
    const uint vwLineStarts = viewSlots+11;     // Pointer to line index array
    const uint vwLineStartsL = viewSlots+11;
    const uint vwLineStartsH = viewSlots+12;
    
    const uint vwLineCount = viewSlots+13;      // Number of lines
    const uint vwLineCountL = viewSlots+13;
    const uint vwLineCountH = viewSlots+14;
    
    // Leaf workspace (M8-M15 to avoid conflict with GapBuffer)
    // Safe to use: renderViewport/renderLine don't call Memory or Time functions
    const uint vwLinePos = viewSlots+15;    // Temp line position
    const uint vwLinePosL = viewSlots+15;
    const uint vwLinePosH = viewSlots+16;
    const byte vwRow = viewSlots+17;       // Current render row
    const byte vwCol = viewSlots+18;       // Current render column
    const uint vwCharPos = viewSlots+19;   // Character position
    const uint vwCharPosL = viewSlots+19;
    const uint vwCharPosH = viewSlots+20;
    const byte vwChar = viewSlots+21;      // Current character
    const byte vwTemp = viewSlots+22;      // Temporary
    
    // Initialize view system
    // Input: A = width, Y = height
    // Output: C set on success
    Initialize()
    {
        PHX
        PHY
        
        // Save dimensions
        STA vwScreenCols
        STY vwScreenRows
        
        // Initialize ScreenBuffer (leave room for status line)
        DEY  // One less row for status
        ScreenBuffer.Initialize();
        if (NC)
        {
            PLY
            PLX
            CLC
            return;
        }
        
        // Allocate line index (assume max 1000 lines, 2 bytes each)
        LDA #(2000 % 256)
        LDY #(2000 / 256)
        STA ZP.ACCL
        STY ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            ScreenBuffer.Dispose();
            PLY
            PLX
            CLC
            return;
        }
        
        LDA ZP.IDXL
        STA vwLineStartsL
        LDA ZP.IDXH
        STA vwLineStartsH
        
        // Initialize state
        STZ vwLogicalCursorL
        STZ vwLogicalCursorH
        STZ vwTopLineL
        STZ vwTopLineH
        STZ vwCurrentLineL
        STZ vwCurrentLineH
        STZ vwCurrentCol
        STZ vwModified
        LDA #1
        STA vwDirty
        
        // Set one line in index (empty document)
        LDA #1
        STA vwLineCountL
        STZ vwLineCountH
        
        // First line starts at position 0
        LDA vwLineStartsL
        STA ZP.IDXL
        LDA vwLineStartsH
        STA ZP.IDXH
        LDY #0
        LDA #0
        STA [ZP.IDX], Y
        INY
        STA [ZP.IDX], Y
        
        PLY
        PLX
        SEC
    }
    
    // Dispose of allocated resources
    Dispose()
    {
        PHX
        PHY
        
        // Free line index
        LDA vwLineStartsL
        ORA vwLineStartsH
        if (NZ)
        {
            LDA vwLineStartsL
            STA ZP.IDXL
            LDA vwLineStartsH
            STA ZP.IDXH
            Memory.Free();
            
            STZ vwLineStartsL
            STZ vwLineStartsH
        }
        
        // Dispose screen buffer
        ScreenBuffer.Dispose();
        
        PLY
        PLX
    }
    
    // Public getters
    GetLogicalCursor()
    {
        LDA vwLogicalCursorL
        STA ZP.ACCL
        LDA vwLogicalCursorH
        STA ZP.ACCH
    }
    
    GetCurrentLine()
    {
        LDA vwCurrentLineL
        STA ZP.ACCL
        LDA vwCurrentLineH
        STA ZP.ACCH
    }
    
    IsModified()
    {
        LDA vwModified
        if (NZ) 
        { 
            SEC 
            return;
        }
        CLC
    }
    
    SetModified()
    {
        LDA #1
        STA vwModified
        STA vwDirty
    }
    
    ClearModified()
    {
        STZ vwModified
        LDA #1
        STA vwDirty
    }
    
    SetDirty()
    {
        LDA #1
        STA vwDirty
    }
    
    // Rebuild line index after text changes
    RebuildLineIndex()
    {
        PHX
        PHY
        
        // Start at beginning
        STZ vwCharPosL
        STZ vwCharPosH
        STZ vwLineCountL
        STZ vwLineCountH
        
        LDA vwLineStartsL
        STA ZP.IDXL
        LDA vwLineStartsH
        STA ZP.IDXH
        
        // First line always starts at 0
        LDY #0
        LDA #0
        STA [ZP.IDX], Y
        INY
        STA [ZP.IDX], Y
        INC vwLineCountL
        
        // Scan for newlines
        GapBuffer.GetTextLength();
        LDA ZP.ACCL
        STA vwLinePosL
        LDA ZP.ACCH
        STA vwLinePosH
        
        loop
        {
            // Check if at end
            LDA vwCharPosH
            CMP vwLinePosH
            if (Z)
            {
                LDA vwCharPosL
                CMP vwLinePosL
            }
            if (C) { break; }  // vwCharPos >= vwLinePos
                
            // Get character
            LDA vwCharPosL
            STA ZP.ACCL
            LDA vwCharPosH
            STA ZP.ACCH
            GapBuffer.GetCharAt();
            
            CMP #'\n'
            if (Z)
            {
                // Found newline - record next line start
                Shared.IncIDX();
                Shared.IncIDX();
                
                // Store position after newline
                LDY #0
                LDA vwCharPosL
                CLC
                ADC #1
                STA [ZP.IDX], Y
                INY
                LDA vwCharPosH
                ADC #0
                STA [ZP.IDX], Y
                
                INC vwLineCountL
                if (Z) { INC vwLineCountH }
            }
            
            // Next character
            INC vwCharPosL
            if (Z) { INC vwCharPosH }
        }
        
        PLY
        PLX
    }
    
    // Get start position of line
    // Input: ACC = line number
    // Output: ACC = position, C clear if out of bounds
    getLineStart()
    {
        // Check bounds
        LDA ZP.ACCH
        CMP vwLineCountH
        if (Z)
        {
            LDA ZP.ACCL
            CMP vwLineCountL
        }
        if (C)  // line_number >= line_count (out of bounds)
        {
            CLC
            return;
        }
        
        // Calculate offset in line index
        ASL ZP.ACCL
        ROL ZP.ACCH  // Line number * 2
        
        CLC
        LDA vwLineStartsL
        ADC ZP.ACCL
        STA ZP.IDXL
        LDA vwLineStartsH
        ADC ZP.ACCH
        STA ZP.IDXH
        
        // Read line start position
        LDY #0
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.ACCH
        
        SEC
    }
    
    // Render the viewport
    Render()
    {
        // Check if dirty
        LDA vwDirty
        if (Z) { return; }
        
        STZ vwDirty
        
        ScreenBuffer.Suspend();

        // Rebuild line index if needed
        RebuildLineIndex();
        
        // Clear and render visible lines
        renderViewport();
        
        // Render status line
        renderStatusLine();
        
        // Position cursor
        updateCursorPosition();
        
        ScreenBuffer.Resume();
    }
    
    // Render visible lines
    renderViewport()
    {
        // Set colors for text area
        LDA #Color.White
        ScreenBuffer.SetForeground();
        LDA #Color.Black
        ScreenBuffer.SetBackground();
        
        STZ vwRow
        
        loop
        {
            // Calculate line number to display
            CLC
            LDA vwTopLineL
            ADC vwRow
            STA ZP.ACCL
            LDA vwTopLineH
            ADC #0
            STA ZP.ACCH
            
            // Check if past end of document
            LDA ZP.ACCH
            CMP vwLineCountH
            if (Z)
            {
                LDA ZP.ACCL
                CMP vwLineCountL
            }
            if (C)  // line_to_display >= line_count (past end)
            {
                clearRestOfViewport();
                break;
            }
            
            // Render this line
            renderLine();
            
            INC vwRow
            LDA vwRow
            CMP vwScreenRows
            if (C) { break; }
        } // loop
    }
    
    // Clear remaining rows in viewport
    clearRestOfViewport()
    {
        loop
        {
            LDA vwRow
            CMP vwScreenRows
            if (C) { break; }
            
            LDA #0
            LDY vwRow
            ScreenBuffer.GotoXY();
            
            // Clear to end of line with spaces
            LDX vwScreenCols
            loop
            {
                LDA #' '
                ScreenBuffer.Char();
                DEX
                if (Z) { break; }
            }
            
            INC vwRow
        }
    }
    
    // Render single line
    // Input: ZP.ACC = line number, vwRow = screen row
    renderLine()
    {
        // Get line start position
        getLineStart();
        if (NC) { return; }
        
        LDA ZP.ACCL
        STA vwCharPosL
        LDA ZP.ACCH
        STA vwCharPosH
        
        // Position cursor on screen
        LDA #0
        LDY vwRow
        ScreenBuffer.GotoXY();
        
        STZ vwCol
        
        loop
        {
            // Get character at position
            LDA vwCharPosL
            STA ZP.ACCL
            LDA vwCharPosH
            STA ZP.ACCH
            GapBuffer.GetCharAt();
            
            // Check for end of line or document
            if (Z) 
            { 
                padRestOfLine();
                break;
            }
            CMP #'\n'
            if (Z) 
            { 
                padRestOfLine();
                break;
            }
            
            // Check if at right edge
            LDX vwCol
            CPX vwScreenCols
            if (NC) { break; }  // Line too long, truncate
            
            // Display character
            CMP #'\t'
            if (Z)
            {
                // Tab - expand to spaces
                LDA #' '
                ScreenBuffer.Char();
                INC vwCol
            }
            else
            {
                // Normal character
                ScreenBuffer.Char();
                INC vwCol
            }
            
            // Next character
            INC vwCharPosL
            if (Z) { INC vwCharPosH }
        }
    }
    
    // Pad rest of line with spaces
    padRestOfLine()
    {
        loop
        {
            LDA vwCol
            CMP vwScreenCols
            if (C) { break; }
            
            LDA #' '
            ScreenBuffer.Char();
            INC vwCol
        }
    }
    
    // Render status line
    renderStatusLine()
    {
        // Position at bottom of screen
        LDA #0
        LDY vwScreenRows
        DEY  // Status line is last row
        ScreenBuffer.GotoXY();
        
        // Inverse video for status line
        ScreenBuffer.SetInverse();
        
        // Show modified flag
        LDA vwModified
        if (NZ)
        {
            LDA #'*'
        }
        else
        {
            LDA #' '
        }
        ScreenBuffer.Char();
        LDA #' '
        ScreenBuffer.Char();
        
        // Show filename
        LDA #(statusFilename % 256)
        STA ZP.STRL
        LDA #(statusFilename / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Show line label
        LDA #(statusLineLabel % 256)
        STA ZP.STRL
        LDA #(statusLineLabel / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Convert line number to decimal (simple version for < 100)
        LDA vwCurrentLineL
        INC  // Display as 1-based
        LDX #'0'
        loop
        {
            CMP #10
            if (NC) { break; } // Break when A < 10
            SEC // Redundant but clear
            SBC #10
            INX
        }
        PHA
        TXA
        ScreenBuffer.Char();
        PLA
        ORA #'0'
        ScreenBuffer.Char();
        
        // Show column label
        LDA #(statusColLabel % 256)
        STA ZP.STRL
        LDA #(statusColLabel / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Column number
        LDA vwCurrentCol
        INC  // Display as 1-based
        LDX #'0'
        loop
        {
            CMP #10
            if (NC) { break; } // Break when A < 10
            SEC // Redundant but clear
            SBC #10
            INX
        }
        PHA
        TXA
        ScreenBuffer.Char();
        PLA
        ORA #'0'
        ScreenBuffer.Char();
        
        // Pad rest of status line
        LDA #(statusPadding % 256)
        STA ZP.STRL
        LDA #(statusPadding / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Return to normal video
        ScreenBuffer.SetNotInverse();
    }
    
    // Update cursor display position
    updateCursorPosition()
    {
        // Calculate screen row (current line - top line)
        SEC
        LDA vwCurrentLineL
        SBC vwTopLineL
        STA vwRow
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwRow
        ScreenBuffer.GotoXY();
    }
    
    // Cursor movement functions
    CursorUp()
    {
        PHY
        
        LDA vwCurrentLineL
        ORA vwCurrentLineH
        if (Z) 
        { 
            PLY
            return;  // Already at top
        }
        
        // Move to previous line
        LDA vwCurrentLineL
        if (Z) { DEC vwCurrentLineH }
        DEC vwCurrentLineL
        
        // Update logical cursor position
        updateLogicalCursor();
        
        // Check if need to scroll
        LDA vwCurrentLineH
        CMP vwTopLineH
        if (Z)
        {
            LDA vwCurrentLineL
            CMP vwTopLineL
        }
        if (C)  // vwCurrentLine >= vwTopLine (visible, no scroll needed)
        {
            PLY
            return;
        }
        
        // Scroll up
        LDA vwTopLineL
        if (Z) { DEC vwTopLineH }
        DEC vwTopLineL
        SetDirty();
        
        PLY
    }
    
    CursorDown()
    {
        PHY
        
        // Check if at last line (current_line >= line_count - 1)
        INC vwCurrentLineL
        if (Z) { INC vwCurrentLineH }
        
        LDA vwCurrentLineH
        CMP vwLineCountH
        if (Z)
        {
            LDA vwCurrentLineL
            CMP vwLineCountL
        }
        if (C)  // (current_line + 1) >= line_count
        {
            // Restore original value and return
            LDA vwCurrentLineL
            if (Z) { DEC vwCurrentLineH }
            DEC vwCurrentLineL
            PLY
            return;
        }
        
        // Update logical cursor position
        updateLogicalCursor();
        
        // Check if need to scroll
        // Calculate row position on screen: current_line - top_line
        SEC
        LDA vwCurrentLineL
        SBC vwTopLineL
        STA ZP.ACCL
        LDA vwCurrentLineH
        SBC vwTopLineH
        STA ZP.ACCH
        
        // Check if row >= screen height (beyond bottom)
        LDA ZP.ACCL
        CMP vwScreenRows
        if (C)  // Row >= vwScreenRows (need to scroll)
        {
            // Scroll down
            INC vwTopLineL
            if (Z) { INC vwTopLineH }
            SetDirty();
        }
        
        PLY
    }
    
    CursorLeft()
    {
        LDA vwCurrentCol
        if (NZ)
        {
            DEC vwCurrentCol
            LDA vwLogicalCursorL
            if (Z)
            {
                DEC vwLogicalCursorH
            }
            DEC vwLogicalCursorL
        }
        else
        {
            // At beginning of line - move to end of previous line
            CursorUp();
            CursorEnd();
        }
    }
    
    CursorRight()
    {
        // Get current character to check for end of line
        LDA vwLogicalCursorL
        STA ZP.ACCL
        LDA vwLogicalCursorH
        STA ZP.ACCH
        GapBuffer.GetCharAt();
        
        if (Z) { return; }  // End of document
        CMP #'\n'
        if (Z)
        {
            // At end of line - move to start of next line
            CursorDown();
            CursorHome();
        }
        else
        {
            INC vwCurrentCol
            INC vwLogicalCursorL
            if (Z) { INC vwLogicalCursorH }
        }
    }
    
    CursorHome()
    {
        STZ vwCurrentCol
        updateLogicalCursor();
    }
    
    CursorEnd()
    {
        PHX
        
        // Find end of current line
        LDA vwCurrentLineL
        STA ZP.ACCL
        LDA vwCurrentLineH
        STA ZP.ACCH
        getLineStart();
        
        LDA ZP.ACCL
        STA vwCharPosL
        LDA ZP.ACCH
        STA vwCharPosH
        
        STZ vwCurrentCol
        
        loop
        {
            LDA vwCharPosL
            STA ZP.ACCL
            LDA vwCharPosH
            STA ZP.ACCH
            GapBuffer.GetCharAt();
            
            if (Z) { break; }  // End of document
            CMP #'\n'
            if (Z) { break; }  // End of line
            
            INC vwCurrentCol
            INC vwCharPosL
            if (Z) { INC vwCharPosH }
        }
        
        updateLogicalCursor();
        
        PLX
    }
    
    CursorPageUp()
    {
        PHX
        
        // Move up by screen height
        LDX vwScreenRows
        loop
        {
            CursorUp();
            LDA vwCurrentLineL
            ORA vwCurrentLineH
            if (Z) { break; }  // Hit top
            DEX
            if (Z) { break; }
        }
        
        PLX
    }
    
    CursorPageDown()
    {
        PHX
        
        // Move down by screen height
        LDX vwScreenRows
        loop
        {
            CursorDown();
            DEX
            if (Z) { break; }
        }
        
        PLX
    }
    
    // Update logical cursor based on current line and column
    updateLogicalCursor()
    {
        // Get start of current line
        LDA vwCurrentLineL
        STA ZP.ACCL
        LDA vwCurrentLineH
        STA ZP.ACCH
        getLineStart();
        if (NC) { return; }  // Line doesn't exist
        
        // Add column offset
        CLC
        LDA ZP.ACCL
        ADC vwCurrentCol
        STA vwLogicalCursorL
        LDA ZP.ACCH
        ADC #0
        STA vwLogicalCursorH
    }
}
