unit View
{
    uses "System/ScreenBuffer"
    uses "Editor/GapBuffer"
    
    
    // Zero page allocation
    const byte vwSlots = 0x90;
    
    // View state
    const byte vwScreenCols = vwSlots+0;     // Screen width (80)
    const byte vwScreenRows = vwSlots+1;     // Screen height (24)
    const byte vwCurrentRow = vwSlots+2;     // Current cursor row on screen (0-23)
    const byte vwCurrentCol = vwSlots+3;     // Current cursor column on screen (0-79)
    const uint vwTopLine = vwSlots+4;        // First visible line number
    const byte vwTopLineL = vwSlots+5;
    const byte vwTopLineH = vwSlots+6;
    const uint vwLineCount = vwSlots+7;      // Total lines in document
    const byte vwLineCountL = vwSlots+8;
    const byte vwLineCountH = vwSlots+9;
    
    // Temporary workspace
    const uint vwPos = vwSlots+10;            // Current position in text
    const byte vwPosL = vwSlots+10;
    const byte vwPosH = vwSlots+11;
    
    const uint vwLeafTemp  = vwSlots+12;     // used only in leaf methods
    const byte vwLeafTempL = vwSlots+12;
    const byte vwLeafTempH = vwSlots+13;
    
    const uint vwSkipCount  = vwSlots+14;     // used only in Render
    const byte vwSkipCountL = vwSlots+14;
    const byte vwSkipCountH = vwSlots+15;
    
    const string memoryMsg = "Out of memory in View!\n";
    const string rowLabel  = "Ln:";
    const string colLabel  = "  Ch:";
    
    const byte viewWidth              = 80;
    const byte viewHeight             = 25;
    const byte statusAreaHeight       = 1;
    const byte positionIndicatorWidth = 20;
    
    // Initialize View
    Initialize()
    {
        // Save screen dimensions
        LDA # viewWidth
        STA vwScreenCols
        LDA # (viewHeight - statusAreaHeight)
        STA vwScreenRows
        
        // Initialize ScreenBuffer
        LDA # viewWidth
        LDY # viewHeight  // Full screen including status line
        ScreenBuffer.Initialize();
        if (NC)
        {
            LDA #(memoryMsg % 256)
            STA ZP.STRL
            LDA #(memoryMsg / 256)
            STA ZP.STRH
            Print.String();
            CLC
            return;
        }
        
        // Initialize GapBuffer (8KB)
        LDA #(8192 % 256)
        LDY #(8192 / 256)
        GapBuffer.Initialize();
        if (NC)
        {
            ScreenBuffer.Dispose();
            LDA #(memoryMsg % 256)
            STA ZP.STRL
            LDA #(memoryMsg / 256)
            STA ZP.STRH
            Print.String();
            CLC
            return;
        }
        
        // Initialize view state
        STZ vwCurrentRow
        STZ vwCurrentCol
        STZ vwTopLineL
        STZ vwTopLineH
        STZ vwLineCountL
        STZ vwLineCountH
        
        SEC  // Success
    }
    ApplyGapBuffer()
    {
        // Initialize view state
        STZ vwCurrentRow
        STZ vwCurrentCol
        STZ vwTopLineL
        STZ vwTopLineH
        
        // Count lines in the loaded text
        countLines();
        
        // Initial render
        ScreenBuffer.Suspend();
        ScreenBuffer.Clear();
        Render();
        ScreenBuffer.Resume();
    }
    
    // Cleanup
    Dispose()
    {
        GapBuffer.Dispose();
        ScreenBuffer.Dispose();
    }
    
    
    // Count lines in the document
    countLines()
    {
        STZ vwLineCountL
        STZ vwLineCountH
        INC vwLineCountL  // Start with 1 line
        
        // Get text length
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA vwLeafTempL
        LDA GapBuffer.GapValueH
        STA vwLeafTempH
        
        // Scan for newlines
        STZ vwPosL
        STZ vwPosH
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP vwLeafTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwLeafTempL
            }
            if (C) { break; }  // pos >= length
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Check for newline
            CMP #'\n'
            if (Z)
            {
                INC vwLineCountL
                if (Z) { INC vwLineCountH }
            }
            
            // Next position
            INC vwPosL
            if (Z) { INC vwPosH }
        }
    }
    
    // Get cursor's logical position in GapBuffer
    // Output: GapBuffer.GapValue = cursor position
    GetCursorPosition()
    {
        // Calculate which document line we're on
        CLC
        LDA vwTopLineL
        ADC vwCurrentRow
        STA ZP.ACCL
        LDA vwTopLineH
        ADC #0
        STA ZP.ACCH
        
        // Find start of that line
        STZ vwPosL
        STZ vwPosH
        
        // Skip to current line if not on first line
        LDA ZP.ACCL
        ORA ZP.ACCH
        if (NZ)
        {
            STZ vwSkipCountL
            STZ vwSkipCountH
            
            loop
            {
                // Check if reached target line
                LDA vwSkipCountH
                CMP ZP.ACCH
                if (Z)
                {
                    LDA vwSkipCountL
                    CMP ZP.ACCL
                }
                if (C) { break; }  // skipCount >= targetLine
                
                findNextNewline();  // Advances vwPos past newline
                
                INC vwSkipCountL
                if (Z) { INC vwSkipCountH }
            }
        }
        
        // Now vwPos is at start of current line
        // Add column offset to get cursor position
        CLC
        LDA vwPosL
        ADC vwCurrentCol
        STA GapBuffer.GapValueL
        LDA vwPosH
        ADC #0
        STA GapBuffer.GapValueH
        
        // That's it - return the logical position
    } 
    
    
    
    
    // Helper: Get length of current line (topLine + currentRow)
    getCurrentLineLength()  // Returns length in A (capped at screenCols)
    {
        // Calculate which document line we're on
        CLC
        LDA vwTopLineL
        ADC vwCurrentRow
        STA ZP.ACCL
        LDA vwTopLineH
        ADC #0
        STA ZP.ACCH
        
        // Find start of current line
        STZ vwPosL
        STZ vwPosH
        
        // Skip to current line if not first
        LDA ZP.ACCL
        ORA ZP.ACCH
        if (NZ)
        {
            STZ vwSkipCountL
            STZ vwSkipCountH
            
            loop
            {
                LDA vwSkipCountH
                CMP ZP.ACCH
                if (Z)
                {
                    LDA vwSkipCountL
                    CMP ZP.ACCL
                }
                if (C) { break; }
                
                findNextNewline();// uses vwLeafTemp
                
                INC vwSkipCountL
                if (Z) { INC vwSkipCountH }
            }
        }
        
        // Now count characters on this line
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA vwLeafTempL
        LDA GapBuffer.GapValueH
        STA vwLeafTempH
        
        LDX #0  // Character counter
        loop
        {
            // Check if at end of text
            LDA vwPosH
            CMP vwLeafTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwLeafTempL
            }
            if (C) { break; }
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Check for newline
            CMP #'\n'
            if (Z) { break; }
            
            INX
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
            
            // Cap at screen width
            CPX vwScreenCols
            if (C) { break; }
        }
        
        TXA  // Return length in A
    }
    
    // Render the current view
    Render()
    {
        ScreenBuffer.Suspend();
        
        // Start at top of viewport
        STZ vwPosL
        STZ vwPosH
        
        STZ vwSkipCountL
        STZ vwSkipCountH
        
        // Find start of top visible line
        LDA vwTopLineL
        ORA vwTopLineH
        if (NZ)
        {
            // Skip to start of top line
            loop
            {
                // Check if reached target line
                LDA vwSkipCountH
                CMP vwTopLineH
                if (Z)
                {
                    LDA vwSkipCountL
                    CMP vwTopLineL
                }
                if (C)
                {
                    break;  // temp >= topLine
                }
                
                // Find next newline
                findNextNewline();
                
                // Count the line
                INC vwSkipCountL
                if (Z) { INC vwSkipCountH }
            }
        }
                
        // Render visible lines
        LDY #0  // Screen row
        loop
        {
            PHY
            
            // Position cursor at start of line
            LDA #0
            ScreenBuffer.GotoXY();
            
            // Render one line
            renderLine();
            
            PLY
            INY
            CPY vwScreenRows
            if (C) { break; }  // row >= screenRows
        }
        
        ScreenBuffer.Resume();
        
        // Position cursor
        View.Update();
    }
    
    Update()
    {
        ScreenBuffer.Suspend();
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        updatePosition();
        
        ScreenBuffer.Resume();
    }
    
    // Helper: Find next newline from current vwPos
    findNextNewline()
    {
        // Get text length
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA vwLeafTempL
        LDA GapBuffer.GapValueH
        STA vwLeafTempH
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP vwLeafTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwLeafTempL
            }
            if (C) { break; }  // pos >= length
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
            
            // Check for newline
            CMP #'\n'
            if (Z) { break; }
        }
    }
    
    // Render one line from current vwPos
    renderLine()
    {
        // Get text length
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA vwLeafTempL
        LDA GapBuffer.GapValueH
        STA vwLeafTempH
        
        LDX #0  // Column counter
        loop
        {
            // Check if at end of document
            LDA vwPosH
            CMP vwLeafTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwLeafTempL
            }
            if (C)
            {
                break;   // pos >= length
            }
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
            
            // Check for newline
            CMP #'\n'
            if (Z)
            {
                break;
            }
            
            // Check if at right edge
            CPX vwScreenCols
            if (C) 
            {
                break;  // col >= screenCols
            }
            
            // Display character
            ScreenBuffer.Char();
            INX
        }
        
        // fill the remainder of the line with ' '
        loop
        {
            // Check if at right edge
            CPX vwScreenCols
            if (C) 
            {
                break;  // col >= screenCols
            }
            
            LDA #' '
            ScreenBuffer.Char();
            INX
        }
    }
    
    CursorUp()
    {
        loop
        {
            LDA vwCurrentRow
            if (NZ)
            {
                // Move up on screen
                DEC vwCurrentRow
            }
            else
            {
                // At top of screen, scroll up if possible
                LDA vwTopLineL
                ORA vwTopLineH
                if (NZ)
                {
                    // Scroll up
                    LDA vwTopLineL
                    if (Z) { DEC vwTopLineH }
                    DEC vwTopLineL
                    
                    Render();
                    break;
                }
            }
            
            View.Update(); // render cursor
            
            break;
        } // loop
    }
    
    CursorDown()
    {
        loop
        {
            // Check if at last line of document
            CLC
            LDA vwTopLineL
            ADC vwCurrentRow
            STA ZP.ACCL
            LDA vwTopLineH
            ADC #0
            STA ZP.ACCH
            
            // Increment to get next line
            INC ZP.ACCL
            if (Z) { INC ZP.ACCH }
            
            // Check if next line exists
            LDA ZP.ACCH
            CMP vwLineCountH
            if (Z)
            {
                LDA ZP.ACCL
                CMP vwLineCountL
            }
            if (C) 
            {
                break;  // nextLine >= lineCount
            }
            
            // Move down
            INC vwCurrentRow
            LDA vwCurrentRow
            CMP vwScreenRows
            if (C)  // row >= screenRows
            {
                // At bottom of screen, scroll down
                DEC vwCurrentRow
                INC vwTopLineL
                if (Z) { INC vwTopLineH }
                
                Render();
                break;
            }
            
            View.Update(); // render cursor
            
            break;
        } // loop
    }
    
    CursorLeft()
    {
        loop
        {
            LDA vwCurrentCol
            if (Z)
            {
                // Already at column 0
                break;
            }
            
            DEC vwCurrentCol
            break;
        }
        
        View.Update();
    }
    
    CursorRight()
    {
        loop
        {
            // Get current line length -> A
            getCurrentLineLength();
            
            // Check if can move right
            CMP vwCurrentCol
            if (NC)     // lineLength < currentCol (includes equal due to 0-indexing)
            {
                break;  // Can't move right
            }
            if (Z)      // lineLength == currentCol  
            {
                break;  // At end of line
            }
            
            INC vwCurrentCol
            break;
        }
        
        View.Update();
    }
    
    // Move cursor to beginning of current line
    CursorHome()
    {
        STZ vwCurrentCol
        View.Update();
    }
    
    // Move cursor to end of current line
    CursorEnd()
    {
        // Get current line length
        getCurrentLineLength();  // Returns length in A
        
        // Position cursor at end (or at line length if shorter than cursor pos)
        CMP vwScreenCols
        if (C)  // lineLength >= screenCols
        {
            LDA vwScreenCols
            DEC A  // Column 79 for 80-column screen
        }
        STA vwCurrentCol
        
        View.Update();
    }

    // Move up one page
    PageUp()
    {
        loop
        {
            // Can scroll a full page
            SEC
            LDA vwTopLineL
            SBC vwScreenRows
            STA vwTopLineL
            LDA vwTopLineH
            SBC #0
            STA vwTopLineH
            if (MI)
            {
                STZ vwTopLineL
                STZ vwTopLineH
            }
            
            Render();
            break;
        }
        
        View.Update();
    }
    
    // Move down one page
    PageDown()
    {
        loop
        {
            // Calculate maximum valid TopLine (LineCount - screenRows)
            SEC
            LDA vwLineCountL
            SBC vwScreenRows
            STA ZP.ACCL
            LDA vwLineCountH
            SBC #0
            STA ZP.ACCH
            if (MI)  // Document shorter than screen
            {
                STZ ZP.ACCL
                STZ ZP.ACCH
            }
            
            // Add screenRows to TopLine
            CLC
            LDA vwTopLineL
            ADC vwScreenRows
            STA vwTopLineL
            LDA vwTopLineH
            ADC #0
            STA vwTopLineH
            
            // Clamp to maximum if exceeded
            LDA vwTopLineH
            CMP ZP.ACCH
            if (Z)
            {
                LDA vwTopLineL
                CMP ZP.ACCL
            }
            if (C)  // TopLine > maximum
            {
                LDA ZP.ACCL
                STA vwTopLineL
                LDA ZP.ACCH
                STA vwTopLineH
            }
            
            Render();
            break;
        }
        
        View.Update();
    }
    
    // Internal: Update line/col display (right side of status)
    updatePosition()
    {
        ScreenBuffer.Suspend();
        
        LDA # (viewWidth - positionIndicatorWidth)
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        
        // Print line
        LDA #(rowLabel / 256)
        STA ZP.STRH
        LDA #(rowLabel % 256)
        STA ZP.STRL
        ScreenBuffer.String();
        
        // Calculate document line (topLine + currentRow + 1)
        CLC
        LDA vwTopLineL
        ADC vwCurrentRow
        STA ZP.ACCL
        LDA vwTopLineH
        ADC #0
        STA ZP.ACCH
        INC ZP.ACCL
        if (Z) { INC ZP.ACCH }
        ScreenBuffer.Decimal();
        
        LDA #'/'
        ScreenBuffer.Char();
        
        // Print total lines (assumes < 256)
        LDA vwLineCountL
        STA ZP.ACCL
        LDA vwLineCountH
        STA ZP.ACCH
        ScreenBuffer.Decimal();
        
        // Print column
        LDA #(colLabel / 256)
        STA ZP.STRH
        LDA #(colLabel % 256)
        STA ZP.STRL
        ScreenBuffer.String();
        
        LDA vwCurrentCol
        INC A           // Make 1-based
        STA ZP.ACCL
        STZ ZP.ACCH
        ScreenBuffer.Decimal();
        
        // Clear to end of line
        loop
        {
            LDA ScreenBuffer.CursorCol
            CMP # viewWidth
            if (C) { break; }
            LDA #' '
            ScreenBuffer.Char();
        }
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        ScreenBuffer.Resume();
    }
    
    // Public: Clear user portion of status line (columns 0-64)
    StatusClear()
    {
        ScreenBuffer.Suspend();
        
        LDA #0
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        
        LDX # (viewWidth - positionIndicatorWidth)
        loop
        {
            PHX
            LDA #' '
            ScreenBuffer.Char();
            PLX
            DEX
            if (Z) { break; }
        }
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        ScreenBuffer.Resume();
    }
    
    // Public: Write string to status line
    StatusString()  // Input: ZP.STR = string, Y = start column (0-64)
    {
        ScreenBuffer.Suspend();
        
        TYA
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }
            
            // Stop at column 65 to protect line/col area
            LDA ScreenBuffer.CursorCol
            CMP # (viewWidth - positionIndicatorWidth)
            if (C) { break; }
            
            LDA [ZP.STR], Y
            ScreenBuffer.Char();
            INY
        }
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        ScreenBuffer.Resume();
    }
    
    // Public: Write character to status line
    StatusChar()  // Input: A = character, Y = column (0-64)
    {
        CPY # (viewWidth - positionIndicatorWidth)
        if (C) { return; }  // Protect line/col area
        
        ScreenBuffer.Suspend();
        
        PHA
        TYA
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        PLA
        
        ScreenBuffer.Char();
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        ScreenBuffer.Resume();
    }
    
       
#ifdef DEBUG
    // Debug helper: Dump view state
    const string viewDumpLabel = "= VIEW STATE DUMP =";
    const string cursorRowLabel = "CursRow:";
    const string cursorColLabel = "CursCol:";
    const string topLineLabel = "TopLine:";
    const string lineCountLabel = "Lines:";
    const string viewportLabel = "Viewport:";
    
    Dump()
    {
        PHX
        PHY
        
        // Save ZP.IDX
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        Debug.Clear();
        
        // Header
        LDA #(viewDumpLabel % 256)
        STA ZP.STRL
        LDA #(viewDumpLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        // Current cursor row on screen
        LDA #(cursorRowLabel % 256)
        STA ZP.STRL
        LDA #(cursorRowLabel / 256)
        STA ZP.STRH
        LDA vwCurrentRow
        Debug.LabeledByte();
        
        // Current cursor row on screen
        LDA #(cursorColLabel % 256)
        STA ZP.STRL
        LDA #(cursorColLabel / 256)
        STA ZP.STRH
        LDA vwCurrentCol
        Debug.LabeledByte();
        
        // Top visible line number
        LDA #(topLineLabel % 256)
        STA ZP.STRL
        LDA #(topLineLabel / 256)
        STA ZP.STRH
        LDA vwTopLineL
        STA ZP.ACCL
        LDA vwTopLineH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Total line count
        LDA #(lineCountLabel % 256)
        STA ZP.STRL
        LDA #(lineCountLabel / 256)
        STA ZP.STRH
        LDA vwLineCountL
        STA ZP.ACCL
        LDA vwLineCountH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Calculate and show viewport range (first to last visible line)
        LDA #(viewportLabel % 256)
        STA ZP.STRL
        LDA #(viewportLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        // First visible line
        LDA vwTopLineL
        STA ZP.ACCL
        LDA vwTopLineH
        STA ZP.ACCH
        Debug.Word();
        
        // Calculate last visible line (topLine + screenRows - 1)
        CLC
        LDA vwTopLineL
        ADC vwScreenRows
        STA ZP.ACCL
        LDA vwTopLineH
        ADC #0
        STA ZP.ACCH
        
        // Subtract 1
        LDA ZP.ACCL
        if (Z) { DEC ZP.ACCH }
        DEC ZP.ACCL
        
        // But don't exceed lineCount - 1
        LDA ZP.ACCH
        CMP vwLineCountH
        if (Z)
        {
            LDA ZP.ACCL
            CMP vwLineCountL
        }
        if (C)  // lastVisible >= lineCount
        {
            // Use lineCount - 1
            LDA vwLineCountL
            STA ZP.ACCL
            LDA vwLineCountH
            STA ZP.ACCH
            
            LDA ZP.ACCL
            if (Z) { DEC ZP.ACCH }
            DEC ZP.ACCL
        }
        
        Debug.Word();
        
        // Restore ZP.IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
    }
#endif
    
    
}
