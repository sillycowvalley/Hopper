unit View
{
    
    // Zero page allocation
    const byte vwSlots = 0xA0;
    
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
    
    const byte vwCurCol    = vwLeafTempL;
    const byte vwCurRowL   = vwLeafTempH;
    const byte vwCurRowH   = vwSkipCount;
    
    
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
        CountLines();
        
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
    CountLines()
    {
        LDA GapBuffer.GapValueL
        PHA
        LDA GapBuffer.GapValueH
        PHA
        
        STZ vwLineCountL
        STZ vwLineCountH
        INC vwLineCountL  // Start with 1 line
        
        GapBuffer.GetCharAtFastPrep();
        
        // Scan for newlines
        STZ vwPosL
        STZ vwPosH
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP FastLengthL
            }
            if (C) { break; }  // pos >= length
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast();
            
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
        
        PLA
        STA GapBuffer.GapValueH
        PLA
        STA GapBuffer.GapValueL
    }
    
    // Get cursor's logical position in GapBuffer
    // Output: GapBuffer.GapValue = logical cursor position in document stream
    GetCursorPosition()
    {
        // Calculate target row (absolute in document)
        CLC
        LDA vwTopLineL
        ADC vwCurrentRow
        STA ZP.ACCL
        LDA vwTopLineH
        ADC #0
        STA ZP.ACCH
        
        GapBuffer.GetCharAtFastPrep();
        
        // Start at beginning of document
        STZ vwPosL
        STZ vwPosH
        STZ vwCurCol
        STZ vwCurRowL
        STZ vwCurRowH
        
        // Walk through document
        loop
        {
            // Check if we're at target position
            LDA vwCurRowL
            CMP ZP.ACCL // absolute target row L
            if (Z)
            {
                LDA vwCurRowH
                CMP ZP.ACCH // absolute target row H
                if (Z)
                {
                    LDA vwCurCol
                    CMP vwCurrentCol // col target
                    if (Z)
                    {
                        // Found it (0-based : don't count the last character)
                        break;
                    }
                }
            }
            
            // Check if at end of document
            LDA vwPosH
            CMP FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP FastLengthL
            }
            if (C)
            {
                // At EOF
                break;
            }
            
            // Get character at current position
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast();
            
            // Update row/column based on character
            CMP #'\n'
            if (Z)
            {
                // Newline - next row, column 0
                INC vwCurRowL
                if (Z) { INC vwCurRowH }
                STZ vwCurCol
            }
            else
            {
                // Regular character - next column
                INC vwCurCol
            }
            
            // Always advance position
            INC vwPosL
            if (Z) { INC vwPosH }
        }
        
        // Return position in GapBuffer.GapValue
        LDA vwPosL
        STA GapBuffer.GapValueL
        LDA vwPosH
        STA GapBuffer.GapValueH
    }
    
    
    // Set cursor position from logical position in GapBuffer
    // Input: GapBuffer.GapValue = target position in document
    // Output: Updates vwCurrentRow, vwCurrentCol, and vwTopLine if needed
    SetCursorPosition()
    {
        PHX
        
        // Save target position
        LDA GapBuffer.GapValueL
        STA ZP.ACCL
        LDA GapBuffer.GapValueH
        STA ZP.ACCH
        
        GapBuffer.GetCharAtFastPrep();
     
        // Clamp target to text length if beyond EOF
        LDA ZP.ACCH
        CMP FastLengthH
        if (Z)  // target.H == length.H
        {
            LDA ZP.ACCL
            CMP FastLengthL
        }
        if (C)  // target.L >= length.L
        {
            LDA FastLengthL
            STA ZP.ACCL
            LDA FastLengthH
            STA ZP.ACCH
        }
        
        // Start at beginning of document
        STZ vwPosL
        STZ vwPosH
        STZ vwCurCol
        STZ vwCurRowL
        STZ vwCurRowH
        
        // Walk through document to find target position
        loop
        {
            // Check if we've reached target position
            LDA vwPosH
            CMP ZP.ACCH
            if (Z)
            {
                LDA vwPosL
                CMP ZP.ACCL
            }
            if (C)  // pos >= target
            {
                // Found it
                break;
            }
            
            // Check if at end of document (shouldn't happen due to clamping)
            LDA vwPosH
            CMP FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP FastLengthL
            }
            if (C)
            {
                // At EOF
                break;
            }
            
            // Get character at current position
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast();
            
            // Update row/column based on character
            CMP #'\n'
            if (Z)
            {
                // Newline - next row, column 0
                INC vwCurRowL
                if (Z) { INC vwCurRowH }  // Fixed: check Z not C
                STZ vwCurCol
            }
            else
            {
                // Regular character - next column
                INC vwCurCol
            }
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
        }
        
        // Now vwCurRow contains absolute row, vwCurCol contains column
        // Check if row is visible on screen
        
        loop
        {
            // Check if row < vwTopLine (scrolled off top)
            LDA vwCurRowH
            CMP vwTopLineH
            if (C)  // row.H >= topLine.H
            {
                if (Z)  // row.H == topLine.H
                {
                    LDA vwCurRowL
                    CMP vwTopLineL
                    if (NC)  // row.L < topLine.L
                    {
                        // Row is above viewport - scroll up
                        LDA vwCurRowL
                        STA vwTopLineL
                        LDA vwCurRowH
                        STA vwTopLineH
                        STZ vwCurrentRow
                        
                        LDX #1 // Render
                        break;
                    }
                }
                // else row.H > topLine.H, so row is below topLine
            }
            else  // row.H < topLine.H
            {
                // Row is above viewport - scroll up
                LDA vwCurRowL
                STA vwTopLineL
                LDA vwCurRowH
                STA vwTopLineH
                STZ vwCurrentRow
                
                LDX #1  // Render
                break;
            }
            
            // Check if row >= vwTopLine + vwScreenRows (scrolled off bottom)
            CLC
            LDA vwTopLineL
            ADC vwScreenRows
            STA ZP.IDYL  // Reuse IDY for bottom limit
            LDA vwTopLineH
            ADC #0
            STA ZP.IDYH
            
            LDA vwCurRowH
            CMP ZP.IDYH
            if (C)  // row.H >= bottom.H
            {
                if (Z)  // row.H == bottom.H
                {
                    LDA vwCurRowL
                    CMP ZP.IDYL
                    if (NC)  // row.L < bottom.L
                    {
                        // Row is visible - calculate screen row
                        SEC
                        LDA vwCurRowL
                        SBC vwTopLineL
                        STA vwCurrentRow
                        
                        LDX #0  // Update
                        break;
                    }
                }
                // Row is below viewport - scroll down
                
                // New topLine = row - (screenRows - 1)
                SEC
                LDA vwCurRowL
                SBC vwScreenRows
                STA vwTopLineL
                LDA vwCurRowH
                SBC #0
                STA vwTopLineH
                
                INC vwTopLineL  // +1 because we subtracted screenRows but want row visible
                if (Z) { INC vwTopLineH }
                
                // Cursor at bottom of screen
                LDA vwScreenRows
                DEC A
                STA vwCurrentRow
                
                LDX #1 // Render
            }
            else  // row.H < bottom.H, so row is visible
            {
                // Row is visible - calculate screen row
                SEC
                LDA vwCurRowL
                SBC vwTopLineL
                STA vwCurrentRow
                
                LDX #0  // Update
            }
            break;
        } // loop
        
        // force Render?
        PLY
        CPY #1
        if (Z)
        {
            LDX #1
        }
        
        // Set column
        LDA vwCurCol
        STA vwCurrentCol
        CPX #1
        if (Z)
        {
            View.Render();
        }
        else
        {
            View.Update();
        }
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
        GapBuffer.GetCharAtFastPrep();
        
        LDX #0  // Character counter
        loop
        {
            // Check if at end of text
            LDA vwPosH
            CMP FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP FastLengthL
            }
            if (C) { break; }
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast();
            
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
    
    Redraw()
    {
        ScreenBuffer.Redraw();
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
                findNextNewline(); // vwPos = position of the newline character (NOT past it!)
                
                // Count the line
                INC vwSkipCountL
                if (Z) { INC vwSkipCountH }
            }
        }
                
        GapBuffer.GetCharAtFastPrep();
        
#ifdef UNIVERSAL
        LDA #0b00000100
        AND Edit.EditorFlags
        if (Z)
        {
            // Render visible lines (no block active)
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
        }
        else
        {
            // Render visible lines (block active)
            LDY #0  // Screen row
            loop
            {
                PHY
                
                // Position cursor at start of line
                LDA #0
                ScreenBuffer.GotoXY();
                
                // Render one line
                renderLineWithBlock();
                
                PLY
                INY
                CPY vwScreenRows
                if (C) { break; }  // row >= screenRows
            }
        }
#else        
        if (BBR2, Edit.EditorFlags) 
        {
            // Render visible lines (no block active)
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
        }
        else
        {
            // Render visible lines (block active)
            LDY #0  // Screen row
            loop
            {
                PHY
                
                // Position cursor at start of line
                LDA #0
                ScreenBuffer.GotoXY();
                
                // Render one line
                renderLineWithBlock();
                
                PLY
                INY
                CPY vwScreenRows
                if (C) { break; }  // row >= screenRows
            }
        }
#endif        
        
        ScreenBuffer.Resume();
        
        // Position cursor
        View.UpdatePosition(); // render position change
    }
    
    Update()
    {
        ScreenBuffer.Suspend();
        
        // Position cursor
        LDA vwCurrentCol
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        UpdatePosition();
        
        ScreenBuffer.Resume();
    }
    
    // Find next newline from current vwPos position
    // Input:  vwPos = starting position to search from
    // Output: vwPos = position of the newline character (NOT past it!)
    //         A = '\n' if newline found, or last char if EOF reached
    // Note:   Scans forward from vwPos until '\n' or EOF is found
    //         Leaves vwPos pointing AT the newline, not AFTER it
    findNextNewline()
    {
        GapBuffer.GetCharAtFastPrep();
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP FastLengthL
            }
            if (C) { break; }  // pos >= length
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast();
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
            
            // Check for newline
            CMP #'\n'
            if (Z) { break; }
        }
    }
    
    
    // Helper: Check if current vwPos is within block
    // Input: vwPos = current position
    // Output: C set if within block, clear if not
    isPositionInBlock()
    {
        // Check vwPos >= BlockStart
        LDA vwPosH
        CMP Edit.BlockStartH
        if (Z)  // vwPos.H == BlockStart.H
        {
            LDA vwPosL
            CMP Edit.BlockStartL
        }
        if (NC) { CLC return; }  // vwPos < BlockStart
        
        // vwPos >= BlockStart, now check upper bound
        
        // Check vwPos < BlockEnd
        LDA vwPosH
        CMP Edit.BlockEndH
        if (Z)  // High bytes equal
        {
            LDA vwPosL
            CMP Edit.BlockEndL
        }
        if (NC)  // vwPos < BlockEnd
        {
            SEC  // In block
            return;
        }
        CLC  // Not in block
    }
    
    // Render one line with block highlighting
    // Warning: assumes GapBuffer.GetCharAtFastPrep() has been called before entering
    renderLineWithBlock()
    {
        ScreenBuffer.CharFastPrep();  // Sets up msbAttribute
        
        LDX #0  // Column counter
        loop
        {
            // Check if at end of document
            LDA vwPosH
            CMP GapBuffer.FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP GapBuffer.FastLengthL
            }
            if (C) { break; }  // pos >= length
            
            // Check if position is in block
            isPositionInBlock();
            if (C)  // In block
            {
                // Inverse On
                LDA ScreenBuffer.msbAttribute
                ORA # Attribute.Inverse
                STA ScreenBuffer.msbAttribute
            }
            else
            {
                // Inverse Off
                LDA ScreenBuffer.msbAttribute
                AND # 0b01111111
                STA ScreenBuffer.msbAttribute
            }
            
            // Get character
            LDA vwPosL
            STA GapBuffer.GapValueL
            LDA vwPosH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAtFast(); // -> A
            
            // Advance position
            INC vwPosL
            if (Z) { INC vwPosH }
            
            // Check for newline
            CMP #'\n'
            if (Z) { break; }
            
            // Check if at right edge
            CPX vwScreenCols
            if (C) { break; }  // col >= screenCols
                       
            // Display character
            ScreenBuffer.CharFast();
            INX
        }
        
        // Inverse Off
        LDA ScreenBuffer.msbAttribute
        AND # 0b01111111
        STA ScreenBuffer.msbAttribute
        
        // Fill the remainder of the line with spaces
        loop
        {
            CPX vwScreenCols
            if (C) { break; }  // col >= screenCols
            
            LDA #' '
            ScreenBuffer.CharFast();
            INX
        }
    }
    
    // Render one line from current vwPos
    // Warning: assumes GapBuffer.GetCharAtFastPrep() has been called before entering
    renderLine()
    {
        ScreenBuffer.CharFastPrep();
        
        LDX #0  // Column counter
        loop
        {
            // Check if at end of document
            LDA vwPosH
            CMP GapBuffer.FastLengthH
            if (Z)
            {
                LDA vwPosL
                CMP GapBuffer.FastLengthL
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
            GapBuffer.GetCharAtFast();
            
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
            ScreenBuffer.CharFast();
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
            ScreenBuffer.CharFast();
            INX
        }
    }
    
    // Helper: Constrain cursor column to current line length
    constrainCursorColumn()
    {
        // Get current line length
        getCurrentLineLength();  // Returns length in A
        
        // If cursor is beyond line length, move it to end of line
        CMP vwCurrentCol
        if (NC)  // length < currentCol
        {
            // Set cursor to end of line (length)
            STA vwCurrentCol
        }
        // else currentCol is valid, leave it alone
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
                constrainCursorColumn();
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
                    constrainCursorColumn();
                    
                    View.Render();
                    break;
                }
            }
            
            View.UpdatePosition(); // render position change
            
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
                
                constrainCursorColumn();
                View.Render();
                break;
            }
            
            constrainCursorColumn();
            View.UpdatePosition(); // render position change
            
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
        
        View.UpdatePosition(); // render position change
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
        
        View.UpdatePosition(); // render position change
    }
    
    // Move cursor to beginning of current line
    CursorHome()
    {
        STZ vwCurrentCol
        View.UpdatePosition(); // render position change
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
        
        View.UpdatePosition(); // render position change
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
            
            constrainCursorColumn();
            View.Render();
            break;
        }
        
        View.UpdatePosition(); // render position change
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
            if (C)  // TopLine >= maximum
            {
                LDA ZP.ACCL
                STA vwTopLineL
                LDA ZP.ACCH
                STA vwTopLineH
            }
            
            constrainCursorColumn();
            View.Render();
            break;
        }
        
        View.UpdatePosition(); // render position change
    }
    
    // Move cursor to block start position
    CursorBlockStart()
    {
        // Check if block is active (defensive)
#ifdef UNIVERSAL
        LDA #0b00000100
        AND Edit.EditorFlags
        if (Z) { return; }
#else        
        if (BBR2, Edit.EditorFlags) { return; }
#endif        
        
        // Load block start position into GapValue
        LDA Edit.BlockStartL
        STA GapBuffer.GapValueL
        LDA Edit.BlockStartH
        STA GapBuffer.GapValueH
        
        // Move cursor to that position (handles scrolling if needed)
        LDX #1  // Force render
        SetCursorPosition();
    }
    
    // Move cursor to block end position
    CursorBlockEnd()
    {
        // Check if block is active (defensive)
#ifdef UNIVERSAL
        LDA #0b00000100
        AND Edit.EditorFlags
        if (Z) { return; }
#else        
        if (BBR2, Edit.EditorFlags) { return; }
#endif
        
        // Load block end position into GapValue
        LDA Edit.BlockEndL
        STA GapBuffer.GapValueL
        LDA Edit.BlockEndH
        STA GapBuffer.GapValueH
        
        // Move cursor to that position (handles scrolling if needed)
        LDX #1  // Force render
        SetCursorPosition();
    }
    
    // Move cursor to top of file (first line, first column)
    CursorTop()
    {
        // Set viewport to top of document
        STZ vwTopLineL
        STZ vwTopLineH
        
        // Position cursor at top-left
        STZ vwCurrentRow
        STZ vwCurrentCol
        
        // Redraw screen from the top
        Render();
    }
    
    // Move cursor to bottom of file (last line)
    CursorBottom()
    {
        loop
        {
            // Check if document fits entirely on screen
            LDA vwLineCountH
            if (Z)  // High byte is 0
            {
                LDA vwLineCountL
                CMP vwScreenRows
                if (NC)  // lineCount < screenRows
                {
                    // Document fits on screen
                    // Keep vwTopLine at 0
                    STZ vwTopLineL
                    STZ vwTopLineH
                    
                    // Position cursor at last line
                    LDA vwLineCountL
                    if (NZ)  // If not empty document
                    {
                        DEC A  // Convert to 0-based
                    }
                    STA vwCurrentRow
                    
                    // Position at beginning of last line (could change to end)
                    STZ vwCurrentCol
                    
                    Render();
                    break;
                }
            }
            
            // Document is longer than screen
            // Calculate topLine = lineCount - screenRows
            SEC
            LDA vwLineCountL
            SBC vwScreenRows
            STA vwTopLineL
            LDA vwLineCountH
            SBC #0
            STA vwTopLineH
            
            // Check for underflow (shouldn't happen but be safe)
            if (MI)
            {
                STZ vwTopLineL
                STZ vwTopLineH
            }
            
            // Position cursor at bottom of screen
            LDA vwScreenRows
            DEC A  // Last visible row (0-based)
            STA vwCurrentRow
            
            // Position at beginning of last line
            STZ vwCurrentCol
            
            // Could alternatively position at end of last line:
            // getCurrentLineLength();  // Get length of last line
            // STA vwCurrentCol        // Position at end
            
            Render();
            break;
        }
    }
    
    // Internal: Update line/col display (right side of status)
    UpdatePosition()
    {
        ScreenBuffer.Suspend();
        
        LDA # (viewWidth - positionIndicatorWidth)
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        ScreenBuffer.SetBold();
        
         // Show modified indicator if bit 0 of EditorFlags is set
#ifdef UNIVERSAL
        LDA #0b00000001
        AND Edit.EditorFlags
        if (NZ)
        {
            LDA #'*'
            ScreenBuffer.Char();
        }
        else
        {
            LDA #' '
            ScreenBuffer.Char();
        }
#else         
        if (BBS0, Edit.EditorFlags)  // Modified?
        {
            LDA #'*'
            ScreenBuffer.Char();
        }
        else
        {
            LDA #' '
            ScreenBuffer.Char();
        }
#endif
        
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
        
        ScreenBuffer.SetNotBold();
        
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
    StatusStringPause()  // Input: ZP.STR = string, Y = start column (0-64)
    {
        StatusString();
        LDA # 255 // 255 ms 
        Shared.LoadTopByte();
        Time.Delay();
    }
    
    // Public: Write string to status line
    StatusString()  // Input: ZP.STR = string, Y = start column (0-64)
    {
        ScreenBuffer.Suspend();
        
        TYA
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        ScreenBuffer.SetBold();
        
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
        
#ifdef UNIVERSAL
        LDA #0b00001000
        AND Edit.EditorFlags
        if (Z) // prompt mode?
        {
            // Position cursor
            LDA vwCurrentCol
            LDY vwCurrentRow
            ScreenBuffer.GotoXY();
        }
#else        
        if (BBR3, Edit.EditorFlags) // prompt mode?
        {
            // Position cursor
            LDA vwCurrentCol
            LDY vwCurrentRow
            ScreenBuffer.GotoXY();
        }
#endif
        
        ScreenBuffer.SetNotBold();
        ScreenBuffer.Resume();
    }
    
    StatusCharPause()  // Input: A = character, Y = column (0-64)
    {
        PHA
        ScreenBuffer.Suspend();
        
        TYA
        LDY # (viewHeight - statusAreaHeight)
        ScreenBuffer.GotoXY();
        
        ScreenBuffer.SetBold();
        PLA
        ScreenBuffer.Char();
        ScreenBuffer.SetNotBold();
        
        ScreenBuffer.Resume();
        
        LDA # 255 // 255 ms 
        Shared.LoadTopByte();
        Time.Delay();
    }
    
    // Public: Write character to status line
    StatusChar()  // Input: A = character, Y = column (0-64)
    {
        CPY # (viewWidth - positionIndicatorWidth)
        if (C) { return; }  // Protect line/col area
        
        ScreenBuffer.Suspend();
        
        CMP # Key.Backspace
        if (Z)
        {
            TYA
            PHA
            LDY # (viewHeight - statusAreaHeight)
            ScreenBuffer.GotoXY();
            LDA #' '
            ScreenBuffer.Char();
            
            PLA
            LDY # (viewHeight - statusAreaHeight)
            ScreenBuffer.GotoXY();
        }
        else
        {
            PHA
            TYA
            LDY # (viewHeight - statusAreaHeight)
            ScreenBuffer.GotoXY();
            PLA
            ScreenBuffer.Char();
        }
        
#ifdef UNIVERSAL
        LDA #0b00001000
        AND Edit.EditorFlags
        if (Z)
        {
            LDA vwCurrentCol
            LDY vwCurrentRow
            ScreenBuffer.GotoXY();
        }
#else        
        if (BBR3, Edit.EditorFlags) // prompt mode?
        {
            // Position cursor
            LDA vwCurrentCol
            LDY vwCurrentRow
            ScreenBuffer.GotoXY();
        }
#endif
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
