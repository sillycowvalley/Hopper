unit Commands
{
    uses "System/Definitions"
    uses "System/File"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Time"
    uses "System/Screen"
    uses "Editor/Keyboard"
    uses "Editor/GapBuffer"
    uses "Editor/View"
    
    // Zero page allocation
    const byte cmdSlots = 0x70;  // After Keyboard
    
    const byte cmdExitFlag = cmdSlots+0;
    const byte cmdSaveNeeded = cmdSlots+1;
    
    // File operations use M0-M7 workspace (not concurrent with GapBuffer)
    const uint cmdFileSize = ZP.M0;
    const uint cmdFileSizeL = ZP.M0;
    const uint cmdFileSizeH = ZP.M1;
    const uint cmdReadPos = ZP.M2;
    const uint cmdReadPosL = ZP.M2;
    const uint cmdReadPosH = ZP.M3;
    const byte cmdChar = ZP.M4;
    
    const string testFileName = "TEST";
    const string loadingMsg = "Loading TEST...\n";
    const string savingMsg = "Saving TEST...\n";
    const string savedMsg = "Saved.\n";
    const string errorMsg = "Error!\n";
    const string newFileMsg = "New file.\n";
    
    // Initialize commands
    Initialize()
    {
        STZ cmdExitFlag
        STZ cmdSaveNeeded
    }
    
    // Process a key press
    // Input: A = key from Keyboard.GetKey()
    ProcessKey()
    {
        // Check for special keys first
        CMP #128
        if (NC)
        {
            // Extended key codes (128+)
            switch (A)
            {
                case Key.Up:
                {
                    View.CursorUp();
                }
                case Key.Down:
                {
                    View.CursorDown();
                }
                case Key.Left:
                {
                    View.CursorLeft();
                }
                case Key.Right:
                {
                    View.CursorRight();
                }
                case Key.Home:
                {
                    View.CursorHome();
                }
                case Key.End:
                {
                    View.CursorEnd();
                }
                case Key.PageUp:
                {
                    View.CursorPageUp();
                }
                case Key.PageDown:
                {
                    View.CursorPageDown();
                }
                case Key.Delete:
                {
                    deleteForward();
                }
                default:
                {
                    // Unknown extended key
                }
            }
        }
        else
        {
            // ASCII range
            switch (A)
            {
                case Key.Backspace:
                {
                    deleteBackward();
                }
                case Key.Enter:
                {
                    insertNewline();
                }
                case Key.Tab:
                {
                    insertTab();
                }
                case Key.CtrlS:
                {
                    SaveFile();
                }
                case Key.CtrlQ:
                {
                    // Quit - check if save needed
                    View.IsModified();
                    if (C)
                    {
                        // TODO: Ask to save
                        SaveFile();
                    }
                    LDA #1
                    STA cmdExitFlag
                }
                case Key.Escape:
                {
                    LDA #1
                    STA cmdExitFlag
                }
                default:
                {
                    // Check if printable
                    Keyboard.IsPrintable();
                    if (C)
                    {
                        insertChar();
                    }
                }
            }
        }
    }
    
    // Insert character at cursor
    // Input: A = character
    insertChar()
    {
        PHA
        
        // Get cursor position and move gap there
        View.GetLogicalCursor();
        GapBuffer.MoveGapTo();
        
        // Insert the character
        PLA
        GapBuffer.InsertChar();
        
        // Move cursor right
        View.CursorRight();
        
        // Mark as modified
        View.SetModified();
    }
    
    // Insert newline
    insertNewline()
    {
        LDA #'\n'
        insertChar();
    }
    
    // Insert tab (as spaces for now)
    insertTab()
    {
        LDA #' '
        insertChar();
        insertChar();
        insertChar();
        insertChar();
    }
    
    // Delete character before cursor (backspace)
    deleteBackward()
    {
        // Check if at beginning
        View.GetLogicalCursor();
        LDA ZP.ACCL
        ORA ZP.ACCH
        if (Z) { return; }  // At beginning, nothing to delete
        
        // Move cursor left
        View.CursorLeft();
        
        // Get new position and move gap
        View.GetLogicalCursor();
        GapBuffer.MoveGapTo();
        
        // Delete the character
        GapBuffer.DeleteChar();
        
        // Mark as modified
        View.SetModified();
    }
    
    // Delete character at cursor (delete key)
    deleteForward()
    {
        // Check if at end
        GapBuffer.GetTextLength();
        LDA ZP.ACCL
        STA cmdFileSizeL
        LDA ZP.ACCH
        STA cmdFileSizeH
        
        View.GetLogicalCursor();
        LDA ZP.ACCH
        CMP cmdFileSizeH
        if (C) { return; }
        if (Z)
        {
            LDA ZP.ACCL
            CMP cmdFileSizeL
            if (NC) { return; }  // At or past end
        }
        
        // Move gap to cursor
        GapBuffer.MoveGapTo();
        
        // Delete forward
        GapBuffer.DeleteForward();
        
        // Mark as modified
        View.SetModified();
    }
    
    // Load file into buffer
    LoadFile()
    {
        // Clear screen and show loading message
        Screen.Clear();
        LDA #(loadingMsg % 256)
        STA ZP.STRL
        LDA #(loadingMsg / 256)
        STA ZP.STRH
        Print.String();
        
        // Set up filename
        LDA #(testFileName % 256)
        STA ZP.STRL
        LDA #(testFileName / 256)
        STA ZP.STRH
        
        // Check if file exists
        LDA #File.FileType.Any
        File.Exists();
        if (NC)
        {
            // File doesn't exist
            LDA #(newFileMsg % 256)
            STA ZP.STRL
            LDA #(newFileMsg / 256)
            STA ZP.STRH
            Print.String();
            
            // Clear buffer and view
            GapBuffer.Clear();
            View.ClearModified();
            View.SetDirty();
            return;
        }
        
        // Open file for reading
        LDA #(testFileName % 256)
        STA ZP.STRL
        LDA #(testFileName / 256)
        STA ZP.STRH
        LDA #File.FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Clear buffer before loading
        GapBuffer.Clear();
        
        // Read file in chunks
        loop
        {
            File.NextStream();
            if (NC) { break; }  // End of file
            
            // Data is in FileDataBuffer at 0x0600
            
            // Insert each byte into gap buffer
            LDY #0
            loop
            {
                // Check if done with this chunk
                CPY File.TransferLengthL
                if (NC)
                {
                    // Check high byte
                    LDA File.TransferLengthH
                    if (Z) { break; }
                }
                
                // Read byte from buffer
                LDA 0x0600, Y
                PHY
                
                // Insert into gap buffer
                GapBuffer.InsertChar();
                
                PLY
                INY
                if (Z) { break; }  // Wrapped after 256 bytes
            }
        }
        
        // Reset cursor to beginning
        STZ View.vwLogicalCursorL
        STZ View.vwLogicalCursorH
        STZ View.vwCurrentLineL
        STZ View.vwCurrentLineH
        STZ View.vwCurrentCol
        STZ View.vwTopLineL
        STZ View.vwTopLineH
        
        // Clear modified flag and mark for redraw
        View.ClearModified();
        View.SetDirty();
    }
    
    // Save buffer to file
    SaveFile()
    {
        // Position cursor for message
        LDA #0
        LDY View.vwScreenRows
        Screen.GotoXY();
        
        LDA #(savingMsg % 256)
        STA ZP.STRL
        LDA #(savingMsg / 256)
        STA ZP.STRH
        Print.String();
        
        // Open file for writing
        LDA #(testFileName % 256)
        STA ZP.STRL
        LDA #(testFileName / 256)
        STA ZP.STRH
        File.StartSave();
        if (NC)
        {
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Get total text length
        GapBuffer.GetTextLength();
        LDA ZP.ACCL
        STA cmdFileSizeL
        LDA ZP.ACCH
        STA cmdFileSizeH
        
        // Save in chunks
        STZ cmdReadPosL
        STZ cmdReadPosH
        
        loop
        {
            // Check if done
            LDA cmdReadPosH
            CMP cmdFileSizeH
            if (C) { break; }
            if (Z)
            {
                LDA cmdReadPosL
                CMP cmdFileSizeL
                if (NC) { break; }
            }
            
            // Fill FileDataBuffer with up to 256 bytes
            LDY #0
            loop
            {
                // Check if at end of text
                LDA cmdReadPosH
                CMP cmdFileSizeH
                if (C) { break; }
                if (Z)
                {
                    LDA cmdReadPosL
                    CMP cmdFileSizeL
                    if (NC) { break; }
                }
                
                // Get character from gap buffer
                PHY
                LDA cmdReadPosL
                STA ZP.ACCL
                LDA cmdReadPosH
                STA ZP.ACCH
                GapBuffer.GetCharAt();
                PLY
                
                // Store in file buffer
                STA 0x0600, Y
                
                // Next position
                INC cmdReadPosL
                if (Z) { INC cmdReadPosH }
                INY
                if (Z) { break; }  // 256 bytes
            }
            
            // Write chunk - Y has byte count (0 means 256)
            STY ZP.FS2  // Transfer length low
            STZ ZP.FS3  // Transfer length high
            CPY #0
            if (Z)
            {
                // Full 256 bytes
                STZ ZP.FS2
                LDA #1
                STA ZP.FS3
            }
            
            // Set source pointer
            LDA #0x00
            STA ZP.FS0
            LDA #0x06
            STA ZP.FS1
            
            File.AppendStream();
            if (NC)
            {
                // Write error
                LDA #(errorMsg % 256)
                STA ZP.STRL
                LDA #(errorMsg / 256)
                STA ZP.STRH
                Print.String();
                return;
            }
        }
        
        // Close file
        LDA #0x00  // Data file (not executable)
        File.EndSave();
        
        // Show saved message
        LDA #(savedMsg % 256)
        STA ZP.STRL
        LDA #(savedMsg / 256)
        STA ZP.STRH
        Print.String();
        
        // Clear modified flag
        View.ClearModified();
        
        // Wait a moment then redraw
        LDA #(500 % 256)
        STA ZP.TOP0
        LDA #(500 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay();
        
        View.SetDirty();
    }
    
    // Check if should exit
    ShouldExit()
    {
        LDA cmdExitFlag
        if (NZ) { SEC return; }
        CLC
    }
}
