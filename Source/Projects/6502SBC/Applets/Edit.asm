program Edit
{
    #define CPU_65C02S
    #define DEBUG
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Screen"
    uses "System/Memory"
    uses "System/File"
    uses "System/Serial"
    uses "System/Debug"
    uses "System/ScreenBuffer"
    
    uses "Editor/Keyboard"
    uses "Editor/GapBuffer"
    uses "Editor/View"
    uses "Editor/Help"
    uses "Editor/Prompt"
    
    const byte edSlots       = 0xA0;
    const byte EditorFlags   = edSlots;
    // Bit 0 - modified
    
    const byte currentFilename  = edSlots+1;
    const byte currentFilenameL = edSlots+1;
    const byte currentFilenameH = edSlots+2;
    
    
    // Messages
    const string saveChanges = "Save modified file";
    const string saveAsPrompt = "Save as: ";

    const string loadingMsg = "Loading BIGTEST...\n";
    const string notFoundMsg = "File not found!\n";
    const string errorMsg = "Error loading file!\n";
    const string fileName = "BIGTEST";      
    
    Initialize()
    {
        loop
        {
            STZ EditorFlags
            STZ currentFilenameL
            STZ currentFilenameH
    
#ifdef DEBUG        
            Debug.Initialize();
#endif
            // Clear screen
            Screen.Clear();
            
            // Initialize keyboard
            Keyboard.Initialize();
            
            // Initialize view
            View.Initialize();
            if (NC) { break; }
            
            Prompt.Initialize();
            if (NC) { break; }
            
            SEC   
            break;
        } // single exit
    }
    disposeFilename()
    {
        LDA currentFilenameL
        ORA currentFilenameH
        if (NZ)
        {
            LDA currentFilenameL
            STA ZP.IDXL
            LDA currentFilenameH
            STA ZP.IDXH
            Memory.Free();
            STZ currentFilenameL
            STZ currentFilenameH
        }
    }
    Dispose()
    {
        View.Dispose();
        Prompt.Dispose();
        Screen.Reset();
        Screen.Clear();
    
        disposeFilename();    
    }
    
    // Check modified flag before operations
    checkModified()  // Returns C clear if should abort operation
    {
        if (BBS0, EditorFlags)  // Modified?
        {
            LDA #(saveChanges % 256)
            STA ZP.STRL
            LDA #(saveChanges / 256)
            STA ZP.STRH
            Prompt.AskYN();
            if (C)  // Yes, save
            {
                saveFile();
                if (NC)  // Save failed
                {
                    CLC
                    return;
                }
            }
        }
        SEC  // OK to proceed
    }
    
    // New file
    newFile()
    {
        checkModified();
        if (NC) { return; }
        
        GapBuffer.Clear();
        disposeFilename();
        RMB0 EditorFlags  // Clear modified
        View.CountLines();
        View.Render();
    }
    
    // Save file
    saveFile()
    {
        // Check if we have a filename
        LDA currentFilenameL
        ORA currentFilenameH
        if (Z)
        {
            saveFileAs();
            return;
        }
        
        // Save to current filename
        LDA currentFilenameL
        STA ZP.STRL
        LDA currentFilenameH
        STA ZP.STRH
        
        // TODO ... actual save code ...
        
        RMB0 EditorFlags  // Clear modified
        SEC
    }
    
    // Save as
    saveFileAs()
    {
        LDA #(saveAsPrompt % 256)
        STA ZP.STRL
        LDA #(saveAsPrompt / 256)
        STA ZP.STRH
        Prompt.GetFilename();
        if (NC)  // Cancelled
        {
            CLC
            return;
        }
        // resulting string in STR
        // length in A
        PHA
        disposeFilename();
        PLA
        STA ZP.ACCL
        INC ZP.ACCL // \0 terminator
        STZ ZP.ACCH
        Memory.Allocate();
        LDA ZP.IDXL
        STA currentFilenameL
        LDA ZP.IDXH
        STA currentFilenameH
        
        LDY #0
        loop
        {
            LDA [STR], Y
            STA [currentFilename], Y
            if (Z) { break; }
            INY
        }
        saveFile();
    }
        
    // Load BIGTEST file into GapBuffer
    loadFile()
    {
        View.StatusClear();
        LDA #(loadingMsg % 256)
        STA ZP.STRL
        LDA #(loadingMsg / 256)
        STA ZP.STRH
        LDY #0
        View.StatusString();
        
        // Set filename
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        
        // Check if exists
        LDA #File.FileType.Any
        File.Exists();
        if (NC)
        {
            View.StatusClear();
            LDA #(notFoundMsg % 256)
            STA ZP.STRL
            LDA #(notFoundMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusString();
            return;
        }
        
        // Open for reading
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        LDA #File.FileType.Any
        File.StartLoad();
        if (NC)
        {
            View.StatusClear();
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusString();
            return;
        }
        
        // Track if last char was CR for \r\n handling
        STZ ZP.TEMP  // 0 = last char not CR, 1 = last char was CR
        
        // Read file chunks and insert into GapBuffer
        loop
        {
            File.NextStream();
            if (NC) { break; }  // End of file
            
            // Insert this chunk into GapBuffer
            LDY #0
            loop
            {
                // Check if done with chunk (Y >= TransferLength)
                // Since TransferLength is typically <= 256, high byte is usually 0
                LDA File.TransferLengthH
                if (NZ)
                {
                    // TransferLength > 255, so Y can't be >= it
                }
                else
                {
                    // TransferLength <= 255, compare Y with low byte
                    CPY File.TransferLengthL
                    if (C) { break; }  // Y >= TransferLength
                }
                
                LDA File.FileDataBuffer, Y

                // Handle line ending normalization
                CMP #'\r'  // CR
                if (Z)
                {
                    // Found CR - convert to LF and remember we saw it
                    PHY
                    LDA #'\n'
                    GapBuffer.InsertChar();
                    PLY
                    LDA #1
                    STA ZP.TEMP  // Mark that we saw CR
                }
                else
                {
                    CMP #'\n'  // LF
                    if (Z)
                    {
                        // Found LF - check if it follows CR
                        LDA ZP.TEMP
                        if (Z)  // Last char was NOT CR
                        {
                            // Standalone LF - insert it
                            PHY
                            LDA #'\n'
                            GapBuffer.InsertChar();
                            PLY
                        }
                        // else: LF after CR (\r\n) - skip it, already inserted \n for the \r
                        STZ ZP.TEMP  // Clear CR flag
                    }
                    else
                    {
                        // Regular character - insert as-is
                        PHY
                        LDA File.FileDataBuffer, Y  // RELOAD THE CHARACTER!
                        GapBuffer.InsertChar();
                        PLY
                        STZ ZP.TEMP  // Clear CR flag
                    }
                }
                
                INY
                if (Z) { break; }  // Y wrapped to 0 after 255
            }
        }
        View.StatusClear();  // Clear the "Loading..." message
    }
    
    
    const string gapPosLabel = "Gap:";
    const string hexDigits = "0123456789ABCDEF";
    
    
    showGapPosition()
    {
        // Get cursor position in GapBuffer
        View.GetCursorPosition();
        
        // Display in status area at column 40
        View.StatusClear();
        
        LDA #(gapPosLabel % 256)
        STA ZP.STRL
        LDA #(gapPosLabel / 256)
        STA ZP.STRH
        LDY #40
        View.StatusString();
        
        LDA GapBuffer.GapValueH
        LSR A LSR A LSR A LSR A
        TAX
        LDA hexDigits, X
        LDY #45
        View.StatusChar();
        
        LDA GapBuffer.GapValueH
        AND #0x0F
        TAX
        LDA hexDigits, X
        LDY #46
        View.StatusChar();
        
        LDA GapBuffer.GapValueL
        LSR LSR LSR LSR
        TAX
        LDA hexDigits, X
        LDY #47
        View.StatusChar();
        
        LDA GapBuffer.GapValueL
        AND #0x0F
        TAX
        LDA hexDigits, X
        LDY #48
        View.StatusChar();
    }
    
    
    
    
    
    
    Hopper()
    {
        Edit.Initialize();
        if (NC)
        {
            return;
        }
                
        // Load the test file
        loadFile();
        View.ApplyGapBuffer();
        
        // Main loop
        loop
        {
//View.Dump(); 
//showGapPosition();
//GapBuffer.Dump();
                      
            // Get key
            Keyboard.GetKey();
            
            // Process key
            switch (A)
            {
                case Key.Escape:
                {
                    break;  // Exit
                }
                
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
                    View.PageUp();
                }
                case Key.PageDown:
                {
                    View.PageDown();
                }
                /*
                case 'r':
                case 'R':
                {
                    View.GetCursorPosition();
                    View.SetCursorPosition();
showGapPosition();
                }
                */
                
                
                case Key.Backspace:
                {
                    View.GetCursorPosition();  // Get logical position
                    
                    // Can't backspace from position 0
                    LDA GapBuffer.GapValueL
                    ORA GapBuffer.GapValueH
                    if (NZ)
                    {
                        GapBuffer.MoveGapTo();
                        GapBuffer.Backspace();
                        if (C)  // Success
                        {
                            // Logical position moves back one
                            LDA GapBuffer.GapValueL
                            if (Z) { DEC GapBuffer.GapValueH }
                            DEC GapBuffer.GapValueL
                            
                            // Recount lines if needed
                            View.CountLines();
                            
                            // Let View handle cursor positioning
                            LDX #1 // Render
                            View.SetCursorPosition();
                        }
                    }
                }
                case Key.Delete:
                {
                    View.GetCursorPosition();  // Get logical position
                    GapBuffer.MoveGapTo();
                    GapBuffer.Delete();
                    if (C)  // Success
                    {
                        // Position doesn't change for delete
                        View.CountLines();
                        LDX #1 // Render
                        View.SetCursorPosition();  // Refreshes display
                    }
                }
                
                case Key.Linefeed: // VT100 Paste
                case Key.Enter:
                {
                    View.GetCursorPosition();
                    GapBuffer.MoveGapTo();
                    
                    LDA #'\n'
                    GapBuffer.InsertChar();
                    if (C)  // Success
                    {
                        // Advance logical position by 1
                        INC GapBuffer.GapValueL
                        if (Z) { INC GapBuffer.GapValueH }
                        
                        // Update line count
                        View.CountLines();
                        
                        // Let View handle positioning to start of new line
                        LDX #1 // Render
                        View.SetCursorPosition();
                    }
                }
                
                case Key.F1:
                {
                    Help.Show();
                    View.Redraw();
                }
                
                
                default:
                {
                    // Check if printable
                    Keyboard.IsPrintable();
                    if (C)  // Is printable
                    {
                        PHA
                        
                        // Get current position and move gap there
                        View.GetCursorPosition();  // Returns position in GapValue
                        GapBuffer.MoveGapTo();     // Moves gap to GapValue position
                        
                        // Insert the character
                        PLA
                        GapBuffer.InsertChar();
                        if (C)  // Success
                        {
                            // Advance logical position by 1
                            INC GapBuffer.GapValueL
                            if (Z) { INC GapBuffer.GapValueH }
                            
                            // Let View figure out where cursor should be
                            LDX #1 // Render
                            View.SetCursorPosition();
                        }
                    }
                    // Ignore other keys
                }
            }
        }
        // Cleanup
        Edit.Dispose();
    }
}
