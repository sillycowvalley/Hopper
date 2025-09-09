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
    // Bit  0:   Modified flag (0=clean, 1=modified)
    // Bit  1:   Exiting
    // Bit  2:   Selection active (0=no, 1=yes)
    // Bits 3-4: Undo state (00=empty, 01=can_undo, 10=can_redo, 11=reserved)
    // Bits 5-7: Reserved for future use
    
    const byte currentFilename  = edSlots+1;
    const byte currentFilenameL = edSlots+1;
    const byte currentFilenameH = edSlots+2;
    
    
    // Messages
    const string saveChangesPrompt = "Save changes? (Y/N/Esc): ";
    const string saveAsPrompt = "Save as: ";
    const string openPrompt = "Open file: ";
    const string notFoundMsg = "File not found!";
    const string errorMsg = "Error loading file!";
    const string loadedMsg = "File loaded.";
    const string ctrlKPrompt = "^K ";
    const string ctrlQPrompt = "^Q ";
    
    
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
            
            // Initialize GapBuffer (4KB)
            LDA #(4096 % 256)
            LDY #(4096 / 256)
            GapBuffer.Initialize();
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
    makeFilename()
    {
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
    }
    Dispose()
    {
        View.Dispose();
        Prompt.Dispose();
        Screen.Reset();
        Screen.Clear();
    
        disposeFilename();    
    }
    
   
    
    
    // Check modified flag before file operations
    // Output: A = 0 (cancel), 1 (proceed without save), 2 (saved and proceed)
    checkModified()
    {
        if (BBR0, EditorFlags)  // Not modified?
        {
            LDA #1  // Proceed
            return;
        }
        
        // Show modified prompt with 3 options
        LDA #(saveChangesPrompt % 256)
        STA ZP.STRL
        LDA #(saveChangesPrompt / 256)
        STA ZP.STRH
        LDY #0
        View.StatusString();
        
        // Wait for Y/N/Escape
        loop
        {
            Keyboard.GetKey();
            
            // Convert to uppercase
            CMP #'y'
            if (Z) { LDA #'Y' }
            CMP #'n'
            if (Z) { LDA #'N' }
            
            switch (A)
            {
                case 'Y':  // Save and continue
                {
                    View.StatusClear();
                    saveFile();
                    if (C)  // Save succeeded
                    {
                        LDA #2
                        break;
                    }
                    else  // Save failed
                    {
                        LDA #0  // Cancel operation
                        break;
                    }
                }
                case 'N':  // Don't save, continue
                {
                    View.StatusClear();
                    LDA #1
                    break;
                }
                case Key.Escape:  // Cancel operation
                {
                    View.StatusClear();
                    LDA #0
                    break;
                }
                default:
                {
                    // Invalid key, continue loop
                }
            }
            break;  // Exit loop once we have a valid choice
        }
    }
    
    
    // New file
    newFile()
    {
        checkModified();
        if (Z)  { return; } // A = 0 means cancelled
        
        GapBuffer.Clear();
        disposeFilename();
        RMB0 EditorFlags  // Clear modified
        View.CountLines();
        View.Render();
    }
    
    // Save file
    saveFile()
    {
        if (BBR0, EditorFlags) { return; } // not modified
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
        makeFilename();
        saveFile();
    }
    
    // Open file (with modified check)
    fileOpen()
    {
        // Check if current file modified
        checkModified();
        if (Z)  { return; } // A = 0 means cancelled
        
        // Prompt for filename
        LDA #(openPrompt % 256)
        STA ZP.STRL
        LDA #(openPrompt / 256)
        STA ZP.STRH
        Prompt.GetFilename();
        if (NC)  // Cancelled
        {
            return;
        }
        makeFilename();
        LDA currentFilenameL
        STA ZP.STRL
        LDA currentFilenameH
        STA ZP.STRH
        
        // Check if file exists
        LDA # FileType.Any
        File.Exists();
        if (NC)
        {
            // File not found
            LDA #(notFoundMsg % 256)
            STA ZP.STRL
            LDA #(notFoundMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusString();
            return;
        }
        
        // Clear current buffer
        GapBuffer.Clear();
        
        // Open for reading
        LDA currentFilenameL
        STA ZP.STRL
        LDA currentFilenameH
        STA ZP.STRH
        LDA #File.FileType.Any
        File.StartLoad();
        if (NC)
        {
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
                // Check if done with chunk
                LDA File.TransferLengthH
                if (NZ)
                {
                    // TransferLength > 255, so Y can't be >= it
                }
                else
                {
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
                        // else: LF after CR (\r\n) - skip it
                        STZ ZP.TEMP  // Clear CR flag
                    }
                    else
                    {
                        // Regular character - insert as-is
                        PHY
                        LDA File.FileDataBuffer, Y  // Reload the character!
                        GapBuffer.InsertChar();
                        PLY
                        STZ ZP.TEMP  // Clear CR flag
                    }
                }
                
                INY
                if (Z) { break; }  // Y wrapped to 0 after 255
            }
        }
               
        // Clear modified flag
        RMB0 EditorFlags
        
        // Reset cursor to top of file
        STZ GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        
        View.ApplyGapBuffer();
        
        // Show success message briefly
        LDA #(loadedMsg % 256)
        STA ZP.STRL
        LDA #(loadedMsg / 256)
        STA ZP.STRH
        LDY #0
        View.StatusString();
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
    
    
    // Handle Ctrl+K prefix commands
    handleCtrlK()
    {
        // Show prefix in status line
        View.StatusClear();
        LDA #(ctrlKPrompt % 256)
        STA ZP.STRL
        LDA #(ctrlKPrompt / 256)
        STA ZP.STRH
        LDY #0
        View.StatusString();
        
        // Get second key
        Keyboard.GetKey();
        
        // Convert to uppercase for comparison
        CMP #'a'
        if (C)  // >= 'a'
        {
            CMP #'z'+1
            if (NC)  // <= 'z'
            {
                AND #0xDF  // Convert to uppercase
            }
        }
        
        // Echo the complete command
        PHA
        LDY #3  // Position after "^K "
        View.StatusChar();
        PLA
        
        // Process the command
        switch (A)
        {
            case 'B':  // Mark block begin
            {
                View.StatusClear();
                // TODO markBlockBegin();
            }
            case 'K':  // Mark block end
            {
                View.StatusClear();
                // TODO markBlockEnd();
            }
            case 'D':  // Done - save and exit
            {
                View.StatusClear();
                saveFile();
                SMB1 EditorFlags // exit
            }
            case 'Q':  // Quit without save
            {
                View.StatusClear();
                SMB1 EditorFlags // exit
            }
            case 'S':  // Save
            {
                View.StatusClear();
                saveFile();
            }
            default:
            {
                // Unknown command - clear status and beep?
                View.StatusClear();
            }
        }
    }

    // Handle Ctrl+Q prefix commands
    handleCtrlQ()
    {
        // Show prefix in status line
        View.StatusClear();
        LDA #(ctrlQPrompt % 256)
        STA ZP.STRL
        LDA #(ctrlQPrompt / 256)
        STA ZP.STRH
        LDY #0
        View.StatusString();
        
        // Get second key
        Keyboard.GetKey();
        // Convert to uppercase
        CMP #'a'
        if (C)
        {
            CMP #'z'+1
            if (NC)
            {
                AND #0xDF
            }
        }
        
        // Echo the complete command
        PHA
        LDY #3  // Position after "^Q "
        View.StatusChar();
        PLA
        
        switch (A)
        {
            case 'S':  // Beginning of line
            {
                View.StatusClear();
                View.CursorHome();
            }
            case 'D':  // End of line
            {
                View.StatusClear();
                View.CursorEnd();
            }
            case 'R':  // Top of file
            {
                View.StatusClear();
                View.CursorTop();
            }
            case 'C':  // End of file
            {
                View.StatusClear();
                View.CursorBottom();
            }
            case 'F':  // Find
            {
                View.StatusClear();
                // TODO findText();
            }
            case 'A':  // Replace
            {
                View.StatusClear();
                // TODO replaceText();
            }
            default:
            {
                View.StatusClear();
            }
        }
    }
    
    
    
    Hopper()
    {
        Edit.Initialize();
        if (NC)
        {
            return;
        }
        
        View.ApplyGapBuffer(); // empty new file
        
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
                case Key.CtrlK:
                {
                    handleCtrlK();
                    if (BBS1, EditorFlags) { break; } // exit
                }
                
                case Key.CtrlQ:
                {
                    handleCtrlQ();
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
                            SMB0 EditorFlags // modified
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
                        SMB0 EditorFlags // modified
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
                        SMB0 EditorFlags // modified
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
                            SMB0 EditorFlags // modified
                            // Advance logical position by 1
                            INC GapBuffer.GapValueL
                            if (Z) { INC GapBuffer.GapValueH }
                            
                            // Let View figure out where cursor should be
                            LDX #1 // Render
                            View.SetCursorPosition();
                        }
                    }
                    // Ignore other keys
                } // default
            } // switch
        } // loop
        
        // Cleanup
        Edit.Dispose();
    }
}
