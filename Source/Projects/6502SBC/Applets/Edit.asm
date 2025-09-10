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
    uses "System/Time"
    uses "System/Debug"
    uses "System/ScreenBuffer"
    uses "System/Char"
    
    uses "Editor/Keyboard"
    uses "Editor/GapBuffer"
    uses "Editor/View"
    uses "Editor/Help"
    uses "Editor/Prompt"
    
    const byte edSlots       = 0xB6;
    const byte EditorFlags   = edSlots;
    // Bit  0:   Modified flag (0=clean, 1=modified)
    // Bit  1:   Exiting
    // Bit  2:   Block active (0=no, 1=yes)
    // Bit  3:   prompt mode
    // Bits 4-5: Undo state (00=empty, 01=can_undo, 10=can_redo, 11=reserved)
    // Bits 6-7: Reserved for future use
    
    const byte currentFilename  = edSlots+1;
    const byte currentFilenameL = edSlots+1;
    const byte currentFilenameH = edSlots+2;
    
    // Block state
    const uint BlockStart = edSlots+3;  // Logical position in GapBuffer
    const byte BlockStartL = edSlots+3;
    const byte BlockStartH = edSlots+4;
    
    const uint BlockEnd = edSlots+5;         // Logical position in GapBuffer  
    const byte BlockEndL = edSlots+5;
    const byte BlockEndH = edSlots+6;
    
#ifdef DEBUG
    const uint crPos   = edSlots+7;
    const byte crPosL  = edSlots+7;
    const byte crPosH  = edSlots+8;
    const byte crCol   = edSlots+9;
#endif    
    
    // Messages
    const string saveChangesPrompt = "Save changes? (Y/N/Esc): ";
    const string saveAsPrompt = "Save as: ";
    const string openPrompt = "Open file: ";
    const string notFoundMsg = "File not found!";
    const string loadingErrorMsg = "Error loading file!";
    const string savingErrorMsg = "Error saving file!";
    const string loadedMsg = "File loaded.";
    const string savedMsg  = "File saved.";
    const string ctrlKPrompt = "^K ";
    const string ctrlQPrompt = "^Q ";
    
    const string noBeginMsg = "No block begin! Use ^K B first.";
    
    const string writeBuffer = " ";
    
    
    Initialize()
    {
        loop
        {
            STZ EditorFlags
            STZ currentFilenameL
            STZ currentFilenameH
            
            // Initialize block bounds to invalid
            LDA #0xFF
            STA Edit.BlockStartL
            STA Edit.BlockStartH
            STA Edit.BlockEndL
            STA Edit.BlockEndH
    
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
    
#ifdef DEBUG
    // Debug helper: Dump block state
    const string blockDumpLabel = "= BLOCK STATE =";
    const string blockActiveLabel = "Active:";
    const string bufferSizeLabel = "BufSize:";
    const string cursorPosLabel = "CursPos:";
    const string blockStartLabel = "BlkStart:";
    const string blockEndLabel = "BlkEnd:";
    const string startCRLabel = "StartCR:";
    const string endCRLabel = "EndCR:";
    
    BlockDump()
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
        LDA #(blockDumpLabel % 256)
        STA ZP.STRL
        LDA #(blockDumpLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        // Block active status
        LDA #(blockActiveLabel % 256)
        STA ZP.STRL
        LDA #(blockActiveLabel / 256)
        STA ZP.STRH
        if (BBS2, Edit.EditorFlags)
        {
            LDA #1
        }
        else
        {
            LDA #0
        }
        Debug.LabeledByte();
        
        // Buffer size
        LDA #(bufferSizeLabel % 256)
        STA ZP.STRL
        LDA #(bufferSizeLabel / 256)
        STA ZP.STRH
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA ZP.ACCL
        LDA GapBuffer.GapValueH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Current cursor position in gap buffer
        LDA #(cursorPosLabel % 256)
        STA ZP.STRL
        LDA #(cursorPosLabel / 256)
        STA ZP.STRH
        View.GetCursorPosition();  // Returns in GapBuffer.GapValue
        LDA GapBuffer.GapValueL
        STA ZP.ACCL
        LDA GapBuffer.GapValueH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        if (BBS2, Edit.EditorFlags)  // Block active?
        {
            // Block start position
            LDA #(blockStartLabel % 256)
            STA ZP.STRL
            LDA #(blockStartLabel / 256)
            STA ZP.STRH
            LDA Edit.BlockStartL
            STA ZP.ACCL
            LDA Edit.BlockStartH
            STA ZP.ACCH
            Debug.LabeledWord();
            
            // Block end position
            LDA #(blockEndLabel % 256)
            STA ZP.STRL
            LDA #(blockEndLabel / 256)
            STA ZP.STRH
            LDA Edit.BlockEndL
            STA ZP.ACCL
            LDA Edit.BlockEndH
            STA ZP.ACCH
            Debug.LabeledWord();
            
            // Convert block start to col,row
            LDA #(startCRLabel % 256)
            STA ZP.STRL
            LDA #(startCRLabel / 256)
            STA ZP.STRH
            Debug.String();
            LDA Edit.BlockStartL
            STA GapBuffer.GapValueL
            LDA Edit.BlockStartH
            STA GapBuffer.GapValueH
            convertPositionToCR();  // Returns col in A, row in ACC
            Debug.Byte();          // Show column
            Debug.Word();          // Show row
            
            // Convert block end to col,row
            LDA #(endCRLabel % 256)
            STA ZP.STRL
            LDA #(endCRLabel / 256)
            STA ZP.STRH
            Debug.String();
            LDA Edit.BlockEndL
            STA GapBuffer.GapValueL
            LDA Edit.BlockEndH
            STA GapBuffer.GapValueH
            convertPositionToCR();  // Returns col in A, row in ACC
            Debug.Byte();          // Show column
            Debug.Word();          // Show row
        }
        
        // Restore ZP.IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
    }

    
    // Helper: Convert logical position to col,row
    // Input: GapBuffer.GapValue = position
    // Output: A = column, ZP.ACC = row
    convertPositionToCR()
    {
        // Save target position
        LDA GapBuffer.GapValueL
        STA crPosL
        LDA GapBuffer.GapValueH
        STA crPosH
        
        GapBuffer.GetCharAtFastPrep();
        
        // Walk through document counting lines and columns
        STZ GapBuffer.GapValueL      // Start at position 0
        STZ GapBuffer.GapValueH
        STZ ZP.ACCL     // Row counter
        STZ ZP.ACCH
        STZ crCol       // Column counter
        
        loop
        {
            // Check if we've reached target
            LDA GapBuffer.GapValueH
            CMP crPosH
            if (Z)
            {
                LDA GapBuffer.GapValueL
                CMP crPosL
            }
            if (Z)  // Found position
            {
                LDA crCol   // Return column in A
                break;
            }
            
            // Check if at end
            LDA GapBuffer.GapValueH
            CMP GapBuffer.FastLengthH
            if (Z)
            {
                LDA GapBuffer.GapValueL
                CMP GapBuffer.FastLengthL
            }
            if (C)  // At EOF
            {
                LDA crCol   // Return current column
                break;
            }
            
            // Get character at current position
            GapBuffer.GetCharAtFast();
            
            // Check for newline
            CMP #'\n'
            if (Z)
            {
                INC ZP.ACCL
                if (Z) { INC ZP.ACCH }
                STZ crCol   // Reset column to 0
            }
            else
            {
                INC crCol   // Next column
            }
            
            // Advance position
            INC GapBuffer.GapValueL
            if (Z) { INC GapBuffer.GapValueH }
        }
        
        // A = column, ACC = row
    }


#endif



    
    
    // discard current block and trigger redraw if needed
    // X = 1 to force a redraw, 0 = no redraw
    clearBlock()
    {
        // Check if there was actually a block to clear
        if (BBR2, Edit.EditorFlags) 
        { 
            return;  // No block active, nothing to do
        }
        
        // Clear the block active flag
        RMB2 Edit.EditorFlags
        
        // Set to invalid position (0xFFFF)
        LDA #0xFF
        STA Edit.BlockStartL
        STA Edit.BlockStartH
        STA Edit.BlockEndL
        STA Edit.BlockEndH
        
        // Force redraw to remove highlighting
        CPX #1
        if (Z)
        {
            View.Render();
        }
    }
    
    // Helper: Mark block begin at current cursor position
    blockBegin()
    {
        // Clear any existing block
        LDX #0 // 
        if (BBS2, Edit.EditorFlags) 
        {
            LDX #1 // unless there was an existing block .. render it away
        }
        clearBlock();
        
        // Get current cursor position
        View.GetCursorPosition();  // Returns in GapBuffer.GapValue
        
        // Store as block start
        LDA GapBuffer.GapValueL
        STA Edit.BlockStartL
        LDA GapBuffer.GapValueH
        STA Edit.BlockStartH
        
        // Clear block active flag (need both endpoints)
        RMB2 Edit.EditorFlags
        
    }
    
    // Helper: Mark block end at current cursor position
    blockEnd()
    {
        // Get current cursor position
        View.GetCursorPosition();  // Returns in GapBuffer.GapValue
        
        // Check if block start has been set (not 0xFFFF)
        LDA Edit.BlockStartL
        AND Edit.BlockStartH
        CMP #0xFF
        if (Z)  // Both bytes are 0xFF
        {
            // No begin marker - show error
            LDA #(noBeginMsg % 256)
            STA ZP.STRL
            LDA #(noBeginMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusStringPause();
            return;
        }
        
        // Store as block end
        LDA GapBuffer.GapValueL
        STA Edit.BlockEndL
        LDA GapBuffer.GapValueH
        STA Edit.BlockEndH
        
        // Normalize: ensure Start <= End
        // Compare End to Start
        LDA Edit.BlockEndH
        CMP Edit.BlockStartH
        if (Z)  // End.H == Start.H
        {
            // High bytes equal, check low
            LDA Edit.BlockEndL
            CMP Edit.BlockStartL
        }                
        if (NC)  // End < Start
        {
            // Swap them
            swapBlockEndpoints();
        }
        
        // Set block active flag
        SMB2 Edit.EditorFlags
        
        // Show the highlighted block
        View.Render();
    }
    
    // Helper: Swap block start and end
    swapBlockEndpoints()
    {
        // Swap Start and End
        LDA Edit.BlockStartL
        PHA
        LDA Edit.BlockEndL
        STA Edit.BlockStartL
        PLA
        STA Edit.BlockEndL
        
        LDA Edit.BlockStartH
        PHA
        LDA Edit.BlockEndH
        STA Edit.BlockStartH
        PLA
        STA Edit.BlockEndH
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
            Char.IsLower();
            if (C)
            {
                Char.ToUpper();
            }
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
                    continue;
                }
            } // switch
            
            // Exit loop once we have a valid choice
            break;
        } // loop
    }
    
    
    // New file
    newFile()
    {
        checkModified();
        if (Z)  { return; } // A = 0 means cancelled
        
        View.StatusClear();
        
        GapBuffer.Clear();
        disposeFilename();
        RMB0 EditorFlags  // Clear modified
        
        LDX #0
        clearBlock(); // discards block, render will happen in View.ApplyGapBuffer() below
        
        // cursor to 0,0
        // CountLines
        // Render
        View.ApplyGapBuffer();
    }
    
    // Save file
    saveFile()
    {
        if (BBR0, EditorFlags) { return; } // not modified
        
        View.StatusClear();
        
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
        
        LDA # FileType.Any // all files
        File.Exists();
        if (C)
        {
            File.Delete();
        }
        
        File.StartSave();
        if (NC)
        {
            LDA #(savingErrorMsg % 256)
            STA ZP.STRL
            LDA #(savingErrorMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusStringPause();
            CLC
            return;
        }
        
        LDA #1
        STA ZP.ACCL   
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            LDA #(savingErrorMsg % 256)
            STA ZP.STRL
            LDA #(savingErrorMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusStringPause();
            CLC
            return;
        }
        LDA ZP.IDXL
        STA File.SectorSourceL
        LDA ZP.IDXH
        STA File.SectorSourceH
        
        loop
        {
            // Prepare for fast character access
            GapBuffer.GetCharAtFastPrep();
            
            STZ GapBuffer.GapValueL
            STZ GapBuffer.GapValueH

            LDA #1
            STA TransferLengthL
            STZ TransferLengthH
            loop
            {
                GetCharAtFast();
                if (Z)
                {
                    SEC
                    break;
                }
                STA [File.SectorSource]
                File.AppendStream();
                if (NC)
                {
                    CLC
                    break;
                }
                
                INC GapBuffer.GapValueL
                if (Z) { INC GapBuffer.GapValueH }      
            }
            if (NC)
            {
                LDA #(savingErrorMsg % 256)
                STA ZP.STRL
                LDA #(savingErrorMsg / 256)
                STA ZP.STRH
                LDY #0
                View.StatusStringPause();
                CLC
                break;
            }
            
            LDA #0x00  // Data file (not executable)
            File.EndSave();
            if (NC)
            {
                LDA #(savingErrorMsg % 256)
                STA ZP.STRL
                LDA #(savingErrorMsg / 256)
                STA ZP.STRH
                LDY #0
                View.StatusStringPause();
                CLC
                break;
            }

            LDA File.SectorSourceL
            STA ZP.IDXL
            LDA File.SectorSourceH
            STA ZP.IDXH
            Memory.Free();
            if (NC)
            {
                LDA #(savingErrorMsg % 256)
                STA ZP.STRL
                LDA #(savingErrorMsg / 256)
                STA ZP.STRH
                LDY #0
                View.StatusStringPause();
                CLC
                break;
            }
        

            RMB0 EditorFlags  // Clear modified
            
            LDA #(savedMsg % 256)
            STA ZP.STRL
            LDA #(savedMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusStringPause();
            
            SEC
            break;
        } // loop
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
        
        LDX #1
        clearBlock(); // discards block and renders
    }
    
    // Open file (with modified check)
    openFile()
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
            View.StatusStringPause();
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
            LDA #(loadingErrorMsg % 256)
            STA ZP.STRL
            LDA #(loadingErrorMsg / 256)
            STA ZP.STRH
            LDY #0
            View.StatusStringPause();
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
        
        LDX #0
        clearBlock(); // discards block, render will happen in View.ApplyGapBuffer() below
               
        // Clear modified flag
        RMB0 EditorFlags
        
        // Reset cursor to top of file
        STZ GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        
        View.ApplyGapBuffer();
        
        View.UpdatePosition();
        
        // Show success message briefly
        LDA #(loadedMsg % 256)
        STA ZP.STRL
        LDA #(loadedMsg / 256)
        STA ZP.STRH
        LDY #0
        View.StatusStringPause();
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
        View.StatusCharPause();
        PLA
        
        // Process the command
        switch (A)
        {
            case Key.CtrlB: // finger still down on <ctrl>
            case 'B':       // Mark block begin
            {
                View.StatusClear();
                blockBegin();
            }
            case Key.CtrlK: // finger still down on <ctrl>
            case 'K':       // Mark block end
            {
                View.StatusClear();
                blockEnd();
            }
            case Key.CtrlD: // finger still down on <ctrl>
            case 'D':       // Done - save and exit
            {
                View.StatusClear();
                saveFile();
                if (C)
                {
                    SMB1 EditorFlags // exit
                }
            }
            case Key.CtrlQ: // finger still down on <ctrl>
            case 'Q':       // Quit without save
            {
                View.StatusClear();
                SMB1 EditorFlags // exit
            }
            case Key.CtrlS: // finger still down on <ctrl>
            case 'S':       // Save
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
        if (C) // >= a
        {
            CMP #'z'+1
            if (NC) // < z+1
            {
                AND #0xDF
            }
        }
        
        // Echo the complete command
        PHA
        LDY #3  // Position after "^Q "
        View.StatusCharPause();
        PLA
        
        switch (A)
        {
            case Key.CtrlB: // finger still down on <ctrl>
            case 'B':       // Beginning of block
            {
                View.StatusClear();
                View.CursorHome();
            }
            case Key.CtrlK: // finger still down on <ctrl>
            case 'K':       // End of block
            {
                View.StatusClear();
                View.CursorEnd();
            }
            
            case Key.CtrlS: // finger still down on <ctrl>
            case 'S':       // Beginning of line
            {
                View.StatusClear();
                View.CursorHome();
            }
            case Key.CtrlD: // finger still down on <ctrl>
            case 'D':       // End of line
            {
                View.StatusClear();
                View.CursorEnd();
            }
            case Key.CtrlR: // finger still down on <ctrl>
            case 'R':       // Top of file
            {
                View.StatusClear();
                View.CursorTop();
            }
            case Key.CtrlC: // finger still down on <ctrl>
            case 'C':       // End of file
            {
                View.StatusClear();
                View.CursorBottom();
            }
            case Key.CtrlF: // finger still down on <ctrl>
            case 'F':       // Find
            {
                View.StatusClear();
                // TODO findText();
            }
            case Key.CtrlA: // finger still down on <ctrl>
            case 'A':       // Replace
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

BlockDump();
                      
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
                case Key.CtrlN:
                {
                    newFile();
                }
                case Key.F3:
                case Key.CtrlO:
                {
                    openFile();
                }
                case Key.F2:
                case Key.CtrlS:
                {
                    saveFile();
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
                    //View.GetCursorPosition();
                    //View.SetCursorPosition();
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
                    Char.IsPrintable();
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
