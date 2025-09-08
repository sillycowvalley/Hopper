program SimpleEditor
{
    #define CPU_65C02S
    
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
    
    
    // Zero page allocation
    const byte vwSlots = 0xB0;
    
    // View state
    const byte vwScreenCols = vwSlots+0;     // Screen width (80)
    const byte vwScreenRows = vwSlots+1;     // Screen height (24)
    const byte vwCurrentRow = vwSlots+2;     // Current cursor row on screen (0-23)
    const uint vwTopLine = vwSlots+3;        // First visible line number
    const byte vwTopLineL = vwSlots+3;
    const byte vwTopLineH = vwSlots+4;
    const uint vwLineCount = vwSlots+5;      // Total lines in document
    const byte vwLineCountL = vwSlots+5;
    const byte vwLineCountH = vwSlots+6;
    
    // Temporary workspace
    const uint vwTemp = vwSlots+7;
    const byte vwTempL = vwSlots+7;
    const byte vwTempH = vwSlots+8;
    const uint vwPos = vwSlots+9;            // Current position in text
    const byte vwPosL = vwSlots+9;
    const byte vwPosH = vwSlots+10;
    
    // Messages
    const string loadingMsg = "Loading BIGTEST...\n";
    const string notFoundMsg = "File not found!\n";
    const string errorMsg = "Error loading file!\n";
    const string memoryMsg = "Out of memory!\n";
    
    const string fileName = "BIGTEST";
        
    
    // Initialize View
    Initialize()
    {
        // Save screen dimensions
        LDA #80
        STA vwScreenCols
        LDA #24  // Leave room for status line
        STA vwScreenRows
        
        // Initialize ScreenBuffer
        LDA #80
        LDY #25  // Full screen including status line
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
        STZ vwTopLineL
        STZ vwTopLineH
        STZ vwLineCountL
        STZ vwLineCountH
        
        // Load the test file
        loadFile();
        
        // Count lines in the loaded text
        countLines();
        
        SEC  // Success
    }
    
    // Load BIGTEST file into GapBuffer
    loadFile()
    {
        LDA #(loadingMsg % 256)
        STA ZP.STRL
        LDA #(loadingMsg / 256)
        STA ZP.STRH
        Print.String();
        
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
            LDA #(notFoundMsg % 256)
            STA ZP.STRL
            LDA #(notFoundMsg / 256)
            STA ZP.STRH
            Print.String();
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
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
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
                PHY
                GapBuffer.InsertChar();
                PLY
                INY
                if (Z) { break; }  // Y wrapped to 0 after 255
            }
        }
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
        STA vwTempL
        LDA GapBuffer.GapValueH
        STA vwTempH
        
        // Scan for newlines
        STZ vwPosL
        STZ vwPosH
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP vwTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwTempL
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
    
    // Render the current view
    Render()
    {
        ScreenBuffer.Suspend();
        
        // Clear screen
        ScreenBuffer.Clear();
        
        // Start at top of viewport
        STZ vwPosL
        STZ vwPosH
        
        // Find start of top visible line
        LDA vwTopLineL
        ORA vwTopLineH
        if (NZ)
        {
            // Skip to start of top line
            STZ vwTempL
            STZ vwTempH
            
            loop
            {
                // Check if reached target line
                LDA vwTempH
                CMP vwTopLineH
                if (Z)
                {
                    LDA vwTempL
                    CMP vwTopLineL
                }
                if (C) { break; }  // temp >= topLine
                
                // Find next newline
                findNextNewline();
                
                // Count the line
                INC vwTempL
                if (Z) { INC vwTempH }
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
        
        // Position cursor
        LDA #0
        LDY vwCurrentRow
        ScreenBuffer.GotoXY();
        
        // Show cursor
        ScreenBuffer.ShowCursor();
        
        ScreenBuffer.Resume();
    }
    
    // Helper: Find next newline from current vwPos
    findNextNewline()
    {
        // Get text length
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA vwTempL
        LDA GapBuffer.GapValueH
        STA vwTempH
        
        loop
        {
            // Check if at end
            LDA vwPosH
            CMP vwTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwTempL
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
        STA vwTempL
        LDA GapBuffer.GapValueH
        STA vwTempH
        
        LDX #0  // Column counter
        loop
        {
            // Check if at end of document
            LDA vwPosH
            CMP vwTempH
            if (Z)
            {
                LDA vwPosL
                CMP vwTempL
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
            
            // Check if at right edge
            CPX vwScreenCols
            if (C) { break; }  // col >= screenCols
            
            // Display character
            ScreenBuffer.Char();
            INX
        }
    }
    
    // Move cursor up
    CursorUp()
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
            }
        }
    }
    
    // Move cursor down
    CursorDown()
    {
        // Check if at last line of document
        CLC
        LDA vwTopLineL
        ADC vwCurrentRow
        STA vwTempL
        LDA vwTopLineH
        ADC #0
        STA vwTempH
        
        // Increment to get next line
        INC vwTempL
        if (Z) { INC vwTempH }
        
LDA vwTopLineL Debug.Byte();
LDA vwCurrentRow Debug.Byte();
        
        // Check if next line exists
        LDA vwTempH
        CMP vwLineCountH
        if (Z)
        {
            LDA vwTempL
            CMP vwLineCountL
        }
        if (C) 
        {
LDA #0x55 Debug.Byte();            
            return;  // nextLine >= lineCount
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
        }
    }
    
    // Cleanup
    Dispose()
    {
        GapBuffer.Dispose();
        ScreenBuffer.Dispose();
    }

    
    Hopper()
    {
        Debug.Initialize();
        
        // Clear screen
        Screen.Clear();
        
        // Initialize keyboard
        Keyboard.Initialize();
        
        // Initialize view
        Initialize();
        if (NC)
        {
            return;
        }
        
        // Initial render
        Screen.Clear();
        Render();
        
        // Main loop
        loop
        {
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
                    CursorUp();
                    
                    // Update cursor position on screen
                    LDA #0
                    LDY vwCurrentRow
                    Screen.GotoXY();
                }
                case Key.Down:
                {
                    CursorDown();
                    
                    // Update cursor position on screen
                    LDA #0
                    LDY vwCurrentRow
                    Screen.GotoXY();
                }
                default:
                {
                    // Ignore other keys
                }
            }
        }
        
        // Cleanup
        Dispose();
        Screen.Reset();
    }
}
