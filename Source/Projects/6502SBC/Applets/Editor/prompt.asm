unit Prompt
{
    uses "System/Serial"
    uses "System/Screen"
    uses "System/Char"
    uses "Keyboard"
    uses "View"
    
    // Zero page for prompt input buffer
    const byte promptSlots    = 0xA0;
    const uint promptBuffer   = promptSlots+0;
    const byte promptBufferL  = promptSlots+0;
    const byte promptBufferH  = promptSlots+1;
    const byte promptLength   = promptSlots+2;
    const byte promptMaxLen   = promptSlots+3;
    const byte promptStartCol = promptSlots+4;
    const byte promptLastChar = promptSlots+5;
    
    const string ynPrompt = " (Y/N)? ";
    
    // Initialize prompt system (allocate buffer)
    Initialize()
    {
        // Allocate 80 bytes for input buffer
        LDA #80
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA promptBufferL
        LDA ZP.IDXH
        STA promptBufferH
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            CLC  // Failed
            return;
        }
        SEC
    }
    
    // Dispose prompt buffer
    Dispose()
    {
        LDA promptBufferL
        STA ZP.IDXL
        LDA promptBufferH
        STA ZP.IDXH
        Memory.Free();
    }
    
    // Ask for Y/N confirmation
    // Input: ZP.STR = prompt message
    // Output: C set for Yes, clear for No
    AskYN()
    {
        // Clear status line
        View.StatusClear();
        
        // Show prompt
        LDY #0
        View.StatusString();
        
        // Add " (Y/N)? "
        LDA #(ynPrompt % 256)
        STA ZP.STRL
        LDA #(ynPrompt / 256)
        STA ZP.STRH
        LDA ScreenBuffer.CursorCol
        TAY
        View.StatusString();
        
        // Get key until Y or N
        loop
        {
            Keyboard.GetKey();
            
            // Convert to uppercase
            CMP #'y'
            if (Z)
            {
                LDA #'Y'
            }
            CMP #'n'
            if (Z)
            {
                LDA #'N'
            }
            
            CMP #'Y'
            if (Z)
            {
                SEC  // Yes
                break;
            }
            CMP #'N'
            if (Z)
            {
                CLC  // No
                break;
            }
            
            // Invalid key, beep or ignore
        }
        
        View.StatusClear();
    }
    
    // Get string input
    // Input: ZP.STR = prompt message, A = max length
    // Output: C set on Enter (string in buffer), clear on Escape
    //         promptLength = actual length entered
    //         resulting string in STR, length in A
    GetString()
    {
        STA promptMaxLen
        STZ promptLength
        
        SMB3 EditorFlags // prompt mode
        
        // Clear status line and show prompt
        View.StatusClear();
        LDY #0
        View.StatusString();
        
        // Remember where input starts
        LDA ScreenBuffer.CursorCol
        STA promptStartCol
        
        // Input loop
        loop
        {
            Keyboard.GetKey();
            STA promptLastChar
            switch (A)
            {
                case Key.Escape:
                {
                    View.StatusClear();
                    CLC  // Cancelled
                    break;
                }
                
                case Key.Enter:
                {
                    // Null terminate the string
                    LDY promptLength
                    LDA #0
                    STA [promptBuffer], Y
                    
                    View.StatusClear();
                    
                    LDA promptBufferL
                    STA ZP.STRL
                    LDA promptBufferH
                    STA ZP.STRH
                    LDA promptLength
                    
                    SEC  // Success
                    break;
                }
                
                case Key.Backspace:
                {
                    LDA promptLength
                    if (NZ)
                    {
                        DEC promptLength
                        
                        // Move cursor back and erase
                        CLC
                        LDA promptStartCol
                        ADC promptLength
                        TAY
                        LDA # Key.Backspace
                        View.StatusChar();
                    }
                }
                
                default:
                {
                    // Check if printable
                    Char.IsPrintable();
                    if (C)
                    {
                        // Check length
                        LDA promptLength
                        CMP promptMaxLen
                        if (NC)  // length < max
                        {
                            // Store character
                            LDY promptLength
                            LDA promptLastChar
                            STA [promptBuffer], Y
                            
                            // Display it
                            CLC
                            LDA promptStartCol
                            ADC promptLength
                            TAY
                            LDA promptLastChar
                            View.StatusChar();
                            INC promptLength
                        }
                    }
                }
            }
        } // loop
        RMB3 EditorFlags // not in prompt mode
    }
    
    // Get filename (convenience wrapper)
    // Input: ZP.STR = prompt (e.g., "Save as: ")
    // Output: C set if entered, promptBuffer contains filename
    GetFilename()
    {
        LDA #13  // Max filename length for EEPROM filesystem
        GetString();
    }
    
    
}
