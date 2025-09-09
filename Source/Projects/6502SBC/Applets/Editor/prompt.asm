unit Prompt
{
    uses "System/Serial"
    uses "System/Screen"
    uses "Keyboard"
    uses "View"
    
    // Zero page for prompt input buffer
    const byte promptSlots   = 0xA3;
    const uint promptBuffer  = promptSlots+0;
    const byte promptBufferL = promptSlots+0;
    const byte promptBufferH = promptSlots+1;
    const byte promptLength  = promptSlots+2;
    const byte promptMaxLen  = promptSlots+3;
    
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
        
        // Clear status line and show prompt
        View.StatusClear();
        LDY #0
        View.StatusString();
        
        // Remember where input starts
        LDA ScreenBuffer.CursorCol
        PHA  // Save start column
        
        // Input loop
        loop
        {
            Keyboard.GetKey();
            
            switch (A)
            {
                case Key.Escape:
                {
                    PLA  // Clean stack
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
                    
                    PLA  // Clean stack
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
                        PLA
                        PHA
                        CLC
                        ADC promptLength
                        TAY
                        LDA #' '
                        View.StatusChar();
                    }
                }
                
                default:
                {
                    // Check if printable
                    PHA
                    Keyboard.IsPrintable();
                    if (C)
                    {
                        // Check length
                        LDA promptLength
                        CMP promptMaxLen
                        if (C)  // length < max
                        {
                            // Store character
                            PLA
                            LDY promptLength
                            STA [promptBuffer], Y
                            
                            // Display it
                            PHA
                            PLA
                            PHA  // Get start column
                            CLC
                            ADC promptLength
                            TAY
                            PLA
                            View.StatusChar();
                            
                            INC promptLength
                        }
                        else
                        {
                            PLA  // Discard character
                        }
                    }
                    else
                    {
                        PLA  // Not printable
                    }
                }
            }
        } // loop
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
