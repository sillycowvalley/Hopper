unit Prompt
{
    
    // Zero page for prompt input buffer
    const byte promptSlots    = 0xB0;
    
    const uint promptBuffer   = promptSlots+0;   // Pointer to 80-byte allocated input buffer
    const byte promptBufferL  = promptSlots+0;   //       Low byte of input buffer pointer
    const byte promptBufferH  = promptSlots+1;   //       High byte of input buffer pointer
    
    const byte promptLength   = promptSlots+2;   // Current number of characters entered (0-promptMaxLen)
    const byte promptMaxLen   = promptSlots+3;   // Maximum input length allowed (set by caller, e.g. 13 for filenames)
    const byte promptStartCol = promptSlots+4;   // Screen column where input begins (after prompt text)
    const byte promptLastChar = promptSlots+5;   // Temp storage for last typed character (for uppercase conversion)
    
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
        
#ifdef UNIVERSAL
        LDA #0b00001000
        ORA EditorFlags
        STA EditorFlags
#else                
        SMB3 EditorFlags // prompt mode
#endif
        
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
                case Key.Linefeed: // VT100 Paste
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
#ifdef UNIVERSAL
                    BIT EditorFlags
                    if (MI)
                    {
                        Char.IsAlphaNumeric();
                        if (NC) 
                        {
                            CMP #'.'
                            if (NZ)
                            {
                                continue;
                            }
                        }
                        Char.ToUpper();
                        STA promptLastChar
                        SEC
                    }
                    else
                    {
                        // Check if printable
                        Char.IsPrintable();
                    }
#else                    
                    if (BBS7, EditorFlags) // filename mode
                    {
                        Char.IsAlphaNumeric();
                        if (NC) 
                        {
                            CMP #'.'
                            if (NZ)
                            {
                                continue;
                            }
                        }
                        Char.ToUpper();
                        STA promptLastChar
                        SEC
                    }
                    else
                    {
                        // Check if printable
                        Char.IsPrintable();
                    }
#endif                    
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
#ifdef UNIVERSAL
        LDA #0b11110111
        AND EditorFlags
        STA EditorFlags
#else        
        RMB3 EditorFlags // not in prompt mode
#endif
    }
    
    // Get filename (convenience wrapper)
    // Input: ZP.STR = prompt (e.g., "Save as: ")
    // Output: C set if entered, promptBuffer contains filename
    GetFilename()
    {
#ifdef UNIVERSAL
        LDA #0b10000000
        ORA EditorFlags
        STA EditorFlags
#else        
        SMB7 EditorFlags // entering a filename
#endif
        LDA #13  // Max filename length for EEPROM filesystem
        GetString();
#ifdef UNIVERSAL
        LDA #0b01111111
        AND EditorFlags
        STA EditorFlags
#else
        RMB7 EditorFlags // entering a filename
#endif
    }
    
    
}
