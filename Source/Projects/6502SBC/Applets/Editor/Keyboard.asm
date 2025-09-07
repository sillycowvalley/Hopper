unit Keyboard
{
    uses "System/Definitions"
    uses "System/Serial"
    
    // Zero page allocation
    const byte kbSlots = 0x6F;  // After View's bytes
    
    // Escape sequence state machine
    const byte kbEscState = kbSlots+0;  // 0=normal, 1=got ESC, 2=got ESC[
    
    // Key codes for special keys
    enum Key
    {
        // ASCII control codes
        Backspace = 0x08,
        Tab       = 0x09,
        Enter     = 0x0D,
        Escape    = 0x1B,
        
        // Control key combinations
        CtrlC     = 0x03,
        CtrlS     = 0x13,
        CtrlQ     = 0x11,
        CtrlO     = 0x0F,
        CtrlN     = 0x0E,
        
        // Extended keys (non-ASCII)
        Up        = 128,
        Down      = 129,
        Right     = 130,
        Left      = 131,
        Home      = 132,
        End       = 133,
        PageUp    = 134,
        PageDown  = 135,
        Delete    = 136,
        Insert    = 137,
    }
    
    // Initialize keyboard
    Initialize()
    {
        STZ kbEscState
    }
    
    // Get next key with escape sequence processing
    // Output: A = key code (may be extended code)
    GetKey()
    {
        loop
        {
            // Check current state
            LDA kbEscState
            if (NZ)
            {
                CMP #1
                if (Z) { BRA gotEsc; }
                CMP #2
                if (Z) { BRA gotEscBracket; }
                CMP #3
                if (Z) { BRA gotEscO; }
            }
            
            // Normal state - get character
            Serial.WaitForChar();
            
            // Check for escape
            CMP #Key.Escape
            if (Z)
            {
                // Start escape sequence
                LDA #1
                STA kbEscState
                continue;
            }
            
            // Check for delete (often sent as 0x7F)
            CMP #0x7F
            if (Z)
            {
                LDA #Key.Delete
            }
            
            // Regular character
            return;
            
gotEsc:
            // Got ESC, waiting for next char
            Serial.WaitForChar();
            
            // Check for [
            CMP #'['
            if (Z)
            {
                LDA #2
                STA kbEscState
                continue;
            }
            
            // Check for O (function keys)
            CMP #'O'
            if (Z)
            {
                LDA #3
                STA kbEscState
                continue;
            }
            
            // Not a recognized sequence, return ESC
            STZ kbEscState
            LDA #Key.Escape
            return;
            
gotEscBracket:
            // Got ESC[, parse VT100 sequence
            Serial.WaitForChar();
            PHA
            
            // Reset state
            STZ kbEscState
            
            PLA
            // Check for arrow keys and special keys
            switch (A)
            {
                case 'A':  // Up arrow
                {
                    LDA #Key.Up
                    return;
                }
                case 'B':  // Down arrow
                {
                    LDA #Key.Down
                    return;
                }
                case 'C':  // Right arrow
                {
                    LDA #Key.Right
                    return;
                }
                case 'D':  // Left arrow
                {
                    LDA #Key.Left
                    return;
                }
                case 'H':  // Home
                {
                    LDA #Key.Home
                    return;
                }
                case 'F':  // End
                {
                    LDA #Key.End
                    return;
                }
                case '1':  // Could be Home or function key
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.Home
                        return;
                    }
                    // Ignore other sequences
                    continue;
                }
                case '2':  // Insert
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.Insert
                        return;
                    }
                    continue;
                }
                case '3':  // Delete
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.Delete
                        return;
                    }
                    continue;
                }
                case '4':  // End
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.End
                        return;
                    }
                    continue;
                }
                case '5':  // Page Up
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.PageUp
                        return;
                    }
                    continue;
                }
                case '6':  // Page Down
                {
                    Serial.WaitForChar();
                    CMP #'~'
                    if (Z)
                    {
                        LDA #Key.PageDown
                        return;
                    }
                    continue;
                }
                default:
                {
                    // Unknown sequence, ignore
                    continue;
                }
            }
            
gotEscO:
            // Got ESC O, check for function keys
            Serial.WaitForChar();
            PHA
            
            // Reset state
            STZ kbEscState
            
            PLA
            switch (A)
            {
                case 'H':  // Home (alternate)
                {
                    LDA #Key.Home
                    return;
                }
                case 'F':  // End (alternate)
                {
                    LDA #Key.End
                    return;
                }
                default:
                {
                    // Unknown, ignore
                    continue;
                }
            }
        }
    }
    
    // Check if a key is printable
    // Input: A = key code
    // Output: C set if printable, clear if control/special
    IsPrintable()
    {
        CMP #32
        if (C) { return; }  // < 32 is control
        CMP #127
        if (NC)  // >= 127 is special or delete
        {
            CLC
            return;
        }
        SEC
    }
}
