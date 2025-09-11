unit Keyboard
{
    // Zero page allocation
    const byte kbSlots = 0x7F;
    
    // Escape sequence state machine
    const byte kbEscState = kbSlots+0;  // 0=normal, 1=got ESC, 2=got ESC[, 3=got ESC O
    
    // Key codes for special keys
    enum Key
    {
        // ASCII control codes
        Backspace = 0x08,
        Tab       = 0x09,
        Linefeed  = 0x0A,
        Enter     = 0x0D,
        Escape    = 0x1B,
        
         // Control key combinations (in ASCII order)
        CtrlA     = 0x01,  // Word left
        CtrlB     = 0x02,  // (unused - available)
        CtrlC     = 0x03,  // Page down (or modern Copy)
        CtrlD     = 0x04,  // Character right
        CtrlE     = 0x05,  // Line up
        CtrlF     = 0x06,  // Word right (or modern Find)
        CtrlG     = 0x07,  // Delete character
        CtrlH     = 0x08,  // Delete left (same as Backspace)
        CtrlI     = 0x09,  // (same as Tab)
        CtrlJ     = 0x0A,  // (same as Linefeed)
        CtrlK     = 0x0B,  // Block operations prefix
        CtrlL     = 0x0C,  // Repeat last find
        CtrlM     = 0x0D,  // (same as Enter)
        CtrlN     = 0x0E,  // New file (modern)
        CtrlO     = 0x0F,  // Open file (modern)
        CtrlP     = 0x10,  // Insert control character
        CtrlQ     = 0x11,  // Quick movement prefix
        CtrlR     = 0x12,  // Page up
        CtrlS     = 0x13,  // Character left (or modern Save)
        CtrlT     = 0x14,  // Delete word right
        CtrlU     = 0x15,  // (unused - available)
        CtrlV     = 0x16,  // Paste (modern)
        CtrlW     = 0x17,  // (unused - available)
        CtrlX     = 0x18,  // Line down (or modern Cut)
        CtrlY     = 0x19,  // Delete entire line
        CtrlZ     = 0x1A,  // Undo/Redo (modern)
        
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
        
        F1        = 138,
        F2        = 139,
        F3        = 140,
        F4        = 141,
        F5        = 142,
        F6        = 143,
        F7        = 144,
        F8        = 145,
        F9        = 146,
        F10       = 147,
        F11       = 148,
        F12       = 149,
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
            if (Z)
            {
                // State 0: Normal - get character
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
                
                // Regular character - exit with it in A
                break;
            }
            
            // We're in an escape sequence
            switch (A)
            {
                case 1:
                {
                    // State 1: Got ESC, waiting for next char
                    processEscState();
                    if (NC)  // Carry clear means continue processing
                    {
                        continue;
                    }
                }
                case 2:
                {
                    // State 2: Got ESC[, parse VT100 sequence
                    processEscBracketState();
                    if (NC)  // Carry clear means continue processing
                    {
                        continue;
                    }
                }
                case 3:
                {
                    // State 3: Got ESC O, check for function keys
                    processEscOState();
                    if (NC)  // Carry clear means continue processing
                    {
                        continue;
                    }
                }
                default:
                {
                    // Invalid state, reset
                    STZ kbEscState
                    continue;
                }
            } // switch
            break;
        } // single exit
        
        // A contains the key code
    }

    // Process ESC state
    // Output: C set if result ready in A, clear to continue
    processEscState()
    {
        // Short delay to see if this is part of an escape sequence
        // Escape sequences arrive quickly (<5ms), human typing is slower
        LDA #10         // 10ms timeout
        Shared.LoadTopByte();
        Time.Delay();
        
        // Check if another character arrived
        Serial.IsAvailable();
        if (NC)  // No character available
        {
            // Standalone ESC key - return it immediately
            STZ kbEscState
            LDA #Key.Escape
            SEC  // Result ready
            return;
        }
        
        // Character available - part of escape sequence
        Serial.WaitForChar();  // Get the character
        
        loop
        {
            // Check for [
            CMP #'['
            if (Z)
            {
                LDA #2
                STA kbEscState
                CLC  // Continue processing
                break;
            }
            
            // Check for O (function keys)
            CMP #'O'
            if (Z)
            {
                LDA #3
                STA kbEscState
                CLC  // Continue processing
                break;
            }
            
            // Not a recognized sequence, return ESC
            STZ kbEscState
            LDA #Key.Escape
            SEC  // Result ready
            break;
        } // single exit
    }
    
    processEscUp()
    {
        LDA # Key.Up
        SEC
    }
    processEscDown()
    {
        LDA # Key.Down
        SEC
    }
    processEscLeft()
    {
        LDA # Key.Left
        SEC
    }
    processEscRight()
    {
        LDA # Key.Right
        SEC
    }
    processEscHome()
    {
        LDA # Key.Home
        SEC
    }
    processEscEnd()
    {
        LDA # Key.End
        SEC
    }
    processEsc1()
    {
        processNumericEscape();
        if (C)  // Valid sequence
        {
            // A contains either '~' (single digit) or the second digit
            CMP #'~'
            if (Z)
            {
                // ESC[1~ = Home
                LDA # Key.Home
                SEC
            }
            else
            {
                // Two-digit sequence, check which one
                switch (A)
                {
                    case '1': { LDA #Key.F1 SEC }   // ESC[11~
                    case '2': { LDA #Key.F2 SEC }   // ESC[12~
                    case '3': { LDA #Key.F3 SEC }   // ESC[13~
                    case '4': { LDA #Key.F4 SEC }   // ESC[14~
                    case '5': { LDA #Key.F5 SEC }   // ESC[15~
                    // Note: ESC[16~ is skipped!
                    case '7': { LDA #Key.F6 SEC }   // ESC[17~
                    case '8': { LDA #Key.F7 SEC }   // ESC[18~
                    case '9': { LDA #Key.F8 SEC }   // ESC[19~
                    default:  { CLC             }
                }
            }
        }
        else
        {
            CLC  // Continue processing
        }
    }
    processEsc2()
    {
        processNumericEscape();
        if (C)  // Valid sequence
        {
            // A contains either '~' (single digit) or the second digit
            CMP #'~'
            if (Z)
            {
                // ESC[2~ = Home
                LDA # Key.Insert
                SEC
            }
            else
            {
                // Two-digit sequence, check which one
                switch (A)
                {
                    case '0': { LDA #Key.F9  SEC }    // ESC[20~
                    case '1': { LDA #Key.F10 SEC }   // ESC[21~
                    // Note: ESC[22~ is skipped!
                    case '3': { LDA #Key.F11 SEC }   // ESC[23~
                    case '4': { LDA #Key.F12 SEC }   // ESC[24~
                    default:  { CLC              }
                }
            }
        }
        else
        {
            CLC  // Continue processing
        }
    }
    
    processEscUnknown()
    {
        CLC  // Continue processing
    }
    processEsc3()
    {
        processNumericEscape();
        if (C)
        {
            LDA #Key.Delete
        }
    }
    processEsc4()
    {
        processNumericEscape();
        if (C)
        {
            LDA #Key.End
        }
    }
    processEsc5()
    {
        processNumericEscape();
        if (C)
        {
            LDA #Key.PageUp
        }
    }
    processEsc6()
    {
        processNumericEscape();
        if (C)
        {
            LDA #Key.PageDown
        }
    }
    
    // Process ESC[ state
    // Output: C set if result ready in A, clear to continue
    processEscBracketState()
    {
        Serial.WaitForChar();
        
        // Reset state
        STZ kbEscState
        
        // Check for arrow keys and special keys
        TAX
        switch (X)
        {
            case 'A':  // Up arrow
            {
                processEscUp();
            }
            case 'B':  // Down arrow
            {
                processEscDown();
            }
            case 'C':  // Right arrow
            {
                processEscRight();
            }
            case 'D':  // Left arrow
            {
                processEscLeft();
            }
            case 'H':  // Home
            {
                processEscHome();
            }
            case 'F':  // End
            {
                processEscEnd();
            }
            case '1':  // Could be Home or function key
            {
                processEsc1();
            }
            case '2':  // Could be Insert or function key
            {
                processEsc2();
            }
            case '3':  // Delete
            {
                processEsc3();
            }
            case '4':  // End
            {
                processEsc4();
            }
            case '5':  // Page Up
            {
                processEsc5();
            }
            case '6':  // Page Down
            {
                processEsc6();
            }
            default:
            {
                // Unknown sequence, continue
                processEscUnknown();
            }
        }
    }

    // Process numeric escape sequences (ESC[n~)
    // Output: C set if ~ found, clear otherwise
    processNumericEscape()
    {
        Serial.WaitForChar();
        CMP #'~'
        if (Z)
        {
            // Single digit sequence (ESC[n~)
            SEC  // Valid but we don't know which key
            return;
        }
        // Check for second digit
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP # ('9'+1)
            if (NC)  // < '9'+1, so it's a digit
            {
                // Save second digit and check for ~
                PHA
                Serial.WaitForChar();
                CMP #'~'
                if (Z)
                {
                    PLA  // Get second digit back
                    SEC  // Valid two-digit sequence
                    return;
                }
                PLA
            }
        }
        
    }

    // Process ESC O state
    // Output: C set if result ready in A, clear to continue
    processEscOState()
    {
        Serial.WaitForChar();
        
        // Reset state
        STZ kbEscState
        
        loop
        {
            switch (A)
            {
                case 'H':  // Home (alternate)
                {
                    LDA #Key.Home
                }
                case 'F':  // End (alternate)
                {
                    LDA #Key.End
                }
                default:
                {
                    // Unknown, continue
                    CLC  // Continue processing
                    break;
                }
            }
            SEC
            break;
        } // single exit
    }
    
}
