unit Char
{
    const byte CtrlC      = 0x03; // <ctrL><C>
    const byte XOFF       = 0x13; // <ctrl><S>
    const byte XON        = 0x11; // <ctrl><Q>
    
    const byte EOL        = 0x0A;
    const byte Escape     = 0x1B;
    const byte Slash      = 0x5C;
    const byte Formfeed   = 0x0C;
    const byte Backspace  = 0x08;
    const byte Tab        = 0x09;
    
    flags CharClass
    {
        Other = 0b00000000,
        Digit = 0b00000001,
        Alpha = 0b00000010,
        Hex   = 0b00000100,
        Lower = 0b00001000,
        Upper = 0b00010000, // redundant - just !Lower
    }
    
    // Check character type
    // Input: A = character
    // Output: A = bitmapped CharClass
    // Preserves: Everything except A
    getCharClass()
    {
        loop
        {
            CMP #'0'
            if (C)
            {
                CMP #('9'+1)
                if (NC) { LDA # (CharClass.Digit | CharClass.Hex) break; }
            }
            CMP #'A'
            if (C)
            {
                CMP #('F'+1) 
                if (NC) { LDA # (CharClass.Alpha | CharClass.Upper | CharClass.Hex) break; }
            }
            CMP #'G'
            if (C)
            {
                CMP #('Z'+1) 
                if (NC) { LDA # (CharClass.Alpha | CharClass.Upper) break; }
            }
            CMP #'a'
            if (C)
            {
                CMP #('f'+1)
                if (NC) { LDA # (CharClass.Alpha | CharClass.Lower | CharClass.Hex) break; }
            }
            CMP #'g'
            if (C)
            {
                CMP #('z'+1)
                if (NC) { LDA # (CharClass.Alpha | CharClass.Lower) break; }
            }
            LDA # CharClass.Other
            break;
        } // single exit
    }
    
    // Input: A = character
    // Output: C = digit, NC = not digit
    IsDigit()
    {
        PHA
        getCharClass();
        AND # CharClass.Digit
        if (NZ)
        {
            SEC // '0'..'9'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = alpha, NC = not alpha
    IsAlpha()
    {
        PHA
        getCharClass();
        AND # CharClass.Alpha
        if (NZ)
        {
            SEC // 'a'..'z' | 'A'..'Z'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input:  A = character
    // Output: A = character (converted to uppercase if it was 'a'..'z')
    ToUpper()
    {
        PHA
        getCharClass();
        AND # CharClass.Lower
        if (NZ)
        {
            // 'a'..'z'
            PLA
            AND #0xDF          // Convert to uppercase
        }
        else
        {
            PLA
        }
    }
    
    // Input: A = character
    // Output: C = alphanumeric, NC = not alphanumeric
    IsAlphaNumeric()
    {
        PHA
        getCharClass();
        AND # (CharClass.Alpha|CharClass.Digit)
        if (NZ)
        {
            SEC // 'a'..'z' | 'A'..'Z' | '0'..'9'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = lower alpha, NC = not lower alpha
    IsLower()
    {
        PHA
        getCharClass();
        AND # CharClass.Lower
        if (NZ)
        {
            SEC // 'a'..'z'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = hex digit, NC = not hex digit
    IsHex()
    {
        PHA
        getCharClass();
        AND # CharClass.Hex
        if (NZ)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A - character to test
    // Output: C for yes, NC for no
    IsPrintable()
    {
        loop
        {
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (NC)  // <= 126
                {
                    SEC
                    break;
                }
            }
            CLC
            break;
        } // exit loop
    }
}

