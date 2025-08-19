unit Char
{
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
    
    // Input: A = character
    // Output: C = alphanumeric, NC = not alphanumeric
    IsAlphaNumeric()
    {
        PHA
        getCharClass();
        AND # (CharClass.Alpha|CharClass.Digit)
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
}