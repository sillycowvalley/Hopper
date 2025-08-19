unit File
{
    // Check if character is valid for filename (alphanumeric + period)
    // Input: A = character to test
    // Output: C set if valid, NC if invalid
    // Preserves: X, Y
    // Munts: A only
    IsValidFilenameChar()
    {
        loop // single exit block
        {
            // First check if it's alphanumeric
            Char.IsAlphaNumeric();
            if (C)
            {
                SEC  // Valid alphanumeric character
                break;
            }
            
            // Not alphanumeric, check if it's a period
            CMP #'.'
            if (Z)
            {
                SEC  // Valid period character
                break;
            }
            
            // Not alphanumeric and not period
            CLC  // Invalid character
            break;
        } // single exit
    }
    
    // Validate string as filename (length 1-12, valid characters)
    // Input: ZP.STR = pointer to null-terminated uppercase string
    // Output: C set if valid filename, NC if invalid
    //         A = actual string length
    // Preserves: X, Y, ZP.STR
    // Munts: A only
    ValidateFilename()
    {
        PHY
        
        loop // single exit block
        {
            // Get string length
            LDX ZP.STRL
            LDY ZP.STRH
            String.Length();  // Returns length in A
            
            // Check length bounds (1-12 characters)
            if (Z)  // Length is 0
            {
                CLC  // Invalid - empty filename
                break;
            }
            
            CMP #13
            if (C)  // Length >= 13
            {
                CLC  // Invalid - filename too long
                break;
            }
            
            // Length is valid (1-12), now check each character
            // Save length for return value
            PHA  // Save length on stack
            
            LDY #0
            loop  // Character validation loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; }  // End of string - all characters valid
                
                IsValidFilenameChar();
                if (NC)
                {
                    // Invalid character found
                    PLA  // Restore length to A
                    CLC  // Invalid filename
                    break;  // Exit both loops
                }
                
                INY
            }
            
            // If we get here, all characters were valid
            PLA  // Restore length to A
            SEC  // Valid filename
            break;
        } // single exit
        
        PLY
    }
}
