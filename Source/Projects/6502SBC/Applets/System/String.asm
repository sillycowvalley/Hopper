unit String
{
    // Get string length
    // Input: STR
    // Output: A = string length (not including null terminator)
    Length()
    {
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }
            INY
        }
        TYA  // Length in A
    }
    
    // Compare two strings
    // Input: ZP.STR = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    Compare()
    {
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.STRL
            CMP ZP.NEXT0
            if (Z)
            {
                LDA ZP.STRH
                CMP ZP.NEXT1
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.STR], Y
            CMP [ZP.NEXT], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.STR], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            INY
        }
    }
}   
