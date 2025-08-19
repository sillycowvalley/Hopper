unit String
{
    // Calculate string length
    // Input: ZP.STR = pointer to null-terminated string
    // Output: Y = string length
    // Modifies: A, X, Y
    // Note: Returns 0 for null pointer
    Length()
    {
        // Check for null pointer
        LDA ZP.STRL
        ORA ZP.STRH
        if (Z)
        {
            // Null pointer - return length 0
            STZ ZP.STRL
            STZ ZP.STRH
            return;
        }
        
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }
            INY
#ifdef DEBUG
            if (Z) // wrapped around from 0xFF to 0x00
            {
                LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
            }
#endif
        }
    }

    // Compare two strings using throwaway registers
    // Input: ZP.STR = first string, ZP.STR2 = second string
    // Output: C set if match, NC if different
    Compare()
    {
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.STRL
            CMP ZP.STR2L
            if (Z)
            {
                LDA ZP.STRH
                CMP ZP.STR2H
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.STR], Y
            CMP [ZP.STR2], Y
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
