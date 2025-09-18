unit Utilities
{
    // Compare two null-terminated strings
    // Input: ZP.STR = first string pointer
    //        ZP.IDY = second string pointer
    // Output: C set if strings are equal, clear if different
    CompareStrings()
    {
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            CMP [ZP.IDY], Y
            if (NZ)
            {
                CLC  // Not equal
                return;
            }
            // Check if we hit null terminator
            CMP #0
            if (Z)
            {
                SEC  // Equal (both null)
                return;
            }
            INY
        }
    }
    
    DuplicateString() // STR -> STR
    {
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {
            // Get string length
            LDY #0
            loop
            {
                LDA [STR], Y
                if (Z) { break; }
                INY
            }
            INY  // Include null terminator
            
            STY ZP.ACCL
            STZ ZP.ACCH
            Memory.Allocate();
            if (NC) { Errors.OutOfMemory(); break; }
            
            // copy string
            LDY #0
            loop
            {
                LDA [STR], Y
                STA [IDX], Y
                if (Z) { break; }
                INY
            }
            
            LDA ZP.IDXL
            STA ZP.STRL
            LDA ZP.IDXH
            STA ZP.STRH
        
            SEC
            break;    
        } // single exit
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        PLY   
    }
}
