unit String
{
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything (saves/restores ZP.TOP)
    Length()
    {
        PHX
        PHY
        
        // Use stack to preserve caller's ZP.TOP instead of corrupting it
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        
        STX ZP.TOP0
        STY ZP.TOP1
        
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y
            if (Z) { break; }
            INY
#ifdef DEBUG
            if (Z) // wrapped around from 0xFF to 0x00
            {
                LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
            }
#endif
        }
        
        // Restore caller's ZP.TOP
        PLA
        STA ZP.TOP0
        PLA
        STA ZP.TOP1
        
        TYA  // Length in A
        
        PLY
        PLX
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    Compare()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.TOP], Y
            CMP [ZP.NEXT], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.TOP], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            
            INY
        }
        
        PLY
        PLA
    }
    
    // Convert string to uppercase in place (compact version)
    // Input: ZP.STR = pointer to null-terminated string
    // Output: String converted to uppercase
    // Munts: A, Y
    ToUpperSTR()
    {
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // Null terminator
            Char.IsLower();    // preserves A
            if (C)
            {
                AND #0xDF      // Convert to uppercase by stripping bit 5
                STA [ZP.STR], Y
            }
            INY
            if (Z) { break; }  // Prevent overflow
        }
    }
}
