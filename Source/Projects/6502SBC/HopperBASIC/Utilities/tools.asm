unit Tools // Tools.asm
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Production utilities only - debug functionality moved to Debug.asm
        
    
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
    /*
    // Print null-terminated string to serial output
    // Input: A - character to print
    // Output: 'A' for printable, 0xXX for unprintable to serial
    // Preserves: Everything
    PrintChar()
    {
        PHA
        loop
        {
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (NC)  // <= 126
                {
                    PHA LDA #'\'' Serial.WriteChar(); PLA
                    Serial.WriteChar();
                    LDA #'\'' Serial.WriteChar();
                    break;
                }
            }
            
            // Not printable
            PHA LDA #'0' Serial.WriteChar(); LDA #'x' Serial.WriteChar(); PLA
            Serial.HexOut();
            
            break;
        } // exit loop   
        PLA
    }
    
    // Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    {
        PHP  // Push processor status (including carry flag)
        PHA
        LDA #'\n' 
        Serial.WriteChar();
        PLA
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Write character preserving carry flag
    // Input: A = character to output
    // Output: Character printed to serial
    // Preserves: Everything
    COut()
    {
        PHP  // Push processor status (including carry flag)
        Serial.WriteChar();
        PLP  // Pull processor status (restore carry flag)
    }
    */
    
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything (saves/restores ZP.TOP)
    StringLength()
    {
        PHX
        PHY
        
        // Use stack to preserve caller's ZP.TOP instead of corrupting it
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH  
        PHA
        
        STX ZP.TOPL
        STY ZP.TOPH
        
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
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        TYA  // Length in A
        
        PLY
        PLX
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    StringCompare()
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
    
    
    
    
    Seconds()
    {
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA NEXT3
        LDA ZP.TICK2
        STA NEXT2
        LDA ZP.TICK1
        STA NEXT1
        LDA ZP.TICK0 
        STA NEXT0
        
        LDA # 0xE8 // 1000 = 0x03E8
        STA TOP0
        LDA # 0x03
        STA TOP1
        STZ TOP2
        STZ TOP3
        
        Long.DivMod(); // Seconds = Millis / 1000   
        
        LDA NEXT0
        STA TOPL
        LDA NEXT1
        STA TOPH
        LDA # Types.UInt
        Stacks.PushTop();
    }
}
