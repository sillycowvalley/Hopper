unit Diagnostic
{
    // used by 'F' command
    PageMemory()
    {
        // page # : A -> IDX
        STA ZP.IDXH
        LDA # 0
        STA ZP.IDXL
        
        // find the first non-zero from the back
        
        LDY #0xFF
        LDA [ZP.IDX], Y
        if (NZ)
        {
            // trivial case: [ZP.IDX], 0xFF is non-zero
            LDY #0
            loop
            {
                LDA [ZP.IDX], Y
                if (Z)
                {
                    LDA # '.'
                    Serial.WriteChar();
                }
                else
                {
                    Serial.HexOut();
                }
                INY
                CPY #0x00 // after 0xFF
                if (Z) { break; }
            }
            return;
        }
        
        loop
        {
            DEY
            LDA [ZP.IDX], Y
            if (NZ) { break; } // [ZP.IDX], Y is first non-zero from the end
            CPY #0
            if (Z) 
            { 
                // entire page is zeroes
                LDA # '+'
                Serial.WriteChar();
                return;
            }  
        }
        
        TYA 
        LDY #0
        TAX // Y -> X
        loop
        {
            LDA [ZP.IDX], Y
            if (Z)
            {
                LDA # '.'
                Serial.WriteChar();
            }
            else
            {
                Serial.HexOut();
            }
            CPX #0
            if (Z) // X == 0?
            {
                LDA # '+'
                Serial.WriteChar();
                break;
            }
            
            INY
            DEX
        }
    }
    /*
    PageMemory()
    {
        // page # is in A
        STA ZP.IDXH
        STZ ZP.IDXL
        
        LDY #0
        loop
        {
            LDA [ZP.IDX], Y
            if (Z)
            {
                LDA # '.'
                Serial.WriteChar();
            }
            else
            {
                Serial.HexOut();
            }
            INY
            CPY #0x00 // ; after $FF
            if (Z) { break; }
        }
    }
    */
}
