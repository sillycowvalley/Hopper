unit Diagnostics
{
    uses "6502/ZeroPage"
    
    friend GC, Free, Variant, String, Array, List;
    
    die()
    {
#ifdef CHECKED
        PHA
        
        registers();
        
        LDA # 0x0A
        Serial.WriteChar();
        LDA # 'D'
        Serial.WriteChar();
        LDA # 'I'
        Serial.WriteChar();
        LDA # 'E'
        Serial.WriteChar();
        LDA # ':'
        Serial.WriteChar();
        
        LDA ZP.PCH
        Serial.HexOut();
        LDA ZP.PCL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        
        PLA
        Serial.HexOut();
        loop { }
#endif        
        INC ZP.SerialBreakFlag // hardware <ctrl><C>
        BRK
    }
    Die()
    {
        Stacks.PopA();         // user error from Hopper in A
        die();   
    }
    
    // used by 'F' command
    PageMemory()
    {
        // page # : A -> IDX
        STA ZP.IDXH
#ifdef CPU_65C02S        
        STZ ZP.IDXL
#else
        LDA # 0
        STA ZP.IDXL
#endif
        
        // find the first non-zero from the back
        
        LDY #0xFF
        LDA [ZP.IDX], Y
        if (NZ)
        {
            // trivial case: [ZP.IDX], 0xFF is non-zero
            LDY #0
            loop
            {
                // avoid ever reading the ACIA status register
                LDA ZP.IDXH
                if (Z) // Zero Page?
                {
                    CPY # ZP.ACIASTATUS
                    if (Z)
                    {
                        LDA # '.'
                        Serial.WriteChar();
                        INY
                        continue;
                    }
                }
                
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
                //CPY #0x00 // after 0xFF
                if (Z) { break; }
            }
            return;
        }
        
        loop
        {
            DEY
            
            // avoid ever reading the ACIA status register
            LDA ZP.IDXH
            if (Z) // Zero Page?
            {
                CPY # (0xFF - ZP.ACIASTATUS)
                if (Z)
                {
                    continue;
                }
            }
            
            
            LDA [ZP.IDX], Y
            if (NZ) { break; } // [ZP.IDX], Y is first non-zero from the end
            CPY # 0
            if (Z) 
            { 
                // entire page is zeroes
                LDA # '+'
                Serial.WriteChar();
                return;
            }  
        }
        
        TYA 
        LDY # 0
        TAX // Y -> X
        loop
        {
            // avoid ever reading the ACIA status or data register
            LDA ZP.IDXH
            if (Z) // Zero Page?
            {
                CPY # ZP.ACIASTATUS
                if (Z)
                {
                    LDA # '.'
                    Serial.WriteChar();
                    INY
                    continue;
                }
            }
            
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
    
    registers()
    {
        PHA
        
        LDA # 0x0A
        Serial.WriteChar();
        LDA # 'R'
        Serial.WriteChar();
        
        LDA # ' '
        Serial.WriteChar();
        LDA PCH
        Serial.HexOut();
        LDA PCL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA SP
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'X'
        Serial.WriteChar();
        LDA IDXH
        Serial.HexOut();
        LDA IDXL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'Y'
        Serial.WriteChar();
        LDA IDYH
        Serial.HexOut();
        LDA IDYL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'P'
        Serial.WriteChar();
        LDA LPREVIOUSH
        Serial.HexOut();
        LDA LPREVIOUSL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'C'
        Serial.WriteChar();
        LDA LCURRENTH
        Serial.HexOut();
        LDA LCURRENTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'N'
        Serial.WriteChar();
        LDA LNEXTH
        Serial.HexOut();
        LDA LNEXTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'I'
        Serial.WriteChar();
        LDA FITEMH
        Serial.HexOut();
        LDA FITEML
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'C'
        Serial.WriteChar();
        LDA LCOUNTH
        Serial.HexOut();
        LDA LCOUNTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'T'
        Serial.WriteChar();
        LDA LTYPE
        Serial.HexOut();
        
        PLA
    }
    HeapDump()
    {
        Diagnostics.registers();
        
        // HEAPSTART
        // HEAPSIZE
        
        LDA # 0x0A
        Serial.WriteChar();
        LDA # 'F'
        Serial.WriteChar();
        LDA # 'r'
        Serial.WriteChar();
        LDA # 'e'
        Serial.WriteChar();
        LDA # 'e'
        Serial.WriteChar();
        LDA # ':'
        Serial.WriteChar();
        
        // End of heap:
        CLC
        LDA HEAPSTART
        ADC HEAPSIZE
        STA FSIZEH
        LDA # 0
        STA FSIZEL
        
        LDY # 0
        LDA FREELISTL
        STA IDXL
        INY
        LDA FREELISTH
        STA IDXH
        
        loop
        {
            LDA IDXH 
            if (Z) 
            { 
                LDA IDXL
                if (Z)
                {
                    // next pointer = 0
                    break; 
                }
            }
            
            LDA # '>'
            Serial.WriteChar();
            // free address
            LDA IDXH
            Serial.HexOut();
            LDA IDXL
            Serial.HexOut();
            LDA # '['
            Serial.WriteChar();
            // free size
            LDY # 1
            LDA [IDX], Y
            Serial.HexOut();
            DEY
            LDA [IDX], Y
            Serial.HexOut();
            LDA # ']'
            Serial.WriteChar();
            
            // load next pointer
            LDY # 2
            LDA [IDX], Y
            TAX
            INY
            LDA [IDX], Y
            STA IDXH
            STX IDXL
        }
        
        LDA # 0x0A
        Serial.WriteChar();
        LDA # 'B'
        Serial.WriteChar();
        LDA # 'l'
        Serial.WriteChar();
        LDA # 'k'
        Serial.WriteChar();
        LDA # 's'
        Serial.WriteChar();
        LDA # ':'
        Serial.WriteChar();
        
        LDY # 0
        LDA # 0
        STA IDXL
        INY
        LDA HEAPSTART
        STA IDXH
        
        loop
        {
            LDA IDXH 
            CMP FSIZEH
            if (Z) 
            { 
                LDA IDXL
                CMP FSIZEL
                if (Z)
                {
                    // end of heap
                    break; 
                }
            }
            isFreeBlock();
            if (Z)
            {
                // not free block
                LDA # '>'
                Serial.WriteChar();
                // free address
                LDA IDXH
                Serial.HexOut();
                LDA IDXL
                Serial.HexOut();
                LDA # '['
                Serial.WriteChar();
                // free size
                LDY # 1
                LDA [IDX], Y
                Serial.HexOut();
                DEY
                LDA [IDX], Y
                Serial.HexOut();
                LDA # ':'
                Serial.WriteChar();
                // type
                LDY # 2
                LDA [IDX], Y
                Serial.HexOut();
                LDA # ']'
                Serial.WriteChar();
            }
            
            LDY # 0
            CLC
            LDA IDXL
            ADC [IDX], Y
            TAX
            INY
            LDA IDXH
            ADC [IDX], Y
            STA IDXH
            STX IDXL
        }
        
        
        LDA # 0x0A
        Serial.WriteChar();
    }
    isFreeBlock()
    {
        // if IDX is on freelist, return NZ
        //   otherwise return Z
        // assume FSIZEL is heap end
        
        LDY # 0
        LDA FREELISTL
        STA IDYL
        INY
        LDA FREELISTH
        STA IDYH
        
        loop
        {
            LDA IDYH 
            CMP IDXH
            if (Z) 
            { 
                LDA IDYL
                CMP IDXL
                if (Z)
                {
                    // on free list
                    LDA # 1
                    return;
                }
            }
            
            LDA IDYH 
            if (Z) 
            { 
                LDA IDYL
                if (Z)
                {
                    // next pointer = 0
                    break; 
                }
            }
            
            // load next pointer
            LDY # 2
            LDA [IDY], Y
            TAX
            INY
            LDA [IDY], Y
            STA IDYH
            STX IDYL
        }
        // not on free list
        LDA # 0
        return;
    }
}
