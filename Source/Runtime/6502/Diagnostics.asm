unit Diagnostics
{
    uses "ZeroPage"
    uses "Array"
    uses "String"
    uses "List"
    
    friend GC, Free, Allocate, Variant, String, Array, List, Instruction, SysCall, IntMath, Type;
    
    die()
    {
#ifdef TIGGERC   
        TSX PHX     // save SP to Y
#endif

        PHA
        
#if !defined(TIGGERC) && !defined(ROM_8K)
        registers();
#endif
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
#ifndef TIGGERC        
        LDA ZP.PCH
        Serial.HexOut();
        LDA ZP.PCL
        Serial.HexOut();
#endif
        LDA # ' '
        Serial.WriteChar();
        
        PLA
        Serial.HexOut();
        
#ifdef TIGGERC   
        PLY             // SP to Y
        
        LDX # 0xFF      // Start at the top of the stack (0x01FF)
        
        loop
        {
            LDA # 0x0A
            Serial.WriteChar();
            TXA
            Serial.HexOut();
            LDA # ' '
            Serial.WriteChar();
            LDA 0x0100, X
            Serial.HexOut();
            DEX
            INY
            if (Z) { break; }
        }
#endif
        loop { }
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
        
        LDY # 0xFF
        LDA [ZP.IDX], Y
        if (NZ)
        {
            // trivial case: [ZP.IDX], 0xFF is non-zero
            LDY # 0
            loop
            {
#if defined(ZEROPAGE_IO)
                // avoid ever reading the ACIA status register on the zero page
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
#endif                
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
            
#if defined(ZEROPAGE_IO)
            // avoid ever reading the ACIA status register on the zero page
            LDA ZP.IDXH
            if (Z) // Zero Page?
            {
                CPY # (0xFF - ZP.ACIASTATUS)
                if (Z)
                {
                    continue;
                }
            }
#endif
            
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
#if defined(ZEROPAGE_IO)
            // avoid ever reading the ACIA status or data register on the zero page
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
#endif
            
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
    
#ifndef TIGGERC    
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
        LDA # 'T'
        Serial.WriteChar();
        LDA TOPH
        Serial.HexOut();
        LDA TOPL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'N'
        Serial.WriteChar();
        LDA NEXTH
        Serial.HexOut();
        LDA NEXTL
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
        LDA # 'L'
        Serial.WriteChar();
        LDA # 'P'
        Serial.WriteChar();LDA LPREVIOUSH
        Serial.HexOut();
        LDA LPREVIOUSL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'L'
        Serial.WriteChar();
        LDA # 'C'
        Serial.WriteChar();
        LDA LCURRENTH
        Serial.HexOut();
        LDA LCURRENTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'L'
        Serial.WriteChar();
        LDA # 'N'
        Serial.WriteChar();
        LDA LNEXTH
        Serial.HexOut();
        LDA LNEXTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'F'
        Serial.WriteChar();
        LDA # 'I'
        Serial.WriteChar();
        LDA FITEMH
        Serial.HexOut();
        LDA FITEML
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'L'
        Serial.WriteChar();
        LDA # 'C'
        Serial.WriteChar();
        LDA LCOUNTH
        Serial.HexOut();
        LDA LCOUNTL
        Serial.HexOut();
        
        LDA # ' '
        Serial.WriteChar();
        LDA # 'L'
        Serial.WriteChar();
        LDA # 'T'
        Serial.WriteChar();
        LDA LTYPE
        Serial.HexOut();
        
        PLA
    }
#endif
    HeapDump()
    {
        PHA
        PHX
        PHY
        
        LDA IDYL
        PHA
        LDA IDYH
        PHA
        
        LDA IDXL
        PHA
        LDA IDXH
        PHA
        
        LDA FSIZEL
        PHA
        LDA FSIZEH
        PHA
        
        LDA FITEML
        PHA
        LDA FITEMH
        PHA
        
        // uses FSIZE, IDX, IDY
        
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
                LDA # 0x0A
                Serial.WriteChar();
                // free address
                LDA IDXH
                STA ACCH
                LDA IDXL
                STA ACCL
                IncACC();
                IncACC();
                
                LDA ACCH
                Serial.HexOut();
                LDA ACCL
                Serial.HexOut();
                
                LDA # '['
                Serial.WriteChar();
                // size
                LDY # 1
                LDA [IDX], Y
                Serial.HexOut();
                DEY
                LDA [IDX], Y
                Serial.HexOut();
                
                validateHeapIDX();
                
                LDA # ':'
                Serial.WriteChar();
                
                // type
                LDY # 2
                LDA [IDX], Y
                TAX
                
                switch (X)
                {
                    case Types.String:
                    {
                        Serial.HexOut(); // type
                        String.dump();
                    }
                    case Types.List:
                    {
                        Serial.HexOut(); // type
                        List.dump();    
                    }
                    case Types.Array:
                    {
                        Serial.HexOut(); // type
                        Array.dump();    
                    }
                    default:
                    {
                        LDY # 1
                        LDA [IDX], Y
                        if (Z)
                        {
                            DEY
                            LDA [IDX], Y
                            CMP # 8
                            if (Z)
                            {
                                LDA # ' '
                                Serial.WriteChar();
                                
                                LDY # 3
                                LDA [IDX], Y
                                Serial.HexOut();
                                DEY    
                                LDA [IDX], Y
                                Serial.HexOut();
                                
                                
                                LDA IDXL
                                PHA
                                LDA IDXH
                                PHA
                                
                                SEC
                                LDA [IDX], Y
                                SBC # 2
                                TAX
                                INY
                                LDA [IDX], Y
                                SBC # 0
                                STA IDXH
                                STX IDXL
                                Diagnostics.validateHeapIDX();
                                
                                PLA
                                STA IDXH
                                PLA
                                STA IDXL
                                
                                
                                
                                
                                LDA # ' '
                                Serial.WriteChar();
                                
                                LDY # 5
                                LDA [IDX], Y
                                Serial.HexOut();
                                DEY    
                                LDA [IDX], Y
                                Serial.HexOut();
                            }
                        }
                    }
                }
                
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
        
        
        PLA
        STA FITEMH
        PLA
        STA FITEML
        
        PLA
        STA FSIZEH
        PLA
        STA FSIZEL
        
        PLA
        STA IDXH
        PLA
        STA IDXL
        
        PLA
        STA IDYH
        PLA
        STA IDYL
        
        PLY
        PLX
        PLA
    }
    
    validateHeapIDX()
    {
        isHeapBlock();
        if (Z)
        {
            LDX # 5
            loop
            {
                LDA # '!'
                Serial.WriteChar();
                DEX
                if (Z) { break; }
            }
        }
    }
    
    isHeapBlock()
    {
        // if IDX is a heap block, return NZ
        //   otherwise return Z
        // assume FSIZEL is heap end
        
        LDY # 0
        LDA # 0
        STA IDYL
        INY
        LDA HEAPSTART
        STA IDYH
        
        loop
        {
            LDA IDYH 
            CMP FSIZEH
            if (Z) 
            { 
                LDA IDYL
                CMP FSIZEL
                if (Z)
                {
                    // end of heap
                    break; 
                }
            }
            LDA IDYH
            CMP IDXH
            if (Z)
            {
                LDA IDYL
                CMP IDXL
                if (Z)
                {
                    isFreeBlock();
                    EOR # 1
                    return;
                }
            }
            LDY # 0
            CLC
            LDA IDYL
            ADC [IDY], Y
            TAX
            INY
            LDA IDYH
            ADC [IDY], Y
            STA IDYH
            STX IDYL
        }
        // not a heap block
        LDA # 0
        return;
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
