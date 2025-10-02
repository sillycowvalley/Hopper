unit Allocate
{

    friend Memory;
    

    const byte maBEST  = M0;
    const byte maBESTL = M0;
    const byte maBESTH = M1;

    const byte maBESTSIZE = M2;
    const byte maBESTSIZEL = M2;
    const byte maBESTSIZEH = M3;

    const byte maBESTNEXT = M4;
    const byte maBESTNEXTL = M4;
    const byte maBESTNEXTH = M5;

    const byte maBESTPREV = M6;
    const byte maBESTPREVL = M6;
    const byte maBESTPREVH = M7;

    const byte maSCRATCH = M8;
    const byte maSCRATCHL = M8;
    const byte maSCRATCHH = M9;

    const byte maNEWHOLE = M10;
    const byte maNEWHOLEL = M10;
    const byte maNEWHOLEH = M11;

    const byte maNEWHOLESIZE = M12;
    const byte maNEWHOLESIZEL = M12;
    const byte maNEWHOLESIZEH = M13;
    
    const byte maIndex = M14;
    const byte maIndexL = M14;
    const byte maIndexH = M15;
       
    // Munts: A, Y, ZP.FREELIST, ZP.ACCL(size), -> ZP.IDX
    Allocate()
    {
        // size is in ACC
        // return address in IDX
        LDA ZP.FREELISTL
        STA maIndexL
        LDA ZP.FREELISTH
        STA maIndexH

        // size += 2 (space for 'size')
        Shared.IncACC();
        Shared.IncACC();
        
        // round size up to the next 8 byte boundary
        LDA ZP.ACCL    // Load the low byte
        CLC            // Clear carry
        ADC # 0x07     // Add 7
        STA ZP.ACCL    // Store back the low byte
        
        LDA ZP.ACCH    // Load the high byte
        ADC # 0        // Add the carry to the high byte
        STA ZP.ACCH    // Store back the high byte

        LDA ZP.ACCL    // Load the adjusted low byte
        AND # 0xF8     // Mask out the lower 3 bits
        STA ZP.ACCL    // Store the masked low byte back

        STZ maBESTL
        STZ maBESTH
        STZ maBESTSIZEL
        STZ maBESTSIZEH
        STZ maBESTNEXTL
        STZ maBESTNEXTH
        STZ maBESTPREVL
        STZ maBESTPREVH

        LDA ACCH
        if (Z) // < 256
        {
            LDA #6
            CMP ZP.ACCL
            if (C) // size < 6?
            {
                // minimum size for participation in free list
                STA ZP.ACCL
            }
        }

        loop
        {
            // available block search loop    
            LDA maIndexL
            ORA maIndexH
            if (Z)
            {
                break;
            }
    
            // read current FreeList
    
            // [maIndex] < ACC
            LDY #1
            LDA [maIndex], Y
            CMP ZP.ACCH
            if (Z)
            {
                DEY
                LDA [maIndex], Y
                CMP ZP.ACCL
            }
            loop
            {
                if (NC) { break; } // [maIndex] < ACC
                // [maIndex] >= ACC
        
                // (0 != maBESTSIZE) ? 
                LDA maBESTSIZEL
                ORA maBESTSIZEH
                if (NZ)
                {
                    // [maIndex] < maBESTSIZE ?
                    LDY #1
                    LDA [maIndex], Y
                    CMP maBESTSIZEH
                    if (Z)
                    {
                        DEY
                        LDA [maIndex], Y
                        CMP maBESTSIZEL
                    }
                    if (C)
                    {
                        break;  // [maIndex] < maBESTSIZE
                    }
                }
    
                // first available block
                //  or
                // better than what we've seen so far in terms of fit   
                LDA maIndexL
                STA maBESTL
                LDA maIndexH
                STA maBESTH
        
                // bestSize = ReadWord(best);
                LDY #0
                LDA [maBEST], Y
                STA maBESTSIZEL
                INY
                LDA [maBEST], Y
                STA maBESTSIZEH
                // bestNext = ReadWord(best + 2);
                INY
                LDA [maBEST], Y
                STA maBESTNEXTL
                INY
                LDA [maBEST], Y
                STA maBESTNEXTH
                // bestPrev = ReadWord(best + 4);
                INY
                LDA [maBEST], Y
                STA maBESTPREVL
                INY
                LDA [maBEST], Y
                STA maBESTPREVH
                break;
            } // loop
    
            LDA maBESTSIZEL
            CMP ZP.ACCL
            if (Z)
            {
                LDA maBESTSIZEH
                CMP ZP.ACCH
                if (Z)
                {       
                    // maBESTSIZE == ACC
                    // can't get better than that
                    break;
                }
            }
    
            // maIndex = ReadWord(maIndex + 2);
            LDY #2
            LDA [maIndex], Y
            PHA
            INY
            LDA [maIndex], Y
            STA maIndexH
            PLA
            STA maIndexL
        } // loop : available block search

        // address = best + 2
        LDA maBESTL
        STA ZP.IDXL
        LDA maBESTH
        STA ZP.IDXH
        // IDX += 2
        IncIDX();
        IncIDX();

        // maSCRATCH = size+6
        CLC
        LDA ZP.ACCL
        ADC #6
        STA maSCRATCHL
        LDA ZP.ACCH
        ADC #0
        STA maSCRATCHH

        // maBESTSIZE < maSCRATCH?
        LDA maBESTSIZEH
        CMP maSCRATCHH
        if (Z)
        {
            LDA maBESTSIZEH
            CMP maSCRATCHL
        }
        
        loop
        {
            if (C)  // maBESTSIZE >= maSCRATCH
            {
                // (bestSize >= size + 6)
        
                // so we now how much to free later
                // block size includes the size of the size field itself
                LDY #0
                LDA ZP.ACCL
                STA [maBEST], Y
                INY
                LDA ACCH
                STA [maBEST], Y
        
                // enough extra to make a new freelist record from the balance
                CLC
                LDA maBESTL
                ADC ACCL
                STA maNEWHOLEL
                LDA maBESTH
                ADC ACCH
                STA maNEWHOLEH
        
                SEC
                LDA maBESTSIZEL
                SBC ZP.ACCL
                STA maNEWHOLESIZEL
                LDA maBESTSIZEH
                SBC ZP.ACCH
                STA maNEWHOLESIZEH
     
                LDY #0
                LDA maNEWHOLESIZEL
                STA [maNEWHOLE], Y
                INY
                LDA maNEWHOLESIZEH
                STA [maNEWHOLE], Y
    
                LDA maBESTPREVL
                ORA maBESTPREVH
                if (Z)
                {
                    // 0 == bestPrev
                    LDA maNEWHOLEL
                    STA FREELISTL
                    LDA maNEWHOLEH
                    STA FREELISTH
                    INY
                    LDA maBESTNEXTL
                    STA [maNEWHOLE], Y
                    INY
                    LDA maBESTNEXTH
                    STA [maNEWHOLE], Y
                    INY
                    LDA #0
                    STA [maNEWHOLE], Y
                    INY
                    STA [maNEWHOLE], Y
        
                    LDA maBESTNEXTL
                    ORA maBESTNEXTH
                    if (Z)
                    {
                        break; // memoryAllocateExit
                    }
            
                    LDY #4
                    LDA maNEWHOLEL
                    STA [maBESTNEXT], Y
                    INY
                    LDA maNEWHOLEH
                    STA [maBESTNEXT], Y
                    break; // memoryAllocateExit
                }
        
                LDY #2
                LDA maBESTNEXTL
                STA [maNEWHOLE], Y
                INY
                LDA maBESTNEXTH
                STA [maNEWHOLE], Y
                INY
                LDA maBESTPREVL
                STA [maNEWHOLE], Y
                INY
                LDA maBESTPREVH
                STA [maNEWHOLE], Y
                LDY #2
                LDA maNEWHOLEL
                STA [maBESTPREV], Y
                INY
                LDA maNEWHOLEH
                STA [maBESTPREV], Y
                
                LDA maBESTNEXTL
                ORA maBESTNEXTH
                if (Z)
                {
                    break; // memoryAllocateExit
                }
        
                LDY #4
                LDA maNEWHOLEL
                STA [maBESTNEXT], Y
                INY
                LDA maNEWHOLEH
                STA [maBESTNEXT], Y
                break; // memoryAllocateExit
            }
            // maBESTSIZE < maSCRATCH

            // maBESTSIZE < ACC?
            LDA maBESTSIZEH
            CMP ZP.ACCH
            if (Z)
            {
                LDA maBESTSIZEL
                CMP ZP.ACCL
            }
    
            if (NC)
            {
                // maBESTSIZE < ACC
                LDA #0x03 Debug.Crash(); // Memory allocation failure
            }
    
            // (bestSize >= size)
    
            // just link the freelist past the new hole
            // and give allocate the entire slot (more than was asked)
    
            // so we now how much to free later
            // block size includes the size of the size field itself
            // WriteWord(best, bestSize);
            
            LDY #0
            LDA maBESTSIZEL
            STA [maBEST], Y
            INY
            LDA maBESTSIZEH
            STA [maBEST], Y
    
            LDA maBESTPREVL
            ORA maBESTPREVH
            if (Z)
            {
                // 0 == bestPrev
        
                // best was the old FreeList
                LDA maBESTNEXTL
                STA FREELISTL
                LDA maBESTNEXTH
                STA FREELISTH
        
                LDA maBESTNEXTL
                ORA maBESTNEXTH
                if (Z)
                {
                    break; // memoryAllocateExit
                }
        
                // WriteWord(freeList+4, 0)// // start of list now so no previous
                LDA # 0
                LDY # 4
                STA [ZP.FREELIST], Y
                INY
                STA [ZP.FREELIST], Y
                break; // memoryAllocateExit
            }
    
            // 0 != bestPrev
    
            // WriteWord(bestPrev+2, bestNext);
            LDY # 2
            LDA maBESTNEXTL
            STA [maBESTPREV], Y
            INY
            LDA maBESTNEXTH
            STA [maBESTPREV], Y
    
            ORA maBESTNEXTL
            if (Z)
            {
                break; // memoryAllocateExit
            }
    
            // WriteWord(bestNext+4, bestPrev);
            LDY #4
            LDA maBESTPREVL
            STA [maBESTNEXT], Y
            INY
            LDA maBESTPREVH
            STA [maBESTNEXT], Y
            break; // memoryAllocateExit
        } // memoryAllocateExit


        // address in ZP.IDX

        // zero initialize

        // size -= 2 (space for 'size')
        DecACCx2();
        
        CLC
        LDA ZP.IDXL
        ADC ZP.ACCL
        STA maSCRATCHL
        LDA ZP.IDXH
        ADC ZP.ACCH
        STA maSCRATCHH

        LDY #0
        loop
        {
            LDA maSCRATCHH
            CMP ZP.IDXH
            if (Z)
            {
                LDA maSCRATCHL
                CMP ZP.IDXL
                if (Z)
                {
                    break; // done
                }
            }

            // maSCRATCH--  
            LDA maSCRATCHL
            if (Z)
            {
                DEC maSCRATCHH
            }
            DEC maSCRATCHL
    
            // clear
            TYA
            STA [maSCRATCH], Y
        }
    }
}
