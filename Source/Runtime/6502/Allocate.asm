unit Allocate
{
    uses "ZeroPage"
    
    friend Memory, GC;
    

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
       
    allocate()
    {
        // size is in ACC
        // return address in IDX
        LDA FREELISTL
        STA IDYL
        LDA FREELISTH
        STA IDYH

        // size += 2 (space for 'size')
        Utilities.IncACC();
        Utilities.IncACC();

        LDA # 0
        STA maBESTL
        STA maBESTH
        STA maBESTSIZEL
        STA maBESTSIZEH
        STA maBESTNEXTL
        STA maBESTNEXTH
        STA maBESTPREVL
        STA maBESTPREVH

        CMP ACCH
        if (Z) // < 256
        {
            LDA #6
            CMP ACCL
            if (C) // size < 6?
            {
                // minimum size for participation in free list
                STA ACCL
            }
        }

        loop
        {
            // available block search loop    
            LDA #0
            CMP IDYL
            if (Z)
            {
                CMP IDYH
                if (Z)
                {
                    break;
                }
            }
    
            // read current FreeList
    
            // [IDY] < ACC
            LDY #1
            LDA [IDY], Y
            CMP ACCH
            if (Z)
            {
                DEY
                LDA [IDY], Y
                CMP ACCL
            }
            loop
            {
                if (NC) { break; } // [IDY] < ACC
                // [IDY] >= ACC
        
                // (0 == maBESTSIZE) ? 
                LDA #0
                CMP maBESTSIZEL
                if (NZ)
                {
                    CMP maBESTSIZEH
                    if (NZ)
                    { 
                        // [IDY] < maBESTSIZE ?
                        LDY #1
                        LDA [IDY], Y
                        CMP maBESTSIZEH
                        if (Z)
                        {
                            DEY
                            LDA [IDY], Y
                            CMP maBESTSIZEL
                        }
                        if (C)
                        {
                            break;  // [IDY] < maBESTSIZE
                        }
                    }
                }
    
                // first available block
                //  or
                // better than what we've seen so far in terms of fit   
                LDA IDYL
                STA maBESTL
                LDA IDYH
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
            CMP ACCL
            if (Z)
            {
                LDA maBESTSIZEH
                CMP ACCH
                if (Z)
                {       
                    // maBESTSIZE == ACC
                    // can't get better than that
                    break;
                }
            }
    
            // IDY = ReadWord(IDY + 2);
            LDY #2
            LDA [IDY], Y
            PHA
            INY
            LDA [IDY], Y
            STA IDYH
            PLA
            STA IDYL
        } // loop : available block search

        // address = best + 2
        LDA maBESTL
        STA IDXL
        LDA maBESTH
        STA IDXH
        // IDX += 2
        IncIDX();
        IncIDX();

        // maSCRATCH = size+6
        CLC
        LDA ACCL
        ADC #6
        STA maSCRATCHL
        LDA ACCH
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
                LDA ACCL
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
                SBC ACCL
                STA maNEWHOLESIZEL
                LDA maBESTSIZEH
                SBC ACCH
                STA maNEWHOLESIZEH
        
                LDY #0
                LDA maNEWHOLESIZEL
                STA [maNEWHOLE], Y
                INY
                LDA maNEWHOLESIZEH
                STA [maNEWHOLE], Y
    
                LDA #0
                CMP maBESTPREVL
                if (Z)
                {
                    CMP maBESTPREVH
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
            
                        CMP maBESTNEXTL
                        if (Z)
                        {
                            CMP maBESTNEXTH
                            if (Z)
                            {
                                break; // memoryAllocateExit
                            }
                        }
                
                        LDY #4
                        LDA maNEWHOLEL
                        STA [maBESTNEXT], Y
                        INY
                        LDA maNEWHOLEH
                        STA [maBESTNEXT], Y
                        break; // memoryAllocateExit
                    }
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
                LDA #0
                CMP maBESTNEXTL
                if (Z)
                {
                    CMP maBESTNEXTH
                    if (Z)
                    {
                        break; // memoryAllocateExit
                    }
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
            CMP ACCH
            if (Z)
            {
                LDA maBESTSIZEL
                CMP ACCL
            }
    
            if (NC)
            {
                // maBESTSIZE < ACC
                LDA 0x0C BRK // Memory allocation failure
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
    
            LDA # 0
            CMP maBESTPREVL
            if (Z)
            {
                CMP maBESTPREVH
                if (Z)
                {
                    // 0 == bestPrev
            
                    // best was the old FreeList
                    LDA maBESTNEXTL
                    STA FREELISTL
                    LDA maBESTNEXTH
                    STA FREELISTH
            
                    LDA # 0
                    CMP maBESTNEXTL
                    if (Z)
                    {
                        CMP maBESTNEXTH
                        if (Z)
                        {
                            break; // memoryAllocateExit
                        }
                    }
            
                    // WriteWord(freeList+4, 0)// // start of list now so no previous
                    LDA # 0
                    LDY # 4
                    STA [FREELIST], Y
                    INY
                    STA [FREELIST], Y
                    break; // memoryAllocateExit
                }
            }
    
            // 0 != bestPrev
    
            // WriteWord(bestPrev+2, bestNext);
            LDY # 2
            LDA maBESTNEXTL
            STA [maBESTPREV], Y
            INY
            LDA maBESTNEXTH
            STA [maBESTPREV], Y
    
            LDA # 0
            CMP maBESTNEXTL
            if (Z)
            {
                CMP maBESTNEXTH
                if (Z)
                {
                    break; // memoryAllocateExit
                }
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


        // address in IDX

        // zero initialize

        // size -= 2 (space for 'size')
        DecACCx2();
        
        CLC
        LDA IDXL
        ADC ACCL
        STA maSCRATCHL
        LDA IDXH
        ADC ACCH
        STA maSCRATCHH

        loop
        {
            LDA maSCRATCHH
            CMP IDXH
            if (Z)
            {
                LDA maSCRATCHL
                CMP IDXL
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
            LDA # 0
            TAY // 0 -> Y
            STA [maSCRATCH], Y
        }
    }
}
