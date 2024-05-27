unit Free
{
    uses "ZeroPage"
    
    friend Memory, GC, String, List;
        
    const byte mfCURRENT = IDYL;
    const byte mfCURRENTL = IDYL;
    const byte mfCURRENTH = IDYH;
      
    const byte mfPREVIOUS  = M0;
    const byte mfPREVIOUSL = M0;
    const byte mfPREVIOUSH = M1;

    const byte mfCURRENTSIZE = M2;
    const byte mfCURRENTSIZEL = M2;
    const byte mfCURRENTSIZEH = M3;

    const byte mfCURRENTNEXT = M4;
    const byte mfCURRENTNEXTL = M4;
    const byte mfCURRENTNEXTH = M5;

    const byte mfCURRENTPREV = M6;
    const byte mfCURRENTPREVL = M6;
    const byte mfCURRENTPREVH = M7;

    const byte mfFREESLOT = M8;
    const byte mfFREESLOTL = M8;
    const byte mfFREESLOTH = M9;

    // the next 3 GAP variables share the same slot
    const byte mfGAPFRONT = M10;
    const byte mfGAPFRONTL = M10;
    const byte mfGAPFRONTH = M11;
    const byte mfGAPBACK = M10;
    const byte mfGAPBACKL = M10;
    const byte mfGAPBACKH = M11;
    const byte mfGAPNEXT = M10;
    const byte mfGAPNEXTL = M10;
    const byte mfGAPNEXTH = M11;

    // same as mfCURRENT which is not used again after mfNEXTNEXT is needed
    const byte mfNEXTNEXT = IDYL;
    const byte mfNEXTNEXTL = IDYL;
    const byte mfNEXTNEXTH = IDYH;

    // these two size variables share the same slot:
    const byte mfPREVSIZE = M12;
    const byte mfPREVSIZEL = M12;
    const byte mfPREVSIZEH = M13;
    const byte mfNEXTSIZE = M12;
    const byte mfNEXTSIZEL = M12;
    const byte mfNEXTSIZEH = M13;

    const byte mfSIZE = M14;
    const byte mfSIZEL = M14;
    const byte mfSIZEH = M15;

    const byte mfOFFSET = M15; // used in releaseSP, no need to preserve
           
    free()
    {
        // address is in IDX
        // uses mfCURRENT

#ifdef CHECKED
        LDA IDXL
        if (Z)
        {
            LDA IDXH
            if (Z)
            {
                // this is a bug (to try to free nullptr)
                LDA 0x0B BRK
            }
        }
#endif
        loop
        {
            // mfSIZE  = ReadWord(IDX - 2)
            
            // IDY = IDX - 2
            SEC
            LDA IDXL
            SBC # 2
            STA IDYL
            LDA IDXH
            SBC # 0
            STA IDYH
            
            LDY # 0
            LDA [IDY], Y
            STA mfSIZEL
            INY
            LDA [IDY], Y
            STA mfSIZEH

            // mfCURRENT = FreeList
            LDA FREELISTL
            STA mfCURRENTL
            LDA FREELISTH
            STA mfCURRENTH

#ifdef CPU_65C02S
            STZ mfPREVIOUSL  
            STZ mfPREVIOUSH
#else
            LDA # 0
            STA mfPREVIOUSL  
            STA mfPREVIOUSH
#endif

            loop 
            {
                LDA mfCURRENTL
                if (Z)
                {
                    LDA mfCURRENTH
                    if (Z) { break; } // 0 == current
                }
        
                // walk current [mfCURRENT] FreeList till next record beyond address (IDX)
                // mfCURRENT <= IDX
                LDA mfCURRENTH
                CMP IDXH
                if (Z)
                {
                    LDA mfCURRENTL
                    CMP IDXL
                }
        
                // http://6502.org/tutorials/compare_instructions.html
                if (NZ) // mfCURRENT != IDX (not >)
                {
                    if (C)
                    {
                        break; // (mfCURRENT > IDX)
                    }
                }

                LDA mfCURRENTL
                STA mfPREVIOUSL
                LDA mfCURRENTH
                STA mfPREVIOUSH

                // currentNext = ReadWord(mfCURRENT + 2);
                LDY # 2
                LDA [mfCURRENT], Y
                STA mfCURRENTNEXTL
                INY
                LDA [mfCURRENT], Y
                STA mfCURRENTNEXTH
                // mfCURRENT = currentNext;
                STA mfCURRENTH
                LDA mfCURRENTNEXTL
                STA mfCURRENTL

            } // loop findBeyondAddress
    
            // currentPrev = previous;
            LDA mfPREVIOUSL
            STA mfCURRENTPREVL
            LDA mfPREVIOUSH
            STA mfCURRENTPREVH
#ifdef CPU_65C02S
            STZ mfCURRENTSIZEL
            STZ mfCURRENTSIZEH
            STZ mfCURRENTNEXTL
            STZ mfCURRENTNEXTH
#else            
            LDA #0
            STA mfCURRENTSIZEL
            STA mfCURRENTSIZEH
            STA mfCURRENTNEXTL
            STA mfCURRENTNEXTH
#endif
            loop
            {
                LDA mfCURRENTL
                if (Z)
                {
                    LDA mfCURRENTH
                    if (Z)
                    {
                        break; // 0 == current
                    }
                }
                // 0 != current
                // currentSize = ReadWord[mfCURRENT]
                LDY # 0
                LDA [mfCURRENT], Y
                STA mfCURRENTSIZEL
                INY
                LDA [mfCURRENT], Y
                STA mfCURRENTSIZEH
                // currentNext = ReadWord(mfCURRENT + 2)
                INY
                LDA [mfCURRENT], Y
                STA mfCURRENTNEXTL
                INY
                LDA [mfCURRENT], Y
                STA mfCURRENTNEXTH
                // currentPrev = ReadWord(mfCURRENT + 4); // already set above
                break;
            } // loop
    
            // freeSlot = IDX-2
            LDA IDXL
            STA mfFREESLOTL
            LDA IDXH
            STA mfFREESLOTH

            SEC
            LDA mfFREESLOTL
            SBC # 2
            STA mfFREESLOTL
            LDA mfFREESLOTH
            SBC # 0
            STA mfFREESLOTH

            LDA mfCURRENTPREVL
            if (Z)
            {
                LDA mfCURRENTPREVH
                if (Z)
                {
                    //////////////////////////////
                    // 0 == currentPrev
                    //////////////////////////////

                    // current [mfCURRENT] is front of FreeList, insert in front of it
                    // WriteWord(freeSlot+2, mfCURRENT)
                    LDY #2
                    LDA mfCURRENTL
                    STA [mfFREESLOT], Y
                    INY
                    LDA mfCURRENTH
                    STA [mfFREESLOT], Y
                    // WriteWord(freeSlot+4, 0);
                    INY
                    LDA #0
                    STA [mfFREESLOT], Y
                    INY
                    STA [mfFREESLOT], Y
                    // WriteWord(mfCURRENT+ 4, freeSlot);
                    LDY #4
                    LDA mfFREESLOTL
                    STA [mfCURRENT], Y
                    INY
                    LDA mfFREESLOTH
                    STA [mfCURRENT], Y

                    // gapFront = freeList - (freeSlot+size);
                    CLC
                    LDA mfFREESLOTL
                    ADC mfSIZEL
                    STA mfGAPFRONTL
                    LDA mfFREESLOTH
                    ADC mfSIZEH
                    STA mfGAPFRONTH
                    SEC
                    LDA FREELISTL
                    SBC mfGAPFRONTL
                    STA mfGAPFRONTL
                    LDA FREELISTH
                    SBC mfGAPFRONTH
                    STA mfGAPFRONTH

                    LDA mfGAPFRONTL
                    if (Z)
                    {
                        LDA mfGAPFRONTH
                        if (Z)
                        {

                            // GAPFRONT == 0

                            // nextSize = ReadWord(freeList)
                            LDY #0
                            LDA [FREELIST], Y
                            STA mfNEXTSIZEL
                            INY
                            LDA [FREELIST], Y
                            STA mfNEXTSIZEH
                            // nextNext = ReadWord(freeList+2);
                            INY
                            LDA [FREELIST], Y
                            STA mfNEXTNEXTL
                            INY
                            LDA [FREELIST], Y
                            STA mfNEXTNEXTH

                            // no gap between freeSlot and freeList so absorb it into freeSlot block
                            CLC
                            LDA mfNEXTSIZEL
                            ADC mfSIZEL
                            STA mfSIZEL
                            LDA mfNEXTSIZEH
                            ADC mfSIZEH
                            STA mfSIZEH

                            // WriteWord(freeSlot, size+nextSize);
                            LDY #0
                            LDA mfSIZEL
                            STA [mfFREESLOT], Y
                            INY
                            LDA mfSIZEH
                            STA [mfFREESLOT], Y

                            // WriteWord(freeSlot+2, nextNext);
                            INY
                            LDA mfNEXTNEXTL
                            STA [mfFREESLOT], Y
                            INY
                            LDA mfNEXTNEXTH
                            STA [mfFREESLOT], Y
                            
                            loop
                            {
                                LDA mfNEXTNEXTL
                                if (Z)
                                {
                                    LDA mfNEXTNEXTH
                                    if (Z) { break; }
                                }

                                // mfNEXTNEXT != 0

                                // WriteWord(nextNext+4, freeSlot)
                                LDY #4
                                LDA mfFREESLOTL
                                STA [mfNEXTNEXT], Y
                                INY
                                LDA mfFREESLOTH
                                STA [mfNEXTNEXT], Y
                                break;
                            } 
                        }
                    }
            
                    LDA mfFREESLOTL
                    STA FREELISTL
                    LDA mfFREESLOTH
                    STA FREELISTH

                    break; // memoryFreeExit
                }
            }

            LDA mfCURRENTL
            if (Z)
            {
                LDA mfCURRENTH
                if (Z)
                {
                    //////////////////////////////
                    // 0 == current
                    //////////////////////////////

                    // currentPrev != 0 means we are at the end of the FreeList
                    // append to end of freelist (after currentPrev)

                    // WriteWord(currentPrev+2, freeSlot);
                    LDY #2
                    LDA mfFREESLOTL
                    STA [mfCURRENTPREV], Y
                    INY
                    LDA mfFREESLOTH
                    STA [mfCURRENTPREV], Y

                    // WriteWord(freeSlot   +4, currentPrev);
                    INY
                    LDA mfCURRENTPREVL
                    STA [mfFREESLOT], Y
                    INY
                    LDA mfCURRENTPREVH
                    STA [mfFREESLOT], Y

                    // WriteWord(freeSlot   +2, 0);
                    LDY #2
                    LDA #0
                    STA [mfFREESLOT], Y
                    INY
                    STA [mfFREESLOT], Y

                    // prevSize = ReadWord(currentPrev);
                    LDY #0
                    LDA [mfCURRENTPREV], Y
                    STA mfPREVSIZEL
                    INY
                    LDA [mfCURRENTPREV], Y
                    STA mfPREVSIZEH

                    // gapBack = freeSlot - (currentPrev+prevSize);

                    CLC
                    LDA mfCURRENTPREVL
                    ADC mfPREVSIZEL
                    STA mfGAPBACKL
                    LDA mfCURRENTPREVH
                    ADC mfPREVSIZEH
                    STA mfGAPBACKH
                    SEC
                    LDA mfFREESLOTL
                    SBC mfGAPBACKL
                    STA mfGAPBACKL
                    LDA mfFREESLOTH
                    SBC mfGAPBACKH
                    STA mfGAPBACKH

                    LDA mfGAPBACKL
                    if (NZ) 
                    {
                        break; // memoryFreeExit
                    }
                    LDA mfGAPBACKH
                    if (NZ) 
                    { 
                        break; // memoryFreeExit
                    }
                    
                    // GAPBACK == 0

                    // no gap between freeSlot and previous so absorb it into previous block
                    // WriteWord(currentPrev, prevSize+size);
                    CLC
                    LDA mfSIZEL
                    ADC mfPREVSIZEL
                    STA mfSIZEL
                    LDA mfSIZEH
                    ADC mfPREVSIZEH
                    STA mfSIZEH
                    LDY #0
                    LDA mfSIZEL
                    STA [mfCURRENTPREV], Y
                    INY
                    LDA mfSIZEH
                    STA [mfCURRENTPREV], Y

                    // WriteWord(currentPrev+2, 0); // nothing beyond freeSlot, tail of FreeList
                    LDA #0
                    INY
                    STA [mfCURRENTPREV], Y
                    INY
                    STA [mfCURRENTPREV], Y
                    break; // memoryFreeExit
                }
            }
    
            //////////////////////////////
            // 0 != current
            //////////////////////////////

            // insert into freelist before current [mfCURRENT]
            // WriteWord(currentPrev+2, freeSlot);
            LDY #2
            LDA mfFREESLOTL
            STA [mfCURRENTPREV], Y
            INY
            LDA mfFREESLOTH
            STA [mfCURRENTPREV], Y

            // WriteWord(freeSlot   +4, currentPrev);
            INY
            LDA mfCURRENTPREVL
            STA [mfFREESLOT], Y
            INY
            LDA mfCURRENTPREVH
            STA [mfFREESLOT], Y

            // WriteWord(freeSlot   +2, mfCURRENT);
            LDY #2
            LDA mfCURRENTL
            STA [mfFREESLOT], Y
            INY
            LDA mfCURRENTH
            STA [mfFREESLOT], Y

            // WriteWord(mfCURRENT    +4, freeSlot);
            INY
            LDA mfFREESLOTL
            STA [mfCURRENT], Y
            INY
            LDA mfFREESLOTH
            STA [mfCURRENT], Y

            // prevSize = ReadWord(currentPrev);
            LDY #0
            LDA [mfCURRENTPREV], Y
            STA mfPREVSIZEL
            INY
            LDA [mfCURRENTPREV], Y
            STA mfPREVSIZEH

            // gapBack = freeSlot - (currentPrev+prevSize);

            CLC
            LDA mfCURRENTPREVL
            ADC mfPREVSIZEL
            STA mfGAPBACKL
            LDA mfCURRENTPREVH
            ADC mfPREVSIZEH
            STA mfGAPBACKH
            SEC
            LDA mfFREESLOTL
            SBC mfGAPBACKL
            STA mfGAPBACKL
            LDA mfFREESLOTH
            SBC mfGAPBACKH
            STA mfGAPBACKH

            LDA #0
            CMP mfGAPBACKL
            if (Z)
            {
                CMP mfGAPBACKH
                if (Z)
                {
                    // no gap between freeSlot and previous so absorb it into previous block
                    // WriteWord(currentPrev, prevSize+size);

                    // size = prevSize+size;
                    CLC
                    LDA mfSIZEL
                    ADC mfPREVSIZEL
                    STA mfSIZEL
                    LDA mfSIZEH
                    ADC mfPREVSIZEH
                    STA mfSIZEH

                    LDY #0
                    LDA mfSIZEL
                    STA [mfCURRENTPREV], Y
                    INY
                    LDA mfSIZEH
                    STA [mfCURRENTPREV], Y

                    // WriteWord(currentPrev+2, mfCURRENT);
                    INY
                    LDA mfCURRENTL
                    STA [mfCURRENTPREV], Y
                    INY
                    LDA mfCURRENTH
                    STA [mfCURRENTPREV], Y

                    // WriteWord(mfCURRENT+4, currentPrev);
                    INY
                    LDA mfCURRENTPREVL
                    STA [mfCURRENT], Y
                    INY
                    LDA mfCURRENTPREVH
                    STA [mfCURRENT], Y

                    LDA mfCURRENTPREVL
                    STA mfFREESLOTL
                    LDA mfCURRENTPREVH
                    STA mfFREESLOTH
                }
            }
    
            // gapNext = mfCURRENT - (freeSlot+size);
            CLC
            LDA mfFREESLOTL
            ADC mfSIZEL
            STA mfGAPNEXTL
            LDA mfFREESLOTH
            ADC mfSIZEH
            STA mfGAPNEXTH
            SEC
            LDA mfCURRENTL
            SBC mfGAPNEXTL
            STA mfGAPNEXTL
            LDA mfCURRENTH
            SBC mfGAPNEXTH
            STA mfGAPNEXTH

            LDA mfGAPNEXTL
            if (NZ) { break; }
            LDA mfGAPNEXTH
            if (NZ) { break; }
            
            // 0 == gapNext

            // no gap between freeSlot and current [mfCURRENT] so absorb it into freeSlot block

            // WriteWord(freeSlot, size+currentSize);
            CLC
            LDA mfSIZEL
            ADC mfCURRENTSIZEL
            STA mfSIZEL
            LDA mfSIZEH
            ADC mfCURRENTSIZEH
            STA mfSIZEH
            
            LDY #0
            LDA mfSIZEL
            STA [mfFREESLOT], Y
            INY
            LDA mfSIZEH
            STA [mfFREESLOT], Y

            // WriteWord(freeSlot+2, currentNext);
            INY
            LDA mfCURRENTNEXTL
            STA [mfFREESLOT], Y
            INY
            LDA mfCURRENTNEXTH
            STA [mfFREESLOT], Y

            LDA mfCURRENTNEXTL
            if (Z)
            {
                LDA mfCURRENTNEXTH
                if (Z)
                {
                    break; // currentNext == 0
                }
            }
            
            // currentNext != 0

            // WriteWord(currentNext+4, freeSlot);
            LDY #4
            LDA mfFREESLOTL
            STA [mfCURRENTNEXT], Y
            INY
            LDA mfFREESLOTH
            STA [mfCURRENTNEXT], Y
            
            break;
        } // loop
    }
}
