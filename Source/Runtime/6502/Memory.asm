unit Memory
{
    uses "ZeroPage"
    
    uses "Allocate.asm"
    uses "Free.asm"
    
    probeTest()
    {
        // set the carry flag if RAM is found at IDX
        loop
        {
            LDA #0xAA
#ifdef CPU_65C02S
            STA [IDX]
            LDA [IDX]
#else
            LDY # 0
            STA [IDX], Y
            LDA [IDX], Y
#endif
            CMP #0xAA
            if (Z)
            {
                LDA #0x55
#ifdef CPU_65C02S
                STA [IDX]
                LDA [IDX]
#else
                STA [IDX], Y
                LDA [IDX], Y
#endif
                CMP #0x55
                if (Z)
                {
                    SEC // RAM found
                    break;
                }
            }
            CLC // not RAM
            break;
        }
    }
    
    probeRAM()
    {
        loop
        {
            // probe to discover RAM size:
            LDA # 0xFF
            STA ZP.IDXL
            LDA # 0xDF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0xE0 for 56K
                LDA # 0xE0
                break;
            }
            LDA # 0xBF
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0xC0 for 48K
                LDA # 0xC0
                break;
            }
            LDA # 0x7F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0x80 for 32K
                LDA # 0x80
                break;
            }
            LDA # 0x4F
            STA ZP.IDXH
            probeTest();
            if (C)
            {
                // A = 0x40 for 16K
                LDA # 0x40
                break;
            }
            LDA # 0x0B
            Die();
            break;
        } // loop
    }        
    
    InitializeHeapSize()
    {
        // Assumes that:
        // - entire program was loaded at HopperData (typically $0800)
        // - size in pages of loaded program is in PROGSIZE
        
        CLC
        LDA # (Address.HopperData >> 8)
        ADC ZP.PROGSIZE  // program size in pages (rounded up to the nearest page)
        STA ZP.HEAPSTART
        
        // if RAM does not end at 0x80 (0x7FFF) then respect the value of RamSize (like 0x5000 for Ben Eater 6502)
        LDA # (Address.RamSize >> 8)
        CMP # 0x80 
        if (Z)
        {
            // probe to discover RAM size:
            // - A = 0x40 for 16K    
            // - A = 0x80 for 32K    
            // - A = 0xC0 for 48K
            // - A = 0xE0 for 56K
            probeRAM(); // munts Y, sets A
        }
        SEC
        SBC ZP.HEAPSTART
        STA ZP.HEAPSIZE
        
        // Zero initialize
        LDA #0
        PHA PHA
        STA IDXL
        LDA ZP.HEAPSTART
        STA IDXH
        LDX ZP.HEAPSIZE // number of 256 byte pages is same as MSB of size
        Utilities.ClearPages(); // munts A, X, Y
        
        // FreeList = Hopper heap start
        LDA ZP.HEAPSTART
        STA ZP.FREELISTH
        PLA // 0 -> A
        TAY // 0 -> Y
        STA ZP.FREELISTL
        
        // all memory is in this single free list record
        STA [ZP.FREELIST], Y
        LDA ZP.HEAPSIZE
        INY
        STA [ZP.FREELIST], Y
        
        // next = null
        PLA // 0 -> A
        INY
        STA [ZP.FREELIST], Y
        INY
        STA [ZP.FREELIST], Y
        
        // prev = null
        INY
        STA [ZP.FREELIST], Y
        INY
        STA [ZP.FREELIST], Y
    }
    Allocate()
    {
        Stacks.PopACC();      // only care about ACCL and ACCH (not ACCT)
        Allocate.Allocate();
        
        // Push IDX:
        LDY ZP.SP
        LDA ZP.IDXL
        STA Address.ValueStackLSB, Y
        LDA ZP.IDXH
        STA Address.ValueStackMSB, Y
        LDA # Types.UInt
        STA Address.TypeStackLSB, Y
        INC ZP.SP
    }
    Free()
    {
        Stacks.PopIDX();
        Free.Free();
    }
    ReadByte()
    {
        Stacks.PopIDX();
        
#ifdef CPU_65C02S
        STZ ZP.NEXTH
        LDA [IDX]
#else
        LDY # 0
        STY ZP.NEXTH
        LDA [IDX], Y
#endif
        STA ZP.NEXTL
        
        LDA # Types.Byte
        STA ZP.NEXTT
        PushNext();
    }
    WriteByte()
    {
        Stacks.PopACC(); // only care about ACCL (not ACCT or ACCH)
        Stacks.PopIDX();
        
        LDA ACCL
#ifdef CPU_65C02S
        STA [IDX]
#else
        LDY # 0
        STA [IDX], Y
#endif
    }
    
    AvailableACC()
    {
        // uses IDXand ACC
        // pushes result to [top]
#ifdef CPU_65C02S
        STZ ZP.ACCL
        STZ ZP.ACCH
#else
        LDA # 0
        STA ZP.ACCL
        STA ZP.ACCH
#endif
        LDA ZP.FREELISTL
        STA ZP.IDXL
        LDA ZP.FREELISTH
        STA ZP.IDXH
            
        loop
        {
            LDA ZP.IDXL
            if (Z)
            {
                LDA ZP.IDXH
                if (Z)
                {
                    break;
                }
            }
        
            LDY #0
            CLC
            LDA [ZP.IDX], Y
            ADC ZP.ACCL
            STA ZP.ACCL
            INY
            LDA [ZP.IDX], Y
            ADC ZP.ACCH
            STA ZP.ACCH
            
            // 2 byte cost for each allocated block:
            DecACCx2();
            
            INY
            LDA [IDX], Y
            PHA
            INY
            LDA [IDX], Y
            STA IDXH
            PLA
            STA IDXL
        } // loop
    }
    Available()
    {
        AvailableACC();
        LDA # Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();  // munts Y, A
    }
    
    MaximumACC()
    {
        // uses ACC, IDX and IDY
        // pushes result to [top]
               
        // available = 0
#ifdef CPU_65C02S
        STZ ACCL
        STZ ACCH
#else        
        LDA #0
        STA ACCL
        STA ACCH
#endif
        
        // current = FREELIST
        LDA FREELISTL
        STA IDXL
        LDA FREELISTH
        STA IDXH
        
        loop
        {
            LDA IDXL
            if (Z)
            {
                LDA IDXH
                if (Z)
                {
                    // current == 0
                    LDA ACCL
                    if (Z)
                    {
                        LDA ACCH
                        if (Z)
                        { 
                            // available== 0
                            break; 
                        } 
                    }
                    
                    // 2 byte cost for the block
                    DecACCx2();
                    break;
                }
            }
    
            // size = ReadWord(current + 0);
            LDY # 0
            LDA [IDX], Y
            STA IDYL
            INY
            LDA [IDX], Y
            STA IDYH
            
            // size <= available?
            LDA IDYH
            CMP ACCH
            if (Z)
            {
                LDA IDYL
                CMP ACCL
            }
    
            if (NZ)    // size == available (not >)
            {
                if (C) // size <  available (not >)
                {
                    // size > available
                    
                    // size > available
                    //   so available = size;
                    LDA IDYL
                    STA ACCL
                    LDA IDYH
                    STA ACCH
                }
            }
    
            // current = ReadWord(current + 2);
            INY
            LDA [IDX], Y
            PHA
            INY
            LDA [IDX], Y
            STA IDXH
            PLA
            STA IDXL
        } // loop
    }
    Maximum()
    {
        MaximumACC();
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
    }
}
