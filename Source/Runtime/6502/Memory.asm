unit Memory
{
    uses "ZeroPage"
    
    uses "Allocate.asm"
    uses "Free.asm"
    
    InitializeHeapSize()
    {
        // Assumes that:
        // - entire program was loaded at HopperData (typically $0800)
        // - size in pages of loaded program is in PROGSIZE
        
        CLC
        LDA # (Address.HopperData >> 8)
        ADC ZP.PROGSIZE  // program size in pages (rounded up to the nearest page)
        STA ZP.HEAPSTART
        
        SEC
        LDA # (Address.RamSize >> 8) // top page of RAM (typically 0x80)
        SBC ZP.HEAPSTART
        STA ZP.HEAPSIZE
        
        // Zero initialize
        LDA #0
        STA IDXL
        LDA HEAPSTART
        STA IDXH
        LDX HEAPSIZE // number of 256 byte pages is same as MSB of size
        Utilities.ClearPages();
        
        // FreeList = Hopper heap start
        LDA ZP.HEAPSTART
        STA ZP.FREELISTH
        LDA # 0
        STA ZP.FREELISTL
        
        // all memory is in this single free list record
        LDA #0
        TAY
        STA [ZP.FREELIST], Y
        LDA ZP.HEAPSIZE
        INY
        STA [ZP.FREELIST], Y
        
        // next = null
        LDA #0
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
        PopACC();
        Allocate.allocate();
        PushIDX();
    }
    Free()
    {
        PopIDX();
        Free.free();
    }
    ReadByte()
    {
        PopIDX();
        LDY # 0
        LDA [IDX], Y
        PushA();
    }
    WriteByte()
    {
        PopACC();
        PopIDX();
        LDY # 0
        LDA ACCL
        STA [IDX], Y
    }
    
    Available()
    {
        // uses IDXand ACC
        // pushes result to [top]
        LDA # 0
        STA ACCL
        STA ACCH
        LDA FREELISTL
        STA IDXL
        LDA FREELISTH
        STA IDXH
            
        loop
        {
            LDA #0
            CMP IDXL
            if (Z)
            {
                CMP IDXH
                if (Z)
                {
                    break;
                }
            }
        
            LDY #0
            CLC
            LDA [IDX], Y
            ADC ACCL
            STA ACCL
            INY
            LDA [IDX], Y
            ADC ACCH
            STA ACCH
            
            // 2 byte cost for each allocated block:
            DecACC();
            DecACC();
            
            INY
            LDA [IDX], Y
            PHA
            INY
            LDA [IDX], Y
            STA IDXH
            PLA
            STA IDXL
        } // loop
        
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
   }
    
    Maximum()
    {
        // uses ACC, IDX and IDY
        // pushes result to [top]
               
        // available = 0
        LDA #0
        STA ACCL
        STA ACCH
        
        // current = FREELIST
        LDA FREELISTL
        STA IDXL
        LDA FREELISTH
        STA IDXH
        
        loop
        {
            LDA #0
            CMP IDXL
            if (Z)
            {
                CMP IDXH
                if (Z)
                {
                    // current == 0
                    CMP ACCL
                    if (Z)
                    {
                        CMP ACCH
                        if (Z)
                        { 
                            // available== 0
                            break; 
                        } 
                    }
                    
                    // 2 byte cost for the block
                    DecACC();
                    DecACC();
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
        
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
    }
}
