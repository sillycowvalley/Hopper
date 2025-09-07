unit Shared
{
    const byte[] BitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
    
    IncIDX()
    {
        INC ZP.IDXL
        if (Z)
        {
            INC ZP.IDXH
        }
    }
    IncIDY()
    {
        INC ZP.IDYL
        if (Z)
        {
            INC ZP.IDYH
        }
    }
    DecIDY()
    {
        LDA IDYL
        if (Z)
        {
            DEC IDYH
        }
        DEC IDYL
    }
    
    IncACC()
    {
        INC ZP.ACCL
        if (Z)
        {
            INC ZP.ACCH
        }
    }

    DecACCx2()
    {
        PHA
        SEC
        LDA ACCL
        SBC # 2
        STA ACCL
        LDA ACCH
        SBC # 0
        STA ACCH
        PLA
    }
    
    SwapNextTop()
    {
        LDY ZP.TOP0
        LDA ZP.NEXT0
        STA ZP.TOP0
        STY ZP.NEXT0
        
        LDY ZP.TOP1
        LDA ZP.NEXT1
        STA ZP.TOP1
        STY ZP.NEXT1
        
        LDY ZP.TOP2
        LDA ZP.NEXT2
        STA ZP.TOP2
        STY ZP.NEXT2
        
        LDY ZP.TOP3
        LDA ZP.NEXT3
        STA ZP.TOP3
        STY ZP.NEXT3
    }
    
    LoadByte()  // A = byte value, X = slot
    {
        STA 0x00, X
        Zero3();
    }
    Zero3() // X = slot
    {
        STZ 0x01, X
        STZ 0x02, X
        STZ 0x03, X
    }
    
    MoveNextTo() // X = slot
    {
        LDA ZP.NEXT0
        STA 0x00, X
        LDA ZP.NEXT1
        STA 0x01, X
        LDA ZP.NEXT2
        STA 0x02, X
        LDA ZP.NEXT3
        STA 0x03, X
    }
    
    MoveToNext() // X = slot
    {
        LDA 0x00, X
        STA ZP.NEXT0
        LDA 0x01, X
        STA ZP.NEXT1
        LDA 0x02, X
        STA ZP.NEXT2
        LDA 0x03, X
        STA ZP.NEXT3
    }
    
    MoveNextToTop()
    {
        LDA ZP.NEXT0
        STA ZP.TOP0
        LDA ZP.NEXT1
        STA ZP.TOP1
        LDA ZP.NEXT2
        STA ZP.TOP2
        LDA ZP.NEXT3
        STA ZP.TOP3
    }
    MoveAccToTop()
    {
        LDA ZP.ACCL
        STA ZP.TOP0
        LDA ZP.ACCH
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
    }
    MoveTopToNext()
    {
        LDA ZP.TOP0
        STA ZP.NEXT0
        LDA ZP.TOP1
        STA ZP.NEXT1
        LDA ZP.TOP2
        STA ZP.NEXT2
        LDA ZP.TOP3
        STA ZP.NEXT3
    }
    MoveNextToResult()
    {
        LDA ZP.NEXT0
        STA ZP.RESULT0
        LDA ZP.NEXT1
        STA ZP.RESULT1
        LDA ZP.NEXT2
        STA ZP.RESULT2
        LDA ZP.NEXT3
        STA ZP.RESULT3
    }
    
    MoveResultToTop()
    {
        LDA ZP.RESULT0
        STA ZP.TOP0
        LDA ZP.RESULT1
        STA ZP.TOP1
        LDA ZP.RESULT2
        STA ZP.TOP2
        LDA ZP.RESULT3
        STA ZP.TOP3
    }
    MoveResultToNext()
    {
        LDA ZP.RESULT0
        STA ZP.NEXT0
        LDA ZP.RESULT1
        STA ZP.NEXT1
        LDA ZP.RESULT2
        STA ZP.NEXT2
        LDA ZP.RESULT3
        STA ZP.NEXT3
    }
    
    LoadTopByte()  // A = byte value
    {
        STA ZP.TOP0
        ZeroTop3();
    }
    
    
    
    ZeroTop()
    {
        STZ ZP.TOP0
        ZeroTop3();
    }
    ZeroTop3()
    {
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
    }
    ZeroNext()
    {
        STZ ZP.NEXT0
        ZeroNext3();
    }
    ZeroNext3()
    {
        STZ ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
    }
    ZeroCheckTop()
    {
        LDA ZP.TOP0
        ORA ZP.TOP1
        ORA ZP.TOP2
        ORA ZP.TOP3
    }
    ZeroCheckNext()
    {
        LDA ZP.NEXT0
        ORA ZP.NEXT1
        ORA ZP.NEXT2
        ORA ZP.NEXT3
    }
    
    ZeroResult()
    {
        STZ ZP.RESULT0
        STZ ZP.RESULT1
        STZ ZP.RESULT2
        STZ ZP.RESULT3
    }
    ZeroResult8()
    {
        ZeroResult();
        STZ ZP.RESULT4
        STZ ZP.RESULT5
        STZ ZP.RESULT6
        STZ ZP.RESULT7
    }    
    
}
