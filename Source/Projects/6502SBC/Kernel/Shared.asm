unit Shared
{
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
#ifdef UNIVERSAL
        LDA #0
        STA ZP.TOP2
        STA ZP.TOP3
#else        
        STZ ZP.TOP2
        STZ ZP.TOP3
#endif
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
