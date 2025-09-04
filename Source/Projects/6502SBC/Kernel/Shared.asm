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
}