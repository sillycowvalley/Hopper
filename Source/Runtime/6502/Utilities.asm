unit Utilities
{
    const byte Slash  = 0x5C;
    const byte Escape = 0x1B;
    const byte Enter  = 0x0A;
    
    // Converts '0'..'9' or 'A'..'F' to 4-bit value
    // only uses A
    MakeNibble()
    {
        // only touches A
        CMP #'9'+1
        if (C)
        {
            SBC #7+1
        }
        SBC     #'0'-1
        AND     #0x0F
    }
    
    SendSlash()
    {
        LDA #Slash // "\"
        Serial.WriteChar();
    }
    WaitForEnter()
    {
        loop
        {
            Serial.WaitForChar();
            CMP #Enter
            if (Z) { break; }
        }
        Utilities.SendSlash();  // '\' response : acknowledge <enter> received
    }
    
    // IDX (memory location) and X (number of pages):
    //    inspired by: https://forums.atariage.com/topic/186656-clearing-a-section-of-memory/
    //    munts A, X, Y And IDXH but doesn't modify IDXL
    ClearPages()
    {
        LDA #0
        loop
        {
            LDY #0
            loop
            {
                STA [IDX], Y
                DEY
                if (Z) { break; }
            }
            // next page ..
            INC IDXH
            DEX
            if (Z) { break; }
        }
            
    }
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
    IncPC()
    {
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
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
    DecIDY()
    {
        PHA
        LDA IDYL
        if (Z)
        {
            DEC IDYH
        }
        DEC IDYL
        PLA
    }
    DecIDX()
    {
        PHA
        LDA IDXL
        if (Z)
        {
            DEC IDXH
        }
        DEC IDXL
        PLA
    }
    DecLENGTH()
    {
        PHA
        LDA FLENGTHL
        if (Z)
        {
            DEC FLENGTHH
        }
        DEC FLENGTHL
        PLA
    }
    DecCOUNT()
    {
        PHA
        LDA LCOUNTL
        if (Z)
        {
            DEC LCOUNTH
        }
        DEC LCOUNTL
        PLA
    }
    IncSOURCEADDRESS()
    {
        INC ZP.FSOURCEADDRESSL
        if (Z)
        {
            INC ZP.FSOURCEADDRESSH
        }
    }
    IncDESTINATIONADDRESS()
    {
        INC ZP.FDESTINATIONADDRESSL
        if (Z)
        {
            INC ZP.FDESTINATIONADDRESSH
        }
    }
    IncLENGTH()
    {
        INC ZP.FLENGTHL
        if (Z)
        {
            INC ZP.FLENGTHH
        }
    }
    IncSIZE()
    {
        INC ZP.FSIZEL
        if (Z)
        {
            INC ZP.FSIZEH
        }
    }
    DecSIZE()
    {
        PHA
        LDA FSIZEL
        if (Z)
        {
            DEC FSIZEH
        }
        DEC FSIZEL
        PLA
    }
    DecSOURCEADDRESS()
    {
        PHA
        LDA FSOURCEADDRESSL
        if (Z)
        {
            DEC FSOURCEADDRESSH
        }
        DEC FSOURCEADDRESSL
        PLA
    }
    DecDESTINATIONADDRESS()
    {
        PHA
        LDA FDESTINATIONADDRESSL
        if (Z)
        {
            DEC FDESTINATIONADDRESSH
        }
        DEC FDESTINATIONADDRESSL
        PLA
    }
    
    
}
