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
        CMP # ('9'+1)
        if (C)
        {
            SBC # (7+1)
        }
        SBC     # ('0'-1)
        AND     # 0x0F
    }
    
    SendSlash()
    {
        LDA # Slash // "\"
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
            TAY // 0 -> Y
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
    DecIDY()
    {
        LDA IDYL
        if (Z)
        {
            DEC IDYH
        }
        DEC IDYL
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
#ifdef HOPPER_BASIC
    IncSTR()
    {
        INC ZP.STRL
        if (Z)
        {
            INC ZP.STRH
        }
    }
#endif
    IncNEXT()
    {
        INC ZP.NEXTL
        if (Z)
        {
            INC ZP.NEXTH
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
    DecSOURCEADDRESS()
    {
        LDA FSOURCEADDRESSL
        if (Z)
        {
            DEC FSOURCEADDRESSH
        }
        DEC FSOURCEADDRESSL
    }
    
    // copy LCOUNT bytes from FSOURCEADDRESS to FDESTINATIONADDRESS
    //     munts LCOUNT, FSOURCEADDRESS, FDESTINATIONADDRESS, A, Y
    CopyBytes()
    {
        LDY # 0
        loop
        {
            LDA LCOUNTL
            ORA LCOUNTH
            if (Z)
            {
                return;
            }
            LDA [FSOURCEADDRESS], Y
            STA [FDESTINATIONADDRESS], Y
            IncDESTINATIONADDRESS();
            IncSOURCEADDRESS();
            
            LDA LCOUNTL
            if (Z)
            {
                DEC LCOUNTH
            }
            DEC LCOUNTL
        } // loop
    }
}
