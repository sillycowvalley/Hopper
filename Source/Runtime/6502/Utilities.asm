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
}
