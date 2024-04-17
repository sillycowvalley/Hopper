unit Time
{
    uses "6502/ZeroPage"
    
    const byte Ticks   = ZP.T0;
    const byte Target  = ZP.T4;
    
    Delay()
    {
        PopTop();
        
        PHA
        
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        CLC
        LDA ZP.TOPL
        ADC Ticks +0  // reading Ticks +0 makes a snapshot of all 4 registers
        STA Target+0
        LDA ZP.TOPH
        ADC Ticks +1
        STA Target+1
        LDA Ticks +2
        ADC #0 // to collect the carry
        STA Target+2
        LDA Ticks +3
        ADC #0 // to collect the carry
        STA Target+3
        
        // while Ticks0..3 < Target0..3, loop here
        loop
        {
            LDA Ticks +0          // reading Ticks +0 makes a snapshot of all 4 registers
            STA ZP.ACCL
            
            LDA Ticks +3
            CMP Target+3
            if (NC) { continue; } // Ticks+3 < Target+3
            LDA Ticks +2
            CMP Target+2
            if (NC) { continue; } // Ticks+2 < Target+2
            LDA Ticks +1
            CMP Target+1
            if (NC) { continue; } // Ticks+1 < Target+1
            LDA ZP.ACCL
            CMP Target+0
            if (NC) { continue; } // Ticks+0 < Target+0
            
            break; // Target >= Ticks : match ->
        }
        PLA        
    }
    Seconds()
    {
        // LNEXT = LNEXT / LTOP + LRESULT
        LDA Ticks +0
        STA LNEXT0
        LDA Ticks +1
        STA LNEXT1
        LDA Ticks +2
        STA LNEXT2
        LDA Ticks +3
        STA LNEXT3
        
        LDA # 0xE8 // 1000 = 0x3E8
        STA LTOP0
        LDA # 0x03
        STA LTOP1
#ifdef CPU_65C02S
        STZ LTOP2
        STZ LTOP3
#else        
        LDA #0
        STA LTOP2
        STA LTOP3
#endif
        
        Long.DivMod(); // Seconds = Millis / 1000
        
        LDA LNEXT0
        STA TOPL
        LDA LNEXT1
        STA TOPH
        LDA # Types.UInt
        Stacks.PushTop();
    }
}
