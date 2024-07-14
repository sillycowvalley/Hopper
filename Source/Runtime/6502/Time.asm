unit Time
{
    uses "/Source/Runtime/6502/ZeroPage"
    
#ifdef W65C22_VIA
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Long"
    uses "/Source/Runtime/6502/Types"
#endif    
    
    
    // TimerDelay         : value in [top] in milliseconds, returns after delay
    Delay()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else        
#ifdef HOPPER_STACK
        PopTop();
#endif
        DelayTOP();
#endif
    }
    DelayTOP()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else        
        PHA
        
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        CLC
        LDA ZP.TOPL
        ADC ZP.TICK0     // reading TICK0 makes a snapshot of all 4 registers on the emulator
        STA ZP.TARGET0
        LDA ZP.TOPH
        ADC ZP.TICK1
        STA ZP.TARGET1
        LDA ZP.TICK2
        ADC #0 // to collect the carry
        STA ZP.TARGET2
        LDA ZP.TICK3
        ADC #0 // to collect the carry
        STA ZP.TARGET3
        
        loop
        {
            // while Ticks0..3 < Target0..3, loop here
            
            LDA ZP.TICK3   // reading TICK3 makes a snapshot of all 4 registers on the emulator      
            CMP ZP.TARGET3
            if (NC) { continue; }
            LDA ZP.TICK2
            CMP ZP.TARGET2
            if (NC) { continue; }
            LDA ZP.TICK1
            CMP ZP.TARGET1
            if (NC) { continue; }
            //LDA ZP.ACCL
            LDA ZP.TICK0
            CMP ZP.TARGET0
            if (NC) { continue; }
            
            break; // Target >= Ticks : match ->
        }
        
        PLA
#endif
    }
    SampleMicrosSet()
    {
        PopTop();
        // TODO
    }
    SampleMicrosGet()
    {
        // TODO : default 1000us for now
        LDA # 0xE8
        STA TOPL
        LDA # 0x03
        STA TOPH

        LDA # Types.UInt
        Stacks.PushTop();
    }

#ifdef LONGS
    Millis()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else
        // LNEXT = LNEXT / LTOP + LRESULT
        
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA LNEXT3
        LDA ZP.TICK2
        STA LNEXT2
        LDA ZP.TICK1
        STA LNEXT1
        LDA ZP.TICK0 
        STA LNEXT0
        
        LDA # Types.Long
        Long.pushNewFromL();
#endif
    }
#endif        
    Seconds()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else
        // LNEXT = LNEXT / LTOP + LRESULT
        
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA LNEXT3
        LDA ZP.TICK2
        STA LNEXT2
        LDA ZP.TICK1
        STA LNEXT1
        LDA ZP.TICK0 
        STA LNEXT0
        
        LDA # 0xE8 // 1000 = 0x03E8
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
#ifdef HOPPER_STACK
        LDA # Types.UInt
        Stacks.PushTop();
#endif
#endif
    }
}
