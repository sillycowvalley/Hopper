unit Time
{
    uses "/Source/Runtime/6809/ZeroPage"
    
#ifdef W65C22_VIA
    uses "/Source/Runtime/6809/Devices/W65C22"
    uses "/Source/Runtime/6809/Long"
    uses "/Source/Runtime/6809/Types"
#endif    
    
    // TimerDelay         : value in [top] in milliseconds, returns after delay
    Delay()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else        
#ifdef HOPPER_STACK
        JSR PopTop
#endif
        JSR DelayTOP
#endif
    }

    DelayTOP()
    {
#ifndef W65C22_VIA
        TXA BRK // VIA not included?
#else        
        PSHS A
        
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
            LDA ZP.TICK0
            CMP ZP.TARGET0
            if (NC) { continue; }
            
            break; // Target >= Ticks : match ->
        }
        
        PULS A
#endif
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
        JSR Long.pushNewFromL
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
        CLR LTOP2
        CLR LTOP3
        
        JSR Long.DivMod // Seconds = Millis / 1000
        
        LDA LNEXT0
        STA TOPL
        LDA LNEXT1
        STA TOPH
#ifdef HOPPER_STACK
        LDA # Types.UInt
        JSR Stacks.PushTop
#endif
#endif
    }
}
