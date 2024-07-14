unit Time
{
    friend W65C22;
    
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
        sharedSamplesMicroSet();
    }
    SampleMicrosGet()
    {
#ifdef W65C22_VIA
        LDA ZP.T1LL 
        STA ZP.TOPL
        LDA ZP.T1LH 
        STA ZP.TOPH
        
        // The timer counts down from n-1 to 0, including the 0 as part of the count
        // +1
        INC ZP.TOPL
        if (Z)
        {
            INC ZP.TOPH
        }
        
#if defined(CPU_2MHZ) || defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // /2
        LSR ZP.TOPH
        ROR ZP.TOPL        
#endif  
#if defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // /4
        LSR ZP.TOPH
        ROR ZP.TOPL             
#endif          
#if defined(CPU_8MHZ)
        // /8
        LSR ZP.TOPH
        ROR ZP.TOPL     
#endif       

#else
        LDA # 0xE8
        STA ZP.TOPL
        LDA # 0x03
        STA ZP.TOPH
#endif
        
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
    
    sharedSamplesMicroSet()
    {
#ifdef W65C22_VIA
        LDA # 0b00000000 // Disable Timer1 interrupt
        STA ZP.IER
      
        // At a CPU clock of 1 mHz = 1000 cycles - 1 = 999 / 0x03E7 would give us a sample cycle of 1ms

#if defined(CPU_2MHZ) || defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // x2
        ASL ZP.TOPL
        ROL ZP.TOPH        
#endif  
#if defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // x4
        ASL ZP.TOPL
        ROL ZP.TOPH        
#endif          
#if defined(CPU_8MHZ)
        // x8
        ASL ZP.TOPL
        ROL ZP.TOPH        
#endif       
        // The timer counts down from n-1 to 0, including the 0 as part of the count
        // -1
        LDA ZP.TOPL
        if (Z)
        {
            DEC ZP.TOPH
        }
        DEC ZP.TOPL
        
        LDA ZP.TOPL
        STA ZP.T1CL
        LDA ZP.TOPH
        STA ZP.T1CH
        
        LDA # 0b11000000 // Set Timer1 bit in IER to put Timer1 into free run mode
        STA ZP.IER
#endif
    }
}
