unit Time // Times.asm
{
    friend W65C22, PIA6821;
    
#if defined(W65C22_VIA) || defined(M6840_PTM)
  #if !defined(HOPPER_BASIC)
    uses "/Source/Runtime/6502/Long"
    uses "/Source/Runtime/6502/Types"
  #endif
#endif    
    
    
    // TimerDelay         : value in [top] in milliseconds, returns after delay
    Delay()
    {
#ifdef HOPPER_STACK
        PopTop();
#endif
        DelayTOP();
    }
    DelayTOP()
    {
        PHA
        PHX
        
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        LDX ZP.TICK3     // reading TICK3 makes a snapshot of all 4 registers on the emulator      
        
        CLC
        LDA ZP.TOPL
        ADC ZP.TICK0
        STA ZP.TARGET0
        LDA ZP.TOPH
        ADC ZP.TICK1
        STA ZP.TARGET1
        LDA ZP.TICK2
        ADC #0 // to collect the carry
        STA ZP.TARGET2
        TXA // restore ZP.TICK3
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
        
        PLX
        PLA
    }
    
    SampleMicrosSet()
    {
        PopTop();
#ifdef W65C22_VIA
        W65C22.sharedSamplesMicroSet();
#endif  
#ifdef M6840_PTM
       PIA6821.sharedSamplesMicroSet();
#endif      
    }
    SampleMicrosGet()
    {
#ifdef W65C22_VIA
        W65C22.sharedSamplesMicroGet();
#endif
#ifdef M6840_PTM
       PIA6821.sharedSamplesMicroGet();
#endif

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
        LDA # Types.UInt
        Stacks.PushTop();
    }

#ifdef LONGS
    Millis()
    {
        // LNEXT = LNEXT / LTOP + LRESULT
        
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA LNEXT3
        LDA ZP.TICK2
        STA LNEXT2
        LDA ZP.TICK1
        STA LNEXT1
        LDA ZP.TICK0 
        STA LNEXT0
#ifdef HOPPER_STACK
        LDA # Types.Long
        Long.pushNewFromL();
#endif
    }


    Seconds()
    {
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
      
        // LNEXT = LNEXT / LTOP + LRESULT  
        Long.DivMod(); // Seconds = Millis / 1000
        
        LDA LNEXT0
        STA TOPL
        LDA LNEXT1
        STA TOPH
#ifdef HOPPER_STACK
        LDA # Types.UInt
        Stacks.PushTop();
#endif
    }
#endif
}
