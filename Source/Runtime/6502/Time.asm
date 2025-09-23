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
    
#if defined(W65C22_VIA) || defined(M6840_PTM)    
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
#else
    
    delay1ms()
    {
        PHX      // 3 cycles
        PHY      // 3 cycles
        
        // Function call overhead: JSR (6) + PHX (3) + PHY (3) + PLY (4) + PLX (4) + RTS (6) = 26 cycles
        // DelayTOP loop overhead we're compensating for: 34 cycles
        // Total overhead to subtract: 60 cycles
        
    #ifdef CPU_8MHZ
        // Target: 8000 - 60 = 7940 cycles
        LDX #32                      // 2 cycles
        loop  // Outer loop: 32 iterations
        {
            LDY #49                  // 2 cycles × 32 = 64 cycles
            loop  // Inner loop: 49 × 32 = 1568 iterations
            {
                DEY                  // 2 cycles × 1568 = 3136 cycles
                if (NZ) { continue; }  // 3 cycles × 1536 + 2 cycles × 32 = 4672 cycles
                break;
            }
            DEX                      // 2 cycles × 32 = 64 cycles
            if (NZ) { continue; }      // 3 cycles × 31 + 2 cycles × 1 = 95 cycles
            break;
        }
        // Loop total: 2 + 64 + 3136 + 4672 + 64 + 95 = 8033 cycles
        // Actual with overhead: 8033 + 60 = 8093 (~1.01ms)
    #endif
    
    #ifdef CPU_4MHZ
        // Target: 4000 - 60 = 3940 cycles
        LDX #16                      // 2 cycles
        loop  // Outer loop: 16 iterations
        {
            LDY #48                  // 2 cycles × 16 = 32 cycles
            loop  // Inner loop: 48 × 16 = 768 iterations
            {
                DEY                  // 2 cycles × 768 = 1536 cycles
                if (NZ) { continue; }  // 3 cycles × 752 + 2 cycles × 16 = 2288 cycles
                break;
            }
            DEX                      // 2 cycles × 16 = 32 cycles
            if (NZ) { continue; }      // 3 cycles × 15 + 2 cycles × 1 = 47 cycles
            break;
        }
        // Loop total: 2 + 32 + 1536 + 2288 + 32 + 47 = 3937 cycles
        // Actual with overhead: 3937 + 60 = 3997 (~1.00ms)
    #endif
    
    #ifdef CPU_2MHZ
        // Target: 2000 - 60 = 1940 cycles
        LDX #8                       // 2 cycles
        loop  // Outer loop: 8 iterations
        {
            LDY #48                  // 2 cycles × 8 = 16 cycles
            loop  // Inner loop: 48 × 8 = 384 iterations
            {
                DEY                  // 2 cycles × 384 = 768 cycles
                if (NZ) { continue; }  // 3 cycles × 376 + 2 cycles × 8 = 1144 cycles
                break;
            }
            DEX                      // 2 cycles × 8 = 16 cycles
            if (NZ) { continue; }      // 3 cycles × 7 + 2 cycles × 1 = 23 cycles
            break;
        }
        // Loop total: 2 + 16 + 768 + 1144 + 16 + 23 = 1969 cycles
        // Actual with overhead: 1969 + 60 = 2029 (~1.01ms)
    #endif
    
    #ifdef CPU_1MHZ
        // Target: 1000 - 60 = 940 cycles
        LDY #187                     // 2 cycles
        loop  // Single loop: 187 iterations
        {
            DEY                      // 2 cycles × 187 = 374 cycles
            if (NZ) { continue; }      // 3 cycles × 186 + 2 cycles × 1 = 560 cycles
            break;
        }
        // Loop total: 2 + 374 + 560 = 936 cycles
        // Actual with overhead: 936 + 60 = 996 (~1.00ms)
    #endif
        
        PLY      // 4 cycles
        PLX      // 4 cycles
    }


    DelayTOP()
    {
        PHA                      // 3 cycles
        
        loop
        {
            // Check if delay count is zero
            LDA ZP.TOPL          // 3 cycles
            ORA ZP.TOPH          // 3 cycles
            if (Z) { break; }    // 2 cycles (not taken), 3 cycles (taken)
            
            // Delay 1ms
            delay1ms();          // 6 (JSR) + ~1000-8000 + 6 (RTS) cycles
            
            // Decrement 16-bit counter
            LDA ZP.TOPL          // 3 cycles
            if (NZ)              // 2 cycles (not taken), 3 cycles (taken)
            {
                DEC ZP.TOPL      // 5 cycles
            }
            else
            {
                DEC ZP.TOPL      // 5 cycles (makes it 0xFF)
                DEC ZP.TOPH      // 5 cycles
            }
            
            continue;            // 3 cycles (BRA to loop start)
        }
        // Per iteration overhead:
        // - Normal case (TOPL != 0): 34 cycles (~0.034% @ 1MHz, ~0.004% @ 8MHz)
        // - Rollover case (TOPL == 0): 38 cycles
        
        PLA                      // 4 cycles
    }

#endif    
    
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
