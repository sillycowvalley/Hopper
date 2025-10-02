unit Time // Time.asm
{
    Delay()
    {
        PHA
#ifdef UNIVERSAL
        TXA PHA
#else
        PHX
#endif
        
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        LDX ZP.TICK3     // reading TICK3 makes a snapshot of all 4 registers on the emulator      
        
        CLC
        LDA ZP.TOP0
        ADC ZP.TICK0
        STA ZP.TARGET0
        LDA ZP.TOP1
        ADC ZP.TICK1
        STA ZP.TARGET1
        LDA ZP.TICK2
        ADC ZP.TOP2
        STA ZP.TARGET2
        TXA // restore ZP.TICK3
        ADC ZP.TOP3
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
#ifdef UNIVERSAL
        PLA TAX
#else        
        PLX
#endif
        PLA
    }
    
    Millis()
    {
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA ZP.TOP3
        LDA ZP.TICK2
        STA ZP.TOP2
        LDA ZP.TICK1
        STA ZP.TOP1
        LDA ZP.TICK0 
        STA ZP.TOP0
    }

    Seconds()
    {
        LDA ZP.TICK3 // reading TICK3 makes a snapshot of all 4 registers on the emulator
        STA NEXT3
        LDA ZP.TICK2
        STA NEXT2
        LDA ZP.TICK1
        STA NEXT1
        LDA ZP.TICK0 
        STA NEXT0
        
        LDA # 0xE8 // 1000 = 0x03E8
        STA TOP0
        LDA # 0x03
        STA TOP1
#ifdef UNIVERSAL
        LDA #0
        STA TOP2
        STA TOP3
#else        
        STZ TOP2
        STZ TOP3
#endif
        
        LDX #0 // Div
        Long.DivMod(); // Seconds = Millis / 1000   
        
        LDA NEXT0
        STA TOP0
        LDA NEXT1
        STA TOP1
        LDA NEXT2
        STA TOP2
        LDA NEXT3
        STA TOP3
    }
}
