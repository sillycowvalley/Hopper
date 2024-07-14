unit W65C22
{
    friend Parallel;
    
#if !defined(CPU_8MHZ) && !defined(CPU_4MHZ) && !defined(CPU_2MHZ) && !defined(CPU_1MHZ)
    #define CPU_8MHZ
#endif

    uses "/Source/Runtime/6502/ZeroPage"
    
    initialize()
    {
        // VIA initialization assuming the RESB pulse was too short
        // "Reset clears all internal registers (except T1 and T2 counters and latches, and the SR"

#ifdef CPU_65C02S       
        STZ ZP.DDRA // set all pins on port A to input (good default in case we don't use it)
        STZ ZP.DDRB // set all pins on port B to input (good default in case we don't use it) 
#else        
        LDA # 0b00000000
        STA ZP.DDRA // set all pins on port A to input (good default in case we don't use it)
        STA ZP.DDRB // set all pins on port B to input (good default in case we don't use it)
#endif               
        // start timer last since it generates interrupts
        // VIA interval timer to tick every 1ms or 1000 clock cycles

#ifdef CPU_65C02S               
        STZ ZP.TICK0
        STZ ZP.TICK1
        STZ ZP.TICK2
        STZ ZP.TICK3
#else
        STA ZP.TICK0
        STA ZP.TICK1
        STA ZP.TICK2
        STA ZP.TICK3
#endif
      
        LDA # 0b01000000 // put the timer into free run mode
        STA ZP.ACR
        
        // 1000us per sample = 1ms units for Time.Delay(..)
        LDA # 0xE8
        STA ZP.TOPL
        LDA # 0x03
        STA ZP.TOPH
        Time.sharedSamplesMicroSet();
    }
    
    isr()
    {
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        if (BBS7, ZP.IFR) // IRQ by VIA
        {
            if (BBS6, ZP.IFR) // Timer 1 IRQ
            {
                BIT  ZP.T1CL   // clear the interrupt by reading T1 Counter L
                
                INC  ZP.TICK0
                if (Z)
                {
                    INC  ZP.TICK1
                    if (Z)
                    {
                        INC  ZP.TICK2
                        if (Z)
                        {
                            INC  ZP.TICK3
                        }
                    }
                }
            }
        }
#else
        BIT ZP.IFR
        if (MI) // IRQ by VIA
        {
            if (V) // Timer 1 IRQ
            {
                BIT  ZP.T1CL   // clear the interrupt by reading T1 Counter L
                INC  ZP.TICK0
                if (Z)
                {
                    INC  ZP.TICK1
                    if (Z)
                    {
                        INC  ZP.TICK2
                        if (Z)
                        {
                            INC  ZP.TICK3
                        }
                    }
                }
            }
        }
#endif        
    }
}
