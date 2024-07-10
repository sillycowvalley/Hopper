unit W65C22
{
    
#if !defined(W65C22_VIA)
    #define W65C22_VIA
#endif    
#if !defined(CPU_8MHZ) && !defined(CPU_4MHZ) && !defined(CPU_2MHZ) && !defined(CPU_1MHZ)
    #define CPU_8MHZ
#endif

    uses "/Source/Runtime/6502/ZeroPage"
    
    Initialize()
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
      
      // 1 millisecond intervals:
      //   A CPU clock of 1 mHz = 1000 cycles - 2 = 998 / 0x03E6
      //   A CPU clock of 2mHz would be 2x faster so this count would need to be 2000 cycles - 2 = 1998 / 0x07CE
      //   A CPU clock of 4mHz would be 4x faster so this count would need to be 4000 cycles - 2 = 3998 / 0x0F9E
      //   A CPU clock of 4.9152 mHz would be 4.9152x faster so this count would need to be 4915 cycles - 2 = 4913 / 0x1061: 
      //   A CPU clock of 8mHz would be 8x faster so this count would need to be 8000 cycles - 2 = 7998 / 0x1F40
      //   A CPU clock of 10 kHz would be  100x slower so this would be a 100ms interval
      //   A CPU clock of  1 kHz would be 1000x slower so this would be a 1000ms interval (1 second)
#ifdef CPU_1MHZ
        LDA # 0xE6
        STA ZP.T1CL
        LDA # 0x03
        STA ZP.T1CH
#endif      
#ifdef CPU_2MHZ
        LDA # 0xCE
        STA ZP.T1CL
        LDA # 0x07
        STA ZP.T1CH
#endif      
#ifdef CPU_4MHZ
        LDA # 0x9E
        STA ZP.T1CL
        LDA # 0x0F
        STA ZP.T1CH
#endif      
#ifdef CPU_8MHZ
        LDA # 0x40
        STA ZP.T1CL
        LDA # 0x1F
        STA ZP.T1CH
#endif      
        LDA # 0b11000000 // set Timer1 bit in IER to put Timer1 into free run mode
        STA ZP.IER
    }
    
    ISR()
    {
#if defined(CPU_65C02S) && (defined(ZEROPAGE_IO) || defined(ZEROPAGE_16K_IO))
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
