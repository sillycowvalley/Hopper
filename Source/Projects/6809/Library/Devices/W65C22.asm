unit W65C22
{
#if !defined(W65C22_VIA)
    #define W65C22_VIA
#endif    
#if !defined(CPU_8MHZ) && !defined(CPU_4MHZ) && !defined(CPU_2MHZ) && !defined(CPU_1MHZ)
    #define CPU_8MHZ
#endif

    uses "/Source/Runtime/6809/ZeroPage"
    
    Initialize()
    {
        // VIA initialization assuming the RESB pulse was too short
        // "Reset clears all internal registers (except T1 and T2 counters and latches, and the SR"

        CLR ZP.DDRA    // set all pins on port A to input (good default in case we don't use it)
        CLR ZP.DDRB    // set all pins on port B to input (good default in case we don't use it)

        // start timer last since it generates interrupts
        // VIA interval timer to tick every 1ms or 1000 clock cycles

        CLR ZP.TICK0
        CLR ZP.TICK1
        CLR ZP.TICK2
        CLR ZP.TICK3
        
        LDA #0b01000000 // put the timer into free run mode
        STA ZP.ACR
      
        // 1 millisecond intervals:
        //   A CPU clock of 1 MHz = 1000 cycles - 2 = 998 / 0x03E6
        //   A CPU clock of 2 MHz would be 2x faster so this count would need to be 2000 cycles - 2 = 1998 / 0x07CE
        //   A CPU clock of 4 MHz would be 4x faster so this count would need to be 4000 cycles - 2 = 3998 / 0x0F9E
        //   A CPU clock of 8 MHz would be 8x faster so this count would need to be 8000 cycles - 2 = 7998 / 0x1F40
        
#ifdef CPU_1MHZ
        LDA #0xE6
        STA ZP.T1CL
        LDA #0x03
        STA ZP.T1CH
#endif      
#ifdef CPU_2MHZ
        LDA #0xCE
        STA ZP.T1CL
        LDA #0x07
        STA ZP.T1CH
#endif      
#ifdef CPU_4MHZ
        LDA #0x9E
        STA ZP.T1CL
        LDA #0x0F
        STA ZP.T1CH
#endif      
#ifdef CPU_8MHZ
        LDA #0x40
        STA ZP.T1CL
        LDA #0x1F
        STA ZP.T1CH
#endif      
        LDA #0b11000000 // set Timer1 bit in IER to put Timer1 into free run mode
        STA ZP.IER
    }
    
    ISR()
    {
        PSHS A, X, Y, U

        LDA ZP.IFR
        AND #0b10000000
        if (NZ) // IRQ by VIA
        {
            LDA ZP.IFR
            AND #0b01000000
            if (NZ) // Timer 1 IRQ
            {
                LDA ZP.T1CL   // clear the interrupt by reading T1 Counter L
                INC ZP.TICK0
                if (Z)
                {
                    INC ZP.TICK1
                    if (Z)
                    {
                        INC ZP.TICK2
                        if (Z)
                        {
                            INC ZP.TICK3
                        }
                    }
                }
            }
        }

        PULS A, X, Y, U
    }
}
