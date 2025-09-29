unit PIA6821
{
    friend Parallel, Time;
    
#if !defined(CPU_8MHZ) && !defined(CPU_4MHZ) && !defined(CPU_2MHZ) && !defined(CPU_1MHZ)
    #define CPU_8MHZ
#endif
    
#ifdef M6840_PTM    
    // Better Register Alias:
    const uint Timer1Control   = TCR;
    const uint Timer3Control   = TCR;
    const uint Timer2Control   = TCSR2; // (least significant bit selects TCR as Timer1Control (1) or Timer3Control (0))
    const uint TimerStatus     = TCSR2;
    
    const uint Timer1MSBBuffer = TIMER1_MSB;
    const uint Timer1LSBLatch  = TIMER1_LSB;
    const uint Timer1Counter   = TIMER1_MSB;
    const uint Timer1LSBBuffer = TIMER1_LSB;
    
    const uint Timer2MSBBuffer = TIMER2_MSB;
    const uint Timer2LSBLatch  = TIMER2_LSB;
    const uint Timer2Counter   = TIMER2_MSB;
    const uint Timer2LSBBuffer = TIMER2_LSB;
    
    const uint Timer3MSBBuffer = TIMER3_MSB;
    const uint Timer3LSBLatch  = TIMER3_LSB;
    const uint Timer3Counter   = TIMER3_MSB;
    const uint Timer3LSBBuffer = TIMER3_LSB;
#endif

    initialize()
    {
        // Motorola 6821 PIA
        // soft reset by zeroing all 6 registers (like a hard reset would do)
        
        //LDA #0b00000000 // Select DDRA
        LDA #0b00110000 // Bits 5-3=110 (CA2 output high), bit 2=0 (select DDRA)
        STA CRA
        
        LDA #0b00000000 // Set all pins of PORTA as inputs (DDRA)
        STA DDRA
        
        //LDA #0b00000100 // Select PRA (PORTA) and clear interrupt flags in CRA
        LDA #0b00110100 // Bits 5-3=110 (CA2 output high), bit 2=1 (select PORTA)
        STA CRA
        
        LDA PORTA        // READ to clear interrupt flags! 
        
        LDA #0b00000000 // Clear all output latches
        STA PORTA       // This writes to output register even though pins are inputs
        
        LDA #0b00110000 // Select DDRB, configure CB2 as output high (like CA2)
        STA CRB
        
        LDA #0b00000000 // Set all pins of PORTB as inputs (DDRB)
        STA DDRB
        
        LDA #0b00110100 // Select PRB (PORTB), maintain CB2 as output high
        STA CRB
        
        LDA PORTB       // READ to clear interrupt flags!
        
        LDA #0b00000000 // Clear all output latches
        STA PORTB       // This writes to output register even though pins are inputs
        
#ifdef M6840_PTM        
        // 1000us per sample = 1ms units for Time.Delay(..)
        LDA # 0xE8
        STA ZP.TOP0
        LDA # 0x03
        STA ZP.TOP1
        
        sharedSamplesMicroSet();
#endif
    }
    isr()
    {
#ifdef M6840_PTM        
        BIT TimerStatus
        if (MI) // IRQ by Timer 1, 2 or 3
        {
            PHA
            LDA TimerStatus
            AND # 0b00000001 // Timer 1 interrupt
            if (NZ)
            {
                // Read Timer 1 counter to clear interrupt
                BIT Timer1Counter
                BIT Timer1LSBBuffer
                
                // Increment the Tick
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
            PLA
        }
#endif        
    }   
    
    sharedSamplesMicroSet()
    {
        // Motorola 6840 Timer
        SEI
        
        // Zero the tick counter
#ifdef CPU_65C02S
        STZ ZP.TICK0
        STZ ZP.TICK1
        STZ ZP.TICK2
        STZ ZP.TICK3
#else
        LDA #0
        STA ZP.TICK0
        STA ZP.TICK1
        STA ZP.TICK2
        STA ZP.TICK3
#endif        
        
        // At a CPU clock of 1 mHz = 1000 cycles - 1 = 999 / 0x03E7 would give us a sample cycle of 1ms
#if defined(CPU_2MHZ) || defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // x2
        ASL ZP.TOP0
        ROL ZP.TOP1        
#endif  
#if defined(CPU_4MHZ) || defined(CPU_8MHZ)
        // x4
        ASL ZP.TOP0
        ROL ZP.TOP1        
#endif          
#if defined(CPU_8MHZ)
        // x8
        ASL ZP.TOP0
        ROL ZP.TOP1        
#endif       
        // The timer counts down from n-1 to 0, including the 0 as part of the count
        // -1
        LDA ZP.TOP0
        if (Z)
        {
            DEC ZP.TOP1
        }
        DEC ZP.TOP0
        
        // Always write the MSB to the single MSB buffer register first:
        LDA ZP.TOP1          // Load MSB of 1000 cycles
        STA Timer1MSBBuffer  // Write to Timer 1 MSB register
        
        LDA ZP.TOP0          // Load LSB of 1000 cycles
        STA Timer1LSBLatch   // Write to Timer 1 LSB register
        
        
        // Select Timer 1:
        LDA # 0b00000001            // Select Timer 1 Control Register, preset all timers
        STA Timer2Control
        
        // CR10 = 0 - all timers are allowed to operate
        // CR21 = 0 - use external clock source
        // CR22 = 0 - normal 16 bit counting mode
        
        // CR23 = 0 - continuous mode (write to latches or reset causes initialization)
        // CR24 = 0
        // CR25 = 0
        
        // CR26 = 1 - interrupt flag enabled to IRQ
        // CR27 - 0 - output masked on output OX
     
        LDA # 0b01000010            // interrupt enabled, external clock source
        STA Timer1Control
        
        LDA # 0b00000000
        STA Timer2Control

        CLI
    }
    sharedSamplesMicroGet()
    {
        LDA Timer1Counter 
        STA ZP.TOP1
        LDA Timer1LSBBuffer 
        STA ZP.TOP0
        
        // The timer counts down from n-1 to 0, including the 0 as part of the count
        // +1
        INC ZP.TOP0
        if (Z)
        {
            INC ZP.TOP1
        }
    }
}

