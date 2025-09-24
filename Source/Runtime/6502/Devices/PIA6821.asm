unit PIA6821
{
    friend Parallel, Time;
    
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
        
        LDA #0b00000000 // Select DDRA
        STA CRA
        
        LDA #0b00000000 // Set all pins of PORTA as inputs (DDRA)
        STA DDRA
        
        LDA #0b00000100 // Select PRA (PORTA) and clear interrupt flags in CRA
        STA CRA
        
        LDA PORTA        // READ to clear interrupt flags! 
        
        LDA #0b00000000 // Clear all output latches
        STA PORTA       // This writes to output register even though pins are inputs
        
        LDA #0b00000000 // Select DDRB
        STA CRB
        
        LDA #0b00000000 // Set all pins of PORTB as inputs (DDRB)
        STA DDRB
        
        LDA #0b00000100 // Select PRB (PORTB) and clear interrupt flags in CRB
        STA CRB
        
        LDA PORTB       // READ to clear interrupt flags!
        
        LDA #0b00000000 // Clear all output latches
        STA PORTB       // This writes to output register even though pins are inputs
        
#ifdef M6840_PTM        
        // 1000us per sample = 1ms units for Time.Delay(..)
        LDA # 0xE8
        STA ZP.TOPL
        LDA # 0x03
        STA ZP.TOPH
        
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
        
        // Always write the MSB to the single MSB buffer register first:
        LDA ZP.TOPH          // Load MSB of 1000 cycles
        STA Timer1MSBBuffer  // Write to Timer 1 MSB register
        
        LDA ZP.TOPL          // Load LSB of 1000 cycles
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
        STA ZP.TOPH
        LDA Timer1LSBBuffer 
        STA ZP.TOPL
        
        // The timer counts down from n-1 to 0, including the 0 as part of the count
        // +1
        INC ZP.TOPL
        if (Z)
        {
            INC ZP.TOPH
        }
    }
}
