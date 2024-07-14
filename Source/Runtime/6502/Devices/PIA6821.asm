unit PIA6821
{
    friend Parallel, Time;
    
    #define M6840_PTM
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    
    initialize()
    {
        // Motorola 6821 PIA
        // soft reset by zeroing all 6 registers (like a hard reset would do)
        
        LDA #0b00000000 // Select DDRA and clear interrupt flags in CRA
        STA CRA
        
        LDA #0b00000000 // Set all pins of PORTA as inputs (DDRA)
        STA DDRA
        
        LDA #0b00000100 // Select PRA (PORTA) and clear interrupt flags in CRA
        STA CRA
        
        LDA #0b00000000 // Select DDRB and clear interrupt flags in CRB
        STA CRB
        
        LDA #0b00000000 // Set all pins of PORTB as outputs (DDRB)
        STA DDRB
        
        LDA #0b00000100 // Select PRB (PORTB) and clear interrupt flags in CRB
        STA CRB
        
        // 1000us per sample = 1ms units for Time.Delay(..)
        LDA # 0xE8
        STA ZP.TOPL
        LDA # 0x03
        STA ZP.TOPH
        sharedSamplesMicroSet();
    }
    isr()
    {
        BIT ZP.TCSR1
        if (MI) // IRQ by Timer 1
        {
            // Clear the interrupt flag by writing to TCSR1
            PHA
            LDA TCSR1
            AND # 0b01111111 //Clear the interrupt flag (bit 7)
            STA TCSR1
            PLA
            
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
    
    sharedSamplesMicroSet()
    {
        // Motorola 6840 Timer
        LDA # 0b00000000            // Disable all timers during configuration
        STA ZP.TCR
        
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
        
        LDA #0b01000000      // Set Timer 1 to operate in continuous mode
        STA ZP.TCSR1            // Write to Timer 1 Status and Control Register
        
        LDA #0b00100000      // Enable Timer 1 interrupts
        STA ZP.TCSR1            // Write to Timer 1 Status and Control Register
        
        LDA ZP.TOPH          // Load MSB of 1000 cycles
        STA ZP.TIMER1_MSB       // Write to Timer 1 MSB register
        
        LDA ZP.TOPL          // Load LSB of 1000 cycles
        STA ZP.TIMER1_LSB       // Write to Timer 1 LSB register
        
        LDA #0b01000001      // Enable Timer 1 and set the prescaler if necessary
        STA ZP.TCSR1            // Write to Timer 1 Status and Control Register
        
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
        
        LDA #0b00000001      // Enable Timer 1
        STA TCR              // Write to Timer Control Register 
    }
    sharedSamplesMicroGet()
    {
        /* TODO
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
        */
    }
}
