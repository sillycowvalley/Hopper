program Blink
{
    #define CPU_65C02S
    #define ROM_8K                   // by default the CPLD is programmed for 8K ROM
    
    const uint MECB_IO  = 0xF000;
    
    // Motorola 6840 PTM (Programmable Timer Module)
    const uint PTM      = MECB_IO;
    const uint PTM_CR13 = PTM;       // Write: Timer Control Registers 1 & 3   Read: NOP
    const uint PTM_SR   = PTM+1;
    const uint PTM_CR2  = PTM+1;     // Write: Control Register 2              Read: Status Register (least significant bit selects TCR as TCSR1 or TCSR3)
    
    const uint PTM_T1MSB_LATCH = PTM+2;    // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint PTM_T1LSB_LATCH = PTM+3;    // Write: Timer #1 Latches                Read: LSB Buffer Register
    
    const uint PTM_T1MSB_TIMER = PTM+2;    // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint PTM_T1LSB_TIMER = PTM+3;    // Write: Timer #1 Latches                Read: LSB Buffer Register
    
    // Motorola 6821 PIA (Peripheral Interface Adapter)
    const uint PIA                  = MECB_IO + 0x0010;
    const uint PORTA                = PIA;   // Peripheral A Data Register
    const uint PORTB                = PIA+1; // Peripheral B Data Register
    const uint CRA                  = PIA+2; // Control Register A
    const uint CRB                  = PIA+3; // Control Register B
    
    const uint DDRA                 = PIA;   // Data Direction Register A (Shared with PORTA)
    const uint DDRB                 = PIA+1; // Data Direction Register B (Shared with PORTB)
    
    // Zero Page
    const byte TICK0                = 0x28;
    const byte TICK1                = 0x29;
    const byte TICK2                = 0x2A;
    const byte TICK3                = 0x2B;
    const byte TARGET0              = 0x2C;
    const byte TARGET1              = 0x2D;
    const byte TARGET2              = 0x2E;
    const byte TARGET3              = 0x2F;
    
    IRQ()
    {
        PHA
        LDA PTM_SR
        if (MI) // Interrupt flag, 0b10000000, set in the status register?
        {
            ROR A  // put bit 0 into Carry Flag
            if (C) // Timer 1 caused interrupt?
            {
                // Reading the timer LSB clears the interrupt flag and reset the counter
                LDA PTM_T1MSB_TIMER
                LDA PTM_T1LSB_TIMER
                
                INC TICK0 // Increment the Tick (on Zero Page)
                if (Z)
                {
                    INC TICK1
                    if (Z)
                    {
                        INC TICK2
                        if (Z)
                        {
                            INC TICK3
                        }
                    }
                }
            }
        }
        PLA
    }
    
    InitPTR()
    {
        SEI     // disable interrupts (seems like a bad idea to allow interrupts while configuring interrupts)
        
        // on a 1MHz clock we'd need ~1000 clock cycles to get 1ms period
        // on a 4MHz clock we'd need ~4000 clock cycles to get 1ms period
        LDA     # (4000 >> 8) // Set up the countdown timer for timer 1
        STA     PTM_T1MSB_LATCH   // MSB must be written first!
        LDA     # (4000 & 0xFF)
        STA     PTM_T1LSB_LATCH

        LDA     # 0b00000001 // Select CR1 
        STA     PTM_CR2
        LDA     # 0b01000010 // CRX6=1 (interrupt); CRX1=1 (enable clock)
        STA     PTM_CR13
        
        STA     TICK0        // Reset the tick counter
        STA     TICK1
        STA     TICK2
        STA     TICK3
        
        CLI     // enable interrupts
    }
    
    Delay500()
    {
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        CLC
        LDA # (500 & 0xFF) // LSB of our 500ms delay
        ADC TICK0      // reading TICK0 makes a snapshot of all 4 registers on the emulator
        STA TARGET0
        LDA # (500 >> 8)    // MSB of our 500ms delay
        ADC TICK1
        STA TARGET1
        LDA TICK2
        ADC #0
        STA TARGET2
        LDA TICK3
        ADC #0
        STA TARGET3
        
        loop
        {
            // while Ticks0..3 < Target0..3, loop here
            
            LDA TICK3
            CMP TARGET3
            if (NC) { continue; }
            LDA TICK2
            CMP TARGET2
            if (NC) { continue; }
            LDA TICK1
            CMP TARGET1
            if (NC) { continue; }
            LDA TICK0
            CMP TARGET0
            if (NC) { continue; }
            
            break; // Target >= Ticks : match ->
        }
    }
    
    Hopper()
    {
        LDA #0b00000000 // Select DDRA and clear interrupt flags in CRA
        STA CRA
        LDA #0b00000000 // Set PA0 of PORTA as output (DDRA)
        STA DDRA
        LDA #0b00000100 // Select PORTA
        STA CRA
       
        InitPTR();
        loop
        {
            LDA #0b00000100 // Select PORTA
            STA CRA
            LDA #0b00000001
            STA PORTA
            Delay500();
            
            LDA #0b00000100 // Select PORTA
            STA CRA
            LDA #0b00000000
            STA PORTA
            Delay500();
        }
    }
    
}
