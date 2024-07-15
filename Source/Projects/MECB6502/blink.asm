program Blink
{
    #define CPU_65C02S
    #define ROM_16K
    
    const uint MECB_IO  = 0xF000;
    
    // Motorola 6840 PTM (Programmable Timer Module)
    const uint PTM      = MECB_IO;
    const uint PTM_CR13 = PTM;       // Write: Timer Control Registers 1 & 3   Read: NOP
    const uint PTM_SR   = PTM+1;
    const uint PTM_CR2  = PTM+1;     // Write: Control Register 2              Read: Status Register (least significant bit selects TCR as TCSR1 or TCSR3)
    
    const uint PTM_T1MSB = PTM+2;    // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint PTM_T1LSB = PTM+3;    // Write: Timer #1 Latches                Read: LSB Buffer Register
    
    // Motorola 6850 ACIA
    //
    const uint ACIA         = MECB_IO + 0x0008;
    const uint ACIA_STATUS  = ACIA;       // Status
    const uint ACIA_CONTROL = ACIA;       // Control
    const uint ACIA_DATA    = ACIA+1;     // Data

    // Motorola 6821 PIA (Peripheral Interface Adapter)
    const uint PIA                  = MECB_IO + 0x0010;
    const uint PORTA                = PIA;   // Peripheral A Data Register
    const uint PORTB                = PIA+1; // Peripheral B Data Register
    const uint CRA                  = PIA+2; // Control Register A
    const uint CRB                  = PIA+3; // Control Register B
    // Data Direction Registers
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
    
    
    WriteChar()
    {
        PHA
        loop
        {
            LDA ACIA_STATUS  
            AND #0b00000010    // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA ACIA_DATA           // output character to TDRE
    }
    HexOut()
    {
        PHA
        PHA
        
        // most significant nibble
        LSR LSR LSR LSR
        CMP #0x0A
        if (C)
        {
            ADC #6 // add 7 (6+C=1)
        }
        ADC #'0'
        WriteChar();
        
        // least significant nibble
        PLA        
        AND #0x0F
        CMP #0x0A
        if (C)
        {
            ADC #6 // add 7 (6+C=1)
        }
        ADC #'0'
        WriteChar();
        
        PLA
    }
    
    
    NMI()
    {
    }
    
    IRQ()
    {
        BIT PTM_SR
        if (MI) // Interrupt flag, 0b10000000, set in the status register?
        {
            // Reading the timer clears the interrupt flag. Use the BIT 
            // instruction to 'read' without modifying any CPU registers.
            BIT PTM_T1MSB
            BIT PTM_T1LSB
            
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
    
    InitPTR()
    {
        SEI
        // on a 4MHz clock we'd need ~4000 clock cycles to get 1ms period
        LDA     (4000 >> 8) // Set up the countdown timer for timer 1
        STA     PTM_T1MSB   // MSB must be written first!
        LDA     (4000 & 0xFF)
        STA     PTM_T1LSB

        LDA     # 0x01      // Preset all timers
        STA     PTM_CR2     // Write to CR1
        LDA     # 0x42      // CRX6=1 (interrupt); CRX1=1 (enable clock)
        STA     PTM_CR13
        LDA     # 0x00
        STZ     PTM_CR2
        STZ     TICK0       // Reset the tick counter
        STZ     TICK1
        STZ     TICK2
        STZ     TICK3

        LDA     PTM_SR      // Read the interrupt flag from the status register
        CLI
    }
    
    Delay500()
    {
        // add ArgumentWord to Ticks0..3 and store in Target0..3
        CLC
        LDA (500 & 0xFF) // LSB of our 500ms delay
        ADC TICK0      // reading TICK0 makes a snapshot of all 4 registers on the emulator
        STA TARGET0
        LDA (500 >> 8)    // MSB of our 500ms delay
        ADC TICK1
        STA TARGET1
        LDA TICK2
        ADC #0 // to collect the carry
        STA TARGET2
        LDA TICK3
        ADC #0 // to collect the carry
        STA TARGET3
        
        loop
        {
            // while Ticks0..3 < Target0..3, loop here
            
            LDA TICK3   // reading TICK3 makes a snapshot of all 4 registers on the emulator      
            CMP TARGET3
            if (NC) { continue; }
            LDA TICK2
            CMP TARGET2
            if (NC) { continue; }
            LDA TICK1
            CMP TARGET1
            if (NC) { continue; }
            //LDA ZP.ACCL
            LDA TICK0
            CMP TARGET0
            if (NC) { continue; }
            
            break; // Target >= Ticks : match ->
        }
    }
    TimerBlink()
    {
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
        
    DelayCounters()
    {
        LDX #0
        loop {
            LDY #0
            loop {
                DEY
                if (Z) { break; }
            }
            DEX
            if (Z) { break; }
        }
    }
        
    LoopBlink()
    {
        loop
        {
            LDA #0b00000001
            STA PORTA
            DelayCounters();
            LDA # '+'
            WriteChar();
            
            LDA #0b00000000
            STA PORTA
            DelayCounters();
            
            LDA # '-'
            WriteChar();
        }
    }
    Hopper()
    {
        LDA #0b00000011        // reset the 6850
        STA ACIA_CONTROL
        
        LDA # 0b00010101       // 8-N-1, (/16 for 4.9152MHz), no rx interrupt
        //ORA #0b10000000      // has rx interrupt
        
        STA ACIA_CONTROL
        
        LDA # 0x0A
        WriteChar();
        LDA # ':'
        WriteChar();
        
        
        LDA #0b00000000 // Select DDRA and clear interrupt flags in CRA
        STA CRA
        LDA #0b00000000 // Set PA0 of PORTA as output (DDRA)
        STA DDRA
        LDA #0b00000100 // Select PORTA
        STA CRA
        
        LDA # '<'
        WriteChar();
        InitPTR();
        LDA # '>'
        WriteChar();
        
        loop
        {
            DelayCounters();
            DelayCounters();
            DelayCounters();
            DelayCounters();
            
            LDA # 0x0A
            WriteChar();
            LDA TICK3
            HexOut();
            LDA TICK2
            HexOut();
            LDA TICK1
            HexOut();
            LDA TICK0
            HexOut();
        }
        
        //LoopBlink();
        //TimerBlink();
    }
    
}
