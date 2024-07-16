program Blink
{
    #define CPU_65C02S
    #define ROM_8K                  // by default the CPLD is programmed for 8K ROM
    
    const uint MECB_IO  = 0xF000;   // yours is most likely at 0xE000

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
    
    Delay()
    {
        //       JSR : 6 cycles
        PHA         // 4 cycles
        LDA # 185   // 2 cycles
        STA TICK0   // 3 cycles  
        LDA # 151   // 2 cycles
        STA TICK1   // 3 cycles  
        LDA # 4     // 2 cycles   // for a mello blink, make this #1 for 1 MHz and #4 for 4MHz
        STA TICK2   // 3 cycles  
        PLA         // 4 cycles
        loop
        {
            loop
            {
                loop
                {
                    DEC TICK0           // 5 cycles
                    if (Z) { break; }   // 2 cycles, 3 if taken
                }
                DEC TICK1               // 5 cycles
                if (Z) { break; }       // 5 cycles, 3 if taken
            }
            DEC TICK2                   // 5 cycles
            if (Z) { break; }           // 5 cycles, 3 if taken
        }
        // RTS : 6 cycles
    }
        
    Hopper()
    {
        LDA #0b00000000 // Select DDRA and clear interrupt flags in CRA
        STA CRA
        LDA #0b00000001 // Set PA0 of PORTA as output (DDRA)
        STA DDRA
        LDA #0b00000100 // Select PORTA
        STA CRA
        
        loop
        {
            LDA #0b00000001
            STA PORTA
            Delay();
            
            LDA #0b00000000
            STA PORTA
            Delay();
        }
    }
    
}
