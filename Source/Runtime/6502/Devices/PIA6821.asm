unit PIA6821
{
    friend Parallel;
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    // Motorola 6821
    initialize()
    {
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
    }
    isr()
    {
    }   
}
