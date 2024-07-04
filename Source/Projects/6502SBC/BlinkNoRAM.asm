program Blink
{
    #define CPU_8MHZ
    #define CPU_65C02S
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    NMI()
    {
    }
    
    Hopper()
    {
        SEI
        
        LDA # 0b00000001 // PA0 is output (LED)
        STA ZP.DDRA
               
        LDA # 0b00000000 // PB all inputs
        STA ZP.DDRB
        
        LDY #0
        loop
        {
            LDA # 0b00000000  // LED off
            STA ZP.PORTA
            
            // Delay 256 x 256 loops:
            LDX # 0
            loop
            {
                LDY # 0
                loop
                {
                    DEY
                    if (Z) { break;} 
                }
                DEX
                if (Z) { break;} 
            }
            
            LDA # 0b00000001  // LED on
            STA ZP.PORTA
            
            // Delay 256 x 256 loops:
            LDX # 0
            loop
            {
                LDY # 0
                loop
                {
                    DEY
                    if (Z) { break;} 
                }
                DEX
                if (Z) { break;} 
            }
        }
    }
}
