program Blink
{
    #define CPU_8MHZ
    #define CPU_6502
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    Hopper()
    {
        LDA # 0b00000001 // PA0 is output (LED)
        STA ZP.DDRA
               
        LDA # 0b00000000 // PB7 is output (GREEN LED)
        STA ZP.DDRB
        
        loop
        {
            LDA # 0b00000000  // LED off
            STA ZP.PORTA
            
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
