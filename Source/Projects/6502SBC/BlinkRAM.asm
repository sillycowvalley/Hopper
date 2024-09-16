program Blink
{
    #define CPU_65C02S
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    NMI()
    {
    }
    Delay()
    {
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
    Hopper()
    {
        SEI
        
        LDA # 0b00000001 // PA0 is output (LED)
        STA ZP.DDRA
               
        LDA # 0b00000000 // PB all inputs
        STA ZP.DDRB
        
        loop
        {
            LDA # 0b00000000  // LED off
            STA ZP.PORTA
            Delay();       
            
            LDA # 0b00000001  // LED on
            STA ZP.PORTA
            Delay(); 
        }
    }
}
