program StackTest
{
    #define CPU_65C02S
    #define ROM_16K
    
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
        
        LDA # 0b00000001  // LED on
        STA ZP.PORTA
        
        
        // verify the RAM in the Stack page:
        LDX #0
        loop
        {
            DEX
            
            LDA # 0x55
            STA 0x100, X
            LDA 0x100, X
            CMP # 0x55
            if (NZ)
            {
                return;
            }
            LDA # 0xAA
            STA 0x100, X
            LDA 0x100, X
            CMP # 0xAA
            if (NZ)
            {
                return;
            }
            
            CPX #0
            if (Z) { break; }
        }
        LDA # 0b00000000  // LED off
        STA ZP.PORTA
        
        // On success, blink the light
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
