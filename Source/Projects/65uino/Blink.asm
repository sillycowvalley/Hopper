program Blink
{
    #define CPU_65UINO
    
    uses "RIOT"
    uses "Utilities"
        
    Delay500()
    {
        // 1024 ticks x A = 1.024ms X A (at a CPU speed of 1MHz)
        // 244 x 2 = 488 x 1.024 = 499.712ms, close enough
        LDA # 244
        DelayLong();
        LDA # 244
        DelayLong();
    }
    
    Hopper()
    {
        LDA # 0b00000000 // PA all input
        STA RIOT.DDRA
        
        LDA # 0b10000000 // PB7 is output (GREEN LED)
        STA RIOT.DDRB
        
        LDA 0b00000000  // GREEN LED on to start
        STA RIOT.DRB
        loop
        {
            BIT RIOT.DRB // read BTN
            if (NV)
            {
                LDA 0b00000000  // set GREEN LED on
                STA RIOT.DRB
                Delay500();
            }
            LDA 0b10000000  // set GREEN LED off
            STA RIOT.DRB
            Delay500();
        }
    }
}
