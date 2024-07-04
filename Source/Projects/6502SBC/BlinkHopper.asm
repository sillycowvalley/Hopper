program Blink
{
    #define CPU_8MHZ
    #define CPU_65C02S
    
    #define W65C22_VIA
    //#define ACIA_6850
    
    //uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Time"
    
    IRQ()
    {
        //Serial.ISR();
        W65C22.ISR();
    }
    NMI()
    {
        // clear the timer tick
        STZ ZP.TICK0
        STZ ZP.TICK1
        STZ ZP.TICK2
        STZ ZP.TICK3
        
        // don't hang if we are currently inside Time.Delay(), rather just exit:
        STZ ZP.TARGET0
        STZ ZP.TARGET1
        STZ ZP.TARGET2
        STZ ZP.TARGET3
    }
    Hopper()
    {
        //Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
        
        RMB0 ZP.DDRB   // PB0 as input
        RMB1 ZP.DDRB   // PB1 as input
        RMB0 ZP.PORTB  // PB0 set LOW
        RMB1 ZP.PORTB  // PB1 set LOW
        
        loop
        {
            RMB0 ZP.DDRB // PB0 as input
            SMB1 ZP.DDRB // PB1 as output (should be LOW)
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (2000 % 256)
            STA ZP.TOPL
            LDA # (2000 / 256)
            STA ZP.TOPH
            Time.Delay();
            
            SMB0 ZP.DDRB // PB0 as output (should be LOW)
            RMB1 ZP.DDRB // PB1 as input
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (2000 % 256)
            STA ZP.TOPL
            LDA # (2000 / 256)
            STA ZP.TOPH
            Time.Delay();
        }
    }
}
