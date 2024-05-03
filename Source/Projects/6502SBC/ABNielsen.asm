program ABNielsen
{
    #define CPU_8MHZ
    #define CPU_65C02S
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Time"
    
    runtimeInit()
    {
        Stacks.Initialize(); // we need the runtime stacks to work to use runtime APIs like Time.Delay()
        Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
    }
    delay500()
    {
        LDA # (500 % 256)
        STA ZP.TOPL
        LDA # (500 / 256)
        STA ZP.TOPH
        PushTop();
        Time.Delay();
    }
    
    IRQ()
    {
        Serial.ISR();
        W65C22.ISR();
    }
    NMI()
    {
        // do nothing if NMI button is pressed
    }
    Hopper()
    {
        runtimeInit();
        
        LDA # 0b10000000  // bit 7 as output for the LED
        STA ZP.DDRB
        
        LDA # 0b00000001  // bit 1 as output for testing
        STA ZP.DDRA
        STZ ZP.PORTA
        
        loop
        {
            LDA ZP.PORTB
            if (MI)
            {
                AND # 0b01111111
            }    
            else
            {
                ORA # 0b10000000
            }
            STA ZP.PORTB
            
            delay500(); // use the Hopper runtime delay (VIA timer)
        }
    }
}
