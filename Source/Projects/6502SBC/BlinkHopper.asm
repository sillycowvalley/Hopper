program Blink
{
    #define CPU_8MHZ
    #define CPU_65C02S
    
    //#define BENEATER_IO
    #define X16_IO
    
    #define W65C22_VIA
    #define ACIA_6850
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Time"
    
    IRQ()
    {
        Serial.ISR();
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
        SEI
        
        Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
        
        LDA # 0b00000001  // LED as output
        STA ZP.DDRA
            
        loop
        {
            LDA # 0b00000000  // LED off
            STA ZP.PORTA
            
            LDA # '-'
            Serial.WriteChar();
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (1000 % 256)
            STA ZP.TOPL
            LDA # (1000 / 256)
            STA ZP.TOPH
            Time.Delay();
            
            LDA # 0b00000001  // LED on
            STA ZP.PORTA
            
            LDA # '+'
            Serial.WriteChar();
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (1000 % 256)
            STA ZP.TOPL
            LDA # (1000 / 256)
            STA ZP.TOPH
            Time.Delay();
        }
    }
}
