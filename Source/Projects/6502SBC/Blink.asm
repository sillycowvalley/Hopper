program Blink
{
    #define CPU_8MHZ
    #define CPU_65C02S
    
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
        Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
        
        LDA # 0b10000000     // bit 7 as output for the LED
        STA ZP.DDRB
        
        loop
        {
            // blink an LED on the VIA PB7
            
            if (BBS7, ZP.PORTB)
            {
                RMB7 ZP.PORTB
            }    
            else
            {
                SMB7 ZP.PORTB
            }
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (500 % 256)
            STA ZP.TOPL
            LDA # (500 / 256)
            STA ZP.TOPH
            Time.Delay();
                       
            // use the Hopper runtime Time.Seconds() (VIA timer) 
            Time.Seconds();
            
            // use the Hopper runtime Serial.WriteChar() and Serial.OutHex() (Motorola 6850)
            LDA TOPH
            Serial.HexOut();
            LDA TOPL
            Serial.HexOut();
            LDA # 0x0A
            Serial.WriteChar();       
        }
    }
}
