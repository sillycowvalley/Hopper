program Blink
{
    #define CPU_8MHZ
    #define CPU_6809

    uses "/Source/Projects/6809/Serial"
    uses "/Source/Projects/6809/Devices/W65C22"
    uses "/Source/Projects/6809/Time"
    
    IRQ()
    {
        Serial.ISR();
        W65C22.ISR();
    }

    NMI()
    {
        // clear the timer tick
        CLR ZP.TICK0
        CLR ZP.TICK1
        CLR ZP.TICK2
        CLR ZP.TICK3
        
        // don't hang if we are currently inside Time.Delay(), rather just exit:
        CLR ZP.TARGET0
        CLR ZP.TARGET1
        CLR ZP.TARGET2
        CLR ZP.TARGET3
    }

    Hopper()
    {
        Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
        
        BCLR ZP.DDRB, #0x01   // PB0 as input
        BCLR ZP.DDRB, #0x02   // PB1 as input
        BCLR ZP.PORTB, #0x01  // PB0 set LOW
        BCLR ZP.PORTB, #0x02  // PB1 set LOW
        
        loop
        {
            BCLR ZP.DDRB, #0x01 // PB0 as input
            BSET ZP.DDRB, #0x02 // PB1 as output (should be LOW)
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (2000 % 256)
            STA ZP.TOPL
            LDA # (2000 / 256)
            STA ZP.TOPH
            JSR Time.Delay
            
            BSET ZP.DDRB, #0x01 // PB0 as output (should be LOW)
            BCLR ZP.DDRB, #0x02 // PB1 as input
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # (2000 % 256)
            STA ZP.TOPL
            LDA # (2000 / 256)
            STA ZP.TOPH
            JSR Time.Delay
                 
            /*      
            // use the Hopper runtime Time.Seconds() (VIA timer) 
            JSR Time.Seconds
            
            // use the Hopper runtime Serial.WriteChar() and Serial.HexOut() (Motorola 6850)
            LDA ZP.TOPH
            JSR Serial.HexOut
            LDA ZP.TOPL
            JSR Serial.HexOut
            LDA # 0x0A
            JSR Serial.WriteChar
            */
        }
    }
}
