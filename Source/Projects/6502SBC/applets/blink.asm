program Blink
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Time"
    uses "System/GPIO"
    uses "System/Serial"
    
    const string blinkString = "Blink Applet\n";
    
    Hopper()
    {
        LDA #(blinkString % 256)
        STA ZP.STRL
        LDA #(blinkString / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #0 // 0 - builtin LED
        LDY # PINMODE.OUTPUT
        GPIO.PinMode();
        loop
        {
            LDA #0 // builtin LED
            LDY #1
            GPIO.PinWrite();
            
            // 500ms delay
            LDA # (500 % 256)
            STA ZP.TOP0
            LDA # (500 / 256)
            STA ZP.TOP1
            STZ ZP.TOP2
            STZ ZP.TOP3
            Time.Delay();
            
            LDA #0 // builtin LED
            LDY #0
            GPIO.PinWrite();
            
            // 500ms delay
            LDA # (500 % 256)
            STA ZP.TOP0
            LDA # (500 / 256)
            STA ZP.TOP1
            STZ ZP.TOP2
            STZ ZP.TOP3
            Time.Delay();
            
            LDA #'.'
            Print.Char();
            
            Serial.IsBreak();
            if (C) { break; }
        }
    }
}
