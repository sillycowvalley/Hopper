program ABNielsen
{
    // https://wilsonminesco.com/6502primer/GENRLI2C.ASM
    
    // https://github.com/hauerdie/6502_i2c/blob/master/i2c.s
    
    #define CPU_8MHZ
    #define CPU_65C02S
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Time"
    
    uses "I2C.asm"
    uses "SSD1306.asm"
    
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
        
        LDA # 250
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        Time.Delay();
        
        LDA # 0x3C      // Address of the device (0x78 on the back of the module is 0x3C << 1)
        STA ZP.I2CADDR
        SSD1306.Initialize();
        
        loop
        {
            // use the Hopper runtime Time.Delay() (VIA timer)
            LDA # 250
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.Delay();
            
            LDA # 0xFF
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            SSD1306.Clear();               
            
            LDA # 250
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.Delay();
            
            LDA # 0xAA
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            SSD1306.Clear();  
            
            LDA # 250
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.Delay();
            
            LDA # 0x00
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            SSD1306.Clear(); 
         }
    }
}
