program Blink
{
    #define CPU_65UINO
    
    uses "RIOT"
    uses "Utilities"
    uses "I2C"
    uses "SSD1306"
        
    Hopper()
    {
        LDA # 0b00000000 // PA all input
        STA RIOT.DDRA
        
        LDA # 0b10000000 // PB7 is output (GREEN LED)
        STA RIOT.DDRB
        
        
        LDA # 0x3C            // Address of the device (78 on the back of the module is 3C << 1)
        STA ZP.I2CADDR
        SSD1306.Initialize(); // Initialize SSD1306 display
        SSD1306.Clear();
        
        loop
        {
        }
    }
}
