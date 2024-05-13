unit Display
{    
    uses "I2C.asm"
 
    const byte SSD1306_MEMORYMODE       = 0x20;
    const byte SSD1306_COLUMNADDR       = 0x21;
    const byte SSD1306_PAGEADDR         = 0x22;
    const byte SSD1306_DEACTIVATESCROLL = 0x2E;
    
    const byte SSD1306_SETSTARTLINE    = 0x40;
    
    const byte SSD1306_SETCONTRAST     = 0x81;
    const byte SSD1306_CHARGEPUMP      = 0x8D;
    const byte SSD1306_SEGREMAP        = 0xA0;
    const byte SSD1306_SETSEGMENTREMAP = 0xA1;
    const byte SSD1306_DISPLAYALLONRESUME = 0xA4;
    const byte SSD1306_DISPLAYALLON    = 0xA5;
    const byte SSD1306_NORMALDISPLAY   = 0xA6;
    const byte SSD1306_INVERTDISPLAY   = 0xA7;
    const byte SSD1306_SETMULTIPLEX    = 0xA8;
    const byte SSD1306_DISPLAYOFF      = 0xAE;
    const byte SSD1306_DISPLAYON       = 0xAF;
    
    const byte SSD1306_SETPAGEADDR     = 0xB0;
    
    const byte SSD1306_COMSCANINC      = 0xC0;
    const byte SSD1306_COMSCANDEC      = 0xC8;
        
    const byte SSD1306_SETDISPLAYOFFSET   = 0xD3;
    const byte SSD1306_SETDISPLAYCLOCKDIV = 0xD5;
    
    const byte SSD1306_SETPRECHARGE       = 0xD9;
    const byte SSD1306_SETCOMPINS         = 0xDA;
    const byte SSD1306_SETVCOMDETECT      = 0xDB;
    
    const byte[] ssd1306InitTable = {
                                        0x00,
                                        SSD1306_DISPLAYOFF,
                                        SSD1306_SETDISPLAYCLOCKDIV, 0x80,
                                        SSD1306_SETMULTIPLEX, 63, // PixelHeight-1
                                        SSD1306_SETDISPLAYOFFSET, 0x00,
                                        SSD1306_SETSTARTLINE, 
                                        SSD1306_CHARGEPUMP, 0x14,
                                        SSD1306_MEMORYMODE, 0x00,
                                        SSD1306_SETSEGMENTREMAP,
                                        SSD1306_COMSCANDEC,
                                        SSD1306_SETCOMPINS, 0x12,
                                        SSD1306_SETCONTRAST, 0xCF,
                                        SSD1306_SETPRECHARGE, 0xF1,
                                        SSD1306_SETVCOMDETECT, 0x40,
                                        SSD1306_DISPLAYALLONRESUME,
                                        SSD1306_NORMALDISPLAY,
                                        SSD1306_DEACTIVATESCROLL,
                                        SSD1306_DISPLAYON,
                                        0xFF // Stop byte
                                     };  
    Initialize()
    {
        CLC            // Write flag
        I2C.Start();
        LDY # 0
        loop
        {
            LDA ssd1306InitTable, Y
            CMP # 0xFF                // stop byte
            if (Z) { break; }
            STA ZP.OutB
            I2C.ByteOut();
            INY
        }
        I2C.Stop();
    }
    
    GotoXY()
    {
        // Y 0..7
        // X 0..127
        
        LDA # 0x3C // SSD1306 address
        STA ZP.I2CADDR
        CLC // write flag
        I2C.Start();
        LDA # 0x00     // 0x00 for commands or 0x40 for data
        STA ZP.OutB
        I2C.ByteOut();
        
        TYA
        ORA # 0xB0     // set the page address
        STA ZP.OutB
        I2C.ByteOut();
        
        TXA LSR LSR LSR LSR
        ORA # 0x10     // set column high nibble
        STA ZP.OutB
        I2C.ByteOut();
        
        TXA
        AND # 0x0F     // set column low nibble
        STA ZP.OutB
        I2C.ByteOut();
        I2C.Stop();
    }
    
    Clear()
    {
        LDY # 8
        loop
        {
            DEY
            
            LDX # 0
            GotoXY(); // Y : 7..0, X: 0
            
            LDA # 0x3C // SSD1306 address
            STA ZP.I2CADDR
            CLC // write flag
            I2C.Start();
            LDA # 0x40     // 0x00 for commands or 0x40 for data
            STA ZP.OutB
            I2C.ByteOut();
            
#ifdef CPU_65C02S
            PHY
#else
            TYA PHA
#endif
            
            LDY 128
            loop
            {
                LDA # 0
                STA ZP.OutB
                I2C.ByteOut();
                
                DEY
                if (Z) { break; }
            }
#ifdef CPU_65C02S            
            PLY
#else
            PLA TAY
#endif
            if (Z) { break; }
        }
        I2C.Stop();
    }
}
