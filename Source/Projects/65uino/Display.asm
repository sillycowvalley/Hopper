unit Display
{    
    uses "RIOT"
    uses "I2C"
 
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
            STA Sprites.OutB
            I2C.ByteOut();
            INY
        }
        I2C.Stop();
    }
    
    GotoXY() // munts A, CellX and CellY
    {
        // CellY : 0..15
        // CellX : 0..31
        LDA CellY
        LSR        // /= 2
        STA CellY
        LDA CellX
        ASL ASL    // *= 4
        STA CellX
        
        // Page:   0..7
        // Column: 0..127
        
        I2C.Start();
        LDA # 0x00     // 0x00 for commands or 0x40 for data
        STA Sprites.OutB
        I2C.ByteOut();
        
        LDA CellY
        ORA # 0xB0     // set the page address
        STA Sprites.OutB
        I2C.ByteOut();
        
        LDA CellX
        LSR LSR LSR LSR
        ORA # 0x10     // set column high nibble
        STA Sprites.OutB
        I2C.ByteOut();
        
        LDA CellX
        AND # 0x0F     // set column low nibble
        STA Sprites.OutB
        I2C.ByteOut();
        I2C.Stop();
    }
    WriteCol() // A contains a column of bits
    {
        PHA
        
        I2C.Start();
        LDA # 0x40     // 0x00 for commands or 0x40 for data
        STA Sprites.OutB
        I2C.ByteOut();
        
        PLA
        STA Sprites.OutB
        I2C.ByteOut();
        
        I2C.Stop();
    }
    
    Clear()
    {
        LDY # 8
        loop
        {
            DEY
            
            LDA # 0
            STA CellX
            TYA
            ASL
            STA CellY
            GotoXY(); // Y : 14..0, X: 0
            
            I2C.Start();
            LDA # 0x40     // 0x00 for commands or 0x40 for data
            STA Sprites.OutB
            I2C.ByteOut();
            
            TYA PHA
            
            LDY 128
            loop
            {
                LDA # 0
                STA Sprites.OutB
                I2C.ByteOut();
                
                DEY
                if (Z) { break; }
            }
            PLA TAY
            if (Z) { break; }
        }
        I2C.Stop();
    }
}
