unit SSD1306
{
    uses "I2C"
    
    const byte[] ssd1306InitTable = {
                                0xAE,   // Turn off display
                                0xD5,   // set display clock divider
                                0xF0,   // 0x80 default - $f0 is faster for less tearing
                                0xA8,   // set multiplex
                                0x3F,   // for 128x64
                                0x40,   // Startline 0
                                0x8D,   // Charge pump
                                0x14,   // VCCstate 14
                                0xA1,   // Segment remap
                                0xC8,   // Com output scan direction
                                0x20,   // Memory mode
                                0x00,   //
                                0xDA,   // Com-pins
                                0x12,
                                0xFE,   // Set contrast - full power!
                                0x7F,   // About half
                                0xD9,   // Set precharge
                                0x11,   // VCC state 11
                                0xA4,   // Display all on resume
                                0xAF,   // Display on
                                //0xA5,   // Entire display ON  A5 Enable / A4 Disable 
                                0xB0, 0x10, 0x00, // Page 0, column 0.
                                0xFF // Stop byte
                              };
    
    byte cursor;
    byte tflags;
    byte scroll;
    
    Initialize()
    {
        I2C.Start(true);
        uint i;
        loop
        {
            byte value = ssd1306InitTable[i];
            if (value == 0xFF) { break; }
            _ = I2C.ByteOut(value);
            i++;
        }
        I2C.Stop();
    }
    
    cmd(byte command)
    {
        I2C.Address = 0x3C;
        I2C.Start(true);
        _ = I2C.ByteOut(0x00);    // 0x00 for commands or 0x40 for data
        _ = I2C.ByteOut(command);
    }
    
    Clear(byte colour)
    {
        cursor = 0;
        tflags = 0;
        SetLine(0);
        SetColumn(0);
        I2C.Start(true);
        _ = I2C.ByteOut(0x40); // 0x00 for commands or 0x40 for data
        for (uint i=0; i < 256; i++)
        {
            _ = I2C.ByteOut(colour);
            _ = I2C.ByteOut(colour);
            _ = I2C.ByteOut(colour);
            _ = I2C.ByteOut(colour);
        }
        I2C.Stop();
        
        cmd(0xD3); // Clear scroll
        scroll = 0;
        _ = I2C.ByteOut(0x00);
        I2C.Stop();
    }
    SetLine(byte line)
    {
        cmd(0x22); // Set page cmd
        _ = I2C.ByteOut(line);
        _ = I2C.ByteOut(0x07); // Ensure range
        I2C.Stop(); 
    }
    SetColumn(byte column)
    {
        cmd(0x21); // Set column command (0-127)
        _ = I2C.ByteOut(column << 3); // 15 << 3 = 120
        _ = I2C.ByteOut(0x7F); // 127
        I2C.Stop();
    }
    
}
