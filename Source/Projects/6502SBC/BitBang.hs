program BitBang
{
    uses "/Source/Library/Boards/PiPico"
    
    uses "I2C"
    uses "SSD1306"
    
    Hopper()
    {
        // good defaults
        I2C.Address = 0x3C; // address of the device (78 on the back of the module is 3C << 1)
        
        I2C.Initialize();
        
        loop
        {
            Delay(250);
            SSD1306.Initialize();
            Delay(250);
            SSD1306.Clear(0xFF);
            Delay(250);
            SSD1306.Clear(0xAA);
            Delay(250);
            SSD1306.Clear(0x00);
        }
    }
}
