unit DeviceDriver
{
    #define NEOPIXEL_DEVICE_DRIVER
    uses "/Source/Library/Displays/MatrixDriver"
    
    const int PW = 8;
    const int PH = 4;
    uint[PW*PH] pixelBuffer;
    
    UpdateDisplay()
    {
        for (uint address = 0; address < PW*PH; address++)
        {
            uint rgb444 = pixelBuffer[address];
            byte rColor = byte((rgb444 >> 8) << 4);
            byte gColor = byte(((rgb444 >> 4) & (0x0F)) << 4);
            byte bColor = byte((rgb444 & 0x0F) << 4);
            if ((rColor & 0x10) != 0)
            {
                rColor |= 0x0F;
            }
            if ((gColor & 0x10) != 0)
            {
                gColor |= 0x0F;
            }
            if ((bColor & 0x10) != 0)
            {
                bColor |= 0x0F;
            }
            NeoPixel.SetColor(address, rColor, gColor, bColor);
        }
        NeoPixel.Show();
    }
    
    SetPixel(int x, int y, uint rgb444)
    {
        uint address = uint(y * PW + x);
        pixelBuffer[address] = (rgb444 == Colour.Invert) ? ~pixelBuffer[address] : rgb444;
    }
    
    bool Begin()
    {
        NeoPixel.Begin(32 // number of LEDs
                     , 8  // data GPIO pin
                     , PixelType.GRB | PixelType.KHz800);
        NeoPixel.Brightness = 1;
        NeoPixel.Clear();
        
        return Display.Begin();
    }
    
}
