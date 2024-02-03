unit NeoPixelDeviceDriver
{
    #define NEOPIXEL_DEVICE_DRIVER
    uses "/Source/Library/NeoPixel"
    
    Begin()
    {
        NeoPixel.Begin(32 // number of LEDs
                     , 8  // data GPIO pin
                     , PixelType.GRB | PixelType.KHz800);
        NeoPixel.Brightness = 6;
        NeoPixel.Clear();
    }
    
}
