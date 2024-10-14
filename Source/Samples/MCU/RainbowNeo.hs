program RainbowNeo
{
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    
    uses "/Source/Library/NeoPixel"
    
    {
        //NeoPixel.BuiltIn();
        //uint totalPixels = NeoPixel.Length;
          
        
        NeoPixel.Begin(32 // number of LEDs
                     , Board.GP6  // data GPIO pin
                     , PixelType.GRB | PixelType.KHz800);
        NeoPixel.Brightness = 5; // maximum is 255
        NeoPixel.Clear();
        
        uint totalPixels = NeoPixel.Length;
        
        long hueLimit = 0x10000 * totalPixels;
        
        byte r;
        byte g;
        byte b;
        
        loop
        {   
            for (long firstPixelHue = 0; firstPixelHue < hueLimit; firstPixelHue += 256) 
            {
                for (byte pixel = 0; pixel < totalPixels; pixel++)
                {
                    
                    long pixelHue = firstPixelHue + (pixel * 0x10000 / totalPixels);
                    NeoPixel.HueToRGB(pixelHue, ref r, ref g, ref b);
                    NeoPixel.SetColor(pixel, r, g, b);
                }
                NeoPixel.Show();
                Time.Delay(100);
            }
        }
    }
}
