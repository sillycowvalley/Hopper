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
        
        byte r;
        byte g;
        byte b;
        
        loop
        {   
            for (uint firstPixelHue = 0; firstPixelHue < 256; firstPixelHue ++) 
            {
                for (byte pixel = 0; pixel < totalPixels; pixel++)
                {
                    uint pixelHue = firstPixelHue + (pixel * 256 / totalPixels);
                    NeoPixel.SetHue(pixel, byte(pixelHue % 256));
                }
                NeoPixel.Show();
            }
        }
    }
}
