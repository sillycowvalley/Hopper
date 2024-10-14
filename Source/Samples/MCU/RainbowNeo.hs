program RainbowNeo
{
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    //uses "/Source/Library/Boards/WaveshareRP2040Matrix"

    Hopper()
    {    
        
        NeoPixel.Begin(8*4, Board.GP6, PixelType.GRB | PixelType.KHz800);
        //NeoPixel.BuiltIn();
        
        NeoPixel.Brightness = 50; // maximum is 255
        NeoPixel.Clear();
        
        uint totalPixels = NeoPixel.Length;
        
        loop
        {   
            for (uint firstPixelHue = 0; firstPixelHue < 256; firstPixelHue ++) 
            {
                for (byte pixel = 0; pixel < totalPixels; pixel++)
                {
                    uint pixelHue = firstPixelHue + (pixel * 255 / totalPixels);
                    pixelHue = pixelHue % 256;
                    NeoPixel.SetHue(pixel, byte(pixelHue));
                }
                NeoPixel.Show();
                Time.Delay(10);
            }
        }
    }
}
