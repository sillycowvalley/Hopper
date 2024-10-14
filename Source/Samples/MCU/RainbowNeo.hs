program BlinkRGB
{
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    
    //uses "/Source/Library/NeoPixel"
    
    {
        //NeoPixel.BuiltIn();
        //uint totalPixels = NeoPixel.Length;
          
        
        NeoPixel.Begin(32 // number of LEDs
                     , Board.GP6  // data GPIO pin
                     , PixelType.GRB | PixelType.KHz800);
        NeoPixel.Brightness = 50; // maximum is 255
        NeoPixel.Clear();
        
        //uint totalPixels = 32;
        uint totalPixels = NeoPixel.Length;
        
        loop
        {
            for (byte pixel = 0; pixel < totalPixels; pixel++)
            {
                NeoPixel.SetColor(pixel, 255, 0, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "R");
                Delay(100);
                
                NeoPixel.SetColor(pixel, 0, 255, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "G");
                Delay(100);
                
                NeoPixel.SetColor(pixel, 0, 0, 255);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "B");
                Delay(100);
                
                NeoPixel.SetColor(pixel, 0, 0, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "0");
                Delay(100);
            }
        }
    }
}
