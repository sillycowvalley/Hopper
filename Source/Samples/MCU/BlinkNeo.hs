program BlinkRGB
{
    //uses "/Source/Library/Boards/WaveshareRP2040Matrix"
    //uses "/Source/Library/Boards/AdafruitFeather"
    //uses "/Source/Library/Boards/SeeedXIAORP2040"
    uses "/Source/Library/Boards/AdafruitQTPy"
    
    {
        NeoPixel.BuiltIn();
      
        uint totalPixels = NeoPixel.Length;
        
        loop
        {
            for (byte pixel = 0; pixel < totalPixels; pixel++)
            {
                NeoPixel.SetColor(pixel, 255, 0, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "R");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 255, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "G");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 0, 255);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "B");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 0, 0);
                NeoPixel.Show();
                WriteLn(pixel.ToString() + "0");
                Delay(500);
            }
        }
    }
}
