program BlinkRGB
{
    //#define WAVESHARE_RP2040_ONE
    //#define WAVESHARE_RP2040_MATRIX
    //#define ADAFRUIT_METRO_RP2040
    //#define ADAFRUIT_FEATHER_RP2040
    //#define SPARKFUN_THING_PLUS_RP2040
    //#define SEEED_XIAO_RP2040
    #define CHALLENGER_RP2040_WIFI
    //#define MAKER_NANO_RP2040
            
    uses "/Source/Library/MCU"
    
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
