program BlinkRGB
{
    //#define WAVESHARE_RP2040_ONE
    #define WAVESHARE_RP2040_MATRIX
    //#define ADAFRUIT_METRO_RP2040
    //#define ADAFRUIT_FEATHER_RP2040
    //#define SPARKFUN_THING_PLUS_RP2040
    //#define SEEED_XIAO_RP2040
    //#define CHALLENGER_RP2040_WIFI

#ifdef WAVESHARE_RP2040_MATRIX
    const uint totalPixels = 25; 
#else
    const uint totalPixels = 1; 
#endif        
            
    uses "/Source/Library/MCU"
    
    {
        NeoPixel.BuiltIn();
      
        
        loop
        {
            for (byte pixel = 0; pixel < totalPixels; pixel++)
            {
                NeoPixel.SetColor(pixel, 255, 0, 0);
                NeoPixel.Show();
                WriteLn("R");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 255, 0);
                NeoPixel.Show();
                WriteLn("G");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 0, 255);
                NeoPixel.Show();
                WriteLn("B");
                Delay(500);
                
                NeoPixel.SetColor(pixel, 0, 0, 0);
                NeoPixel.Show();
                WriteLn("0");
                Delay(500);
            }
        }
    }
}
