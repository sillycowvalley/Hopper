program RainbowNeo
{
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    //uses "/Source/Library/Boards/WaveshareRP2040Matrix"
    //uses "/Source/Library/Boards/PimoroniPlasma2350"
    
    //#define RAINBOW_LOW

    RainbowLow()
    {
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
    Rainbow8x4()
    {
        uint width = 8;
        loop
        {
            for (uint firstPixelHue = 0; firstPixelHue < 256; firstPixelHue++)
            {
                for (byte pixel = 0; pixel < width; pixel++)
                {
                   NeoPixel.SetHue(pixel+0,  byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+8,  byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+16, byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+24, byte(firstPixelHue + pixel));
                }        
                NeoPixel.Show();
                Time.Delay(100);   
            }
        }
    }
    Rainbow5x5()
    {
        uint width = 5;
        loop
        {
            for (uint firstPixelHue = 0; firstPixelHue < 256; firstPixelHue++)
            {
                for (byte pixel = 0; pixel < width; pixel++)
                {
                   NeoPixel.SetHue(pixel+0,  byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+5,  byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+10, byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+15, byte(firstPixelHue + pixel));
                   NeoPixel.SetHue(pixel+20, byte(firstPixelHue + pixel));
                }        
                NeoPixel.Show();
                Time.Delay(100);   
            }
        }
    }
    Rainbow10x1()
    {
        uint width = 10;
        loop
        {
            for (uint firstPixelHue = 0; firstPixelHue < 256; firstPixelHue++)
            {
                for (byte pixel = 0; pixel < width; pixel++)
                {
                   NeoPixel.SetHue(pixel,  byte(firstPixelHue + pixel));
                }        
                NeoPixel.Show();
                Time.Delay(100);
                uint angle = uint(360.0 * firstPixelHue / 255);
                IO.WriteLn(angle.ToString());
                if (angle % 30 == 0)
                {
                    int stopHere = 0;
                }
            }
        }
    }
    
    AdafruitMatrix()
    {
        NeoPixel.Begin(8*4, Board.GP6, PixelType.GRB | PixelType.KHz800);
        NeoPixel.Brightness = 5; // maximum is 255
        NeoPixel.Clear();
        
#ifdef RAINBOW_LOW        
        RainbowLow();
#else
        Rainbow8x4();
#endif
    }
    WaveshareMatrix()
    {
        NeoPixel.BuiltIn();
        NeoPixel.Brightness = 10; // maximum is 255
        NeoPixel.Clear();
     
#ifdef RAINBOW_LOW     
        RainbowLow();   
#else
        Rainbow5x5();
#endif
    }
    PimoroniPlasma()
    {
        NeoPixel.Begin(10, Board.BuiltInNeoPixel, PixelType.RGB | PixelType.KHz800);
        NeoPixel.Brightness = 50; // maximum is 255
        NeoPixel.Clear();

#ifdef RAINBOW_LOW     
        RainbowLow();   
#else
        Rainbow10x1();
#endif
    }
    Hopper()
    {    
#if !defined(WAVESHARE_RP2040_MATRIX) && !defined(PIMORONI_PLASMA2050)
        AdafruitMatrix();
#endif
#if defined(WAVESHARE_RP2040_MATRIX)
        WaveshareMatrix();       
#endif
#if defined(PIMORONI_PLASMA2050)
        PimoroniPlasma();
#endif       
    }
}
