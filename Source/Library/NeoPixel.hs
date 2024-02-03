unit NeoPixel
{
   /*  'PixelType' definition below came from the Adafruit library:
    *
    *  https://github.com/adafruit/Adafruit_NeoPixel/
    *
    * Adafruit invests time and resources providing this open source code,
    * please support Adafruit and open-source hardware by purchasing products
    * from Adafruit!
    *
    *
    * Adafruit_NeoPixel is free software: you can redistribute it and/or
    * modify it under the terms of the GNU Lesser General Public License as
    * published by the Free Software Foundation, either version 3 of the
    * License, or (at your option) any later version.
    *
    * Adafruit_NeoPixel is distributed in the hope that it will be useful,
    * but WITHOUT ANY WARRANTY; without even the implied warranty of
    * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    * GNU Lesser General Public License for more details.
    *
    * You should have received a copy of the GNU Lesser General Public
    * License along with NeoPixel.  If not, see
    * <http://www.gnu.org/licenses/>.
    *
    */
     
    flags PixelType
    {
        KHz800 = 0x0000,
        KHz400 = 0x0100,
        
        RGB = ((0 << 6) | (0 << 4) | (1 << 2) | (2)),
        RBG = ((0 << 6) | (0 << 4) | (2 << 2) | (1)),
        GRB = ((1 << 6) | (1 << 4) | (0 << 2) | (2)),
        GBR = ((2 << 6) | (2 << 4) | (0 << 2) | (1)),
        BRG = ((1 << 6) | (1 << 4) | (2 << 2) | (0)),
        BGR = ((2 << 6) | (2 << 4) | (1 << 2) | (0)),
        
        WRGB = ((0 << 6) | (1 << 4) | (2 << 2) | (3)),
        WRBG = ((0 << 6) | (1 << 4) | (3 << 2) | (2)),
        WGRB = ((0 << 6) | (2 << 4) | (1 << 2) | (3)),
        WGBR = ((0 << 6) | (3 << 4) | (1 << 2) | (2)),
        WBRG = ((0 << 6) | (2 << 4) | (3 << 2) | (1)),
        WBGR = ((0 << 6) | (3 << 4) | (2 << 2) | (1)),

        RWGB = ((1 << 6) | (0 << 4) | (2 << 2) | (3)),
        RWBG = ((1 << 6) | (0 << 4) | (3 << 2) | (2)),
        RGWB = ((2 << 6) | (0 << 4) | (1 << 2) | (3)),
        RGBW = ((3 << 6) | (0 << 4) | (1 << 2) | (2)),
        RBWG = ((2 << 6) | (0 << 4) | (3 << 2) | (1)),
        RBGW = ((3 << 6) | (0 << 4) | (2 << 2) | (1)),

        GWRB = ((1 << 6) | (2 << 4) | (0 << 2) | (3)),
        GWBR = ((1 << 6) | (3 << 4) | (0 << 2) | (2)),
        GRWB = ((2 << 6) | (1 << 4) | (0 << 2) | (3)),
        GRBW = ((3 << 6) | (1 << 4) | (0 << 2) | (2)),
        GBWR = ((2 << 6) | (3 << 4) | (0 << 2) | (1)),
        GBRW = ((3 << 6) | (2 << 4) | (0 << 2) | (1)),

        BWRG = ((1 << 6) | (2 << 4) | (3 << 2) | (0)),
        BWGR = ((1 << 6) | (3 << 4) | (2 << 2) | (0)),
        BRWG = ((2 << 6) | (1 << 4) | (3 << 2) | (0)),
        BRGW = ((3 << 6) | (1 << 4) | (2 << 2) | (0)),
        BGWR = ((2 << 6) | (3 << 4) | (1 << 2) | (0)),
        BGRW = ((3 << 6) | (2 << 4) | (1 << 2) | (0)),
    }
#if defined(NEOPIXEL_DEVICE_DRIVER)
    Begin()
    {
        NeoPixelDeviceDriver.Begin();
    }
#endif
    
    Begin(byte pin) library;
    Begin(uint length, byte pin) library;
    Begin(uint length, byte pin, PixelType pixelType) library;
    byte Brightness { get library; set library; }
    
    SetColor(uint pixel, byte r, byte g, byte b) library;
    SetColor(uint pixel, byte r, byte g, byte b, byte w) library;
    Show() library;
    
    uint Length { get library; }
    
    Clear()
    {
        uint length = Length;
        for (uint pixel=0; pixel < length; pixel++)
        {
            SetColor(pixel, 0,0,0,0);
        }
        Show();    
    }
    Fill(uint first, uint count, byte r, byte g, byte b)
    {
        uint pixel = first;
        loop
        {
            if (count == 0) { break; }
            SetColor(pixel, r, g, b);
            count--;
        }
        Show();
    }
    Fill(uint first, uint count, byte r, byte g, byte b, byte w)
    {
        uint pixel = first;
        loop
        {
            if (count == 0) { break; }
            SetColor(pixel, r, g, b, w);
            count--;
        }
        Show();
    }
    
    // built-in board RGB LED configuration
#if defined(WAVESHARE_RP2040_ONE) || defined(WAVESHARE_RP2040_MATRIX)
    byte RGBLED { get { return 16; } } 
    BuiltIn()
    {
        uint length = 1;
#ifdef WAVESHARE_RP2040_MATRIX
        length = 25;
#endif
        Begin(length, RGBLED, PixelType.RGB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif    

#ifdef MAKER_NANO_RP2040
    byte RGBLED { get { return 11; } } 
    BuiltIn()
    {
        uint length = 2;
        Begin(length, RGBLED, PixelType.RGB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif

#if defined(SPARKFUN_THING_PLUS_RP2040)
    byte RGBLED { get { return 8; } } 
    BuiltIn()
    {
        Begin(1, RGBLED, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif 

#if defined(SEEED_XIAO_RP2040)
    byte RGBLED { get { return 12; } } 
    BuiltIn()
    {
        // https://wiki.seeedstudio.com/XIAO-RP2040-with-Arduino/#rgb-led
        MCU.PinMode(11, PinModeOption.Output);
        MCU.DigitalWrite(11, true);
        
        Begin(1, RGBLED, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif 

#if defined(CHALLENGER_RP2040_WIFI)
    byte RGBLED { get { return 11; } } 
    BuiltIn()
    {
        Begin(1, RGBLED, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif         

#if defined(ADAFRUIT_METRO_RP2040)
    byte RGBLED { get { return 14; } } 
    BuiltIn()
    {
        Begin(1, RGBLED, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif 

#if defined(ADAFRUIT_FEATHER_RP2040)
    byte RGBLED { get { return 16; } } 
    BuiltIn()
    {
        Begin(1, RGBLED, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif

 }
