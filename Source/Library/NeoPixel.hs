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
    bool Begin()
    {
        return DeviceDriver.Begin();
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

#if defined(BOARD_HAS_NEOPIXEL)
    BuiltIn()
    {
#if defined(BOARD_HAS_NEOPIXEL_POWER)        
        MCU.PinMode(Board.BuiltInNeoPixelPower, PinModeOption.Output);
        MCU.DigitalWrite(Board.BuiltInNeoPixelPower, true);
#endif
        Begin(Board.BuiltInNeoPixelLength, Board.BuiltInNeoPixel, PixelType.GRB | PixelType.KHz800);
        Brightness = 10;
        Clear();
    }
#endif  
    HueToRGB(long hue, ref byte r, ref byte g, ref byte b)
    {
        // Normalize hue to 0-0xFFFF
        hue = hue % 0x10000;
    
        // Each 1/6th of the range represents a primary color sector
        uint sector    = uint(hue / 0x2AAA); // 0x10000 / 6 = 0x2AAA
        uint remainder = uint(hue % 0x2AAA); // Hue within the sector
    
        long q = (long(255) * (0x2AAA - remainder)) / 0x2AAA;
        long t = (long(255) * remainder) / 0x2AAA;
    
        switch(sector)
        {
            case 0:
            {
                // Red to yellow
                r = 255;
                g = t.GetByte(0);
                b = 0;
            }
            case 1:
            {
                // Yellow to green
                r = q.GetByte(0);
                g = 255;
                b = 0;
            }
            case 2:
            {
                // Green to cyan
                r = 0;
                g = 255;
                b = t.GetByte(0);
            }
            case 3:
            {
                // Cyan to blue
                r = 0;
                g = q.GetByte(0);
                b = 255;
            }
            case 4:
            {
                // Blue to magenta
                r = t.GetByte(0);
                g = 0;
                b = 255;
            }
            case 5:
            {
                // Magenta to red
                r = 255;
                g = 0;
                b = q.GetByte(0);
            }
        }
    }
         

 }
