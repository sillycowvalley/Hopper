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
      
    HueToRGB(byte hue, ref byte r, ref byte g, ref byte b)
    {
        // Divide the hue range (0-255) into 6 sectors to represent the transitions 
        // between primary and secondary colors in the RGB color wheel.
        
        byte sector = hue / 43;                // Each sector spans ~43 hues (256 / 6 ~ 43)
        byte remainder = byte((hue % 43) * 6); // The remainder determines how far into the sector we are.
        
        // Use switch to determine the RGB values for each sector:
        switch (sector)
        {
            case 0:  // Sector 0: Red (255, 0, 0) to Yellow (255, 255, 0)
            {
                // Increase green (from 0 to 255), keeping red at 255 and blue at 0.
                r = 255; g = remainder; b = 0;  
            }
            case 1:  // Sector 1: Yellow (255, 255, 0) to Green (0, 255, 0)
            {
                // Decrease red (from 255 to 0), keeping green at 255 and blue at 0.
                r = 255 - remainder; g = 255; b = 0;  
            }
            case 2:  // Sector 2: Green (0, 255, 0) to Cyan (0, 255, 255)
            {
                // Increase blue (from 0 to 255), keeping green at 255 and red at 0.
                r = 0; g = 255; b = remainder;  
            }
            case 3:  // Sector 3: Cyan (0, 255, 255) to Blue (0, 0, 255)
            {
                // Decrease green (from 255 to 0), keeping blue at 255 and red at 0.
                r = 0; g = 255 - remainder; b = 255;  
            }
            case 4:  // Sector 4: Blue (0, 0, 255) to Magenta (255, 0, 255)
            {
                // Increase red (from 0 to 255), keeping blue at 255 and green at 0.
                r = remainder; g = 0; b = 255;  
            }
            case 5:  // Sector 5: Magenta (255, 0, 255) to Red (255, 0, 0)
            {
                // Decrease blue (from 255 to 0), keeping red at 255 and green at 0.
                r = 255; g = 0; b = 255 - remainder;  
            }
        }
    }      
    byte[256*3] hues;
    GenerateHues()
    {
        byte hue;
        byte r; byte g; byte b;
        loop
        {
            hue++;
            NeoPixel.HueToRGB(hue, ref r, ref g, ref b);
            uint index = hue * 3;
            hues[index+0] = r;
            hues[index+1] = g;
            hues[index+2] = b;
            if (hue == 255) { break; }
        }
    }               
    
    SetHue(uint pixel, byte hue)
    {
        byte r; byte g; byte b;
        NeoPixel.HueToRGB(hue, ref r, ref g, ref b);
        NeoPixel.SetColor(pixel, r, g, b);
    }
    /*
    SetHue(uint pixel, byte hue)
    {
        uint index = hue * 3;
        NeoPixel.SetColor(pixel, hues[index+0], hues[index+1], hues[index+2]);
    }
    */
    /*
    SetHue(uint pixel, byte hue)
    {
        // Divide the hue range (0-255) into 6 sectors to represent the transitions 
        // between primary and secondary colors in the RGB color wheel.
        
        byte sector = hue / 43;                // Each sector spans ~43 hues (256 / 6 ~ 43)
        byte remainder = byte((hue % 43) * 6); // The remainder determines how far into the sector we are.
        
        // Use switch to determine the RGB values for each sector:
        switch (sector)
        {
            case 0:  // Sector 0: Red (255, 0, 0) to Yellow (255, 255, 0)
            {
                // Increase green (from 0 to 255), keeping red at 255 and blue at 0.
                NeoPixel.SetColor(pixel, 255, remainder, 0);
            }
            case 1:  // Sector 1: Yellow (255, 255, 0) to Green (0, 255, 0)
            {
                // Decrease red (from 255 to 0), keeping green at 255 and blue at 0.
                NeoPixel.SetColor(pixel, 255 - remainder, 255, 0);
            }
            case 2:  // Sector 2: Green (0, 255, 0) to Cyan (0, 255, 255)
            {
                // Increase blue (from 0 to 255), keeping green at 255 and red at 0.
                NeoPixel.SetColor(pixel, 0, 255, remainder);  
            }
            case 3:  // Sector 3: Cyan (0, 255, 255) to Blue (0, 0, 255)
            {
                // Decrease green (from 255 to 0), keeping blue at 255 and red at 0.
                NeoPixel.SetColor(pixel, 0, 255 - remainder, 255);
            }
            case 4:  // Sector 4: Blue (0, 0, 255) to Magenta (255, 0, 255)
            {
                // Increase red (from 0 to 255), keeping blue at 255 and green at 0.
                NeoPixel.SetColor(pixel, remainder, 0, 255);
            }
            case 5:  // Sector 5: Magenta (255, 0, 255) to Red (255, 0, 0)
            {
                // Decrease blue (from 255 to 0), keeping red at 255 and green at 0.
                NeoPixel.SetColor(pixel, 255, 0, 255 - remainder);
            }
        }
    } 
    */

 }
