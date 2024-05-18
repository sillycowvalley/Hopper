program NeoPixelDemo
{
    uses "/Source/Library/Boards/CytronMakerNanoRP2040"
    
    const byte pixelPin = Board.BuiltInNeoPixel;
    const uint numPixels = 2;

    byte brightness = 20;

    <string, <byte> > colors;
    <byte> RGB(byte r, byte g, byte b)
    {
        <byte> rgb;
        rgb.Append(r);
        rgb.Append(g);
        rgb.Append(b);
        return rgb;
    }
    InitializeColors()
    {
        colors["RED"] = RGB(255, 0, 0);
        colors["YELLOW"] = RGB(255, 150, 0);
        colors["GREEN"] = RGB(0, 255, 0);
        colors["CYAN"] = RGB(0, 255, 255);
        colors["BLUE"] = RGB(0, 0, 255);
        colors["PURPLE"] = RGB(180, 0, 255);
        colors["WHITE"] = RGB(50, 50, 50);
    }
    <byte> Wheel(byte pos)
    {
        <byte> rgb;
        if (pos < 85)
        {
            return RGB(byte(255 - pos * 3), byte(pos * 3), 0);
        }
        else if (pos < 170)
        {
            pos -= 85;
            return RGB(0, byte(255 - pos * 3), byte(pos * 3));
        }
        else
        {
            pos -= 170;
            return RGB(byte(pos * 3), 0, byte(255 - pos * 3));
        }
    }

    ColorChase(<byte> color, float wait)
    {
        for (uint i = 0; i < numPixels; i++)
        {
            NeoPixel.SetColor(i, color[0], color[1], color[2]);
            NeoPixel.Show();
            Time.Delay(uint(wait * 1000));
        }
        Time.Delay(1000);
    }

    RainbowCycle(float wait)
    {
        for (uint j = 0; j < 255; j++)
        {
            for (uint i = 0; i < numPixels; i++)
            {
                byte rcIndex = byte((256 / numPixels) + j);
                <byte> color = Wheel(rcIndex & 255);
                NeoPixel.SetColor(i, color[0], color[1], color[2]);
            }
            NeoPixel.Show();
            Time.Delay(uint(wait * 1000));
        }
    }

    PlayDemo()
    {
        NeoPixel.BuiltIn();
        NeoPixel.Brightness = brightness;
            
        InitializeColors();
        
        loop
        {
            foreach (var kv in colors)
            {
                string colorName = kv.key;
                <byte> color     = kv.value;
                NeoPixel.Fill(0, numPixels, color[0], color[1], color[2]);
                NeoPixel.Show();
                Time.Delay(1000);
            }

            // Color Chase
            ColorChase(colors["RED"], 0.5);
            ColorChase(colors["YELLOW"], 0.5);
            ColorChase(colors["GREEN"], 0.5);
            ColorChase(colors["CYAN"], 0.5);
            ColorChase(colors["BLUE"], 0.5);
            ColorChase(colors["PURPLE"], 0.5);

            // Rainbow Cycle
            RainbowCycle(0.05);
            Time.Delay(1000);
        }
    }

    Hopper()
    {
        PlayDemo();
    }
}

