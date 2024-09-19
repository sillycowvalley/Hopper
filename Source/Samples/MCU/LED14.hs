program LED14SegmentDemo
{
   
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    uses "/Source/Library/Devices/Adafruit14Segment"
    
    Scroll(string content)
    {
        uint width = Display.Modules * 4;
        for (uint i = 0; i < width; i++)
        {
            content = " " + content + " ";
        }
        string show = content;
        loop
        {
            Display.Write(show);
            show = show.Substring(1);
            if (show.Length == 0)
            {
                show = content;
            }
            Delay(250);
        }
    }
    Hopper()
    {
        _ = Wire.Initialize();
        
        for (byte i2cAddress = 8; i2cAddress < 120; i2cAddress++)
        {
            Wire.BeginTx(i2cAddress);
            if (0 == Wire.EndTx())
            {
                WriteLn(i2cAddress.ToHexString(2) + " exists");
            }
        }
        
        Display.Add(0x70);
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        /*
        Scroll("CHEESY");
        */
        
        Display.Brightness = 15;
        //Display.Blink = BlinkRate.HalfHz;
        Display.Write("EDAM");
        
        /*
        loop
        {
            for (byte brightness=0; brightness<16; brightness++)
            {
                Display.Brightness = brightness;
                Delay(100);
            }
            for (byte brightness=0; brightness<16; brightness++)
            {
                Display.Brightness = 15-brightness;
                Delay(100);
            }
        }
        */   
        
    }
}

