program LED14SegmentDemo
{
    #define EXPERIMENTAL
    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Devices/Adafruit14Segment"
    
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
        Display.Write("EDAM");
        
    }
}

