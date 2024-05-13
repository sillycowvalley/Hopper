program TS
{
    //uses "/Source/Library/Boards/PiPico" // MCU
    uses "/Source/Minimal/System"      // 6502
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    uses "Tools"
    uses "Sprites"
    
    Restart()
    {
        Sprites.Initialize();
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        Sprites.Render();
    }
    Hopper()
    {
        Seed();
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        PinMode(GP2, PinModeOption.Input);
        PinMode(GP3, PinModeOption.Input);
        
        Restart();
        
        byte iSprite;
        loop
        {
            Sprites.Move(ref iSprite);
            if (!DigitalRead(GP2))
            {
                Restart();
                iSprite = 0;
            }
            if (!DigitalRead(GP3))
            {
                IO.Write("3");
            }
        }
    }
}
