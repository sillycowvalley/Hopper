program TS
{
    uses "/Source/Library/Boards/PiPico" // MCU
    //uses "/Source/Minimal/System"      // 6502
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    uses "Tools"
    uses "Sprites"
    
    Hopper()
    {
        Seed();
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        Sprites.Initialize();
        
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        
        Sprites.Render();
        loop
        {
            Sprites.Move();
        }
    }
}
