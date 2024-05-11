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
        
        //Display.Suspend();
        //Display.HorizontalLine(0,0,3, Colour.White);
        //Display.HorizontalLine(4,2,7, Colour.White);
        //Display.Resume();
        
        Sprites.Render();
        
        //DirectPixel(4, 8, Colour.White);
        /*
        loop
        {
            byte x = Random() % 128;
            byte y = Random() % 64;
            DirectPixel(x, y, Colour.White);   
        }
        */
    }
}
