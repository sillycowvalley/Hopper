program Blink
{
    uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/PiPico"
    
    //uses "/Source/Library/Displays/OLEDSSD1306" 
    //uses "/Source/Library/Fonts/Verdana5x8"
    
    
    Hopper()
    {
        /*
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        */
        
        PinMode(GP0, PinModeOption.Output);
        PinMode(GP1, PinModeOption.Output);
        loop
        {
            DigitalWrite(GP0, true);
            DigitalWrite(GP1, false);
            Delay(500);
            /*
            WriteLn((Time.Seconds).ToString());
            Display.Suspend();
            Screen.DrawText(62, 30, " ", Colour.Black, Colour.White, 1);
            Display.Resume();
            */
            
            DigitalWrite(GP0, false);
            DigitalWrite(GP1, true);
            Delay(250);
            /*
            WriteLn((Time.Seconds).ToString());
            Display.Suspend();
            Screen.DrawText(62, 30, " ", Colour.White, Colour.Black, 1);
            Display.Resume();
            */
        }
    }
}
