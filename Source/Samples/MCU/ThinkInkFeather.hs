program ThinkInkFeatherwing
{   
    #define ADAFRUIT_FEATHER_RP2040
    
    uses "/Source/Library/Devices/AdafruitThinkInk213Mono"
    //uses "/Source/Library/Devices/AdafruitThinkInk213TriColor"
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    const string lorumIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse iaculis tortor vitae imperdiet tempus. Quisque eget sapien ex. Donec molestie tincidunt sem imperdiet condimentum. Nulla facilisi. Class aptent taciti sociosqu ad litora vestibulum.";
    
    DrawText()
    {
        LED = true;
        IO.WriteLn("  DrawText");
        Display.Suspend();
        EchoToLCD = true;
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        Screen.Clear();
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        Screen.ForeColour = Colour.Red;
        IO.WriteLn(lorumIpsum);
        Screen.ForeColour = Colour.Black;
        EchoToLCD = false;
        Display.Resume();
        LED = false;
    }
    
    DrawBoxes(uint colour)
    {
        LED = true;
        IO.WriteLn("  DrawBoxes: " + ((colour == Colour.Black) ? "Black" : "White"));
        Display.Suspend();
        Display.Clear((colour == Colour.Black) ? Colour.White : Colour.Black); // opposite colour
        Rectangle(0, 0, Display.PixelWidth, Display.PixelHeight, colour);
        VerticalLine(Display.PixelWidth/3, 0, Display.PixelHeight-1, colour);
        HorizontalLine(0, Display.PixelHeight/3, Display.PixelWidth-1, colour);
        Display.Resume();
        LED = false;
    }
    
    {
        //DisplayDriver.IsPortrait = true;
        //DisplayDriver.FlipX = true;
        //DisplayDriver.FlipY = true;
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        long start;
        long elapsed;
        long laps;
        
        Screen.Clear();
        loop
        {
            WriteLn("Laps: ");
            WriteLn(laps.ToString());
            laps++;
            
            start = Millis;
            DrawBoxes(Colour.Black);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            Delay(1000);
            
            start = Millis;
            DrawBoxes(Colour.White);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            Delay(1000);
 
            start = Millis;
            DrawText();
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            Delay(1000);
        }
    }
}
