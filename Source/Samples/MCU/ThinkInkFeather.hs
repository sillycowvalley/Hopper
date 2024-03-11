program ThinkInkFeatherwing
{   
    //uses "/Source/Library/Boards/AdafruitFeather"
    //uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    //uses "/Source/Library/Boards/SparkfunThingPlusRP2040"
    //uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    
    //uses "/Source/Library/Devices/AdafruitEInk213Mono"
    //uses "/Source/Library/Devices/AdafruitEInk213TriColor"
    //uses "/Source/Library/Devices/AdafruitThinkInk290TriColor"
    //uses "/Source/Library/Devices/AdafruitThinkInk290Gray"
    //uses "/Source/Library/Devices/WSPicoePaper290"
    //uses "/Source/Library/Devices/Adafruit240x135ColorTFT"
    //uses "/Source/Library/Devices/Adafruit160x80ColorTFT"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    const string lorumIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse iaculis tortor vitae imperdiet tempus. Quisque eget sapien ex. Donec molestie tincidunt sem imperdiet condimentum. Nulla facilisi. Class aptent taciti sociosqu ad litora vestibulum.";

    ButtonISR(byte pin, PinStatus status) 
    { 
        string pinName = PinToButton(pin);
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");
    }
    
    DrawText()
    {
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
    }
    
    DrawShades()
    {
        IO.WriteLn("  DrawShades");
        int pw2  = Display.PixelWidth/2;
        int pw34 = Display.PixelWidth*3/4;
        int pw4  = Display.PixelWidth/4;
        int w = pw4;
        if (Display.PixelWidth % 4 != 0)
        {
            w++;
        }
        Display.Suspend();
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.White);
        FilledRectangle(pw4,  0, w, Display.PixelHeight, Colour.LightGray);
        FilledRectangle(pw2,  0, w, Display.PixelHeight, Colour.DarkGray);
        FilledRectangle(pw34, 0, w, Display.PixelHeight, Colour.Black);
        Display.Resume(); 
    }
    
    DrawBoxes(uint colour)
    {
        uint backColour;
        if (colour == Colour.Black)
        {
            IO.WriteLn("  DrawBoxes: Black");
            backColour = Colour.White;
        }
        else if (colour == Colour.White)
        {
            IO.WriteLn("  DrawBoxes: White");
            backColour = Colour.Black;
        }
        else if (colour == Colour.Red)
        {
            IO.WriteLn("  DrawBoxes: Red");
            backColour = Colour.White;
        }
        else if (colour == Colour.Green)
        {
            IO.WriteLn("  DrawBoxes: Green");
            backColour = Colour.White;
        }
        else if (colour == Colour.Blue)
        {
            IO.WriteLn("  DrawBoxes: Blue");
            backColour = Colour.White;
        }
        else
        {
            IO.WriteLn("  DrawBoxes: colour?");
            backColour = Colour.White;
        }
        
        Display.Suspend();
        Display.Clear(backColour);
        Rectangle(0, 0, Display.PixelWidth, Display.PixelHeight, colour);
        VerticalLine(Display.PixelWidth/3, 0, Display.PixelHeight-1, colour);
        HorizontalLine(0, Display.PixelHeight/3, Display.PixelWidth-1, colour);
        IO.WriteLn("  Resume");
        Display.Resume();
    }
    
    {
        //IsPortrait = true;
        //FlipY = true;
        
        FlipX = true;
        FlipY = true;
        
#if defined(ADAFRUIT_TFT_114) || defined(ADAFRUIT_TFT_096)       
        DeviceDriver.SDCS = Board.GP29;
        DeviceDriver.CS   = Board.SPI0SS;
        DeviceDriver.DC   = Board.GP28;
#endif
     
#ifdef HAS_BUTTONS        
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
#else
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
#endif
        
        long start;
        long elapsed;
        long laps;
        
        //Screen.Clear();
        loop
        {
            WriteLn("Laps: ");
            WriteLn(laps.ToString());
            laps++;
            Delay(250);
            
            start = Millis;
            DrawText();
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawShades();
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawBoxes(Colour.Black);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawBoxes(Colour.White);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawBoxes(Colour.Red);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawBoxes(Colour.Green);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
            
            start = Millis;
            DrawBoxes(Colour.Blue);
            elapsed = Millis - start;
            WriteLn("Elapsed: " + elapsed.ToString());
            DelaySeconds(2);
        }
    }
}
