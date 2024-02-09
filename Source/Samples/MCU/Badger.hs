program Badger
{
    uses "/Source/Library/Devices/Badger2040"
    uses "/Source/Library/Fonts/Verdana5x8"
    
    const uint lineOptionMax = 4;
    uint lineOption;
    
    const string lorumIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse iaculis tortor vitae imperdiet tempus. Quisque eget sapien ex. Donec molestie tincidunt sem imperdiet condimentum. Nulla facilisi. Class aptent taciti sociosqu ad litora vestibulum.";
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        string pinName = PinToButton(pin);
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        switch (pinName)
        {
            case "up":
            {
                lineOption++;
                if (lineOption == lineOptionMax) { lineOption = 0; }
                DrawLines();
            }
            case "down":
            {
                if (lineOption == 0) { lineOption = lineOptionMax; } else { lineOption--; }
                DrawLines();
            }
            case "a":
            {
                DrawBoxes(Colour.Black);
            }
            case "b":
            {
                DrawBoxes(Colour.White);
            }
            case "c":
            {
                DrawText();
            }
        }
    }
    
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
        IO.WriteLn(lorumIpsum);
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
        VerticalLine(Display.PixelWidth/2, 0, Display.PixelHeight-1, colour);
        HorizontalLine(0, Display.PixelHeight/2, Display.PixelWidth-1, colour);
        Display.Resume();
        LED = false;
    }
    
    DrawLines()
    {
        int i;
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        
        LED = true;
        IO.WriteLn("  DrawLines: " + lineOption.ToString());
        
        Display.Suspend();
        Display.Clear(Colour.White);
        switch (lineOption)
        {
            case 0:
            {
                for(i=0; i<pw; i += 4) 
                {
                    Line(0, 0, i, ph-1, Colour.Black);
                }
                for(i=0; i< ph; i += 4) 
                {
                    Line(0, 0, pw-1, i, Colour.Black);
                }
            }
            case 1:
            {
                for(i=0; i< pw; i += 4)
                {
                    Line(0, ph-1, i, 0, Colour.Black);
                }
                for(i= ph-1; i>=0; i -= 4)
                {
                    Line(0, ph-1, pw-1, i, Colour.Black);
                }
            }
            case 2:
            {
                for(i= pw-1; i>=0; i -= 4)
                {
                    Line(pw-1, ph-1, i, 0, Colour.Black);
                }
                for(i= ph-1; i>=0; i -= 4)
                {
                    Line(pw-1, ph-1, 0, i, Colour.Black);
                }
            }
            case 3:
            {
                for(i=0; i< ph; i += 4)
                {
                    Line(pw-1, 0, 0, i, Colour.Black);
                }
                for(i=0; i< pw; i += 4)
                {
                    Line(pw-1, 0, i, ph-1, Colour.Black);
                }
            }
        }
        Display.Resume();
        LED = false;
    }
    
    {
        // Setup code:
        PinISRDelegate buttonDelegate = ButtonISR;
        //DeviceDriver.IsPortrait = true;
        //DisplayDriver.FlipX = true;
        //DisplayDriver.FlipY = true;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Badger 2040");
            return;
        }
        
        // Demo code:
        DrawBoxes(Colour.Black);
        loop
        {
            Delay(50);
        }
    }    
}
