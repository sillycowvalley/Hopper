program Badger
{
    //#define DISPLAY_DIAGNOSTICS
    
    uses "/Source/Library/Devices/WSPicoePaper4200"
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    bool exit;
    ButtonISR(byte pin, PinStatus status) 
    { 
        string pinName = PinToButton(pin);
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        if (pinName == "0")
        {
            exit = true;
        }
    }
    
    TestBox(uint colour)
    {
        Display.Suspend();
        Rectangle(0, 0, Display.PixelWidth, Display.PixelHeight, colour);
        VerticalLine(Display.PixelWidth/2, 0, Display.PixelHeight-1, colour);
        HorizontalLine(0, Display.PixelHeight/2, Display.PixelWidth-1, colour);
        Display.Resume();
    }
    
    {
        // Setup code:
        PinISRDelegate buttonDelegate = ButtonISR;
        //DisplayDriver.IsPortrait = true;
        //DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-ePaper-4.2");
            return;
        }
        
        // Demo code:
        long start;
        long laps;
        loop
        {
            IO.WriteLn("Laps: " + laps.ToString());
            EchoToLCD = true;
            EchoToLCD = false;
            start = Millis;
            Display.Suspend();
            Display.Clear(Colour.Black);
            TestBox(Colour.White);
            LED = false;
            EchoToLCD = true;
            IO.WriteLn("Black Screen");
            EchoToLCD = false;
            Display.Resume();
            IO.WriteLn("Black Screen: " + (Millis - start).ToString());
            Delay(100);
            
            start = Millis;
            Display.Suspend();
            Display.Clear(Colour.White);
            TestBox(Colour.Black);
            LED = true;
            EchoToLCD = true;
            IO.WriteLn("White Screen");
            EchoToLCD = false;
            Display.Resume();
            IO.WriteLn("White Screen: " + (Millis - start).ToString());
            Delay(100);
            
            //LED = !LED;
            laps++;
            Delay(10);
            if (exit) 
            { 
                IO.WriteLn("Exit button pressed");
                break; 
            }
        }
    }    
}
