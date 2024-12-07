program HouseBox
{
    //#define DIAGNOSTICS
    
    uses "/Source/Library/Fonts/Verdana5x8"
    
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/Library/Devices/AdafruitEInk213TriColor"
    
    bool lightsOn;
    bool doorOpen;
    bool doInfo;
    
    Hopper()
    {
        string message;
        
        DisplayDriver.FlipX = true;
        DisplayDriver.FlipY = true;
     
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        UART.Setup(9600);
        
        MCU.PinMode(GP5,  PinModeOption.Output);
        MCU.PinMode(GP6,  PinModeOption.Output);
        MCU.PinMode(GP13, PinModeOption.InputPullup);
        
        if (!MCU.AttachToPin(GP13, infoEvent, PinStatus.Rising))
        {
            IO.WriteLn("Failed to attach to pin " + (GP13).ToString());
        }
        
        doInfo = true;
        
        uint doorCounter;
        uint lightsCounter;
        bool state;
        loop
        {
            if (doorCounter != 0)
            {
                MCU.DigitalWrite(GP5,  state);
                MCU.DigitalWrite(GP6, !state);
                state = !state;
                doorCounter--;
            }
            else
            {
                MCU.DigitalWrite(GP5, false);
                MCU.DigitalWrite(GP6, false);
                doorOpen = false;
            }
            if (lightsCounter != 0)
            {
                lightsCounter--;
            }
            else
            {
                lightsOn = false;
            }
            Time.Delay(250);
            while (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
                if (ch == Char.EOL)
                {
#ifdef DIAGNOSTICS                    
                    IO.WriteLn("Message: " + message);
#endif
                    if (message == "OPEN")
                    {
                        // 10 seconds between "OPEN" signals
                        // = 10
                        // 250ms per cycles so 4 per second
                        // = 4 * 10
                        // multiply by 3 to only need every 3rd message
                        doorCounter = 3 * 4 * 10;
                        doorOpen = true;
                    }
                    else if (message == "ON")
                    {
                        // 3 minutes between "ON" signals
                        // = 3 x 60 seconds
                        // 250ms per cycles so 4 per second
                        // = 4 x 3 x 60
                        lightsCounter = 4 * 3 * 60;
                        lightsOn = true;
                    }
                    else if (message.StartsWith("INFO "))
                    {
                        displayInfo(message);
                    }
                    message = "";
                    break;
                }
                else
                {
                    message += ch;
                }
            }
            if (doInfo)
            {
#ifdef DIAGNOSTICS                    
                IO.WriteLn("INFO");
#endif                
                UART.WriteString("INFO" + Char.EOL);
                doInfo = false;
                Time.Delay(250);
            }
        }
    }
    
    infoEvent(byte pin, PinStatus status)
    {
        doInfo = true;
    }
    
    displayInfo(string message)
    {
        message = message.Replace("INFO ", "");
        <string> parts = message.Split(',');
        
        Display.Suspend();
            
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        Screen.Clear();
        Rectangle(0, 0, Display.PixelWidth,   Display.PixelHeight,   Colour.Black);
        Rectangle(2, 2, Display.PixelWidth-4, Display.PixelHeight-4, Colour.Red);
        
        int cellw = Font.CellWidth+1;
        int cellh = Font.CellHeight+1;
        string title = " Bigger Tigger - Sunset Lights";
        FilledRectangle(3 * cellw - cellw/2,     2 * cellh - cellh/2,     int(title.Length+1) * cellw+2, 2*cellh-2, Colour.Red);
        Rectangle      (3 * cellw - cellw/2 - 1, 2 * cellh - cellh/2 - 1, int(title.Length+1) * cellw+4, 2*cellh,   Colour.Black);
        
        string label = lightsOn ? "On " : "Off";
        int switchCol = 6 + int(title.Length);
        FilledRectangle(switchCol * cellw - cellw/2 - 3, 2 * cellh - cellh/2,     int(label.Length+1) * cellw+3, 2*cellh-2, lightsOn ? Colour.Red : Colour.Black);
        Rectangle      (switchCol * cellw - cellw/2 - 4, 2 * cellh - cellh/2 - 1, int(label.Length+1) * cellw+5, 2*cellh,   lightsOn ? Colour.Black : Colour.Red);
        Screen.SetCursor(byte(switchCol), 2);
        Screen.Print(label, Colour.White, lightsOn ? Colour.Red : Colour.Black);
        
        // Labels:
        Screen.SetCursor(3,2);
        Screen.Print(title, Colour.White, Colour.Red);
        
        Screen.SetCursor(3,4);
        Screen.Print("Reset: ", Colour.Black, Colour.White);
        Screen.Print(parts[3], Colour.Red, Colour.White);
        Screen.Print("  Power: ", Colour.Black, Colour.White);
        Screen.Print(parts[4], Colour.Red, Colour.White);
        
        Screen.SetCursor(3,7);
        Screen.Print("Time:", Colour.Black, Colour.White);
        Screen.SetCursor(3,8);
        Screen.Print("Date:", Colour.Black, Colour.White);
        
        
        
        string dst = (parts[1] == "DST") ? " (+1 hour for DST)" : "";
        Screen.SetCursor(14,7);
        Screen.Print(parts[0] + dst, Colour.Red, Colour.White);
        Screen.SetCursor(14,8);
        Screen.Print(parts[2], Colour.Red, Colour.White);
        
        if (parts.Count >= 7)
        {
            Screen.SetCursor(3,10);
            Screen.Print("Current:", Colour.Black, Colour.White);
            Screen.SetCursor(14,10);
            Screen.Print(parts[5] + " minutes", Colour.Red, Colour.White);
            
            if (parts[6] != "0")
            {
                Screen.SetCursor(3,11);
                Screen.Print("Until:", Colour.Black, Colour.White);
                Screen.SetCursor(14,11);
                Screen.Print(parts[6] + " minutes", Colour.Red, Colour.White);
            }
        }
        
        
        Display.Resume();
        
        doInfo = false; // in case button was pressed while we were drawing
    }
}
