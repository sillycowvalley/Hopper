program DoorSensor
{
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    const string doorLabel = "A";
    const uint secondsPerUpdate = 10;
    
    Tiny2350LEDOff()
    {
        // Pimoroni Tiny 2350 built in RGB is active low
        MCU.PinMode(GP18,  PinModeOption.Output);
        MCU.PinMode(GP19,  PinModeOption.Output);
        MCU.PinMode(GP20,  PinModeOption.Output);
        MCU.DigitalWrite(GP18, true);
        MCU.DigitalWrite(GP19, true);
        MCU.DigitalWrite(GP20, true);
    }
    
    Hopper()
    {
        byte counter;
        bool doorWasOpen;
        
        string openCommand = "O" + doorLabel + Char.EOL;
        string shutCommand = "S" + doorLabel + Char.EOL;
        
        UART.Setup(9600); // for clarity (default)
        
        Tiny2350LEDOff();
        
        PinMode(GP5, PinModeOption.Output);
        PinMode(GP4, PinModeOption.InputPulldown);
        
        loop
        {
            bool doorOpen = DigitalRead(GP4);
            DigitalWrite(GP5, doorOpen);
            
            if (doorOpen)
            {
                if (!doorWasOpen)
                {
                    counter = 0;
                }
                if (counter % secondsPerUpdate == 0)
                {
                    UART.WriteString(openCommand);
                    IO.WriteLn("OPEN");
                }
                doorWasOpen = true;
            }
            else if (doorWasOpen)
            {
                UART.WriteString(shutCommand);
                IO.WriteLn("SHUT");
                doorWasOpen = false;
            }
            
            Time.Delay(1000);
            counter++;
        }
    }
}
