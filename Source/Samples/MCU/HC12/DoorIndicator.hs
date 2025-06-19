program DoorIndicator
{
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    const uint secondsPerUpdate = 10;
    const uint cyclesPerUpdate  = secondsPerUpdate * 4 * 2 * 10;
    
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
        string message;
        
        UART.Setup(9600); // for clarity (default)
        
        Tiny2350LEDOff();
        
        MCU.PinMode(GP5,  PinModeOption.Output);
        MCU.PinMode(GP6,  PinModeOption.Output);
        MCU.PinMode(GP7,  PinModeOption.Output);
        
        MCU.DigitalWrite(GP5,  false);
        MCU.DigitalWrite(GP6,  false);
        MCU.DigitalWrite(GP7,  false);
        
        uint doorCounterA;
        uint doorCounterB;
        uint doorCounterC;
        
        loop
        {
            if (doorCounterA != 0)
            {
                MCU.DigitalWrite(GP5,  true);
                doorCounterA--;
            }
            else
            {
                MCU.DigitalWrite(GP5, false);
            }
            if (doorCounterB != 0)
            {
                MCU.DigitalWrite(GP6,  true);
                doorCounterB--;
            }
            else
            {
                MCU.DigitalWrite(GP6, false);
            }
            if (doorCounterC != 0)
            {
                MCU.DigitalWrite(GP7,  true);
                doorCounterC--;
            }
            else
            {
                MCU.DigitalWrite(GP7, false);
            }
            
            Time.Delay(250);
            while (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
                if (ch == Char.EOL)
                {
                    IO.WriteLn(message);
                    switch (message)
                    {
                        case "OA":
                        {
                            doorCounterA = cyclesPerUpdate;
                        }
                        case "OB":
                        {
                            doorCounterB = cyclesPerUpdate;
                        }
                        case "OC":
                        {
                            doorCounterC = cyclesPerUpdate;
                        }
                        case "SA":
                        {
                            doorCounterA = 0;
                        }
                        case "SB":
                        {
                            doorCounterB = 0;
                        }
                        case "SC":
                        {
                            doorCounterC = 0;
                        }
                    }
                    message = "";
                    break;
                }
                else
                {
                    message += ch;
                }
            }
        }
    }
}
