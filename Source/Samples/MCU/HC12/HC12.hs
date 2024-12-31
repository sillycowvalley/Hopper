program HC12
{
    //uses "/Source/Library/Boards/PiPicoW"
    //uses "/Source/Library/Boards/ChallengerNB2040WiFi"    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    
    const byte setPin = GP2;
    
    record Settings
    {
        byte Channel;      // 001-127
        byte Power;        // 11-20 (dBm)
        uint Baud;         // 1200-115200
        byte Mode;         // 1-4 (FU1-FU4)
        string Version;
    }
    
    EnterCommandMode()
    {
        MCU.DigitalWrite(setPin, false);
        Time.Delay(50); // Wait for HC-12 to enter command mode
    }
    
    ExitCommandMode()
    {
        MCU.DigitalWrite(setPin, true);
        Time.Delay(50); // Wait for HC-12 to exit command mode
    }
    
     // Send a command and read response into buffer
    // Returns number of bytes read
    string SendCommand(string cmd)
    {
        string response;
        
        UART.WriteString(cmd + Char.EOL);
        Time.Delay(50);    // Wait for HC-12 response     
        while (UART.IsAvailable)
        {
            char ch = UART.ReadChar();
            if (ch.IsWhitespace())
            {
                response += ' ';
            }
            else
            {
                response += ch;
            }
        }
        return response.Trim();
    }
    
    bool GetSettings(Settings settings)
    {
        EnterCommandMode();
        
        string response = SendCommand("AT+RB");
        if (response.StartsWith("OK+B"))
        {
            uint baud;
            if (UInt.TryParse(response.Substring(4), ref baud))
            {
                settings.Baud = baud;
            }
        }     
        response = SendCommand("AT+RC");
        if (response.StartsWith("OK+RC"))
        {
            uint channel;
            if (UInt.TryParse(response.Substring(5), ref channel) && (channel >= 1) && (channel <= 127))
            {
                settings.Channel = byte(channel);
            }
        }
        response = SendCommand("AT+RF");
        if (response.StartsWith("OK+FU"))
        {
            // FU1: Faster communication, shorter range.
            // FU2: Balance between speed and range.
            // FU3: Maximum range, slower speed (default).
            // FU4: Fixed transmission mode.
            uint mode;
            if (UInt.TryParse(response.Substring(5), ref mode) && (mode >= 1) && (mode <= 4))
            {
                settings.Mode = byte(mode);
            }
        }
        response = SendCommand("AT+RP");
        if (response.StartsWith("OK+RP"))
        {
            response = response.Replace("dBm", "");
            uint power;
            if (UInt.TryParse(response.Substring(6), ref power) && (power >= 1) && (power <= 255))
            {
                settings.Power = byte(power);
            }
        }
        response = SendCommand("AT+V");
        if (!response.IsEmpty)
        {
            settings.Version = response;
        }
        
        ExitCommandMode();
        return true;
    }
    bool SetChannel(byte channel)
    {
        bool success;
        loop
        {
            if ((channel < 1) || (channel > 127))
            {
                break;
            }         
            string channelString = "C" + (channel.ToString()).LeftPad('0', 3);
            
            EnterCommandMode();
            string response = SendCommand("AT+" + channelString);
            ExitCommandMode();
            
            success = response == ("OK+" + channelString);
            break;
        }
        return success;
    }
    ShowSettings()
    {
        // Get all settings
        Settings config;
        if (GetSettings(config))
        {
            IO.WriteLn("Version: " + config.Version);
            IO.WriteLn("Current channel: " + (config.Channel).ToString());
            IO.WriteLn("Power level: " + (config.Power).ToString() + "dBm");
            IO.WriteLn("Baud rate: " + (config.Baud).ToString());
            IO.WriteLn("Mode: FU" + (config.Mode).ToString());
        }
    }
    
    Hopper()
    {
        MCU.PinMode(setPin, MCU.PinModeOption.Output);
        MCU.DigitalWrite(setPin, true); // Normal mode by default
        
        UART.Setup(9600);
        
        ShowSettings();
        if (!SetChannel(53))
        {
            IO.WriteLn("Failed to set channel");
        }
        ShowSettings();
        
        
    }
}
