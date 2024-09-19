program EEPROM
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Boards/Hopper6502"
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    const byte i2cEEPROMaddress = 0x51;
    
    const byte buttonAPin = Board.GP14;
    const byte buttonBPin = Board.GP15;
    
    uint cheesyIndex;
    <string> cheesyBytes;
    
    Show()
    {
        Screen.Clear();
        Display.Suspend();
        Screen.PrintLn(cheesyBytes[cheesyIndex]);
        Display.Resume();
    }
    
    InitializeBytes()
    {
        cheesyBytes.Append("Big cheese");
        cheesyBytes.Append("Cut the cheese");
        cheesyBytes.Append("Say cheese");
    }
    
    WriteBytes()
    {
        uint address = 0;
        foreach (var cheesyByte in cheesyBytes)
        {
            Wire.BeginTx(i2cEEPROMaddress);
            Wire.Write(byte(address >> 8));
            Wire.Write(byte(address & 0xFF));
            foreach (var c in cheesyByte)
            {
                Wire.Write(byte(c));
                Time.Delay(5);
                address++;
            }
            // null terminated:
            Wire.Write(0);
            Time.Delay(5);
            address++;
            
            _ = Wire.EndTx();
            Time.Delay(5);
        }
        
        // emit an empty string to mark the end:
        Wire.BeginTx(i2cEEPROMaddress);
        Wire.Write(byte(address >> 8));
        Wire.Write(byte(address & 0xFF));
        Wire.Write(0);
        Time.Delay(5);
        address++;
        
        _ = Wire.EndTx();
        Time.Delay(5);
    }
    ReadBytes()
    {
        uint address;
        Wire.BeginTx(i2cEEPROMaddress);
        Wire.Write(byte(address >> 8));
        Wire.Write(byte(address & 0xFF));
        _ = Wire.EndTx();
        
        // clear to make sure we are getting EEPROM data:
        cheesyBytes.Clear();
        
        string currentByte;
        loop
        {
            byte bytes = Wire.RequestFrom(i2cEEPROMaddress, 1);
            if (bytes == 1)
            {
                byte data = Wire.Read();
                if (data != 0)
                {
                    currentByte += char(data);
                }
                else
                {   
                    if (currentByte.Length == 0)
                    {
                        break; // an empty string means we're done
                    }
                    cheesyBytes.Append(currentByte);
                    currentByte = "";
                }
            }
        }
    }
    Hopper()
    {
        // initialize the OLED display
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }    

        // set up the two buttons:        
        MCU.PinMode(buttonAPin, PinModeOption.Input);
        MCU.PinMode(buttonBPin, PinModeOption.Input);
        
        // some I2C diagnostics: (to verify I2C addresses)
        //IO.EchoToLCD = true;
        Display.Suspend();
        Screen.Clear();
        //_ = Wire.Initialize();
        for (byte i2cAddress = 8; i2cAddress < 120; i2cAddress++)
        {
            Wire.BeginTx(i2cAddress);
            if (0 == Wire.EndTx())
            {
                IO.WriteLn(i2cAddress.ToHexString(2) + " exists");
            }
        }
        Display.Resume();
        //IO.EchoToLCD = false;
        
        // write Cheesy Bytes to serial EEPROM:
        //InitializeBytes();
        //WriteBytes();
        
        // read Cheesy Bytes from serial EEPROM:
        ReadBytes();
        foreach (var cheesyByte in cheesyBytes)
        {
            IO.WriteLn(cheesyByte);
        }
        
        // display the first one and then loop waiting for buttons ..
        cheesyIndex = 0;
        Show();
        loop
        {
            Delay(5);
            if (!DigitalRead(buttonAPin)) // pin goes low
            {
                while (!DigitalRead(buttonAPin)) // wait for it to go high
                {
                    
                }
                if (cheesyIndex > 0)
                {
                    cheesyIndex--;
                    Show();
                }
                
            }
            if (!DigitalRead(buttonBPin)) // pin goes low
            {
                while (!DigitalRead(buttonBPin)) // wait for it to go high
                {
                    
                }
                if (cheesyIndex < cheesyBytes.Count-1)
                {
                    cheesyIndex++;
                    Show();
                }
                
            }
        }
    }
}
