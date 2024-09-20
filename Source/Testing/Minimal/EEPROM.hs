program EEPROM
{
    //#define MCU
    uses "/Source/Library/Boards/Hopper6502"
    
    const byte i2cEEPROMaddress = 0x54;
    Hopper()
    {
        _ = Wire.Initialize();
        
        for (byte i2cAddress = 8; i2cAddress < 120; i2cAddress++)
        {
            Wire.BeginTx(i2cAddress);
            if (0 == Wire.EndTx())
            {
                WriteLn(i2cAddress.ToHexString(2) + " exists");
            }
        }
        
        /*
        // Clear : test pattern
        for (byte p = 0; p < 80; p++)
        {
            uint address = p * 16;
            Wire.BeginTx(i2cEEPROMaddress);
            Wire.Write(byte(address >> 8));
            Wire.Write(byte(address & 0xFF));
            for (uint i= 0; i < 8; i++)
            {
                Wire.Write(p);
                // 5ms delay for EEPROM write
                Time.Delay(5);
                Wire.Write(0x55);
                // 5ms delay for EEPROM write 
                Time.Delay(5);
            }
            _ = Wire.EndTx();
            Time.Delay(5);
        }
        return;
        */       
        for (uint j = 0; j < 8 /*40*/; j++)
        {
            uint address = j * 32;
            Wire.BeginTx(i2cEEPROMaddress);
            Wire.Write(byte(address >> 8));
            Wire.Write(byte(address & 0xFF));
            _ = Wire.EndTx();
            
            if (address % 128 == 0)
            {
                IO.WriteLn();
            }
            IO.Write(address.ToHexString(4));
            
            byte bytes2 = Wire.RequestFrom(i2cEEPROMaddress, 32);
            for (uint i = 0; i < bytes2; i++)
            {
                byte data = Wire.Read();
                IO.Write(" " + data.ToHexString(2));
                if (i % 16 == 15)
                {
                    IO.Write(" ");
                }
            }
            IO.WriteLn();
        }
        /*
        uint now = Time.Seconds;  // time stamp to be sure our write is working
        string writeStr = "Hello Serial EEPROM! (at " + now.ToString() + "s)";
        uint startAddress = 4242; // arbitrary address on the EEPROM
        
        IO.WriteLn("Now: " + now.ToString() + "s");
        IO.Write("Writing: '");
        
        uint address = startAddress /*+ i*/;
        Wire.BeginTx(i2cEEPROMaddress);
        Wire.Write(byte(address >> 8));
        Wire.Write(byte(address & 0xFF));
        for (uint i = 0; i < writeStr.Length; i++)
        {
            Wire.Write(byte(writeStr[i]));
            IO.Write(writeStr[i]);
            // 5ms delay for EEPROM write (4ms worked but let's not push our luck)
            Time.Delay(5); 
        }
        _ = Wire.EndTx();
        IO.WriteLn("'");
        IO.Write("Reading: '");
        string readStr;
        
        Wire.BeginTx(i2cEEPROMaddress);
        Wire.Write(byte(startAddress >> 8)); 
        Wire.Write(byte(startAddress & 0xFF));
        _ = Wire.EndTx();
        uint bytes = Wire.RequestFrom(i2cEEPROMaddress, byte(writeStr.Length));
        for (uint i = 0; i < writeStr.Length; i++)
        {
            readStr += char(Wire.Read());
        }
        IO.WriteLn(readStr + "'");
        WriteLn("Bytes: " + bytes.ToString());
        
        // page 0 contains header
        Wire.BeginTx(i2cEEPROMaddress);
        Wire.Write(0); 
        Wire.Write(0);
        _ = Wire.EndTx();
        bytes = Wire.RequestFrom(i2cEEPROMaddress, 3);
        for (uint i = 0; i < bytes; i++)
        {
            byte data = Wire.Read();
            IO.Write(" " + data.ToHexString(2));
        }
        IO.WriteLn();
        
        // program data starting from address 128
        for (uint j = 0; j < 24; j++) // 3 pages in 32 byte chunks = 3x8 = 24
        {
            // code starts at EEPROM page 1
            address = 128 + j * 32;
            Wire.BeginTx(i2cEEPROMaddress);
            Wire.Write(byte(address >> 8)); 
            Wire.Write(byte(address & 0xFF));
            _ = Wire.EndTx();
            
            bytes = Wire.RequestFrom(i2cEEPROMaddress, 32);
            for (uint i = 0; i < bytes; i++)
            {
                byte data = Wire.Read();
                IO.Write(" " + data.ToHexString(2));
                if (i % 16 == 15)
                {
                    IO.Write(" ");
                }
            }
            IO.WriteLn();
        }
        */
    }
}
