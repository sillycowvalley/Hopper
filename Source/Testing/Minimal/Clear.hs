program EEPROM
{
    //#define MCU
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/MCU"
    
    const byte i2cEEPROMaddress = 0x50;
    Hopper()
    {
        _ = Wire.Initialize();
        // Clear : test pattern
        for (byte p = 0; p < 44; p++)
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
                //Time.Delay(5);
            }
            _ = Wire.EndTx();
            Time.Delay(10);
        }
        for (uint j = 0; j < 22; j++)
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
    }
}
