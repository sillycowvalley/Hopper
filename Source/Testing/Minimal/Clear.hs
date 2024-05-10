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
        for (byte p = 0; p < 48; p++)
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
        for (uint j = 0; j < 6; j++)
        {
            uint address = j * 128;
            Wire.BeginTx(i2cEEPROMaddress);
            Wire.Write(byte(address >> 8));
            Wire.Write(byte(address & 0xFF));
            _ = Wire.EndTx();
            
            byte bytes2 = Wire.RequestFrom(i2cEEPROMaddress, 128);
            for (uint i = 0; i < bytes2; i++)
            {
                if (i % 32 == 0)
                {
                    IO.WriteLn();
                    IO.Write((address+i).ToHexString(4));
                }
                byte data = Wire.Read();
                IO.Write(" " + data.ToHexString(2));
                if (i % 16 == 15)
                {
                    IO.Write(" ");
                }
            }
        }
        IO.WriteLn();
    }
}
