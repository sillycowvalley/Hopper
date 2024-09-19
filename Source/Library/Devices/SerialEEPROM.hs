unit BlockStorage
{
    byte i2cEEPROMaddress;
    uint blockSize;
    byte pageSize;
    
    enum SerialEEPROM
    {
        XX256,  // 24AA256
        XX512,  // 24AA512
        XX1024, // 24AA1026 and 24LC1025
    }
    
    uint BlockSize { get { return blockSize; } }
    
    Configure(SerialEEPROM serialEEPROM)
    {
        Configure(0x50, serialEEPROM);
    }
    Configure(byte i2cAddress, SerialEEPROM serialEEPROM)
    {
        i2cEEPROMaddress = i2cAddress;
        switch (serialEEPROM)
        {
            case SerialEEPROM.XX256:
            {
                blockSize = 256;
                pageSize  = 64;
            }
            case SerialEEPROM.XX512:
            {
                blockSize = 256;
                pageSize  = 128;
            }
            case SerialEEPROM.XX1024:
            {
                blockSize = 512;
                pageSize  = 128;
            }
            default: 
            { 
                Die(0x0A); // not implemented
            }
        }
    }
    
    WriteBlock(byte blockNumber, byte[] dataBlock)
    {
        uint address = blockNumber * blockSize; // 16 bit type so will truncate
        byte blockAddress = i2cEEPROMaddress;
        uint index;
        if ((blockSize == 512) && (blockNumber > 127))
        {
            blockAddress |= 0x04;
        }
        for (uint pageOffset = 0; pageOffset < blockSize; pageOffset += pageSize)
        {
            uint pageAddress = address + pageOffset;
            Wire.BeginTx(blockAddress);
            Wire.Write(byte(pageAddress >> 8));
            Wire.Write(byte(pageAddress & 0xFF));
            for (byte i = 0; i < pageSize; i++)
            {
                Wire.Write(dataBlock[index]);
                index++;
            }
            byte result = Wire.EndTx();
            // 5ms delay for EEPROM write (up to pageSize bytes)
            Time.Delay(5);
        }
    }
    ReadBlock(byte blockNumber, byte[] dataBlock)
    {
        uint address = blockNumber * blockSize; // 16 bit type so will truncate
        uint index;
        byte blockAddress = i2cEEPROMaddress;
        if ((blockSize == 512) && (blockNumber > 127))
        {
            blockAddress |= 0x04;
        }
        for (uint pageOffset = 0; pageOffset < blockSize; pageOffset += pageSize)
        {
            uint pageAddress = address + pageOffset;
            Wire.BeginTx(blockAddress);
            Wire.Write(byte(pageAddress >> 8));
            Wire.Write(byte(pageAddress & 0xFF));
            byte result = Wire.EndTx();
            
            byte bytes2 = Wire.RequestFrom(blockAddress, pageSize);
            for (uint i = 0; i < bytes2; i++)
            {
                dataBlock[index] = Wire.Read();
                index++;
            }
        }
    }
}
