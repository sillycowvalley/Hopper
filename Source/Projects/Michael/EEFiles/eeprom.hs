program EEPROM
{
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    
    uses "/Source/Library/Devices/SerialEEPROM"
    
    WriteBlock(byte blockNumber)
    {
        byte[512] buffer;
        
        uint blockSize = BlockStorage.BlockSize;
        for (uint i = 0; i < blockSize; i++)
        {
            buffer[i] = blockNumber;
        }
        BlockStorage.WriteBlock(blockNumber, buffer);
    }
    ReadBlock(byte blockNumber)
    {
        byte[512] buffer;
        BlockStorage.ReadBlock(blockNumber, buffer);
        
        uint blockSize = BlockStorage.BlockSize;
        
        long address = long(blockNumber) * blockSize;
        IO.WriteLn(address.ToHexString(5) + " " + blockSize.ToString());
        
        for (uint i = 0; i < blockSize; i++)
        {
            IO.Write(" " + (buffer[i]).ToHexString(2));
            if (i % 32 == 31)
            {
                IO.Write(" ");
            }
            if (i % 64 == 63)
            {
                IO.WriteLn();
            }
        }
    }
    
    
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
        
        BlockStorage.Configure(SerialEEPROM.XX1024);
        //BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        
        
        // Clear : test pattern
        
        //WriteBlock(0);
        //WriteBlock(1);
        //WriteBlock(2);
        //WriteBlock(3);
        //WriteBlock(128);
        //WriteBlock(129);
        
        
        ReadBlock(0);
        //ReadBlock(1);
        //ReadBlock(128);
        //ReadBlock(129);
             
    }
}
