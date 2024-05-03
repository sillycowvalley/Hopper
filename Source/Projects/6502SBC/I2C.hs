unit I2C
{
    const byte SDA = 16;
    const byte SCL = 17;
    
    byte address;
    byte Address { get { return address; } set { address = value; } }
       
    
    delay()
    {
        //uint i = 1; // 'NOP' that optimizer doesn't remove
    }
    sdaLow()
    {
        PinMode(I2C.SDA, PinModeOption.Output);
        DigitalWrite(I2C.SDA, false);
    }
    sdaHigh()
    {
        PinMode(I2C.SDA, PinModeOption.Input);
    }
    sclLow()
    {
        PinMode(I2C.SCL, PinModeOption.Output);
        DigitalWrite(I2C.SCL, false);
    }
    sclHigh()
    {
        PinMode(I2C.SCL, PinModeOption.Input);
        DigitalWrite(I2C.SCL, false); // do we need this?!
    }
    bool sdaRead()
    {
        PinMode(I2C.SDA, PinModeOption.Input);
        return DigitalRead(I2C.SDA);
    }
    bool checkAck()
    {
        sclHigh();
        delay();
        bool ack = sdaRead();
        sclLow();
        //Write(ack ? '+' : '-');
        return ack;
    }
    
    Initialize()
    {
        sdaHigh();
        sclHigh();
    }
    
    Start(bool isWrite)
    {
        Write('[');
        
        sdaHigh();
        delay();
        sclHigh();
        delay();
        sdaLow();
        delay();
        sclLow();
        delay();
        
        byte value = (address << 1) + (isWrite ? 0 : 1);
        _ = ByteOut(value);
    }
    bool ByteOut(byte value)
    {   
        Write(" " + value.ToHexString(2));
        
        for (byte bit = 0; bit < 8; bit++)
        {
            if (value & 0b10000000 == 0)
            {
                sdaLow();
            }
            else
            {
                sdaHigh();
            }
            delay();
            sclHigh();
            delay();
            sclLow();
            delay();
            value = value << 1;
        }
        
        // clock in the ack bit
        return checkAck();
    }
    Stop()
    {
        sdaLow();
        delay();
        sclHigh();
        delay();
        sdaHigh();
        delay();
        
        WriteLn("]");
    }
}
