unit Wire
{
    #define BITBANG_I2C
    
    const byte DefaultI2CController = 0;
    
    const byte DefaultI2CSDAPin     = 17;
    const byte DefaultI2CSCLPin     = 16;
    
    byte scl;
    byte sda;
    byte address;
    
    bool Initialize()
    {
        return Initialize(DefaultI2CController, DefaultI2CSDAPin, DefaultI2CSCLPin);
    }
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin)
    {
        return Initialize(i2cController, sdaPin, sclPin, 400);
    }
    bool Initialize(byte i2cController, byte sdaPin, byte sclPin, uint freqkHz)
    {
        sda = sdaPin;
        scl = sclPin;
        return true;
    }
#ifdef MCU    
    BeginTx(byte i2cAddress)
    {
        address = i2cAddress;
        start(true);
    }
    byte EndTx()
    {
        stop();
        return 0;
    }
    Write(byte data)
    {
        _ = byteOut(data);
    }
#else
    BeginTx(byte i2cAddress) library;
    byte EndTx() library;
    Write(byte data) library;
#endif    
    
    BeginTx(byte controller, byte i2cAddress)
    {
        BeginTx(i2cAddress);
    }
    byte EndTx(byte controller)
    {
        return EndTx();
    }
    Write(byte controller, byte data)
    {
        Write(data);
    }
    Write(byte controller, byte[] data, uint startIndex, uint length)
    {
        uint i = startIndex;
        loop
        {
            if (length == 0) { break; }
            Write(data[i]);
            i++;
            length--;
        }
    }
       
    
    delay()
    {
        //uint i = 1; // 'NOP' that optimizer doesn't remove
    }
    sdaLow()
    {
        PinMode(sda, PinModeOption.Output);
        DigitalWrite(sda, false);
    }
    sdaHigh()
    {
        PinMode(sda, PinModeOption.Input);
    }
    sclLow()
    {
        PinMode(scl, PinModeOption.Output);
        DigitalWrite(scl, false);
    }
    sclHigh()
    {
        PinMode(scl, PinModeOption.Input);
    }
    bool sdaRead()
    {
        PinMode(sda, PinModeOption.Input);
        return DigitalRead(sda);
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
    
    start(bool isWrite)
    {
        //Write('[');
        
        sdaHigh();
        delay();
        sclHigh();
        delay();
        sdaLow();
        delay();
        sclLow();
        delay();
        
        byte value = (address << 1) + (isWrite ? 0 : 1);
        _ = byteOut(value);
    }
    bool byteOut(byte value)
    {   
        //Write(" " + value.ToHexString(2));
        
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
    stop()
    {
        sdaLow();
        delay();
        sclHigh();
        delay();
        sdaHigh();
        delay();
        
        //WriteLn("]");
    }
}
