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
    bool lastAck;
    string buffer;
    uint bufferIndex;
    
    BeginTx(byte i2cAddress)
    {
        lastAck = false;
        address = i2cAddress;
        start(true); // true means we are writing
    }
    byte EndTx()
    {
        stop();
        return lastAck ? 0 : 1;
    }
    Write(byte data)
    {
        byteOut(data);
    }
    byte RequestFrom(byte i2cAddress, byte bytes)
    {
        lastAck = false;
        buffer = "";
        bufferIndex = 0;
        if (bytes == 0)
        {
            return 0;
        }
        
        address = i2cAddress;
        start(false); // false means we are reading
        
        loop
        {
            byte data = byteIn();
            buffer += char(data); // Hopper strings are not null terminated so any char value, 0..255, is not a problem
            bytes--;
            // Acknowledge the byte unless it's the last one to read
            if (bytes == 0)
            {
                sendNack();
                break;
            }
            sendAck();
        } // loop
        
        stop();
        
        return byte(buffer.Length); // can never be more than 255 since 'bytes' argument is a byte
    }
    byte Read()
    {
        byte data; // Default return value if no data is available
        if (bufferIndex < buffer.Length)
        {
            data = byte(buffer[bufferIndex]);
            bufferIndex++;
        }
        return data;
    }
    
#else
    BeginTx(byte i2cAddress) library;
    byte EndTx() library;
    Write(byte data) library;
    byte RequestFrom(byte i2cAddress, byte bytes) library;
    byte Read() library;
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
    
    
    byte RequestFrom(byte i2cController, byte i2cAddress, byte bytes)
    {
        return RequestFrom(i2cAddress, bytes);
    }
    byte Read(byte i2cController)
    {
        return Read();
    }
    
    delay()
    {
        uint i = 0; // so the optimizer doesn't remove 'delay'
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
        // Ensure SDA is set as input to receive ACK
        PinMode(sda, PinModeOption.Input);
        
        // Pulse SCL to read ACK
        sclHigh();
        Time.Delay(10);
        bool ack = !DigitalRead(sda); // Read ACK; true if pulled low
        sclLow();
        //IO.Write(ack ? '+' : '-');
        return ack;
    }
    
    sendAck()
    {
        sdaLow(); // Pull SDA low to indicate ACK
        sclHigh();
        delay();
        sclLow();
        delay();
    }
    
    sendNack()
    {
        sdaHigh(); // Leave SDA high to indicate NACK
        sclHigh();
        delay();
        sclLow();
        delay();
    }
    
    start(bool isWrite)
    {
        //IO.Write('[');
        
        sdaHigh();
        delay();
        sclHigh();
        delay();
        sdaLow();
        delay();
        sclLow();
        delay();
        
        byte value = (address << 1) + (isWrite ? 0 : 1);
        byteOut(value);
    }
    byteOut(byte value)
    {   
        //IO.Write(" " + value.ToHexString(2));
        
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
        lastAck = checkAck();
    }
    
    byte byteIn()
    {
        byte data = 0;
        for (int bit = 0; bit < 8; bit++)
        {
            data = data << 1;
            sclHigh();
            delay();
            if (sdaRead())
            {
                data = data | 1;
            }
            sclLow();
            delay();
        }
        return data;
    }
    
    stop()
    {
        sdaLow();
        delay();
        sclHigh();
        delay();
        sdaHigh();
        delay();
        
        //IO.WriteLn("]");
    }
}
