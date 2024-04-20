program rZ80
{
    #define CPU_Z80
    #define ROM_8K
    #define CDECL
    
    uses "/Source/Minimal/System"
    
    uses "/Source/Minimal/Memory"
    
    writeDigit(uint uthis)
    {
        byte digit = byte(uthis % 10);
        char c = digit.ToDigit();
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Serial.WriteChar(c);
    }
    WriteInt(int this)
    {
        if (this < 0)
        {
            Write('-');
            this = 0 - this;
        }
        uint uthis = uint(this);
        writeDigit(uthis);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    
    WriteHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        byte lsn = b & 0xF;
        Serial.WriteChar(msn.ToHex());
        Serial.WriteChar(lsn.ToHex());
    }
    WriteHex(uint u)
    {
        byte msb = byte(u >> 8);
        byte lsb = byte(u & 0xFF);
        WriteHex(msb);
        WriteHex(lsb);
    }
    
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
    
    Hopper()
    {
        //uint result = Fibo(10);
        //WriteUInt(result);
        loop
        {
            if (Serial.IsAvailable)
            {
                char ch = Serial.ReadChar();
                Serial.WriteChar(ch);
            }
        }
    }
}
