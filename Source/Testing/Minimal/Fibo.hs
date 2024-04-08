program Fibo
{
    //#define CPU_Z80
    //#define MCU

    uses "/Source/Minimal/System"
    uses "/Source/Minimal/Serial"
    
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
    
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
    Hopper()
    {
        uint start = Time.Seconds;
        uint result = Fibo(10); // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
        uint elapsed = Time.Seconds - start;
        Serial.WriteChar(Char.EOL); WriteUInt(elapsed); Serial.WriteChar('s');
        Serial.WriteChar(Char.EOL); WriteUInt(result); Serial.WriteChar('!');    
    }
}
