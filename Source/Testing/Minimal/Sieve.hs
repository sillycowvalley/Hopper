program Sieve
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
    
    const uint size = 200; // 8191;  // 200 -> 78
    const uint iterations = 1;
    
    bool[size] flagsGlobal;
    
    uint Sieve()
    {
        uint i; 
        uint k;
        uint prime;
        uint count;
        uint iter = iterations;
        
        for (iter = 1; iter <= iterations; iter ++)
        {
            count=0 ; 
            
            for (i = 0; i < size; i++)
            {
                flagsGlobal[i] = true;
            }
            for (i = 0; i < size; i++) 
            { 
                if (flagsGlobal[i])
                {
                    prime = i + i + 3; 
                    k = i + prime; 
                    while (k < size) 
                    { 
                        flagsGlobal[k] = false; 
                        k = k + prime; 
                    }
                    count = count + 1;
                }
            }
        }
        return count;
    }
    
    Hopper()
    {
        uint start = Time.Seconds;
        uint result = Sieve();
        uint elapsed = Time.Seconds - start;
        Serial.WriteChar(Char.EOL); WriteUInt(elapsed); Serial.WriteChar('s');
        Serial.WriteChar(Char.EOL); WriteUInt(result); Serial.WriteChar('!');    
    }
}
