unit External
{
    
    uint GetMillis()
    {
        return hopperLongFromNativeLong(Millis); 
    }
    Delay(uint ms)
    {
        Time.Delay(ms);
    }
    DigitalWrite(byte pin, byte value)
    {
    }
    byte DigitalRead(byte pin)
    {
        return 0;
    }
    PinMode(byte pin, byte value)
    {
    }
    
    
    uint IntToUInt(int value) // RUNTIME
    {
        <byte> bytes = value.ToBytes();
        uint result = uint(bytes[0]) + uint(bytes[1]) << 8;
        return result;
    }
    
    int UIntToInt(uint value) // RUNTIME
    {
        int result;
        if ((value & 0x8000) != 0) // sign bit set?
        {
            // two's complement
            value = ~value; // 0xFFFF -> 0x0000, 0xFFFE -> 0x0001
            long lvalue = 0 - long(value) - 1;
            result = int(lvalue);
        }
        else
        {
            result = int(value); // '+int'
        }
        return result;
    }
    
    uint hopperLongFromNativeLong(long ln)
    {
        uint this = HRLong.New();    
        <byte> b = ln.ToBytes();
        WriteByte(this+2, b[0]);
        WriteByte(this+3, b[1]);
        WriteByte(this+4, b[2]);
        WriteByte(this+5, b[3]);
        return this;
    }
    
    HashKey(uint key, ref uint hashLSW, ref uint hashMSW)
    {
        uint length = ReadWord(key+2);
        
        // https://github.com/laubzega/sha256_6502/blob/master/sha256.s
        
        // 0x811C9DC5
        uint hash0 = 0xC5;
        uint hash1 = 0x9D;
        uint hash2 = 0x1C;
        uint hash3 = 0x81;
        
        uint a0;
        uint a1;
        uint a2;
        uint a3;
        uint b0;
        uint b1;
        uint b2;
        uint c0;
        uint c1;
        uint d0;
        for (uint i = 0; i < length; i++)
        {
            uint ch = ReadByte(key+4+i);
            
            // hash ^= ch
            hash0 = hash0 ^ ch;
          
            // hash *= 0x01000193;
            
            a0 = hash0 * 0x93;
            a1 = hash0 + (a0 >> 8);
            a2 = (a1 >> 8);
            a3 = (hash0 + (a2 >> 8));
            
            a0 = (a0 & 0xFF);
            a1 = (a1 & 0xFF);
            a2 = (a2 & 0xFF);
            a3 = (a3 & 0xFF);
            
            b0 = hash1 * 0x93;
            b1 = hash1 + (b0 >> 8);
            b2 = (b1 >> 8); 
            
            b0 = (b0 & 0xFF);
            b1 = (b1 & 0xFF);
            b2 = (b2 & 0xFF);
            
            c0 = hash2 * 0x93;
            c1 = (hash2 + (c0 >> 8)); 
            
            c0 = (c0 & 0xFF);
            c1 = (c1 & 0xFF);
            
            d0 = (hash3 * 0x93);
            d0 = (d0 & 0xFF);
            
            hash0 = a0;
            hash1 = a1 + b0 + (hash0 >> 8);
            hash2 = a2 + b1 + c0 + (hash1 >> 8);
            hash3 = a3 + b2 + c1 + d0 + + (hash2 >> 8);
            hash0 = hash0 & 0xFF;
            hash1 = hash1 & 0xFF;
            hash2 = hash2 & 0xFF;
            hash3 = hash3 & 0xFF;
        }
        
        hashLSW = hash0 + hash1 << 8;
        hashMSW = hash2 + hash3 << 8;
    }
    
    
    
    long nativeLongFromHopperLong(uint hrlong)
    {
        uint lsw = ReadWord(hrlong+2);
        uint msw = ReadWord(hrlong+4);
        
        long result;
        if (msw & 0x8000 != 0)
        {
            // two's complement
            msw = ~msw; lsw = ~lsw; // 0xFFFFFFFF -> 0x0000000, 0xFFFFFFFE -> 0x0000001
            lsw = lsw + 1;          //               0x0000000 -> 1           0x0000001 -> 2
            if (lsw == 0) // carry ...
            {
                msw++;
            }
            result = long(lsw) + long(msw) * 256 * 256; // << 16
            result = - result;   
        }
        else
        {
            result = long(lsw) + long(msw) * 256 * 256; // << 16
        }
        return result;
    }
    
    uint LongAdd(uint next, uint top)
    {
        return hopperLongFromNativeLong(nativeLongFromHopperLong(next) + nativeLongFromHopperLong(top)); 
    }
    uint LongSub(uint next, uint top)
    {
        return hopperLongFromNativeLong(nativeLongFromHopperLong(next) - nativeLongFromHopperLong(top)); 
    }
    
    uint LongDiv(uint next, uint top)
    {
        long ltop = nativeLongFromHopperLong(top);
        if (ltop == 0)
        {
            Error = 0x04; // division by zero attempted
        }
        return hopperLongFromNativeLong(nativeLongFromHopperLong(next) / ltop); 
    }
    uint LongMul(uint next, uint top)
    {
        return hopperLongFromNativeLong(nativeLongFromHopperLong(next) * nativeLongFromHopperLong(top)); 
    }
    uint LongMod(uint next, uint top)
    {
        long ltop = nativeLongFromHopperLong(top);
        if (ltop == 0)
        {
            Error = 0x04; // division by zero attempted
        }
        return hopperLongFromNativeLong(nativeLongFromHopperLong(next) % ltop); 
    }
    
    uint LongEQ(uint next, uint top)
    {
        return (nativeLongFromHopperLong(next) == nativeLongFromHopperLong(top)) ? 1 : 0; 
    }
    uint LongLT(uint next, uint top)
    {
        return (nativeLongFromHopperLong(next) < nativeLongFromHopperLong(top)) ? 1 : 0; 
    }
    uint LongLE(uint next, uint top)
    {
        return (nativeLongFromHopperLong(next) <= nativeLongFromHopperLong(top)) ? 1 : 0; 
    }
    uint LongGT(uint next, uint top)
    {
        return (nativeLongFromHopperLong(next) > nativeLongFromHopperLong(top)) ? 1 : 0; 
    }
    uint LongGE(uint next, uint top)
    {
        return (nativeLongFromHopperLong(next) >= nativeLongFromHopperLong(top)) ? 1 : 0; 
    }
    WatchDog()
    {
        // ping the MCU watchdog so it knows we are still alive
#ifdef SERIALCONSOLE
        // any code to prevent the optimizer from removing WatchDog()
        for (uint i = 0; i < 1; i++)
        {
        }
#endif
    }
}
