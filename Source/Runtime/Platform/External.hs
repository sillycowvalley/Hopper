unit External
{
    uses "/Source/Runtime/Emulation/Long.hs"
    uses "/Source/Runtime/Emulation/Float.hs"
    
    uint GetSegmentSizes()
    {
        return 0x8000; // size in Words: 0x8000 for Pi Pico, 0x4000 for Wemos D1 Mini
    }
    
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
    
    
    uint IntToUInt(int value)
    {
        uint result = UInt.FromBytes(value.GetByte(0), value.GetByte(1));
        return result;
    }
    
    int UIntToInt(uint value)
    {
        int result = Int.FromBytes(value.GetByte(0), value.GetByte(1));
        return result;
    }
    
    uint hopperLongFromNativeLong(long ln)
    {
        uint this = HRLong.New();    
        WriteByte(this+2, ln.GetByte(0));
        WriteByte(this+3, ln.GetByte(1));
        WriteByte(this+4, ln.GetByte(2));
        WriteByte(this+5, ln.GetByte(3));
        return this;
    }
    
    long nativeLongFromHopperLong(uint hrlong)
    {
        return Long.FromBytes(ReadByte(hrlong+2), ReadByte(hrlong+3), ReadByte(hrlong+4), ReadByte(hrlong+5));
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
    
    uint LongToFloat(uint hrlong)
    {
        long ln = nativeLongFromHopperLong(hrlong);
        return hopperFloatFromNativeFloat(float(ln));
    }
    uint IntToFloat(int i)
    {
        return hopperFloatFromNativeFloat(float(i));
    }
    uint UIntToFloat(uint ui)
    {
        return hopperFloatFromNativeFloat(float(ui));
    }
    uint FloatToString(uint hrfloat)
    {
        float fl = nativeFloatFromHopperFloat(hrfloat);
        string str = fl.ToString();
        uint result = HRString.New(); 
        foreach (var c in str)
        {
            HRString.Build(ref result, c);
        }
        return result;
    }
    
    
    uint hopperFloatFromNativeFloat(float fl)
    {
        uint this = HRFloat.New(); 
        WriteByte(this+2, fl.GetByte(0));
        WriteByte(this+3, fl.GetByte(1));
        WriteByte(this+4, fl.GetByte(2));
        WriteByte(this+5, fl.GetByte(3));
        return this;
    }
    
    float nativeFloatFromHopperFloat(uint hrfloat)
    {
        return Float.FromBytes(ReadByte(hrfloat+2), ReadByte(hrfloat+3), ReadByte(hrfloat+4), ReadByte(hrfloat+5));
    }
    
    uint FloatAdd(uint next, uint top)
    {
        return hopperFloatFromNativeFloat(nativeFloatFromHopperFloat(next) + nativeFloatFromHopperFloat(top)); 
    }
    uint FloatSub(uint next, uint top)
    {
        return hopperFloatFromNativeFloat(nativeFloatFromHopperFloat(next) - nativeFloatFromHopperFloat(top)); 
    }
    
    uint FloatDiv(uint next, uint top)
    {
        float ltop = nativeFloatFromHopperFloat(top);
        if (ltop == 0)
        {
            Error = 0x04; // division by zero attempted
        }
        return hopperFloatFromNativeFloat(nativeFloatFromHopperFloat(next) / ltop); 
    }
    uint FloatMul(uint next, uint top)
    {
        return hopperFloatFromNativeFloat(nativeFloatFromHopperFloat(next) * nativeFloatFromHopperFloat(top)); 
    }
    
    uint FloatEQ(uint next, uint top)
    {
        return (nativeFloatFromHopperFloat(next) == nativeFloatFromHopperFloat(top)) ? 1 : 0; 
    }
    uint FloatLT(uint next, uint top)
    {
        return (nativeFloatFromHopperFloat(next) < nativeFloatFromHopperFloat(top)) ? 1 : 0; 
    }
    uint FloatLE(uint next, uint top)
    {
        return (nativeFloatFromHopperFloat(next) <= nativeFloatFromHopperFloat(top)) ? 1 : 0; 
    }
    uint FloatGT(uint next, uint top)
    {
        return (nativeFloatFromHopperFloat(next) > nativeFloatFromHopperFloat(top)) ? 1 : 0; 
    }
    uint FloatGE(uint next, uint top)
    {
        return (nativeFloatFromHopperFloat(next) >= nativeFloatFromHopperFloat(top)) ? 1 : 0; 
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
    bool FunctionCall(uint address, byte opCode)
    {
        return false;    
    }
    WriteToJumpTable(uint jumpTable, byte opCode, InstructionDelegate instructionDelegate)
    {
        WriteWord(jumpTable + (byte(opCode) << 1), uint(instructionDelegate));
    }
}
