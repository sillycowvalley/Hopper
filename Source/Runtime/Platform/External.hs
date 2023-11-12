unit External
{
    uses "/Source/Runtime/Emulation/Directory.hs"
    uses "/Source/Runtime/Emulation/File.hs"
    uses "/Source/Runtime/Emulation/Float.hs"
    uses "/Source/Runtime/Emulation/Long.hs"
    
    bool LoadAuto { get { return true; } }
    uint hopperStringFromNativeString(string str)
    {
        uint hrstring = HRString.New();    
        foreach (var ch in str)
        {
            HRString.Build(ref hrstring, ch);
        }
        return hrstring;
    }
    string nativeStringFromHopperString(uint hrstring)
    {
        string str;
        uint length = HRString.GetLength(hrstring);
        for (uint i = 0; i < length; i++)
        {
            str = str + HRString.GetChar(hrstring, i);
        }
        return str;
    }
    
    uint DirectoryGetFileCount(uint hrpath) 
    {
        string path = nativeStringFromHopperString(hrpath);
        directory dir = Directory.Open(path);
        if (dir.IsValid())
        {
            return dir.GetFileCount();
        }
        return 0;
    }
    uint DirectoryGetDirectoryCount(uint hrpath) 
    {
        string path = nativeStringFromHopperString(hrpath);
        directory dir = Directory.Open(path);
        if (dir.IsValid())
        {
            return dir.GetDirectoryCount();
        }
        return 0;
    }
    uint DirectoryGetFile(uint hrpath, uint index) 
    {
        string path = nativeStringFromHopperString(hrpath);
        directory dir = Directory.Open(path);
        if (dir.IsValid())
        {
            return hopperStringFromNativeString(dir.GetFile(index));
        }
        return HRString.New();
    }
    uint DirectoryGetDirectory(uint hrpath, uint index) 
    {
        string path = nativeStringFromHopperString(hrpath);
        directory dir = Directory.Open(path);
        if (dir.IsValid())
        {
            return hopperStringFromNativeString(dir.GetDirectory(index));
        }
        return HRString.New();
    }
    
    bool DirectoryExists(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        return Directory.Exists(path);
    }
    DirectoryDelete(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        Directory.Delete(path);
    }
    DirectoryCreate(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        Directory.Create(path);
    }
    uint DirectoryGetTime(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        return hopperLongFromNativeLong(Directory.GetTime(path));
    }
    bool FileExists(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        return File.Exists(path);
    }
    
    FileDelete(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        File.Delete(path);
    }
    uint FileGetSize(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        long nativeSize = File.GetSize(path);
        uint size = hopperLongFromNativeLong(nativeSize);
        return size;
    }
    uint FileGetTime(uint hrpath)
    {
        string path = nativeStringFromHopperString(hrpath);
        long nativeTime = File.GetTime(path);
        uint time = hopperLongFromNativeLong(nativeTime);
        return time;
    }
    FileWriteAllBytes(uint hrpath, uint buffer)
    {
        string path = nativeStringFromHopperString(hrpath);
        file f = File.Create(path);
        uint length = HRList.GetLength(buffer);
        for (uint i=0; i < length; i++)
        {
            Type itype;
            byte b = byte(HRList.GetItem(buffer, i, ref itype));
            f.Append(b);
        }
        f.Flush();
    }
    FileReadAllBytes(uint hrpath, uint buffer)
    {
        string path = nativeStringFromHopperString(hrpath);
        file f = File.Open(path);
        loop
        {
            byte b = f.Read();
            if (!f.IsValid())
            {
                break;
            }
            HRList.Append(buffer, b, Type.Byte);
        }
    }
    
    
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
