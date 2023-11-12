unit HRFile
{
    uses "/Source/Runtime/Platform/External"
    
    // File memory map:
    //   0000 heap allocator size
    //   0F   type = tFile
    //   00   GC reference count
    //   00   bool:   isValid
    //   00   bool:   isReading
    //   00   bool:   isWriting
    //   0000 string: path
    //   0000 uint:   pos
    //   0000 <byte>: bytes (buffer)
    
    const uint fiValid    = 2;
    const uint fiReading  = 3;
    const uint fiWriting  = 4;
    const uint fiPath     = 5;
    const uint fiPos      = 7;
    const uint fiBuffer   = 9;
    
    uint New()
    {
        uint address = GC.New(11, Type.File);
        WriteByte(address+fiValid, 0);
        WriteByte(address+fiReading, 0);
        WriteByte(address+fiWriting, 0);
        WriteWord(address+fiPath,   HRString.New());
        WriteWord(address+fiPos,    0);
        WriteWord(address+fiBuffer, HRList.New(Type.Byte));
        return address;
    }
    uint Clone(uint original)
    {
        uint address = GC.New(11, Type.File);
        WriteByte(address+fiValid,   ReadByte(original+fiValid));
        WriteByte(address+fiReading, ReadByte(original+fiReading));
        WriteByte(address+fiWriting, ReadByte(original+fiWriting));
        WriteWord(address+fiPath,    HRString.Clone(ReadWord(original+fiPath)));
        WriteWord(address+fiPos,     ReadWord(original+fiPos));
        WriteWord(address+fiBuffer,  HRList.Clone(ReadWord(original+fiBuffer)));
        return address;    
    }
    Clear(uint this)
    {
        WriteByte(this+fiValid, 0);
        WriteByte(this+fiReading, 0);
        WriteByte(this+fiWriting, 0);
        GC.Release(ReadWord(this+fiPath));
        WriteWord(this+fiPath,   0);
        WriteWord(this+fiPos,    0);
        GC.Release(ReadWord(this+fiBuffer));
        WriteWord(this+fiBuffer, 0);
    }
    
    bool Exists(uint str)
    {
        return External.FileExists(str);
    }
    Delete(uint path)
    {
        External.FileDelete(path);
    }
    uint GetSize(uint path)
    {
        return External.FileGetSize(path);
    }
    uint GetTime(uint path)
    {
        return External.FileGetTime(path);
    }
    bool IsValid(uint this)
    {
        return (ReadByte(this+fiValid) != 0);
    }
    Flush(uint this)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0))
        {
            External.FileWriteAllBytes(ReadWord(this+fiPath), ReadWord(this+fiBuffer));
        }
        else
        {
            WriteByte(this+fiValid, 0);
        } 
    }
    Append(uint this, byte b)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0))
        {
            uint buffer = ReadWord(this+fiBuffer);
            HRList.Append(buffer, b, Type.Byte);
        }
        else
        {
            WriteByte(this+fiValid, 0);
        }   
    }
    Append(uint this, uint hrstr)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0))
        {
            uint buffer = ReadWord(this+fiBuffer);
            uint length = HRString.GetLength(hrstr);
            for (uint i = 0; i < length; i++)
            {
                HRList.Append(buffer, byte(HRString.GetChar(hrstr, i)), Type.Byte);
            }
        }
        else
        {
            WriteByte(this+fiValid, 0);
        }   
        
    }
    uint Create(uint hrpath)
    {
        if (Exists(hrpath))
        {
            Delete(hrpath);
        }
        uint address = HRFile.New();
        WriteByte(address+fiValid,   1);
        WriteByte(address+fiWriting, 1);
        GC.Release(ReadWord(address+fiPath));
        WriteWord(address+fiPath,    HRString.Clone(hrpath));
        
        return address;
    }
    uint Open(uint hrpath)
    {
        uint address = HRFile.New();
        if (Exists(hrpath))
        {
            WriteByte(address+fiValid,   1);
            WriteByte(address+fiReading, 1);
            GC.Release(ReadWord(address+fiPath));
            WriteWord(address+fiPath,    HRString.Clone(hrpath));
            External.FileReadAllBytes(ReadWord(address+fiPath), ReadWord(address+fiBuffer));
        }
        return address;
    }
    byte Read(uint this)
    {
        byte b;
        loop
        {
            if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiReading) != 0))
            {
                uint buffer = ReadWord(this+fiBuffer);
                uint pos    = ReadWord(this+fiPos);
                
                uint length = HRList.GetLength(buffer);
                if (pos < length)
                {
                    Type itype;
                    b = byte(HRList.GetItem(buffer, pos, ref itype));
                    pos++;
                    WriteWord(this+fiPos, pos);
                    break;
                }
            }
            WriteByte(this+fiValid, 0);
            break;
        } 
        return b;
    }
    byte Read(uint this, uint hrseekpos)
    {
        byte b;
        loop
        {
            if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiReading) != 0))
            {
                uint buffer  = ReadWord(this+fiBuffer);
                uint seekpos = HRLong.ToUInt(hrseekpos);
                
                uint length = HRList.GetLength(buffer);
                if (seekpos < length)
                {
                    Type itype;
                    b = byte(HRList.GetItem(buffer, seekpos, ref itype));
                    break;
                }
            }
            WriteByte(this+fiValid, 0);
            break;
        } 
        return b;
    }
    uint ReadLine(uint this)
    {
        uint str = HRString.New();
        loop
        {
            if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiReading) != 0))
            {
                uint buffer = ReadWord(this+fiBuffer);
                uint pos    = ReadWord(this+fiPos);
                
                uint length = HRList.GetLength(buffer);
                if (pos < length)
                {
                    loop
                    {
                        if (pos == length)
                        {
                            if (HRString.GetLength(str) > 0)
                            {
                                WriteByte(this+fiValid, 1);    
                            }
                            else
                            {
                                WriteByte(this+fiValid, 0); // EOF    
                            }
                            break;
                        }
                        Type itype;
                        byte b = byte(HRList.GetItem(buffer, pos, ref itype));
                        pos++;
                        WriteWord(this+fiPos, pos);
                        if (b == 0x0D)
                        {
                            continue;
                        }
                        if (b == 0x0A)
                        {
                            break;
                        }
                        HRString.Build(ref str, char(b));
                    }
                    break;
                }
            }
            WriteByte(this+fiValid, 0);
            break;
        } 
        return str;
    }
}
