unit HRFile
{
    uses "/Source/Runtime/Platform/External"
    
    // File memory map:
    //   0000 heap allocator size
    //   0F   type = tFile
    //   00   GC reference count
    //   00   bool:   isValid
    //   00   bool:   isReading: 32 bit pos is position of next byte to read (Read and ReadLine)
    //   00   bool:   isWriting
    //   00   bool:   isCode:    16 bit pos is length and buffer is start in codeSegment
    //   0000     string: path
    //   00000000 uint32: pos
    //   0000     uint:   string (buffer)
    //   00000000 uint32: size: 32 bit file size in bytes used to read (Read and ReadLine)
    
    const uint fiValid    = 2;
    const uint fiReading  = 3;
    const uint fiWriting  = 4;
    const uint fiCode     = 5;
    const uint fiPath     = 6;
    const uint fiPos      = 8;
    const uint fiBuffer   = 12; // writing
    const uint fiSize     = 14; // reading
    
    uint New()
    {
        uint address = GC.New(16, Type.File);
        WriteByte(address+fiValid,   0);
        WriteByte(address+fiReading, 0);
        WriteByte(address+fiWriting, 0);
        WriteByte(address+fiCode,    0);
        WriteWord(address+fiPath,   HRString.New());
        WriteWord(address+fiPos,    0);
        WriteWord(address+fiPos+2,  0);
        WriteWord(address+fiBuffer, HRString.New());
        WriteWord(address+fiSize,   0);
        WriteWord(address+fiSize+2, 0);
        return address;
    }
    uint Clone(uint original)
    {
        uint address = GC.New(16, Type.File);
        WriteByte(address+fiValid,   ReadByte(original+fiValid));
        WriteByte(address+fiReading, ReadByte(original+fiReading));
        WriteByte(address+fiWriting, ReadByte(original+fiWriting));
        WriteByte(address+fiCode,    ReadByte(original+fiCode));
        WriteWord(address+fiPath,    HRString.Clone(ReadWord(original+fiPath)));
        WriteWord(address+fiPos,     ReadWord(original+fiPos));
        WriteWord(address+fiPos+2,   ReadWord(original+fiPos+2));
        if (IsCode(original))
        {
            WriteWord(address+fiBuffer,     ReadWord(original+fiBuffer));
        }
        else
        {
            WriteWord(address+fiBuffer, HRString.Clone(ReadWord(original+fiBuffer)));
        }
        WriteWord(address+fiSize,     ReadWord(original+fiSize));
        WriteWord(address+fiSize+2,   ReadWord(original+fiSize+2));
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
        WriteWord(this+fiPos+2,  0);
        if (!IsCode(this))
        {
            GC.Release(ReadWord(this+fiBuffer));
        }
        WriteWord(this+fiBuffer, 0);
        WriteWord(this+fiSize, 0);
        WriteWord(this+fiSize+2, 0);
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
    bool IsCode(uint this)
    {
        return (ReadByte(this+fiCode) != 0);
    }
    Flush(uint this)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0))
        {
            if (ReadByte(this+fiCode) == 0)
            {
                uint content = ReadWord(this+fiBuffer);
                External.FileWriteAllBytes(ReadWord(this+fiPath), content, true);
                HRString.BuildClear(ref content);
            }
            else
            {
                // isCode: pos is length and buffer is start in codeSegment
                uint posLSW = ReadWord(this+fiPos); // code is <= 64K, LSW of long should be good
                External.FileWriteAllCodeBytes(ReadWord(this+fiPath), ReadWord(this+fiBuffer), posLSW);
            }
        }
        else
        {
            WriteByte(this+fiValid, 0);
        } 
    }
    Append(uint this, byte b)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0) && (ReadByte(this+fiCode) == 0))
        {
            uint buffer = ReadWord(this+fiBuffer);
            HRString.BuildChar(ref buffer, char(b));
            uint length = HRString.GetLength(buffer);
            if (length >= 256) // auto-Flush the buffer
            {
                External.FileWriteAllBytes(ReadWord(this+fiPath), buffer, true); // appends if file exists
                HRString.BuildClear(ref buffer);
            }
            WriteWord(this+fiBuffer, buffer);
        }
        else
        {
            WriteByte(this+fiValid, 0);
        }   
    }
    Append(uint this, uint hrstr)
    {
        if ((ReadByte(this+fiValid) != 0) && (ReadByte(this+fiWriting) != 0) && (ReadByte(this+fiCode) == 0))
        {
            uint buffer = ReadWord(this+fiBuffer);
            HRString.BuildString(ref buffer, hrstr);
            
            uint length = HRString.GetLength(buffer);
            if (length >= 256) // auto-Flush the buffer
            {
                External.FileWriteAllBytes(ReadWord(this+fiPath), buffer, true); // appends if file exists
                HRString.BuildClear(ref buffer);
            }
            WriteWord(this+fiBuffer, buffer);
        }
        else
        {
            WriteByte(this+fiValid, 0);
        }   
        
    }
    uint CreateFromCode(uint hrpath, uint codeStart, uint codeLength)
    {
        if (Exists(hrpath))
        {
            Delete(hrpath);
        }
        uint address = HRFile.New();
        WriteByte(address+fiValid,   1);
        WriteByte(address+fiWriting, 1);
        WriteByte(address+fiCode, 1);
        
        GC.Release(ReadWord(address+fiPath));
        WriteWord(address+fiPath,    HRString.Clone(hrpath));
        
        // release the default string buffer created in HRFile.New(..)
        GC.Release(ReadWord(address+fiBuffer));
        
        // isCode: pos is length and buffer is start in codeSegment
        WriteWord(address+fiPos,       codeLength); // LSW
        WriteWord(address+fiPos+2,              0); // MSW
        WriteWord(address+fiBuffer,    codeStart);
        
        return address;
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
            
            // buffer is probably empty string (doesn't matter)
            WriteWord(address+fiPos,  0);
            WriteWord(address+fiPos+2,  0);
            
            uint hrsize = HRFile.GetSize(hrpath);
            WriteByte(address+fiSize+0, HRLong.GetByte(hrsize, 0));
            WriteByte(address+fiSize+1, HRLong.GetByte(hrsize, 1));
            WriteByte(address+fiSize+2, HRLong.GetByte(hrsize, 2));
            WriteByte(address+fiSize+3, HRLong.GetByte(hrsize, 3));
            GC.Release(hrsize);
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
                uint posLSW    = ReadWord(this+fiPos);
                uint posMSW    = ReadWord(this+fiPos+2);
                uint sizeLSW   = ReadWord(this+fiSize);
                uint sizeMSW   = ReadWord(this+fiSize+2);
                if (lt32(posLSW, posMSW, sizeLSW, sizeMSW)) // pos < size
                {
                    uint hrpos  = HRLong.FromBytes(ReadByte(this+fiPos+0), ReadByte(this+fiPos+1), 
                                                   ReadByte(this+fiPos+2), ReadByte(this+fiPos+3));
                    if (External.TryFileReadByte(ReadWord(this+fiPath), hrpos, ref b))
                    {
                        GC.Release(hrpos);
                        
                        if (posLSW == 0xFFFF)
                        {
                            posLSW = 0;
                            posMSW++;
                        }
                        else
                        {
                            posLSW++;
                        }
                        WriteWord(this+fiPos,   posLSW);
                        WriteWord(this+fiPos+2, posMSW);
                        
                        break;
                    }
                    GC.Release(hrpos);
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
                uint seekposLSW    = HRLong.GetByte(hrseekpos, 0) + (HRLong.GetByte(hrseekpos, 1) << 8);
                uint seekposMSW    = HRLong.GetByte(hrseekpos, 2) + (HRLong.GetByte(hrseekpos, 3) << 8);
                uint sizeLSW   = ReadWord(this+fiSize);
                uint sizeMSW   = ReadWord(this+fiSize+2);
                if (lt32(seekposLSW, seekposMSW, sizeLSW, sizeMSW)) // seekpos < size
                {
                    if (External.TryFileReadByte(ReadWord(this+fiPath), hrseekpos, ref b))
                    {
                        break;
                    }
                }
            }
            WriteByte(this+fiValid, 0);
            break;
        } 
        return b;
    }
    bool lt32(uint nextLSW, uint nextMSW, uint topLSW, uint topMSW)
    {
        if (nextMSW < topMSW)
        {
            return true;
        }
        if (nextMSW == topMSW)
        {
            return nextLSW < topLSW;
        }
        return false;
    }
    uint ReadLine(uint this)
    {
        return External.ReadLine(this);
    }
}
