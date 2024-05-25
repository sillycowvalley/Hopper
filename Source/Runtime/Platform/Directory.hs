unit HRDirectory
{
    // Directory memory map:
    //   0000 heap allocator size
    //   10   type = tDirectory
    //   00   GC reference count
    //   00   bool:   isValid
    //   0000 string: path
    
    const uint diValid    = 2;
    const uint diPath     = 3;
    
    uint New()
    {
        uint address = GC.New(3, Type.Directory);
        WriteByte(address+diValid, 0);
        WriteWord(address+diPath,   HRString.New());
        return address;
    }
    uint Clone(uint original)
    {
        uint address = GC.New(3, Type.Directory);
        WriteByte(address+diValid,   ReadByte(original+diValid));
        WriteWord(address+diPath,    HRString.Clone(ReadWord(original+diPath)));
        return address;    
    }
    Clear(uint this)
    {
        WriteByte(this+diValid, 0);
        GC.Release(ReadWord(this+diPath));
    }
    
    bool IsValid(uint this)
    {
        return (ReadByte(this+diValid) != 0);
    }
    
    bool Exists(uint hrpath)
    {
        return External.DirectoryExists(hrpath);
    }
    uint Open(uint hrpath)
    {
        uint address = HRDirectory.New();
        WriteWord(address+diPath,    HRString.Clone(hrpath));
        if (Exists(hrpath))
        {
            WriteByte(address+diValid, 1);
        }
        return address;
    }
    Create(uint hrpath)
    {
        if (!Exists(hrpath))
        {
            External.DirectoryCreate(hrpath);
        }
    }
    Delete(uint hrpath)
    {
        External.DirectoryDelete(hrpath);
    }
    uint GetTime(uint path)
    {
        return External.DirectoryGetTime(path);
    }
    uint GetDate(uint path)
    {
        return External.DirectoryGetDate(path);
    }
    uint GetFileCount(uint hrdir)
    {
        uint skipped;
        return IsValid(hrdir) ? External.DirectoryGetFileCount(ReadWord(hrdir+diPath), ref skipped) : 0;
    }
    uint GetFileCount(uint hrdir, ref uint skipped)
    {
        return IsValid(hrdir) ? External.DirectoryGetFileCount(ReadWord(hrdir+diPath), ref skipped) : 0;
    }
    uint GetDirectoryCount(uint hrdir)
    {
        uint skipped;
        return IsValid(hrdir) ? External.DirectoryGetDirectoryCount(ReadWord(hrdir+diPath), ref skipped) : 0;
    }
    uint GetDirectoryCount(uint hrdir, ref uint skipped)
    {
        return IsValid(hrdir) ? External.DirectoryGetDirectoryCount(ReadWord(hrdir+diPath), ref skipped) : 0;
    }
    uint GetFile(uint hrdir, uint index)
    {
        if (!IsValid(hrdir))
        {
            return HRString.New();
        }
        return External.DirectoryGetFile(ReadWord(hrdir+diPath), index);
    }
    uint GetDirectory(uint hrdir, uint index)
    {
        if (!IsValid(hrdir))
        {
            return HRString.New();
        }
        return External.DirectoryGetDirectory(ReadWord(hrdir+diPath), index);
    }
}
