unit GC
{
    uint New(uint size, Type htype)
    {
        uint address = Allocate(size+2);
        WriteByte(address, byte(htype));
        WriteByte(address+1, 1); // reference count of 1 to start
        return address;   
    }
    uint Clone(uint original)
    {
        Type htype = Type(ReadByte(original));
        switch (htype)
        {
            case Type.Long:
            {
#ifdef INCLUDE_LONGS
                return HRLong.Clone(original);
#else
                Error = 0x0A;
#endif
            }
            case Type.Directory:
            {
#ifdef INCLUDE_FILESYSTEM
                return HRDirectory.Clone(original);
#else
                Error = 0x0A;
#endif
            }
            case Type.File:
            {
#ifdef INCLUDE_FILESYSTEM
                return HRFile.Clone(original);
#else
                Error = 0x0A;
#endif
            }
            case Type.Float:
            {
#ifdef INCLUDE_FLOATS
                return HRFloat.Clone(original);
#else
                Error = 0x0A;
#endif
            }
            case Type.String:
            {
                return HRString.Clone(original);
            }
            case Type.List:
            {
#ifdef INCLUDE_LISTS
                return HRList.Clone(original);
#endif
            }
            case Type.Array:
            {
                return HRArray.Clone(original);
            }
            case Type.Dictionary:
            {
#ifdef INCLUDE_DICTIONARIES
                return HRDictionary.Clone(original);
#endif
            }
            case Type.Pair:
            {
#ifdef INCLUDE_DICTIONARIES
                return HRPair.Clone(original);
#endif
            }
            case Type.Variant:
            {
                return HRVariant.Clone(original);
            }
            default:
            {
#ifdef CHECKED
                Runtime.Out4Hex(PC);
                WriteHex(PC); Write(':'); WriteHex(original); Write('-'); WriteHex(byte(htype)); ErrorDump(86);
                Error = 0x0A; // clone
#endif
            }
        }
        return 0;
    }
    AddReference(uint address)
    {
        byte referenceCount = ReadByte(address+1);
#ifdef CHECKED        
        if (referenceCount == 255)
        {
            ErrorDump(3);
            Error = 0x0B;
            return;
        }
#endif        
        referenceCount++;
        WriteByte(address+1, referenceCount);
    }
    Release(uint address)
    {
        byte referenceCount = ReadByte(address+1);
#ifdef CHECKED                
        if (referenceCount == 0)
        {
            WriteLn(); WriteHex(address); Write(':');WriteHex(ReadWord(address)); Write(' '); ErrorDump(78);
            Error = 0x0B;
            return;
        }
#endif
        referenceCount--;
        WriteByte(address+1, referenceCount);
        Type htype = Type(ReadByte(address));
        if (referenceCount == 0)
        {
            switch (htype)
            {
                case Type.String:
                {
                    Memory.Free(address); // easy : nothing to walk
                }
                case Type.Array:
                {
                    Memory.Free(address); // easy : nothing to walk
                }
                case Type.Long:
                {
#ifdef INCLUDE_FLOATS
                    Memory.Free(address); // easy : nothing to walk
#else
                    Error = 0x0A;
#endif
                }
                case Type.Float:
                {
#ifdef INCLUDE_LONGS
                    Memory.Free(address); // easy : nothing to walk
#else
                    Error = 0x0A;
#endif
                }
                case Type.Directory:
                {
#ifdef INCLUDE_FILESYSTEM
                    HRDirectory.Clear(address);
                    Memory.Free(address);
#else
                    Error = 0x0A;
#endif
                }
                case Type.File:
                {
#ifdef INCLUDE_FILESYSTEM
                    HRFile.Clear(address);
                    Memory.Free(address);
#else
                    Error = 0x0A;
#endif
                }
                case Type.List:
                {
#ifdef INCLUDE_LISTS
                    HRList.Clear(address);
                    Memory.Free(address);
#endif
                }
                case Type.Dictionary:
                {
#ifdef INCLUDE_DICTIONARIES
                    HRDictionary.Clear(address);
                    Memory.Free(address);
#endif
                }
                case Type.Pair:
                {
#ifdef INCLUDE_DICTIONARIES
                    HRPair.Clear(address);
                    Memory.Free(address);
#endif
                }
                case Type.Variant:
                {
                    HRVariant.Clear(address);
                    Memory.Free(address);
                }
                default:
                {
#ifdef CHECKED
                    Runtime.Out4Hex(PC);
                    ErrorDump(11);
                    Error = 0x0A; // release
#endif
                }
            }
        }
    }
    Dump(uint address)
    {
        Dump(address, 0);
    }
    Dump(uint address, uint indent)
    {
        if (indent > 0)
        {
            for (uint i = 0; i < indent; i++)
            {
                IO.Write(' ');
            }
            indent = indent + 2;
        }
        Type htype = Type(ReadByte(address));
        switch (htype)
        {
            case Type.Long:
            {
                HRLong.Dump(address, indent);
            }
            case Type.String:
            {
                HRString.Dump(address, indent);
            }
            case Type.Array:
            {
                HRArray.Dump(address, indent);
            }
            case Type.List:
            {
                HRList.Dump(address, indent);
            }
            case Type.Pair:
            {
                HRPair.Dump(address, indent);
            }
            case Type.Variant:
            {
                HRVariant.Dump(address, indent);
            }
            case Type.Dictionary:
            {
                HRDictionary.Dump(address, indent);
            }
            default:
            {
#ifdef CHECKED
                IO.WriteHex(byte(htype));
                ErrorDump(163);
                //Error = 0x0A; // dump
#endif
            }
        }
    }
}
