unit HRVariant
{
    
    uses "/Source/Runtime/Platform/Types"
    
// Variant memory map:
//   0000 heap allocator size
//   14   type = tVariant
//   00   reference count
//   xx   actual type for item
//   xxxx data for value types, pData for reference types

    const uint ivType  = 2;
    const uint ivValue = 3;
    
    uint CreateValueVariant(uint value, Type vtype)
    {
#ifdef CHECKED
        if (IsReferenceType(vtype))
        {
            ErrorDump(67);
            Error = 0x0B;
        }
#endif
        uint address = GC.New(3, Type.Variant);
        WriteByte(address+ivType, byte(vtype));
        WriteWord(address+ivValue, value);
        return address;
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
        
        byte vtype    = ReadByte(address+ivType);
        IO.Write(' '); 
        IO.Write('v'); IO.WriteHex(vtype);
        uint value    = ReadWord(address+ivValue);
        Write(' '); WriteHex(value);
    }
    
    uint New(uint value, Type vtype)
    {
        uint address = GC.New(3, Type.Variant);
        WriteByte(address+ivType, byte(vtype));
        if (IsReferenceType(vtype))
        {
            value = GC.Clone(value);
        }
        WriteWord(address+ivValue, value);    
        return address;    
    }
    
    uint GetValue(uint this, ref Type vtype)
    {
        vtype = Type(ReadByte(this+ivType));
        uint value = ReadWord(this+ivValue);
        if (IsReferenceType(vtype))
        {
            value = GC.Clone(value);
            vtype = Type(ReadByte(value));
        }
        return value;
    }
    
    uint UnBox(uint this, ref Type vtype)
    {
        vtype = Type(ReadByte(this+ivType));
        return ReadWord(this+ivValue);
    }
    
    uint Clone(uint original)
    {
        Type vtype = Type(ReadByte(original+ivType));
        uint value = ReadWord(original+ivValue);
        return New(value, vtype);
    }
    Clear(uint this)
    {
        Type vtype = Type(ReadByte(this+ivType));
        uint value = ReadWord(this+ivValue);
        
        if (IsReferenceType(vtype) && (0 != value))
        {
            GC.Release(value);
        }
        WriteWord(this+ivType, 0);
        WriteWord(this+ivValue, 0);
    }
    
    bool IsEqual(uint left, Type ltype, uint right, Type rtype)
    {
        bool lref = Types.IsReferenceType(ltype);
        bool rref = Types.IsReferenceType(rtype);
        if (lref == rref)
        {
            if (lref)
            {
                // reference
                if (ltype == rtype)
                {
                    switch (ltype)
                    {
                        case Type.String:
                        {
                            return 0 == HRString.Compare(left, right);
                        }
                        case Type.Long:
                        {
                            return 0 != External.LongEQ(left, right);
                        }
                        //case Type.Float:
                        //{
                        //    ErrorDump(76);
                        //    Error = 0x0A;
                        //}
                        default:
                        {
                            ErrorDump(75);
                            Error = 0x0A;
                        }
                    }
                }
            }
            else
            {
                // value
                switch (ltype)
                {
                    case Type.Bool:
                    {
                        if (rtype != Type.Bool)
                        {
                            return false;    
                        }
                        return left == right;
                    }
                    case Type.Char:
                    {
                        if (rtype != Type.Char)
                        {
                            return false;    
                        }
                        return left == right;
                    }
                    case Type.Byte:
                    case Type.UInt:
                    {
                        switch (rtype)
                        {
                            case Type.Byte:
                            case Type.UInt:
                            {
                                return left == right;
                            }
                            case Type.Int:
                            {
                                if (0 == (0x8000 & right))
                                {
                                    return left == right;
                                }
                            }
                        }
                    }
                    case Type.Int:
                    {
                        switch (rtype)
                        {
                            case Type.Byte:
                            case Type.UInt:
                            {
                                if (0 == (0x8000 & left))
                                {
                                    return left == right;
                                }
                            }
                            case Type.Int:
                            {
                                return left == right;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }
    
    
}
