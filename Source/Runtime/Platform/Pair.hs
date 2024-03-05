unit HRPair
{
    uses "GC"
    uses "Dictionary"
     
// Pair memory map:
//   0000 heap allocator size
//   10   type = tPair
//   00   reference count
//   xx   kType
//   xx   vType
//   xxxx pKey   - always a variant
//   xxxx pData  - always a variant

    const uint ipKType = 2;
    const uint ipVType = 3;
    const uint ipKey   = 4;
    const uint ipValue = 6;

    uint New(Type ktype, uint key, Type vtype, uint value)
    {
        uint address = GC.New(6, Type.Pair);
        WriteByte(address+ipKType, byte(ktype)); // key type
        WriteByte(address+ipVType, byte(vtype)); // value type
        
        if (IsReferenceType(ktype) && (0 != key))
        {
            key = GC.Clone(key);
        }
        if (IsReferenceType(vtype) && (0 != value))
        {
            value = GC.Clone(value);
        }
        WriteWord(address+ipKey, key);
        WriteWord(address+ipValue, value);
        return address;
    }
    Type GetKeyType(uint this)
    {
        return Type(ReadByte(this+ipKType));
    }
    Type GetValueType(uint this)
    {
        return Type(ReadByte(this+ipVType));
    }
  
    uint Clone(uint original)
    {
        Type dkType = Type(ReadByte(original+ipKType));
        Type dvType = Type(ReadByte(original+ipVType));
        
        uint key   = ReadWord(original+ipKey);
        uint value = ReadWord(original+ipValue);
        
        return New(dkType, key, dvType, value);
    }
    Clear(uint this)
    {
        Type dkType = Type(ReadByte(this+ipKType));
        Type dvType = Type(ReadByte(this+ipVType));
        
        uint key   = ReadWord(this+ipKey);
        uint value = ReadWord(this+ipValue);
        
        if (IsReferenceType(dkType) && (0 != key))
        {
            GC.Release(key);
        }
        if (IsReferenceType(dvType) && (0 != value))
        {
            GC.Release(value);
        }
        WriteWord(this+ipKey, 0);
        WriteWord(this+ipValue, 0);
    }
    uint GetKey(uint this, ref Type ktype)
    {
        ktype = Type(ReadByte(this+ipKType));
        uint key   = ReadWord(this+ipKey);
        if (IsReferenceType(ktype))
        {
            key = GC.Clone(key);
            ktype = Type(ReadByte(key)); // should be Type.String ..
        }
        return key;
    }
    uint GetValue(uint this, ref Type vtype)
    {
        vtype = Type(ReadByte(this+ipVType));
        uint value = ReadWord(this+ipValue);
        if (vtype == Type.Variant)
        {
            value = HRVariant.GetValue(value, ref vtype); // returns a clone of the boxed value
        }
        else if (IsReferenceType(vtype))
        {
            value = GC.Clone(value);
            vtype = Type(ReadByte(value));
        }
        return value;
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
        
        byte ktype    = ReadByte(address+ipKType);
        byte vtype    = ReadByte(address+ipVType);
        
        IO.Write(' '); 
        IO.Write('k'); IO.WriteHex(ktype);IO.Write(' '); 
        IO.Write('v'); IO.WriteHex(vtype);
        
        uint key    = ReadWord(address+ipKey);
        Write(' '); WriteHex(key);
        uint value    = ReadWord(address+ipValue);
        Write(' '); WriteHex(value);
    }
}
