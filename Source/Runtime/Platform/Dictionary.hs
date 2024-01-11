unit HRDictionary
{
    uses "/Source/Runtime/Platform/GC"
    uses "/Source/Runtime/Platform/Pair"
    
// Dictionary memory map:
//   0000 heap allocator size
//   13   type = tDictionary
//   00   GC reference count
//   xx   kType: key type tString or tUint
//   xx   vType: value type
//   xxxx count of entries
//   xxxx capacity (number of slots in block of entries)
//   xxxx pEntries (memory block of HashStringEntry or HashUIntEntry entries)

// HashStringEntry memory block: (8 bytes per entry)
//   0000 heap allocator size
//   xxxx pKey (string)
//   xxxx hash; (16 bits)
//   ---- (blank to stay compatible with other platforms)
//   xxxx pValue variant
//   ...
//   xxxx pKey (string)
//   xxxx hash; (16 bits)
//   ---- (blank to stay compatible with other platforms)
//   xxxx pValue variant

// HashUIntEntry memory block:  (8 bytes per entry)
//   0000 heap allocator size
//   xxxx key
//   XXxx isOccupied; ; like key != nullptr in HashEntry (zero initialization means all !isOccupied == isEmpty to start)
//   ---- (blank to stay compatible with other platforms)
//   xxxx pValue variant
//   ...
//   xxxx key
//   XXxx isOccupied;
//   ---- (blank to stay compatible with other platforms)
//   xxxx pValue variant

    const uint idKType     = 2;
    const uint idVType     = 3;
    const uint idCount     = 4;
    const uint idCapacity  = 6;
    const uint idEntries   = 8;
    
    const uint ieKey       = 0;
    const uint ieOccupied  = 2;
    const uint ieHash      = 2;
    const uint ieValue     = 6;
    
    const uint entrySize   = 8;
    
    uint hashKey16(uint key)
    {
        uint length = ReadWord(key+2);
        uint hash = 0x9DC5;
        for (uint i = 0; i < length; i++)
        {
            uint ch = ReadByte(key+4+i);
            hash = hash ^ ch;
            hash = hash * 0x0193;
        }
        return hash;
    }
    
    
    uint New(Type ktype, Type vtype)
    {
        uint address = GC.New(8, Type.Dictionary);
        WriteByte(address+idKType, byte(ktype)); // key type
        WriteByte(address+idVType, byte(vtype)); // value type
        WriteWord(address+idCount, 0);           // count
        WriteWord(address+idCapacity, 0);        // capacity
        WriteWord(address+idEntries, 0);         // pEntries
        return address;
    }
    
    Type GetKeyType(uint this)
    {
        return Type(ReadByte(this+idKType));
    }
    Type GetValueType(uint this)
    {
        return Type(ReadByte(this+idVType));
    }
    
    uint GetCount(uint this)
    {
        return ReadWord(this+idCount);
    }
    
    
#ifdef CHECKED    
    // These checks verify that the type will fit and has the correct sign.
    // They do not do type checking in the compiler sense since this is a Runtime.
    bool verifyKey(uint this, Type ktype, uint key)
    {
        bool ok = true;
        loop
        {
            Type dkType = Type(ReadByte(this+idKType));
            if (dkType == Type.String)
            {
                if (ktype != Type.String)
                {
                    ok = false;
                    break;
                }
                Type stype = Type(ReadByte(key));
                if (stype != Type.String)
                {
                    ok = false;
                    break;
                }
            }
            else if (IsReferenceType(ktype))
            {
                ok = false;
                break;
            }
            else
            {
                switch (dkType)
                {
                    case Type.Char:
                    case Type.Byte:
                    {
                        if ((ktype == Type.Char) || (ktype == Type.Byte) || (ktype == Type.UInt) || (ktype == Type.Int))
                        {
                            ok = (key & 0xFF00) == 0; // 0..255
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                    case Type.UInt:
                    {
                        if ((ktype == Type.Char) || (ktype == Type.Byte) || (ktype == Type.UInt))
                        {
                        }
                        else if (ktype == Type.Int)
                        {
                            ok = (key & 0x8000) == 0; // +int
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                    case Type.Int:
                    {
                        if ((ktype == Type.Byte) || (ktype == Type.UInt))
                        {
                             ok = (key & 0x8000) == 0;       
                        }
                        else if (ktype == Type.Int)
                        {
                            
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                }
            }
            break;
        } // loop
        if (!ok)
        {
            ErrorDump(83);
            Error = 0x0B;
        }
        return ok;
    }
    bool verifyValue(uint this, Type vtype, uint value)
    {
        bool ok = true;
        Type dvType = Type(ReadByte(this+idVType));
        loop
        {
            if (IsReferenceType(dvType))
            {
                if (dvType == Type.Variant)
                {
                    // anything goes
                }
                else if (dvType != vtype)
                {
                    ok = false;
                }
            }
            else if (IsReferenceType(vtype))
            {
                ok = false;
            }
            else // both value types
            {
                switch (dvType)
                {
                    case Type.Char:
                    case Type.Byte:
                    {
                        if ((vtype == Type.Char) ||(vtype == Type.Byte) || (vtype == Type.UInt) || (vtype == Type.Int))
                        {
                            ok = (value & 0xFF00) == 0; // 0..255
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                    case Type.UInt:
                    {
                        if ((vtype == Type.Char) || (vtype == Type.Byte) || (vtype == Type.UInt))
                        {
                        }
                        else if (vtype == Type.Int)
                        {
                            ok = (value & 0x8000) == 0; // +int
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                    case Type.Int:
                    {
                        if ((vtype == Type.Byte) || (vtype == Type.UInt))
                        {
                             ok = (value & 0x8000) == 0;       
                        }
                        else if (vtype == Type.Int)
                        {
                            
                        }
                        else
                        {
                            ok = false;
                        }
                    }
                }
            }
            break;
        }
        if (!ok)
        {
            WriteHex(byte(dvType)); Write(':'); WriteHex(byte(vtype)); ErrorDump(84);
            Error = 0x0B;
        }
        return ok;
    }
#endif
    
    
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
        
        uint elements = ReadWord(address+idCount);
        byte ktype    = ReadByte(address+idKType);
        byte vtype    = ReadByte(address+idVType);
        
        IO.WriteHex(elements); IO.Write(' '); 
        IO.Write('k'); IO.WriteHex(ktype);IO.Write(' '); 
        IO.Write('v'); IO.WriteHex(vtype);
        
        uint pEntries    = ReadWord(address+idEntries);
        Write(' '); WriteHex(pEntries);
        
        uint iterator;
        Type kType;
        uint key;
        Type vType;
        uint value;
        while (next(address, ref iterator, ref kType, ref key, ref vType, ref value))
        {
            IO.WriteLn();
            for (uint i = 0; i < indent; i++)
            {
                IO.Write(' ');
            }
            if (kType == Type.String)
            {
                GC.Dump(key, indent);                
            }
            else
            {
                IO.WriteHex(key);
            }
            IO.Write(' ');
            if (IsReferenceType(vType))
            {
                GC.Dump(value, indent);    
            }
            else
            {
                IO.WriteHex(value);
            }
        }
        if (false)
        {
            WriteLn();
            uint capacity = ReadWord(address+idCapacity);
            uint pEntries = ReadWord(address+idEntries);
            for (uint i = 0; i < capacity; i++)
            {
                
                WriteHex(pEntries); Write(':');
                uint key = ReadWord(pEntries+ieKey);
                WriteHex(key); Write('-');
                uint hash = ReadWord(pEntries+ieHash);
                WriteHex(hash);
                Write('-');
                uint value = ReadWord(pEntries+ieValue);
                WriteHex(value);
                
                pEntries = pEntries + entrySize;
                Write(' ');
                if (i % 5 == 6) { WriteLn(); }
            }
        }
        
    }
    
    uint Clone(uint original)
    {
        Type dkType = Type(ReadByte(original+idKType));
        Type dvType = Type(ReadByte(original+idVType));
        
        uint clone = New (dkType, dvType);
        
        uint iterator;
        Type ktype;
        uint key;
        Type vtype;
        uint value;
        while (next(original, ref iterator, ref ktype, ref key, ref vtype, ref value))
        {
            Set(clone, key, ktype, value, vtype);
        }
        
        return clone;
    }
    Clear(uint this)
    {
        uint iterator;
        Type ktype;
        uint key;
        Type vtype;
        uint value;
        while (next(this, ref iterator, ref ktype, ref key, ref vtype, ref value))
        {
            if (ktype == Type.String)
            {
                GC.Release(key);
            }
            if (IsReferenceType(vtype))
            {
                GC.Release(value);
            }
        }
        uint pEntries = ReadWord(this+idEntries);
        if (pEntries != 0)
        {
            Memory.Free(pEntries);
        }
        WriteWord(this+idCount, 0);           // count
        WriteWord(this+idCapacity, 0);        // capacity
        WriteWord(this+idEntries, 0);         // pEntries
    }
    
    bool next(uint this, ref uint iterator, ref Type ktype, ref uint key, ref Type vtype, ref uint value) // utility
    {
        uint count = ReadWord(this+idCount);
        if (count == 0)
        {
            return false;
        }
        if (iterator == 0xFFFF)
        {
            return false;
        }
        bool success = false;
        ktype = Type(ReadByte(this+idKType));
        vtype = Type(ReadByte(this+idVType));
        bool isValueTable = (ktype != Type.String);
        
        uint pEntries = ReadWord(this+idEntries);
        uint capacity = ReadWord(this+idCapacity);
        loop
        {
            uint pEntry = pEntries + entrySize * iterator;
            iterator = (iterator + 1) % capacity;
            
            if (isValueTable)
            {
                value = ReadWord(pEntry+ieValue);
                bool isOccupied = ReadByte(pEntry+ieOccupied) != 0;
                if (!isOccupied)
                {
                    // empty entry or tombstone
                }
                else
                {
                    key   = ReadWord(pEntry+ieKey);  
                    value = ReadWord(pEntry+ieValue);
                    if (iterator == 0)
                    {
                        iterator = 0xFFFF; // end indicator for foreach loop
                    }
                    success = true;
                    break;
                     
                }
            }
            else // reference table
            {
                key = ReadWord(pEntry+ieKey);    
                if (key == 0)
                {
                    // empty entry or tombstone
                }
                else
                {
                    value = ReadWord(pEntry+ieValue);
                    if (iterator == 0)
                    {
                        iterator = 0xFFFF; // end indicator for foreach loop
                    }
                    success = true;
                    break;
                }
                
            }
            
            if (iterator == 0)
            {
                success = false;
                break;
            }
        }
        if (success && IsReferenceType(vtype))
        {
            vtype = Type(ReadByte(value));
        }
        return success;
    }
    
    uint findEntry(uint pEntries, uint capacity, uint key, uint hash, bool valueKeys)
    {
        uint index;
        if (valueKeys)
        {
            index = key % capacity;
        }
        else
        {
            index = hash % capacity;
        }
        uint tombstone = 0;
        loop
        {
            uint pEntry = pEntries + index * entrySize;
            uint ekey   = ReadWord(pEntry+ieKey);
            if (valueKeys)
            {
                bool isOccupied = ReadByte(pEntry+ieOccupied) != 0;
                if (!isOccupied) 
                {
                    uint value = ReadWord(pEntry+ieValue);
                    if (value == 0)
                    {
                        // empty entry
                        return tombstone != 0 ? tombstone : pEntry;
                    }
                    else
                    {
                        // tombstone
                        if (tombstone == 0)
                        {
                            tombstone = pEntry;
                        }
                    }
                }
                else if (ekey == key)
                {
                    return pEntry;
                }
            }
            else // string keys
            {
                if (ekey == 0)
                {
                    uint value = ReadWord(pEntry+ieValue);
                    if (value == 0)
                    {
                        // empty entry
                        return tombstone != 0 ? tombstone : pEntry;
                    }
                    else
                    {
                        // tombstone
                        if (tombstone == 0)
                        {
                            tombstone = pEntry;
                        }
                    }
                }
                else if (HRString.Compare(ekey, key) == 0)
                {
                    return pEntry;
                }
            }
            index = (index + 1) % capacity;
        } // loop
        
        return 0;
    }
    
    bool validEntry(uint pEntry, bool valueKeys)
    {
        if (pEntry == 0)
        {
            return false;
        }
        else if (valueKeys)
        {
            return ReadByte(pEntry+ieOccupied) != 0; // isOccupied
        }
        else
        {
            return ReadWord(pEntry+ieKey) != 0;
        }
        return false;
    }
    
    adjustCapacity(uint this, uint newCapacity)
    {
        Type dkType = Type(ReadByte(this+idKType));
        bool valueKeys = (dkType != Type.String);
        uint sizeRequired = entrySize * newCapacity;
        
        uint pNewEntries = Memory.Allocate(sizeRequired);
        Memory.Set(pNewEntries, 0, sizeRequired);
        
        uint pOldEntries = ReadWord(this+idEntries);
        uint capacity = ReadWord(this+idCapacity);
                
        
        uint count = 0;
        for (uint i = 0; i < capacity; i++)
        {
            uint pOldEntry = pOldEntries + i * entrySize;
            if (!validEntry(pOldEntry, valueKeys))
            {
                continue;
            }
            uint key   = ReadWord(pOldEntry+ieKey);
            uint hash  = ReadWord(pOldEntry+ieHash);
            uint pNewEntry = findEntry(pNewEntries, newCapacity, key, hash, valueKeys);
            WriteWord(pNewEntry+ieKey, key);
            WriteWord(pNewEntry+ieHash, hash);
            WriteWord(pNewEntry+ieValue, ReadWord(pOldEntry+ieValue));
            count++;
        }
        
        if (0 != pOldEntries)
        {
            Memory.Free(pOldEntries);
        }
        WriteWord(this+idCount, count);           // count
        WriteWord(this+idCapacity, newCapacity);  // capacity
        WriteWord(this+idEntries, pNewEntries);   // pEntries
    }
    
    bool Next(uint this, ref uint iterator, ref uint hrpair) // syscall
    {
        Type ktype;
        uint key;
        Type vtype;
        uint value;
        
        bool found = next(this, ref iterator, ref ktype, ref key, ref vtype, ref value);
        if (found && (0 != value) && (vtype == Type.Variant))
        {
            value = HRVariant.UnBox(value, ref vtype); // no cloning
        }
        hrpair = HRPair.New(ktype, key, vtype, value); // clones reference type members, unboxes variants
        return found;
    }
    
    Set(uint this, uint key, Type ktype, uint value, Type vtype)
    {
        Type dkType = Type(ReadByte(this+idKType));
        Type dvType = Type(ReadByte(this+idVType));
#ifdef CHECKED
        if (!verifyKey(this, ktype, key))
        {
            return;
        }
        if (!verifyValue(this, vtype, value))
        {
            return;
        }
#endif     
        uint count    = ReadWord(this + idCount);
        uint capacity = ReadWord(this + idCapacity);
        bool valueKeys = (dkType != Type.String);
        
        if (4 * (count + 1) > capacity * 3) // max load of 75%
        {
            uint newCapacity = 32;
            if (capacity >= 128)
            {
                newCapacity = capacity + 32;
            }
            else if (capacity > 0)
            {   
                newCapacity = capacity*2;
            }
            adjustCapacity(this, newCapacity);
        }
        
        uint pEntries = ReadWord(this+idEntries);
        capacity = ReadWord(this+idCapacity);
        
        uint hash;
        if (!valueKeys)
        {
            hash = hashKey16(key);
        }
        uint pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
        
        uint ekey   = ReadWord(pEntry+ieKey);
        uint evalue = ReadWord(pEntry+ieValue);
        
        bool isNewKey = (ekey == 0);
        bool existingValue = evalue != 0;
        if (valueKeys)
        {
            bool isOccupied = ReadByte(pEntry+ieOccupied) != 0;
            isNewKey = !isOccupied;
            existingValue = existingValue && isOccupied;
        }
        if (isNewKey && (evalue == 0))// new key so count++
        {
            WriteWord(this+idCount, ReadWord(this+idCount)+1);
        }
        if (valueKeys)
        {
            WriteWord(pEntry+ieKey, key);
            WriteWord(pEntry+ieOccupied, 1);
        }
        else
        {
            if (0 != ekey)
            {
                GC.Release(ekey);
            }
            WriteWord(pEntry+ieKey, GC.Clone(key));
            WriteWord(pEntry+ieHash, hash);
        }
        if (IsReferenceType(dvType))
        {
            if (existingValue)
            {
                GC.Release(evalue); // replacing an existing entry
            }
            WriteWord(pEntry+ieValue, GC.Clone(value));
        }
        else
        {
            WriteWord(pEntry+ieValue, value);
        }
    }
    
    uint Get(uint this, uint key, ref Type vtype)
    {
        Type dkType = Type(ReadByte(this+idKType));
        bool valueKeys = (dkType != Type.String);
        uint pEntries = ReadWord(this+idEntries);
        uint capacity = ReadWord(this+idCapacity);

        uint hash;
        if (!valueKeys)
        {
            hash = hashKey16(key);
        }
        uint pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
        if (!validEntry(pEntry, valueKeys))
        {
            ErrorDump(89); 
            Error = 0x03; // no entry for key in dictionary
            return 0;
        }
        Type dvType = Type(ReadByte(this+idVType));
        uint value = ReadWord(pEntry+ieValue);
        vtype = dvType;
        if (0 != value)
        {
            if (vtype == Type.Variant)
            {
                vtype = Type(ReadByte(value)); // <key,variant> can contain reference types or boxed value types (actual variants)
            }
            if (vtype == Type.Variant)
            {
                value = HRVariant.GetValue(value, ref vtype); // returns a clone of the boxed value
            }
            else if (IsReferenceType(vtype))
            {
                vtype = Type(ReadByte(value));
                value = GC.Clone(value);
            }
        }
        return value;
    }
    
    bool Contains(uint this, uint key)
    {
        Type dkType = Type(ReadByte(this+idKType));
        bool valueKeys = (dkType != Type.String);
        uint pEntries = ReadWord(this+idEntries);
        uint capacity = ReadWord(this+idCapacity);
        uint hash;
        if (!valueKeys)
        {
            hash = hashKey16(key);
        }
        uint pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
        return validEntry(pEntry, valueKeys);
    }
    
    
    

}
