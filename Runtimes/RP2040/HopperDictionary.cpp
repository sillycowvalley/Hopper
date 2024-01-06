#include "HopperDictionary.h"

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
//   xxxxxxxx pValue variant
//   ...
//   xxxx pKey (string)
//   xxxx hash; (16 bits)
//   xxxxxxxx pValue variant

// HashUIntEntry memory block:  (8 bytes per entry)
//   0000 heap allocator size
//   xxxx key
//   XXxx isOccupied; ; like key != nullptr in HashEntry (zero initialization means all !isOccupied == isEmpty to start)
//   xxxxxxxx pValue variant
//   ...
//   xxxx key
//   XXxx isOccupied;
//   xxxxxxxx pValue variant

const UInt idKType     = 2;
const UInt idVType     = 3;
const UInt idCount     = 4;
const UInt idCapacity  = 6;
const UInt idEntries   = 8;

const UInt ieKey       = 0;
const UInt ieOccupied  = 2;
const UInt ieHash      = 2;
const UInt ieValue     = 4;

const UInt entrySize   = 8;

UInt hashKey16(UInt key)
{
    UInt length = Memory_ReadWord(key+2);
    UInt hash = 0x9DC5;
    for (UInt i = 0; i < length; i++)
    {
        UInt ch = Memory_ReadByte(key+4+i);
        hash = hash ^ ch;
        hash = hash * 0x0193;
    }
    return hash;
}


UInt HRDictionary_New(Type ktype, Type vtype)
{
    UInt address = GC_New(8, Type::eDictionary);
    Memory_WriteByte(address+idKType, (Byte)ktype); // key type
    Memory_WriteByte(address+idVType, (Byte)vtype); // value type
    Memory_WriteWord(address+idCount, 0);           // count
    Memory_WriteWord(address+idCapacity, 0);        // capacity
    Memory_WriteWord(address+idEntries, 0);         // pEntries
    return address;
}

bool dictionaryNext(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt32 & value) // utility
{
    UInt count = Memory_ReadWord(_this+idCount);
    if (count == 0)
    {
        return false;
    }
    if (iterator == 0xFFFF)
    {
        return false;
    }
    bool success = false;
    ktype = (Type)(Memory_ReadByte(_this+idKType));
    vtype = (Type)(Memory_ReadByte(_this+idVType));
    bool isValueTable = (ktype != Type::eString);
    
    UInt pEntries = Memory_ReadWord(_this+idEntries);
    UInt capacity = Memory_ReadWord(_this+idCapacity);
    for(;;) // loop
    {
        UInt pEntry = pEntries + entrySize * iterator;
        iterator = (iterator + 1) % capacity;
        
        if (isValueTable)
        {
            value = Memory_ReadWord(pEntry+ieValue) + (Memory_ReadWord(pEntry+ieValue+2) << 16);
            bool isOccupied = Memory_ReadByte(pEntry+ieOccupied) != 0;
            if (!isOccupied)
            {
                // empty entry or tombstone
            }
            else
            {
                key   = Memory_ReadWord(pEntry+ieKey);  
                value = Memory_ReadWord(pEntry+ieValue) + (Memory_ReadWord(pEntry+ieValue+2) << 16);
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
            key = Memory_ReadWord(pEntry+ieKey);    
            if (key == 0)
            {
                // empty entry or tombstone
            }
            else
            {
                value = Memory_ReadWord(pEntry+ieValue) + (Memory_ReadWord(pEntry+ieValue+2) << 16);
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
        vtype = (Type)(Memory_ReadByte(value & 0xFFFF));
    }
    return success;
}

UInt findEntry(UInt pEntries, UInt capacity, UInt key, UInt hash, Bool valueKeys)
{
    UInt index;
    if (valueKeys)
    {
        index = key % capacity;
    }
    else
    {
        index = hash % capacity;
    }
    UInt tombstone = 0;
    for (;;) // loop
    {
        UInt pEntry = pEntries + index * entrySize;
        UInt ekey   = Memory_ReadWord(pEntry+ieKey);
        if (valueKeys)
        {
            Bool isOccupied = Memory_ReadByte(pEntry+ieOccupied) != 0;
            if (!isOccupied) 
            {
                UInt value0 = Memory_ReadWord(pEntry+ieValue);
                UInt value1 = Memory_ReadWord(pEntry+ieValue+2);
                if ((value0 == 0) && (value1 == 0))
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
                UInt value0 = Memory_ReadWord(pEntry+ieValue);
                UInt value1 = Memory_ReadWord(pEntry+ieValue+2);
                if ((value0 == 0) && (value1 == 0))
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
            else if (HRString_Compare(ekey, key) == 0)
            {
                return pEntry;
            }
        }
        index = (index + 1) % capacity;
    } // loop
    
    return 0;
}

Bool validEntry(UInt pEntry, Bool valueKeys)
{
    if (pEntry == 0)
    {
        return false;
    }
    else if (valueKeys)
    {
        return Memory_ReadByte(pEntry+ieOccupied) != 0; // isOccupied
    }
    else
    {
        return Memory_ReadWord(pEntry+ieKey) != 0;
    }
    return false;
}

void adjustCapacity(UInt _this, UInt newCapacity)
{
    Type dkType = (Type)Memory_ReadByte(_this+idKType);
    Bool valueKeys = (dkType != Type::eString);
    UInt sizeRequired = entrySize * newCapacity;
    
    UInt pNewEntries = Memory_Allocate(sizeRequired);
    Memory_Set(pNewEntries, 0, sizeRequired);
    
    UInt pOldEntries = Memory_ReadWord(_this+idEntries);
    UInt capacity = Memory_ReadWord(_this+idCapacity);
            
    
    UInt count = 0;
    for (UInt i = 0; i < capacity; i++)
    {
        UInt pOldEntry = pOldEntries + i * entrySize;
        if (!validEntry(pOldEntry, valueKeys))
        {
            continue;
        }
        UInt key   = Memory_ReadWord(pOldEntry+ieKey);
        UInt hash  = Memory_ReadWord(pOldEntry+ieHash);
        UInt pNewEntry = findEntry(pNewEntries, newCapacity, key, hash, valueKeys);
        Memory_WriteWord(pNewEntry+ieKey, key);
        Memory_WriteWord(pNewEntry+ieHash, hash);
        Memory_WriteWord(pNewEntry+ieValue, Memory_ReadWord(pOldEntry+ieValue));
        Memory_WriteWord(pNewEntry+ieValue+2, Memory_ReadWord(pOldEntry+ieValue+2));
        count++;
    }
    
    if (0 != pOldEntries)
    {
        Memory_Free(pOldEntries);
    }
    Memory_WriteWord(_this+idCount, count);           // count
    Memory_WriteWord(_this+idCapacity, newCapacity);  // capacity
    Memory_WriteWord(_this+idEntries, pNewEntries);   // pEntries
}

void HRDictionary_Clear(UInt _this)
{
    UInt iterator;
    Type ktype;
    UInt key;
    Type vtype;
    UInt32 value;
    while (dictionaryNext(_this, iterator, ktype, key, vtype, value))
    {
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        if (IsReferenceType(vtype))
        {
            GC_Release(value & 0xFFFF);
        }
    }
    UInt pEntries = Memory_ReadWord(_this+idEntries);
    if (pEntries != 0)
    {
        Memory_Free(pEntries);
    }
    Memory_WriteWord(_this+idCount, 0);           // count
    Memory_WriteWord(_this+idCapacity, 0);        // capacity
    Memory_WriteWord(_this+idEntries, 0);         // pEntries
}

void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt32 value, Type vtype)
{
    Type dkType = (Type)Memory_ReadByte(_this+idKType);
    Type dvType = (Type)Memory_ReadByte(_this+idVType);
    
    if (dvType == Type::eVariant)
    {
        if ((vtype == Type::eLong) || (vtype == Type::eFloat))
        {
            // box these types when running 32 bit stack slots:
            value = HRVariant_New(value, vtype);
        }
    }

    UInt count    = Memory_ReadWord(_this + idCount);
    UInt capacity = Memory_ReadWord(_this + idCapacity);
    bool valueKeys = (dkType != Type::eString);
    
    if (4 * (count + 1) > capacity * 3) // max load of 75%
    {
        UInt newCapacity = 32;
        if (capacity >= 128)
        {
            newCapacity = capacity + 32;
        }
        else if (capacity > 0)
        {   
            newCapacity = capacity*2;
        }
        adjustCapacity(_this, newCapacity);
    }
    
    UInt pEntries = Memory_ReadWord(_this+idEntries);
    capacity = Memory_ReadWord(_this+idCapacity);
    
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = hashKey16(key);
    }
    UInt pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
    
    UInt ekey    = Memory_ReadWord(pEntry+ieKey);
    UInt evalue0 = Memory_ReadWord(pEntry+ieValue);
    UInt evalue1 = Memory_ReadWord(pEntry+ieValue+2);
    
    bool isNewKey = (ekey == 0);
    bool existingValue = ((evalue0 != 0) || (evalue1 != 0));
    if (valueKeys)
    {
        bool isOccupied = Memory_ReadByte(pEntry+ieOccupied) != 0;
        isNewKey = !isOccupied;
        existingValue = existingValue && isOccupied;
    }
    if (isNewKey && (evalue0 == 0) && (evalue1 == 0))// new key so count++
    {
        Memory_WriteWord(_this+idCount, Memory_ReadWord(_this+idCount)+1);
    }
    if (valueKeys)
    {
        Memory_WriteWord(pEntry+ieKey, key);
        Memory_WriteWord(pEntry+ieOccupied, 1);
    }
    else
    {
        if (0 != ekey)
        {
            GC_Release(ekey);
        }
        Memory_WriteWord(pEntry+ieKey, GC_Clone(key));
        Memory_WriteWord(pEntry+ieHash, hash);
    }
    
    if (IsReferenceType(dvType))
    {
        if (existingValue)
        {
            GC_Release(evalue0); // replacing an existing entry
        }
        Memory_WriteWord(pEntry+ieValue, GC_Clone((value & 0xFFFF)));
        Memory_WriteWord(pEntry+ieValue+2, 0);
    }
    else
    {
        Memory_WriteWord(pEntry+ieValue,   (value & 0xFFFF));
        Memory_WriteWord(pEntry+ieValue+2, (value >> 16));
    }
}

bool HRDictionary_Next(UInt _this, UInt & iterator, UInt & hrpair)
{
    Type ktype;
    UInt key;
    Type vtype;
    UInt32 value;
    
    bool found = dictionaryNext(_this, iterator, ktype, key, vtype, value);
    if (found && (0 != value) && (vtype == Type::eVariant))
    {
        value = HRVariant_UnBox(value, vtype); // no cloning
    }
    hrpair = HRPair_New(ktype, key, vtype, value); // clones reference type members, unboxes variants
    return found;
}

Bool HRDictionary_Contains(UInt _this, UInt key)
{
    Type dkType = Type(Memory_ReadByte(_this+idKType));
    bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this+idEntries);
    UInt capacity = Memory_ReadWord(_this+idCapacity);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = hashKey16(key);
    }
    UInt pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
    return validEntry(pEntry, valueKeys);
}

UInt32 HRDictionary_Get(UInt _this, UInt key, Type & vtype)
{
    Type dkType = (Type)Memory_ReadByte(_this+idKType);
    bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this+idEntries);
    UInt capacity = Memory_ReadWord(_this+idCapacity);

    UInt hash = 0;
    if (!valueKeys)
    {
        hash = hashKey16(key);
    }
    UInt pEntry = findEntry(pEntries, capacity, key, hash, valueKeys);
    if (!validEntry(pEntry, valueKeys))
    {
        SetError(0x03, (42)); // no entry for key in dictionary
        return 0;
    }
    Type dvType = (Type)Memory_ReadByte(_this+idVType);
    UInt value = Memory_ReadWord(pEntry+ieValue) + (Memory_ReadWord(pEntry+ieValue+2) << 16);
    vtype = dvType;
    if (0 != value)
    {
        if (vtype == Type::eVariant)
        {
            vtype = (Type)Memory_ReadByte(value & 0xFFFF); // <key,variant> can contain reference types or boxed value types (actual variants)
        }
        if (vtype == Type::eVariant)
        {
            value = HRVariant_GetValue(value & 0xFFFF, vtype); // returns a clone of the boxed value
        }
        else if (IsReferenceType(vtype))
        {
            vtype = (Type)Memory_ReadByte(value & 0xFFFF);
            value = GC_Clone(value & 0xFFFF);
        }
    }
    return value;
}
UInt HRDictionary_Clone(UInt original)
{
    Type dkType = (Type)Memory_ReadByte(original+idKType);
    Type dvType = (Type)Memory_ReadByte(original+idVType);
    
    UInt clone = HRDictionary_New (dkType, dvType);
    
    UInt iterator;
    Type ktype;
    UInt key;
    Type vtype;
    UInt32 value;
    while (dictionaryNext(original, iterator, ktype, key, vtype, value))
    {
        HRDictionary_Set(clone, key, ktype, value, vtype);
    }
    
    return clone;
}
void HRDictionary_Dump(UInt address, UInt indent)
{
    if (indent > 0)
    {
        for (UInt i = 0; i < indent; i++)
        {
            putchar(' ');
        }
        indent = indent + 2;
    }
    
    UInt elements = Memory_ReadWord(address+idCount);
    Byte ktype    = Memory_ReadByte(address+idKType);
    Byte vtype    = Memory_ReadByte(address+idVType);
    UInt pEntries = Memory_ReadWord(address+idEntries);
    
    printf("%04X k%02X v%02X %04X", elements, ktype, vtype, pEntries);
    
    UInt iterator;
    Type kType;
    UInt key;
    Type vType;
    UInt32 value;
    while (dictionaryNext(address, iterator, kType, key, vType, value))
    {
        putchar((Char)0x0D);
        for (UInt i = 0; i < indent; i++)
        {
            putchar(' ');
        }
        if (kType == Type::eString)
        {
            GC_Dump(key, indent);                
        }
        else
        {
            printf("%04X", key);
        }
        putchar(' ');
        if (IsReferenceType(vType))
        {
            GC_Dump(value & 0xFFFF, indent);    
        }
        else
        {
            printf("%08lX", value);
        }
    }
    if (false)
    {
        putchar((Char)0x0D);
        UInt capacity = Memory_ReadWord(address+idCapacity);
        UInt pEntries = Memory_ReadWord(address+idEntries);
        for (UInt i = 0; i < capacity; i++)
        {
            UInt key = Memory_ReadWord(pEntries+ieKey);
            UInt hash = Memory_ReadWord(pEntries+ieHash);
            UInt32 value = Memory_ReadWord(pEntries+ieValue) + (Memory_ReadWord(pEntries+ieValue+2) << 16);
            printf("%04X:%04X-%04X-%08lX", pEntries, key, hash, value);
            pEntries = pEntries + entrySize;
            putchar(' ');
            if (i % 5 == 6) { putchar((Char)0x0D); }
        }
    }
    
}
UInt HRDictionary_GetCount(UInt _this)
{
    return Memory_ReadWord(_this+idCount);
}