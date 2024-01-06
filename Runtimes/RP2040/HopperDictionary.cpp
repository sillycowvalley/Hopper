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