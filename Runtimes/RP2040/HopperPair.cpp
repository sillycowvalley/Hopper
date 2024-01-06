#include "HopperPair.h"

// Pair memory map:
//   0000 heap allocator size
//   10   type = tPair
//   00   reference count
//   xx   kType
//   xx   vType
//   xxxx pKey   - always a variant
//   xxxxxxxx pData

const UInt ipKType = 2;
const UInt ipVType = 3;
const UInt ipKey   = 4;
const UInt ipValue = 6;

UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt32 value)
{
    UInt address = GC_New(8, Type::ePair);
    Memory_WriteByte(address+ipKType, Byte(ktype)); // key type
    Memory_WriteByte(address+ipVType, Byte(vtype)); // value type
    
    if (IsReferenceType(ktype) && (0 != key))
    {
        key = GC_Clone(key);
    }
    if (IsReferenceType(vtype) && (0 != value))
    {
        value = GC_Clone(value & 0xFFFF);
    }
    Memory_WriteWord(address+ipKey, key);
    Memory_WriteWord(address+ipValue, value & 0xFFFF);
    Memory_WriteWord(address+ipValue + 2, value >> 16);
    return address;
}

UInt HRPair_Clone(UInt original)
{
    Type dkType = (Type)Memory_ReadByte(original+ipKType);
    Type dvType = (Type)Memory_ReadByte(original+ipVType);
    
    UInt key   = Memory_ReadWord(original+ipKey);
    UInt32 value = Memory_ReadWord(original+ipValue) + (Memory_ReadWord(original+ipValue + 2) << 16);
    
    return HRPair_New(dkType, key, dvType, value);
}

void HRPair_Clear(UInt _this)
{
    Type dkType = (Type)Memory_ReadByte(_this+ipKType);
    Type dvType = (Type)Memory_ReadByte(_this+ipVType);
    
    UInt key   = Memory_ReadWord(_this+ipKey);
    UInt32 value = Memory_ReadWord(_this+ipValue) + (Memory_ReadWord(_this+ipValue + 2) << 16);
    
    if (IsReferenceType(dkType) && (0 != key))
    {
        GC_Release(key);
    }
    if (IsReferenceType(dvType) && (0 != value))
    {
        GC_Release(value & 0xFFFF);
    }
    Memory_WriteWord(_this+ipKey, 0);
    Memory_WriteWord(_this+ipValue, 0);
    Memory_WriteWord(_this+ipValue+2, 0);
}

UInt HRPair_GetKey(UInt _this, Type & ktype)
{
    ktype = (Type)Memory_ReadByte(_this+ipKType);
    UInt key   = Memory_ReadWord(_this+ipKey);
    if (IsReferenceType(ktype))
    {
        key = GC_Clone(key);
        ktype = (Type)Memory_ReadByte(key); // should be Type::eString ..
    }
    return key;
}

UInt32 HRPair_GetValue(UInt _this, Type & vtype)
{
    vtype = (Type)Memory_ReadByte(_this+ipVType);
    UInt32 value = Memory_ReadWord(_this+ipValue) + (Memory_ReadWord(_this+ipValue + 2) << 16);
    if (vtype == Type::eVariant)
    {
        value = HRVariant_GetValue(value & 0xFFFF, vtype); // returns a clone of the boxed value
    }
    else if (IsReferenceType(vtype))
    {
        value = GC_Clone(value & 0xFFFF);
        vtype = (Type)Memory_ReadByte(value);
    }
    return value;
}
void HRPair_Dump(UInt address, UInt indent)
{
    if (indent > 0)
    {
        for (UInt i = 0; i < indent; i++)
        {
            putchar(' ');
        }
        indent = indent + 2;
    }
    Byte ktype    = Memory_ReadByte(address+ipKType);
    Byte vtype    = Memory_ReadByte(address+ipVType);
    UInt key    = Memory_ReadWord(address+ipKey);
    UInt32 value = Memory_ReadWord(address+ipValue) + (Memory_ReadWord(address+ipValue + 2) << 16);
    printf(" k%02X v%02X %04X %08lX", ktype, vtype, key, value);
}