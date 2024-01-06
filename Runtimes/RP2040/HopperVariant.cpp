#include "HopperVariant.h"

// Variant memory map:
//   0000 heap allocator size
//   14   type = tVariant
//   00   reference count
//   xx   actual type for item
//   xxxx data for value types, pData for reference types
//   xxxx 2nd half of 32 bit value types (or zero)
const UInt ivType  = 2;
const UInt ivValue = 3;

UInt HRVariant_CreateValueVariant(UInt32 value, Type vtype)
{
#ifdef CHECKED
    if (IsReferenceType(vtype))
    {
        SetError(0x0B, (27));
    }
#endif
    UInt address = GC_New(5, Type::eVariant);
    Memory_WriteByte(address+ivType, (Byte)vtype);
    Memory_WriteWord(address+ivValue,   (value & 0xFFFF));
    Memory_WriteWord(address+ivValue+2, (value >> 16));
    return address;
}

UInt HRVariant_New(UInt32 value, Type vtype)
{
    UInt address = GC_New(5, Type::eVariant);
    Memory_WriteByte(address+ivType, (Byte)vtype);
    if (IsReferenceType(vtype))
    {
        value = GC_Clone((value & 0xFFFF));
    }
    Memory_WriteWord(address+ivValue,   (value & 0xFFFF));    
    Memory_WriteWord(address+ivValue+2, (value >> 16));    
    return address;    
}

UInt32 HRVariant_GetValue(UInt _this, Type & vtype)
{
    vtype = (Type)Memory_ReadByte(_this+ivType);
    UInt value0 = Memory_ReadWord(_this+ivValue);
    UInt value1 = 0;
    if (IsReferenceType(vtype))
    {
        value0 = GC_Clone(value0);
        vtype = (Type)Memory_ReadByte(value0);
    }
    else
    {
        value1 = Memory_ReadWord(_this+ivValue + 2);
    }
    return value0 + (value1 << 16);
}

UInt32 HRVariant_UnBox(UInt _this, Type & vtype)
{
    vtype = (Type)Memory_ReadByte(_this+ivType);
    UInt32 value = Memory_ReadWord(_this+ivValue) + (Memory_ReadWord(_this+ivValue +2) << 16);
    //printf("\nUnBox: %02X %08lX", vtype, value);
    return value;
}

UInt HRVariant_Clone(UInt original)
{
    Type vtype = (Type)Memory_ReadByte(original+ivType);
    UInt32 value = Memory_ReadWord(original+ivValue) + (Memory_ReadWord(original+ivValue+2) << 16);
    return HRVariant_New(value, vtype);
}
void HRVariant_Clear(UInt _this)
{
    Type vtype = (Type)Memory_ReadByte(_this+ivType);
    UInt value0 = Memory_ReadWord(_this+ivValue);
    
    if (IsReferenceType(vtype) && (0 != value0))
    {
        GC_Release(value0);
    }
    Memory_WriteWord(_this+ivType,   0);
    Memory_WriteWord(_this+ivValue,   0);
    Memory_WriteWord(_this+ivValue+2, 0);
}

bool HRVariant_IsEqual(UInt32 left, Type ltype, UInt32 right, Type rtype)
{
    bool lref = IsReferenceType(ltype);
    bool rref = IsReferenceType(rtype);
    if (lref == rref)
    {
        if (lref)
        {
            // reference
            if (ltype == rtype)
            {
                switch (ltype)
                {
                    case Type::eString:
                    {
                        return 0 == HRString_Compare(left, right);
                    }
                    default:
                    {
                        SetError(0x0A, (28));
                        break;
                    }
                }
            }
        }
        else
        {
            // value
            switch (ltype)
            {
                case Type::eBool:
                {
                    if (rtype != Type::eBool)
                    {
                        return false;    
                    }
                    return left == right;
                }
                case Type::eChar:
                {
                    if (rtype != Type::eChar)
                    {
                        return false;    
                    }
                    return left == right;
                }
                case Type::eByte:
                case Type::eUInt:
                {
                    switch (rtype)
                    {
                        case Type::eByte:
                        case Type::eUInt:
                        {
                            return left == right;
                        }
                        case Type::eInt:
                        {
                            if (0 == (0x8000 & right))
                            {
                                return left == right;
                            }
                            break;
                        }
                        default: { break; }
                    }
                    break;
                }
                case Type::eInt:
                {
                    switch (rtype)
                    {
                        case Type::eByte:
                        case Type::eUInt:
                        {
                            if (0 == (0x8000 & left))
                            {
                                return left == right;
                            }
                            break;
                        }
                        case Type::eInt:
                        {
                            return left == right;
                        }
                        default: { break; }
                    }
                    break;
                }
                case Type::eLong:
                {
                    if (rtype != Type::eLong)
                    {
                        return false;    
                    }
                    return left == right;
                }
                case Type::eFloat:
                {
                    if (rtype != Type::eFloat)
                    {
                        return false;    
                    }
                    return left == right;
                }
                default: { break; }
            }
        }
    }
    return false;
}

void HRVariant_Dump(UInt address, UInt indent)
{
    if (indent > 0)
    {
        for (UInt i = 0; i < indent; i++)
        {
            putchar(' ');
        }
        indent = indent + 2;
    }
    
    Byte vtype  = Memory_ReadByte(address+ivType);
    UInt value0 = Memory_ReadWord(address+ivValue);
    UInt value1 = Memory_ReadWord(address+ivValue+2);
    printf(" v%02X %04X%04X", vtype, value1, value0);
}


