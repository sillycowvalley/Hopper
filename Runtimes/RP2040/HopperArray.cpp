#include "HopperArray.h"

// Array memory map:
//   0000 heap allocator size
//   0F   type = tArray
//   00   GC reference count
//   0000 number of elements
//   xx   type of elements
//   0000 first element in array
//   ..
//   <nn>  last element in array

const UInt aiCount    = 2;
const UInt aiType     = 4;
const UInt aiElements = 5;

const Byte setSlots[]   = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
const Byte clearSlots[] = {0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F};

UInt HRArray_New(Type htype, UInt count)
{
    UInt elementbytes;
    switch (htype)
    {
        case Type::eBool:
        {
            elementbytes = (count+7) >> 3;
            break;
        }
        case Type::eChar:
        case Type::eByte:
        {
            elementbytes = count;
            break;
        }
        default:
        {
            elementbytes = count *2;
            break;
        }
    }
    
    UInt _this = GC_New(3 + elementbytes, Type::eArray);
    Memory_WriteWord(_this+aiCount, count);
    Memory_WriteByte(_this+aiType, (Byte)htype);
    
    // zero initialize
    UInt address = _this + aiElements;
    for (UInt i = 0; i < elementbytes; i++)
    {
        Memory_WriteByte(address, 0);
        address++;
    }
    return _this;
}

UInt HRArray_GetCount(UInt _this)
{
    return Memory_ReadWord(_this+aiCount);
}

UInt HRArray_GetItem(UInt _this, UInt index, Type & etype)
{
    etype    = (Type)Memory_ReadByte(_this+aiType);
#ifdef CHECKED        
    UInt elements = Memory_ReadWord(_this+aiCount);
    if (index >= elements)
    {
        SetError(0x02, (23)); // array index out of range
        return 0;
    }
#endif
    UInt address = _this + aiElements;
    UInt value;
    switch (etype)
    {
        case Type::eBool:
        {
            UInt offset = address + (index >> 3);
            Byte slotIndex = Byte(index & 0x07);
            Byte b = Memory_ReadByte(offset);
            value = b & setSlots[slotIndex];
            if (value != 0)
            {
                value = 1;
            }
            break;
        }
        case Type::eChar:
        case Type::eByte:
        {
            value = Memory_ReadByte(address + index);
            break;
        }
        default:
        {
            UInt offset = address + (index << 1);
            Byte lsb = Memory_ReadByte(offset);
            Byte msb = Memory_ReadByte(offset+1);
            value = (msb << 8) | lsb;
            break;
        }
    }
    return value;     
}

void HRArray_SetItem(UInt _this, UInt index, UInt value)
{
    Type etype    = (Type)Memory_ReadByte(_this+aiType);
#ifdef CHECKED        
    UInt elements = Memory_ReadWord(_this+aiCount);
    if (index >= elements)
    { 
        SetError(0x02, (24)); // array index out of range
        return;
    }
#endif
    UInt address = _this + aiElements;
    switch (etype)
    {
        case Type::eBool:
        {
            UInt offset = address + (index >> 3);
            Byte slotIndex = Byte(index & 0x07);
            Byte b = Memory_ReadByte(offset);
            if (value == 0)
            {
                // clear the bit
                b = b & clearSlots[slotIndex];
            }
            else
            {
                // set the bit
                b = b | setSlots[slotIndex];
            }
            Memory_WriteByte(offset, b);
            break;
        }
        case Type::eChar:
        case Type::eByte:
        {
            Memory_WriteByte(address + index,   Byte(value & 0xFF));
            break;
        }
        default:
        {
            UInt offset = address + (index << 1);
            Memory_WriteByte(offset,   Byte(value & 0xFF));
            Memory_WriteByte(offset+1, Byte(value >> 8));
            break;
        }
    }
}

