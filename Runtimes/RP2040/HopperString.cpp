#include "HopperString.h"

UInt HRString_new(UInt size)
{
    UInt blockSize = 0x06 + size;
    blockSize = (blockSize + 0x0F) & 0xFFF0;
    return GC_New(blockSize - 0x04, Type::eString);
}

UInt HRString_getCapacity(UInt _this)
{
    return Memory_ReadWord(_this - 0x02) - 0x06;
}

UInt HRString_clone(UInt original, UInt extra)
{
    UInt length = Memory_ReadWord(original + 2);
    UInt address = HRString_new(length + extra);
    Memory_WriteWord(address + 2, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(address + 4 + i, Memory_ReadByte(original + 4 + i));
    }
    return address;
}

UInt HRString_Clone(UInt original)
{
    return HRString_clone(original, 0x00);
}


UInt HRString_New()
{
    UInt address = HRString_new(0x00);
    Memory_WriteWord(address + 2, 0x00);
    return address;
}

void HRString_BuildClear_R(UInt & _this)
{
    Memory_WriteWord(_this + 2, 0x00);
}

void HRString_BuildChar_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 0x01)
    {
        UInt copy = HRString_clone(_this, 0x01);
        GC_Release(_this);
        _this = copy;
    }
    Memory_WriteByte(_this + 4 + length, (Byte)ch);
    Memory_WriteWord(_this + 2, length + 0x01);
}

UInt HRString_GetLength(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

Char HRString_GetChar(UInt _this, UInt index)
{
#ifdef CHECKED
    UInt length = HRString_GetLength(_this);
    if (index >= length)
    {
        SetError(0x05, (26)); //  string index out of range
        return 0;
    }
#endif
    return Char(Memory_ReadByte(_this + 4 + index));
}

UInt HRString_NewFromConstant0(UInt location, UInt length)
{
    UInt address = HRString_new(length);
    Memory_WriteWord(address + 2, length);;
    for (UInt i = 0; i < length; i++)
    {
        Memory_WriteByte(address + 4 + i, Memory_ReadCodeByte(location + i));
    }
    return address;
}

UInt HRString_NewFromConstant1(UInt doubleChar)
{
    Byte lsb = (Byte)(doubleChar & 0xFF);
    Byte msb = (Byte)(doubleChar >> 0x08);
    UInt address = HRString_new(  (msb == 0) ? 1 : 2);
    Memory_WriteWord(address + 2, (msb == 0) ? 1 : 2);
    Memory_WriteByte(address + 4, lsb);
    if (msb != 0)
    {
        Memory_WriteByte(address + 4 + 1, msb);
    }
    return address;
}

UInt HRString_InsertChar(UInt _this, UInt index, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_new(length + 1);
    UInt j = 0;
    for (UInt i = 0; i < length; i++)
    {
        if (i == index)
        {
            Memory_WriteByte(result + 4 + j, (Byte)ch);
            
            j++;
        }
        Memory_WriteByte(result + 4 + j, Memory_ReadByte(_this + 4 + i));
        
        j++;
    }
    if ((length == 0) || (index >= length))
    {
        Memory_WriteByte(result + 4 + j, (Byte)ch);
    }
    Memory_WriteWord(result + 2, length + 1);
    return result;
}

Bool HRString_EndsWith(UInt _this, Char with)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0)
    {
        return false;
    }
    return (Char(Memory_ReadByte(_this + 4 + length - 1)) == with);
}

Bool HRString_EndsWith(UInt _this, UInt with)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(with);
    if (length0 < length1)
    {
        return false;
    }
    if (length1 == 0)
    {
        return true;
    }
    UInt i = 1;
    for (;;)
    {
        Char w = Char(Memory_ReadByte(with + 4 + length1 - i));
        Char t = Char(Memory_ReadByte(_this + 4 + length0 - i));
        if (w != t)
        {
            return false;
        }
        if (i == length1)
        {
            break;;
        }
        
        i++;
    }
    return true;
}

Int HRString_Compare(UInt left, UInt right)
{
    UInt i = 0;
    Int result = 0;
    UInt ll = HRString_GetLength(left);
    UInt rl = HRString_GetLength(right);
    for (;;)
    {
        if (i >= ll)
        {
            break;;
        }
        if (i >= rl)
        {
            break;;
        }
        if (Memory_ReadByte(left + 4 + i) != Memory_ReadByte(right + 4 + i))
        {
            break;;
        }
        
        i++;
    }
    for (;;)
    {
        if ((ll == 0) && (rl == 0))
        {
            break;;
        }
        if ((i < ll) && (i < rl))
        {
            if (Int(Memory_ReadByte(left + 4 + i)) > Int(Memory_ReadByte(right + 4 + i)))
            {
                result = 1;
            }
            else
            {
                result = -1;
            }
            break;
        }
        if (i >= ll)
        {
            if (i >= rl)
            {
                break;;
            }
            result = -1;
            break;
        }
        result = 1;
        break;
    }
    return result;
}

UInt HRString_Replace(UInt _this, UInt pattern, UInt replace)
{
    UInt result = 0;
    UInt patternLength = HRString_GetLength(pattern);
    if (patternLength == 0)
    {
        result = HRString_clone(_this, 0);
        return result;
    }
    UInt originalLength = HRString_GetLength(_this);
    UInt replaceLength = HRString_GetLength(replace);
    if (replaceLength <= patternLength)
    {
        result = HRString_clone(_this, 0);
    }
    else
    {
        result = HRString_clone(_this, (replaceLength - patternLength) * originalLength);
    }
    UInt i = 0;
    UInt j = 0;
    for (;;)
    {
        if (i == originalLength)
        {
            break;
        }
        Bool match = false;
        if (i + patternLength <= originalLength)
        {
            match = true;
            for (UInt n = 0; n < patternLength; n++)
            {
                if (Memory_ReadByte(_this + 4 + i + n) != Memory_ReadByte(pattern + 4 + n))
                {
                    match = false;
                    break;
                }
            }
        }
        if (match)
        {
            i = i + patternLength;;
            for (UInt n = 0; n < replaceLength; n++)
            {
                Memory_WriteByte(result + 4 + j, Memory_ReadByte(replace + 4 + n));
                
                j++;
            }
        }
        else
        {
            Memory_WriteByte(result + 4 + j, Memory_ReadByte(_this + 4 + i));
            
            j++;
            
            i++;
        }
    }
    Memory_WriteWord(result + 2, j);
    return result;
}

UInt HRString_Replace(UInt _this, Char from, Char to)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 0);;
    for (UInt i = 0; i < length; i++)
    {
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch == from)
        {
            Memory_WriteByte(result + 4 + i, (Byte)to);
        }
    }
    return result;
}

UInt HRString_Append(UInt _this, UInt append)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(append);
    UInt result = HRString_new(length0 + length1);
    Memory_WriteWord(result + 2, length0 + length1);;
    for (UInt i = 0; i < length0; i++)
    {
        Memory_WriteByte(result + 4 + i, Memory_ReadByte(_this + 4 + i));
    }
    for (UInt i = 0; i < length1; i++)
    {
        Memory_WriteByte(result + 4 + i + length0, Memory_ReadByte(append + 4 + i));
    }
    return result;
}

UInt HRString_Append(UInt _this, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 1);
    Memory_WriteByte(result + 4 + length, (Byte)ch);
    Memory_WriteWord(result + 2, length + 1);
    return result;
}

UInt HRString_Substring(UInt _this, UInt start)
{
    UInt limit = HRString_GetLength(_this);
    return HRString_Substring(_this, start, limit);
}

UInt HRString_Substring(UInt _this, UInt start, UInt limit)
{
    UInt length0 = HRString_GetLength(_this);
    if (start >= length0)
    {
        start = length0;
    }
    UInt length1 = length0 - start;
    UInt result = HRString_new(length1);
    UInt newLength = 0;
    for (UInt i = 0; i < length1; i++)
    {
        if (newLength == limit)
        {
            break;
        }
        Memory_WriteByte(result + 4 + i, Memory_ReadByte(_this + 4 + i + start));
        
        newLength++;
    }
    Memory_WriteWord(result + 2, newLength);
    return result;
}

void HRString_Substring_R(UInt & _this, UInt start)
{
    UInt length = HRString_GetLength(_this);
    if (start == 0)
    {
        return;
    }
    if (start >= length)
    {
        Memory_WriteWord(_this + 2, 0);
        return;
    }
    Memory_WriteWord(_this + 2, length - start);
    UInt i = start;
    UInt j = 0;
    for (;;)
    {
        Memory_WriteByte(_this + 4 + j, Memory_ReadByte(_this + 4 + i));
        i++;
        j++;
        if (i == length)
        {
            break;;
        }
    }
}

void HRString_BuildString_R(UInt & _this, UInt append)
{
    UInt length1 = HRString_GetLength(append);
    if (length1 > 0)
    {
        UInt capacity = HRString_getCapacity(_this);
        UInt length0 = HRString_GetLength(_this);
        if (capacity < length0 + length1)
        {
            UInt copy = HRString_clone(_this, length1);
            GC_Release(_this);
            _this = copy;
        }
        for (UInt i = 0; i < length1; i++)
        {
            Memory_WriteByte(_this + 4 + length0 + i, Memory_ReadByte(append + 4 + i));
        }
        Memory_WriteWord(_this + 2, length0 + length1);
    }
}

void HRString_BuildFront_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 1)
    {
        UInt copy = HRString_clone(_this, 1);
        GC_Release(_this);
        _this = copy;
    }
    UInt i = length;
    for (;;)
    {
        if (i == 0)
        {
            break;
        }
        Memory_WriteByte(_this + 4 + i, Memory_ReadByte(_this + 4 + i - 1));
        i--;
    }
    Memory_WriteByte(_this + 4, (Byte)ch);
    Memory_WriteWord(_this + 2, length + 1);
    
}

UInt HRString_Trim(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimRight_R(copy);
    HRString_TrimLeft_R(copy);
    return copy;
}

void HRString_TrimRight_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0)
    {
        return;
    }
    UInt i = length - 1;
    for (;;)
    {
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch != ' ')
        {
            Memory_WriteWord(_this + 2, i + 1);
            break;;
        }
        if (i == 0)
        {
            Memory_WriteWord(_this + 2, 0);
            break;;
        }
        
        i--;
    }
}

void HRString_TrimLeft_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch != ' ')
        {
            break;;
        }
        i++;
    }
    HRString_Substring_R(_this, i);
}

UInt HRString_TrimLeft(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimLeft_R(copy);
    return copy;
}

UInt HRString_ToUpper(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_ToUpper_R(copy);
    return copy;
}
UInt HRString_ToLower(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_ToLower_R(copy);
    return copy;
}

void HRString_ToUpper_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = (Char)Memory_ReadByte(_this + 4 + i);
        Memory_WriteByte(_this + 4 + i, HRChar_ToUpper(ch));
        i++;
    }
}
void HRString_ToLower_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = (Char)Memory_ReadByte(_this + 4 + i);
        Memory_WriteByte(_this + 4 + i, HRChar_ToLower(ch));
        i++;
    }
}


void HRString_Dump(UInt address, UInt indent)
{
    for (UInt i = 0; i < indent; i++)
    {
        putchar(' ');
    }
    putchar('\'');
    UInt length = Memory_ReadWord(address + 2);
    if (length > 40)
    {
        length = 40;
    };
    for (UInt i = 0; i < length; i++)
    {
        putchar((Char)Memory_ReadByte(address + 4 + i));
    }
    putchar('\'');
}