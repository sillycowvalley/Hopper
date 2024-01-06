#include "HopperList.h"

// List memory map:
//   0000 heap allocator size
//   19   type = tList
//   00   GC reference count
//   0000 current number of items
//   xx   type of items
//   xxxx pFirst
//   xxxx pRecent
//   xxxx iRecent
// ListItem memory map:
//   0000 heap allocator size
//   xxxx inline for value types, pData for reference types and when item type is variant
//   xxxx 2nd half of 'long' and 'float' if item type is not variant
//   0000 pNext
    
const UInt lsLength = 2;
const UInt lsType   = 4;
const UInt lsFirst  = 5;
const UInt lsRecent = 7;

const UInt liData = 0;
const UInt liNext = 4;


UInt HRList_New(Type htype)
{
    UInt address = GC_New(9, Type::eList);
    Memory_WriteWord(address+lsLength, 0);           // number of items
    Memory_WriteByte(address+lsType, (Byte)htype);   // type of items
    Memory_WriteWord(address+lsFirst,    0);         // pFirst
    Memory_WriteWord(address+lsRecent,   0);         // pRecent
    Memory_WriteWord(address+lsRecent+2, 0);         // iRecent
    return address;
}

UInt HRList_Clone(UInt original)
{
    Type etype  = (Type)Memory_ReadByte(original+lsType);
    UInt clone = HRList_New(etype);
    UInt pCurrentItem  = Memory_ReadWord(original+lsFirst);
    for(;;) // loop
    {
        if (pCurrentItem == 0)
        {
            break;
        }
        UInt itemData = Memory_ReadWord(pCurrentItem+liData);
        Type itype = etype;
        if (IsReferenceType(etype))
        {
            itype = (Type)Memory_ReadByte(itemData);
        }
        HRList_Append(clone, itemData, itype); // Append clones the item
        pCurrentItem = Memory_ReadWord(pCurrentItem+liNext);
    }
    return clone;
}

void clearItem(UInt pCurrent, Type etype)
{
    UInt pData    = Memory_ReadWord(pCurrent+liData);
    Memory_Free(pCurrent);
    if (IsReferenceType(etype))
    {
        GC_Release(pData);
    }
}
void clearAllItems(UInt pCurrent, Type etype)
{
    UInt pNext;
    for(;;) // loop
    {
        if (pCurrent == 0) { break; }
        pNext    = Memory_ReadWord(pCurrent+liNext);
        clearItem(pCurrent, etype);
        pCurrent = pNext;
    }
}

void HRList_Clear(UInt _this)
{
    Type etype  = (Type)Memory_ReadByte(_this+lsType);
    UInt pFirst = Memory_ReadWord(_this+lsFirst);
    if (pFirst != 0)
    {
        clearAllItems(pFirst, etype);
    }
    Memory_WriteWord(_this+lsLength,   0);         // number of items
    Memory_WriteWord(_this+lsFirst,    0);         // pFirst
    Memory_WriteWord(_this+lsRecent,   0);         // pRecent
    Memory_WriteWord(_this+lsRecent+2, 0);         // iRecent    
}

UInt createItem(UInt32 itemData, Type etype, Type itype)
{
    UInt pData0 = (UInt)(itemData & 0xFFFF);
    UInt pData1 = (UInt)(itemData >> 16);
    if (IsReferenceType(etype))
    {
        if (IsReferenceType(itype))
        {
            pData0 = GC_Clone(pData0);
            pData1 = 0;
        }
        else
        {
            pData0 = HRVariant_CreateValueVariant(itemData, itype);
            pData1 = 0;
        }
    }
    UInt pitem = Memory_Allocate(6);
    
    Memory_WriteWord(pitem+liData,   pData0); // pData
    Memory_WriteWord(pitem+liData+2, pData1); // pData
    Memory_WriteWord(pitem+liNext, 0);        // pNext
    return pitem;
}

UInt HRList_GetLength(UInt _this)
{
    return Memory_ReadWord(_this+lsLength);
}

UInt32 HRList_GetItem(UInt _this, UInt index, Type & itype)
{
    itype = (Type)Memory_ReadByte(_this+lsType);
#ifdef CHECKED
    UInt length = Memory_ReadWord(_this+lsLength);
    if (index >= length)
    {
        SetError(0x01, (33)); // list index out of range
        return 0;
    }
#endif
    UInt i = 0;
    UInt pCurrent = Memory_ReadWord(_this+lsFirst);
    UInt pRecent  = Memory_ReadWord(_this+lsRecent);
    if (pRecent != 0)
    {
        UInt iRecent  = Memory_ReadWord(_this+lsRecent+2);    
        if (iRecent <= index)
        {
            i        = iRecent;
            pCurrent = pRecent;
        }
    }
    
    for(;;) // loop
    {
        if (i == index)
        {
            break;
        }
        pCurrent = Memory_ReadWord(pCurrent+liNext);
        i++;
    }
    UInt   pData  = Memory_ReadWord(pCurrent+liData);
    UInt32 result = Memory_ReadWord(pCurrent+liData) + (Memory_ReadWord(pCurrent+liData+2) << 16);
    
    if (itype == Type::eVariant)
    {
        itype = (Type)Memory_ReadByte(pData); // <variant> can contain reference types or boxed value types (actual variants)
    }
    if (itype == Type::eVariant)
    {
        result = HRVariant_GetValue(pData, itype); // returns a clone of the boxed value
    }
    else if (IsReferenceType(itype))
    {
        itype = (Type)Memory_ReadByte(pData);
        result = GC_Clone(pData);
    }
    
    Memory_WriteWord(_this+lsRecent,   pCurrent);    // pRecent
    Memory_WriteWord(_this+lsRecent+2, index);       // iRecent
    return result;
}

void HRList_SetItem(UInt _this, UInt index, UInt32 item, Type itype)
{
    Type etype  = (Type)Memory_ReadByte(_this+lsType);
#ifdef CHECKED
    UInt length = Memory_ReadWord(_this+lsLength);
    if (index >= length)
    {
        SetError(0x01, (29)); // list index out of range
        return;
    }
#endif        
    UInt pData = item;
    if (IsReferenceType(etype))
    {
        if (IsReferenceType(itype))
        {
            pData = GC_Clone(item);
        }
        else
        {
#ifdef CHECKED                
            // item is value type, list element type must be variant
            if (etype != Type::eVariant)
            {
                SetError(0x0B, (30));
                return;
            }
#endif
            pData = HRVariant_CreateValueVariant(item, itype);
        }
    }
    
    UInt i = 0;
    UInt pCurrent = Memory_ReadWord(_this+lsFirst);
    UInt pRecent  = Memory_ReadWord(_this+lsRecent);
    if (pRecent != 0)
    {
        
        UInt iRecent  = Memory_ReadWord(_this+lsRecent+2);    
        if (iRecent <= index)
        {
            i        = iRecent;
            pCurrent = pRecent;
        }
    }
    for(;;) // loop
    {
        if (i == index)
        {
            break;
        }
        pCurrent = Memory_ReadWord(pCurrent+liNext);
        i++;
    }
    UInt oldData = Memory_ReadWord(pCurrent+liData);
    if (IsReferenceType(etype))
    {
        GC_Release(oldData);
    }
        
    Memory_WriteWord(pCurrent+liData, pData);
    Memory_WriteWord(_this+lsRecent,   pCurrent);    // pRecent
    Memory_WriteWord(_this+lsRecent+2, index);       // iRecent  
}

void HRList_Insert(UInt _this, UInt index, UInt32 item, Type itype)
{
    Type etype = (Type)Memory_ReadByte(_this+lsType);
    UInt length = Memory_ReadWord(_this+lsLength);
    UInt pFirst  = Memory_ReadWord(_this+lsFirst);
    if (index >= length)
    {
        HRList_Append(_this, item, itype);
    }
    else if (index == 0)
    {
        UInt pItem  = createItem(item, etype, itype);
        Memory_WriteWord(pItem+liNext, pFirst);
        Memory_WriteWord(_this+lsFirst, pItem);
        
        Memory_WriteWord(_this+lsLength,   length+1); 
        Memory_WriteWord(_this+lsRecent,   0);
        Memory_WriteWord(_this+lsRecent+2, 0);
    }
    else
    {
        UInt pCurrent= pFirst;
        UInt pPrevious = 0;
    
        UInt pRecent  = Memory_ReadWord(_this+lsRecent);
        UInt iRecent  = Memory_ReadWord(_this+lsRecent+2);    
    
        UInt count = 0;
        if ((iRecent != 0) && (index > iRecent)) // > means previous will be set
        {
            pCurrent= pRecent;
            count    = iRecent;
        }
        
        UInt pItem  = createItem(item, etype, itype);
        while (0 != pCurrent)
        {
            if (index == count)
            {
                Memory_WriteWord(pItem+liNext, pCurrent);
                Memory_WriteWord(pPrevious+liNext, pItem);
                
                Memory_WriteWord(_this+lsRecent,   pItem);
                Memory_WriteWord(_this+lsRecent+2, count);
                Memory_WriteWord(_this+lsLength,   length+1); 
                break;
            }
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent+liNext);
            count++;
        }
    }
}

void HRList_Append(UInt _this, UInt32 item, Type itype)
{
    Type etype = (Type)Memory_ReadByte(_this+lsType);
    UInt pNewItem    = createItem(item, etype, itype);
    UInt pFirstItem  = Memory_ReadWord(_this+lsFirst);
    if (pFirstItem == 0)
    {
        Memory_WriteWord(_this+lsFirst, pNewItem);
    }
    else
    {
        UInt pCurrentItem = pFirstItem;
        UInt pRecentItem  = Memory_ReadWord(_this+lsRecent);
        if (pRecentItem != 0)
        {
            pCurrentItem = pRecentItem;
        }
        for(;;) // loop
        {
            UInt pNextItem = Memory_ReadWord(pCurrentItem+liNext);
            if (pNextItem == 0)
            {
                break;
            }
            pCurrentItem = pNextItem;
        }
        Memory_WriteWord(pCurrentItem+liNext, pNewItem);
    }
    UInt length = Memory_ReadWord(_this+lsLength)+1;
    Memory_WriteWord(_this+lsLength,   length);         // number of items
    Memory_WriteWord(_this+lsRecent,   pNewItem);       // pRecent
    Memory_WriteWord(_this+lsRecent+2, length-1);       // iRecent  
}

bool HRList_Contains(UInt _this, UInt32 item, Type itype)
{
    Type etype = (Type)Memory_ReadByte(_this+lsType);
    UInt pCurrent  = Memory_ReadWord(_this+lsFirst);
    for(;;) // loop
    {
        if (0 == pCurrent) { break; }
        Type dtype = etype;
        UInt pData = Memory_ReadWord(pCurrent+liData);
        if (IsReferenceType(dtype))
        {
            dtype = (Type)Memory_ReadByte(pData);
        }
        if (HRVariant_IsEqual(pData, dtype, item, itype))
        {
            return true;
        }
        pCurrent = Memory_ReadWord(pCurrent+liNext);
    }
    return false;
}

void HRList_Remove(UInt _this, UInt index)
{
    Type etype = (Type)Memory_ReadByte(_this+lsType);
    UInt length = Memory_ReadWord(_this+lsLength);
#ifdef CHECKED
    if (index >= length)
    {
        SetError(0x01, (34)); // list index out of range
        return;
    }
#endif
    UInt pCurrent = Memory_ReadWord(_this+lsFirst);
    if (index == 0)
    {
        UInt pNext    = Memory_ReadWord(pCurrent+liNext);
        clearItem(pCurrent, etype);
        Memory_WriteWord(_this+lsFirst, pNext);
    }
    else
    {
        UInt pPrevious = pCurrent;            // 0th item
        pCurrent = Memory_ReadWord(pCurrent+liNext); // 1st item
        
        UInt count = 1;
        while (count < index)
        {
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent+liNext);
            count++;
        }
        Memory_WriteWord(pPrevious+liNext, Memory_ReadWord(pCurrent+liNext));
        clearItem(pCurrent, etype);
    }
    length = Memory_ReadWord(_this+lsLength)-1;
    Memory_WriteWord(_this+lsLength,   length);  // number of items
    Memory_WriteWord(_this+lsRecent,   0);       // pRecent
    Memory_WriteWord(_this+lsRecent+2, 0);       // iRecent   
}

void HRList_Dump(UInt address, UInt indent)
{
    if (indent > 0)
    {
        for (UInt i = 0; i < indent; i++)
        {
            putchar(' ');
        }
        indent = indent + 2;
    }
    
    UInt elements = Memory_ReadWord(address+lsLength);
    Byte etype    = Memory_ReadByte(address+lsType);
    printf("%04X %02X", elements, etype);
    
    if (indent > 0)
    {
        UInt pCurrent  = Memory_ReadWord(address+lsFirst);
        printf(" %04X", pCurrent);
        UInt count = 0;
        for(;;) // loop
        {
            if (pCurrent == 0)
            {
                break;
            }
            putchar((Char)0x0D);
            UInt pData0 = Memory_ReadWord(pCurrent+liData);
            UInt pData1 = Memory_ReadWord(pCurrent+liData+2);
            for (UInt i = 0; i < indent; i++)
            {
                putchar(' ');
            }
            
            if (IsReferenceType(etype))
            {
                UInt pNext = Memory_ReadWord(pCurrent+liNext);
                Byte itype = Memory_ReadByte(pData0);
                Byte iref  = Memory_ReadByte(pData0+1);
                printf("%04X,%04X->%04X:%02X %02X", pCurrent, pNext, pData0, itype, iref);
                GC_Dump(pData0, indent);
            }
            else
            {
                printf("%04X%04X", pData1, pData0);
            }
            count++;
            if (count >= 5)
            {
                putchar((Char)0x0D);
                for (UInt i = 0; i < indent; i++)
                {
                    putchar(' ');
                }
                printf("...");
                break;
            }
            pCurrent = Memory_ReadWord(pCurrent+liNext);
        }
    }
}