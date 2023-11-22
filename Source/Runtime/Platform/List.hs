unit HRList
{
    uses "/Source/Runtime/Platform/GC"
    uses "/Source/Runtime/Platform/Variant"

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
//   0000 pNext
        
    const uint lsLength = 2;
    const uint lsType   = 4;
    const uint lsFirst  = 5;
    const uint lsRecent = 7;

#ifdef CHECKED
    const uint liData = 2;
    const uint liNext = 4;
#else    
    const uint liData = 0;
    const uint liNext = 2;
#endif   
    uint New(Type htype)
    {
        uint address = GC.New(9, Type.List);
        WriteWord(address+lsLength, 0);           // number of items
        WriteByte(address+lsType, byte(htype));   // type of items
        WriteWord(address+lsFirst,    0);         // pFirst
        WriteWord(address+lsRecent,   0);         // pRecent
        WriteWord(address+lsRecent+2, 0);         // iRecent
        return address;
    }
    
    uint Clone(uint original)
    {
        Type etype  = Type(ReadByte(original+lsType));
        uint clone = HRList.New(etype);
        uint pCurrentItem  = ReadWord(original+lsFirst);
        loop
        {
            if (pCurrentItem == 0)
            {
                break;
            }
            uint itemData = ReadWord(pCurrentItem+liData);
            Type itype = etype;
            if (IsReferenceType(etype))
            {
                itype = Type(ReadByte(itemData));
            }
            Append(clone, itemData, itype); // Append clones the item
            pCurrentItem = ReadWord(pCurrentItem+liNext);
        }
        return clone;
    }
    clearItem(uint pCurrent, Type etype)
    {
        uint pData    = ReadWord(pCurrent+liData);

#ifdef CHECKED
        // GC.Release
        byte referenceCount = ReadByte(pCurrent+1);
        if (referenceCount == 0)
        {
            ErrorDump(94);
            Error = 0x0B;
            return;
        }
#endif
        Memory.Free(pCurrent);
        if (IsReferenceType(etype))
        {
            GC.Release(pData);
        }
    }
    clearAllItems(uint pCurrent, Type etype)
    {
        uint pNext;
        loop
        {
            if (pCurrent == 0) { break; }
            pNext    = ReadWord(pCurrent+liNext);
            clearItem(pCurrent, etype);
            pCurrent = pNext;
        }
    }
    Clear(uint this)
    {
        Type etype  = Type(ReadByte(this+lsType));
        uint pFirst = ReadWord(this+lsFirst);
        if (pFirst != 0)
        {
            clearAllItems(pFirst, etype);
        }
        WriteWord(this+lsLength,   0);         // number of items
        WriteWord(this+lsFirst,    0);         // pFirst
        WriteWord(this+lsRecent,   0);         // pRecent
        WriteWord(this+lsRecent+2, 0);         // iRecent    
    }
    
    uint createItem(uint itemData, Type etype, Type itype)
    {
        uint pData = itemData;
        if (IsReferenceType(etype))
        {
            if (IsReferenceType(itype))
            {
                pData = GC.Clone(itemData);
            }
            else
            {
#ifdef CHECKED
                // item is value type, list element type must be variant
                if (etype != Type.Variant)
                {
                    Write('e'); WriteHex(byte(etype)); Write('i'); WriteHex(byte(itype));  ErrorDump(66);
                    Error = 0x0B;
                    return 0;
                }
#endif
                pData = HRVariant.CreateValueVariant(itemData, itype);
            }
        }
#ifdef CHECKED
        uint pitem = GC.New(4, Type.ListItem);
#else        
        uint pitem = Memory.Allocate(4);
#endif
        
        Memory.WriteWord(pitem+liData, pData); // pData
        Memory.WriteWord(pitem+liNext, 0);     // pNext
        return pitem;
    }
    
    uint GetLength(uint this)
    {
        return ReadWord(this+lsLength);
    }
    
    uint GetItem(uint this, uint index, ref Type itype)
    {
        itype = Type(ReadByte(this+lsType));
        uint length = ReadWord(this+lsLength);
#ifdef CHECKED
        if (index >= length)
        {
            ErrorDump(68);
            Error = 0x01; // list index out of range
            return 0;
        }
#endif
        uint i = 0;
        uint pCurrent = ReadWord(this+lsFirst);
        uint pRecent  = ReadWord(this+lsRecent);
        if (pRecent != 0)
        {
            uint iRecent  = ReadWord(this+lsRecent+2);    
            if (iRecent <= index)
            {
                i        = iRecent;
                pCurrent = pRecent;
            }
        }
        
        loop
        {
            if (i == index)
            {
                break;
            }
            pCurrent = ReadWord(pCurrent+liNext);
            i++;
        }
        uint pData = ReadWord(pCurrent+liData);

        
        if (itype == Type.Variant)
        {
            itype = Type(ReadByte(pData)); // <variant> can contain reference types or boxed value types (actual variants)
        }
        if (itype == Type.Variant)
        {
            pData = HRVariant.GetValue(pData, ref itype); // returns a clone of the boxed value
        }
        else if (IsReferenceType(itype))
        {
            itype = Type(ReadByte(pData));
            pData = GC.Clone(pData);
        }
        
        WriteWord(this+lsRecent,   pCurrent);    // pRecent
        WriteWord(this+lsRecent+2, index);       // iRecent
        return pData;
    }
    
    SetItem(uint this, uint index, uint item, Type itype)
    {
        Type etype = Type(ReadByte(this+lsType));
        uint length = ReadWord(this+lsLength);
#ifdef CHECKED
        if (index >= length)
        {
            ErrorDump(70);
            Error = 0x01; // list index out of range
            return;
        }
#endif        
        uint pData = item;
        if (IsReferenceType(etype))
        {
            if (IsReferenceType(itype))
            {
                pData = GC.Clone(item);
            }
            else
            {
#ifdef CHECKED                
                // item is value type, list element type must be variant
                if (etype != Type.Variant)
                {
                    ErrorDump(71);
                    Error = 0x0B;
                    return;
                }
#endif
                pData = HRVariant.CreateValueVariant(item, itype);
            }
        }
        
        uint i = 0;
        uint pCurrent = ReadWord(this+lsFirst);
        uint pRecent  = ReadWord(this+lsRecent);
        if (pRecent != 0)
        {
            
            uint iRecent  = ReadWord(this+lsRecent+2);    
            if (iRecent <= index)
            {
                i        = iRecent;
                pCurrent = pRecent;
            }
        }
        loop
        {
            if (i == index)
            {
                break;
            }
            pCurrent = ReadWord(pCurrent+liNext);
            i++;
        }
        uint oldData = ReadWord(pCurrent+liData);
        if (IsReferenceType(etype))
        {
            GC.Release(oldData);
        }
            
        WriteWord(pCurrent+liData, pData);
        WriteWord(this+lsRecent,   pCurrent);    // pRecent
        WriteWord(this+lsRecent+2, index);       // iRecent  
    }
    
    Insert(uint this, uint index, uint item, Type itype)
    {
        Type etype = Type(ReadByte(this+lsType));
        uint length = ReadWord(this+lsLength);
        uint pFirst  = ReadWord(this+lsFirst);
        if (index >= length)
        {
            Append(this, item, itype);
        }
        else if (index == 0)
        {
            uint pItem  = createItem(item, etype, itype);
            WriteWord(pItem+liNext, pFirst);
            WriteWord(this+lsFirst, pItem);
            
            WriteWord(this+lsLength,   length+1); 
            WriteWord(this+lsRecent,   0);
            WriteWord(this+lsRecent+2, 0);
        }
        else
        {
            uint pCurrent= pFirst;
            uint pPrevious = 0;
        
            uint pRecent  = ReadWord(this+lsRecent);
            uint iRecent  = ReadWord(this+lsRecent+2);    
        
            uint count = 0;
            if ((iRecent != 0) && (index > iRecent)) // > means previous will be set
            {
                pCurrent= pRecent;
                count    = iRecent;
            }
            
            uint pItem  = createItem(item, etype, itype);
            while (0 != pCurrent)
            {
                if (index == count)
                {
                    WriteWord(pItem+liNext, pCurrent);
                    WriteWord(pPrevious+liNext, pItem);
                    
                    WriteWord(this+lsRecent,   pItem);
                    WriteWord(this+lsRecent+2, count);
                    WriteWord(this+lsLength,   length+1); 
                    break;
                }
                pPrevious = pCurrent;
                pCurrent = ReadWord(pCurrent+liNext);
                count++;
            }
        }
    }
    
    Append(uint this, uint item, Type itype)
    {
        Type etype = Type(ReadByte(this+lsType));
        uint pNewItem    = createItem(item, etype, itype);
        uint pFirstItem  = ReadWord(this+lsFirst);
        if (pFirstItem == 0)
        {
            WriteWord(this+lsFirst, pNewItem);
        }
        else
        {
            uint pCurrentItem = pFirstItem;
            uint pRecentItem  = ReadWord(this+lsRecent);
            if (pRecentItem != 0)
            {
                pCurrentItem = pRecentItem;
            }
            loop
            {
                uint pNextItem= ReadWord(pCurrentItem+liNext);
                if (pNextItem == 0)
                {
                    break;
                }
                pCurrentItem = pNextItem;
            }
            WriteWord(pCurrentItem+liNext, pNewItem);
        }
        uint length = ReadWord(this+lsLength)+1;
        WriteWord(this+lsLength,   length);         // number of items
        WriteWord(this+lsRecent,   pNewItem);           // pRecent
        WriteWord(this+lsRecent+2, length-1);       // iRecent  
    }
    
    bool Contains(uint this, uint item, Type itype)
    {
        Type etype = Type(ReadByte(this+lsType));
        uint pCurrent  = ReadWord(this+lsFirst);
        loop
        {
            if (0 == pCurrent) { break; }
            Type dtype = etype;
            uint pData = ReadWord(pCurrent+liData);
            if (IsReferenceType(dtype))
            {
                dtype = Type(ReadByte(pData));
            }
            if (HRVariant.IsEqual(pData, dtype, item, itype))
            {
                return true;
            }
            pCurrent = ReadWord(pCurrent+liNext);
        }
        return false;
    }
    
    Remove(uint this, uint index)
    {
        Type etype = Type(ReadByte(this+lsType));
        uint length = ReadWord(this+lsLength);
#ifdef CHECKED
        if (index >= length)
        {
            ErrorDump(73);
            Error = 0x01; // list index out of range
        }
#endif
        uint pCurrent = ReadWord(this+lsFirst);
        if (index == 0)
        {
            uint pNext    = ReadWord(pCurrent+liNext);
            clearItem(pCurrent, etype);
            WriteWord(this+lsFirst, pNext);
        }
        else
        {
            uint pPrevious = pCurrent;            // 0th item
            pCurrent = ReadWord(pCurrent+liNext); // 1st item
            
            uint count = 1;
            while (count < index)
            {
                pPrevious = pCurrent;
                pCurrent = ReadWord(pCurrent+liNext);
                count++;
            }
            WriteWord(pPrevious+liNext, ReadWord(pCurrent+liNext));
            clearItem(pCurrent, etype);
        }
        length = ReadWord(this+lsLength)-1;
        WriteWord(this+lsLength,   length);  // number of items
        WriteWord(this+lsRecent,   0);       // pRecent
        WriteWord(this+lsRecent+2, 0);       // iRecent   
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
        
        uint elements = ReadWord(address+lsLength);
        byte etype    = ReadByte(address+lsType);
        IO.WriteHex(elements); IO.Write(' '); IO.WriteHex(etype);
        
        if (indent > 0)
        {
            uint pCurrent  = ReadWord(address+lsFirst);
            IO.Write(' '); IO.WriteHex(pCurrent);
            uint count = 0;
            loop
            {
                if (pCurrent == 0)
                {
                    break;
                }
                IO.WriteLn();
                uint pData = ReadWord(pCurrent+liData);
                for (uint i = 0; i < indent; i++)
                {
                    IO.Write(' ');
                }
                
                if (IsReferenceType(Type(etype)))
                {
                    uint pNext = ReadWord(pCurrent+liNext);
                    WriteHex(pCurrent); IO.Write(','); WriteHex(pNext); IO.Write('-');IO.Write('>');IO.WriteHex(pData);
                    byte itype = ReadByte(pData);
                    byte iref  = ReadByte(pData+1);
                    IO.Write(':');IO.WriteHex(itype);
                    IO.Write(' ');IO.WriteHex(iref);
                    GC.Dump(pData, indent);
                }
                else
                {
                    IO.WriteHex(pData);
                }
                count++;
                if (count >= 5)
                {
                    IO.WriteLn();
                    for (uint i = 0; i < indent; i++)
                    {
                        IO.Write(' ');
                    }
                    IO.Write('.');IO.Write('.');IO.Write('.');
                    break;
                }
                pCurrent = ReadWord(pCurrent+liNext);
            }
        }
    }
}
