#include "HopperMemory.h"

UInt Memory_heapStart;
UInt Memory_heapSize;
UInt Memory_freeList;

UInt Memory_HeapStart_Get()
{
    return Memory_heapStart;
}

UInt Memory_HeapSize_Get()
{
    return Memory_heapSize;
}

UInt Memory_FreeList_Get()
{
    return Memory_freeList;
}

void Memory_Free(UInt address)
{
    for (;;)
    {
        if (0x00 == address)
        {
            SetError(0x0B, (12)); // Memory_Free(0)
            break;
        }
        UInt blockAddress = address - 0x02;
        UInt size = Memory_ReadWord(blockAddress);
        UInt current = Memory_freeList;
        UInt previous = 0x00;
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            if (current > address)
            {
                break;;
            }
            previous = current;
            UInt currentNext = Memory_ReadWord(current + 0x02);
            current = currentNext;
        }
        UInt currentPrev = previous;
        UInt currentSize = 0x00;
        UInt currentNext = 0x00;
        if (0x00 != current)
        {
            currentSize = Memory_ReadWord(current);
            currentNext = Memory_ReadWord(current + 0x02);
        }
        UInt freeSlot = address - 0x02;
        if (0x00 == currentPrev)
        {
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(freeSlot + 0x04, 0x00);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt gapFront = Memory_freeList - (freeSlot + size);
            if (0x00 == gapFront)
            {
                UInt nextSize = Memory_ReadWord(Memory_freeList);
                UInt nextNext = Memory_ReadWord(Memory_freeList + 0x02);
                Memory_WriteWord(freeSlot, size + nextSize);
                Memory_WriteWord(freeSlot + 0x02, nextNext);
                if (0x00 != nextNext)
                {
                    Memory_WriteWord(nextNext + 0x04, freeSlot);
                }
            }
            Memory_freeList = freeSlot;
        }
        else if (0x00 == current)
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, 0x00);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, 0x00);
            }
        }
        else
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, current);
                Memory_WriteWord(current + 0x04, currentPrev);
                freeSlot = currentPrev;
                size = prevSize + size;
            }
            UInt gapNext = current - (freeSlot + size);
            if (0x00 == gapNext)
            {
                Memory_WriteWord(freeSlot, size + currentSize);
                Memory_WriteWord(freeSlot + 0x02, currentNext);
                if (0x00 != currentNext)
                {
                    Memory_WriteWord(currentNext + 0x04, freeSlot);
                }
            }
        }
        break;
    }
}

void Memory_Initialize(UInt start, UInt size)
{
    Memory_heapStart = start;
    Memory_heapSize = size;
    Memory_freeList = Memory_heapStart;
    Memory_Set(Memory_freeList, 0x00, Memory_heapSize);
    Memory_WriteWord(Memory_freeList, Memory_heapSize);
    Memory_WriteWord(Memory_freeList + 0x02, 0x00);
    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
}

UInt Memory_Allocate(UInt size)
{
    UInt address = 0;
    for (;;)
    {
        if (0x00 == size)
        {
            SetError(0x0C, (11)); // Memory_Allocate size == 0
            break;;
        }
        UInt best = 0;
        UInt bestSize = 0;
        UInt bestNext = 0;
        UInt bestPrev = 0;
        UInt current = Memory_freeList;
        
        size++;
        
        size++;
        if (size < 0x06)
        {
            size = 0x06;
        }
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            UInt currentSize = Memory_ReadWord(current);
            UInt currentNext = Memory_ReadWord(current + 0x02);
            UInt currentPrev = Memory_ReadWord(current + 0x04);
            if ((currentSize >= size) && ((0x00 == bestSize) || (currentSize < bestSize)))
            {
                best = current;
                bestSize = currentSize;
                bestNext = currentNext;
                bestPrev = currentPrev;
            }
            if (bestSize == size)
            {
                break;;
            }
            current = currentNext;
        }
        address = best + 0x02;
        if (bestSize >= size + 0x06)
        {
            Memory_WriteWord(best, size);
            UInt newHole = best + size;
            UInt newHoleSize = bestSize - size;
            Memory_WriteWord(newHole, newHoleSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = newHole;
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, 0x00);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
            else
            {
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, bestPrev);
                Memory_WriteWord(bestPrev + 0x02, newHole);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
        }
        else if (bestSize >= size)
        {
            Memory_WriteWord(best, bestSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = bestNext;
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
                }
            }
            else
            {
                Memory_WriteWord(bestPrev + 0x02, bestNext);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, bestPrev);
                }
            }
        }
        else
        {
            SetError(0x0C, (10)); // failed to Memory_Allocate
            address = 0x00;
        }
        break;;
    }
    return address;
}

void Memory_Set(UInt memory, Byte value, UInt size)
{;
    for (UInt i = 0x00; i < size; i++)
    {
        Memory_WriteByte(memory + i, value);
    }
}

void GC_Release(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    
    referenceCount--;
    Memory_WriteByte(address + 0x01, referenceCount);
    Type htype = Type(Memory_ReadByte(address));
    if (referenceCount == 0x00)
    {
        switch (htype)
        {
            case Type::eArray:
            case Type::eLong:
            case Type::eFloat:
            case Type::eString:
            {
                Memory_Free(address);
                break;
            }
            /* TODO
            case Type::eDirectory:
            {
                HRDirectory_Clear(address);
                Memory_Free(address);
                break;
            }
            case Type::eFile:
            {
                HRFile_Clear(address);
                Memory_Free(address);
                break;
            }
            case Type::eList:
            {
                HRList_Clear(address);
                Memory_Free(address);
                break;
            }
            case Type::eDictionary:
            {
                HRDictionary_Clear(address);
                Memory_Free(address);
                break;
            }
            case Type::ePair:
            {
                HRPair_Clear(address);
                Memory_Free(address);
                break;
            }
            case Type::eVariant:
            {
                HRVariant_Clear(address);
                Memory_Free(address);
                break;
            }
            */
            default:
            {
                SetError(0x0A, (9)); // TODO : GC_Release
                break;
            }
        } // switch
    }
}
UInt GC_Clone(UInt original)
{
    Type htype = Type(Memory_ReadByte(original));
    switch (htype)
    {
        case Type::eString:
        {
            return HRString_Clone(original);
            break;
        }
        /* TODO
        case Type::eDirectory:
        {
            return HRDirectory_Clone(original);
            break;
        }
        case Type::eFile:
        {
            return HRFile_Clone(original);
            break;
        }
        case Type::eList:
        {
            return HRList_Clone(original);
            break;
        }
        case Type::eDictionary:
        {
            return HRDictionary_Clone(original);
            break;
        }
        case Type::ePair:
        {
            return HRPair_Clone(original);
            break;
        }
        case Type::eVariant:
        {
            return HRVariant_Clone(original);
            break;
        }
        */
        default:
        {
            SetError(0x0A, (21)); // TODO : GC_Clone
            break;
        }
    } // switch
    return 0x00;
}

UInt GC_New(UInt size, Type htype)
{
    UInt address = Memory_Allocate(size + 0x02);
    Memory_WriteByte(address, Byte(htype));
    Memory_WriteByte(address + 0x01, 0x01);
    return address;
}

Bool IsReferenceType(Type htype)
{
    return (Byte(htype) >= 0x0F);
}

void GC_AddReference(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    referenceCount++;
    Memory_WriteByte(address + 0x01, referenceCount);
}
