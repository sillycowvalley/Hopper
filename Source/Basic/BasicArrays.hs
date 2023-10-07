unit BasicArrays
{
    // Clear()
    // uint DimArray(uint elements)
    // ArraySet(uint address, uint index, int value)
    // int ArrayGet(uint address, uint index, uint currentLineNumber, ref BasicType elementType)
    
#ifdef H6502
    uses "/Source/6502/Firmware/Memory"
    <uint> allocatedList;
    
#else
    uses "/Source/System/Array"
    const uint memorySize = 0x6000; // 24K
    
    byte[memorySize] memoryArray;
    uint freeSlot;
    byte[8] setSlots;
    byte[8] clearSlots;
#endif    
    
    <uint, BasicType> elementTypes;
#ifdef CHECKED    
    <uint, uint> rangeList;
#endif

    byte readByte(uint address)
    {
#ifdef H6502
        return ReadByte(address);
#else
        byte b = memoryArray[address];
        return b;
#endif
    }
    
    uint maximumAvail()
    {
#ifdef H6502
        return Maximum();
#else
        uint bytesAvail = memorySize - freeSlot;
        return bytesAvail;     
#endif
    }

    Clear()
    {
#ifdef H6502
        foreach (var ptr in allocatedList)
        {
            uint address = ptr;
            Free(address);
        }
        allocatedList.Clear();
#else 
        freeSlot = 0;
        elementTypes.Clear();
        setSlots[0] = 0x01;
        setSlots[1] = 0x02;
        setSlots[2] = 0x04;
        setSlots[3] = 0x08;
        setSlots[4] = 0x10;
        setSlots[5] = 0x20;
        setSlots[6] = 0x40;
        setSlots[7] = 0x80;
        clearSlots[0] = 0xFE;
        clearSlots[1] = 0xFD;
        clearSlots[2] = 0xFB;
        clearSlots[3] = 0xF7;
        clearSlots[4] = 0xEF;
        clearSlots[5] = 0xDF;
        clearSlots[6] = 0xBF;
        clearSlots[7] = 0x7F;
#endif
#ifdef CHECKED
        rangeList.Clear();
#endif
    }
    uint DimArray(uint elements, BasicType elementType)
    {
        uint sizeInBytes = elements * 2; // INT
        if (elementType == BasicType.Boolean)
        {
            sizeInBytes = elements >> 3;
            if ((elements & 0x0007) != 0)
            {
                sizeInBytes++;
            }
        }
        if (sizeInBytes > maximumAvail())
        {
            return 0;
        }
        uint address;
#ifdef H6502
        address = Allocate(sizeInBytes);
        allocatedList.Append(address);
#else
        address = freeSlot;
        freeSlot = freeSlot + sizeInBytes;
#endif
#ifdef CHECKED        
        rangeList[address] = elements;
#endif
        elementTypes[address] = elementType;
#ifndef H6502
        address++; // to be non-zero
#endif        
        return address;
    }
    ArraySet(uint address, uint index, int value, uint currentLineNumber, BasicType setType)
    {
        loop
        {
#ifndef H6502
            address--; // to be non-zero
#endif        
#ifdef CHECKED        
            uint range = rangeList[address];
            if ((index < 0) || (index >= range))
            {
                Error(28, currentLineNumber); // array index
                break;
            }
#endif   
            BasicType elementType = elementTypes[address];
            if (elementType != setType)
            {
                Error(18, currentLineNumber); // Type mismatch.
                break;
            }
            if (elementType == BasicType.Boolean)
            {
                
#ifdef H6502
                WriteBit(address, index, byte(value));
#else
                uint offset = address + (index >> 3);
                byte slotIndex = byte(index & 0x07);
                byte b = readByte(offset);
                if (value == 0)
                {
                    // clear the bit
                    byte mask = clearSlots[slotIndex];
                    b = b & mask;
                }
                else
                {
                    // set the bit
                    byte mask = setSlots[slotIndex];
                    b = b | mask;
                }
                memoryArray[offset] = b;
#endif                
            }
            else
            {
                uint offset = address + index << 1;
#ifdef H6502
                WriteByte(offset,   byte(value & 0xFF));
                WriteByte(offset+1, byte(value >> 8));
                
#else
                memoryArray[offset]   = byte(value & 0xFF);
                memoryArray[offset+1] = byte(value >> 8);
                
#endif                                
            }
            break;
        }
    }
    int ArrayGet(uint address, uint index, uint currentLineNumber, ref BasicType elementType)
    {
        int value;
        loop
        {
#ifndef H6502
            address--; // to be non-zero
#endif        
#ifdef CHECKED        
            uint range = rangeList[address];
            if ((index < 0) || (index >= range))
            {
                Error(28, currentLineNumber); // array index
                break;
            }
#endif
            elementType = elementTypes[address];
            if (elementType == BasicType.Boolean)
            {
#ifndef H6502                
                uint offset = address + (index >> 3);
                byte slotIndex = byte(index & 0x07);
                byte b = readByte(offset);
                byte mask = setSlots[slotIndex];
                value = b & mask;
                if (value != 0)
                {
                    value = 1;
                }
#else
                value = ReadBit(address, index);
#endif
            }
            else
            {
                uint offset = address + index << 1;
                int lsb = readByte(offset);
                int msb = readByte(offset+1);
                value = (msb << 8) | lsb;
            }
            break;
        }
        return value;
    }
}
