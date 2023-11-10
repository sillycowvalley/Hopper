unit Memory
{
    byte ReadByte(uint address) system;
    WriteByte(uint address, byte value) system;
    
    byte ReadCodeByte(uint address) system;
    WriteCodeByte(uint address, byte value) system;
    
    // defaults don't matter, set in Initialize(..) below
    uint heapStart = 0x8000; // 32K
    uint heapSize  = 0x4000; // 32K-48K    
    
#ifdef SERIALCONSOLE
    uint ReadWord(uint address) system;
    WriteWord(uint address, uint value) system;
    uint ReadCodeWord(uint address) system;
    WriteCodeWord(uint address, uint value) system;
#else
    uint ReadWord(uint address)
    {
        return (ReadByte(address+1) << 8) + ReadByte(address);
    }
    WriteWord(uint address, uint word)
    {
        WriteByte(address,   byte(word & 0xFF));
        WriteByte(address+1, byte(word >> 8));
    }
    uint ReadCodeWord(uint address)
    {
        return (ReadCodeByte(address+1) << 8) + ReadCodeByte(address);
    }
    WriteCodeWord(uint address, uint word)
    {
        WriteCodeByte(address,   byte(word & 0xFF));
        WriteCodeByte(address+1, byte(word >> 8));
    }
#endif
    
    uint freeList;
    const byte mcbSize = 6;
    
    uint FreeList  { get { return freeList; } }
    uint HeapStart { get { return heapStart; } }
    uint HeapSize  { get { return heapSize; } }
    
    
    Initialize(uint start, uint size)
    {
        heapStart = start;
        heapSize  = size;
        
        freeList  = heapStart;
        
        // Zero initialize
        Memory.Set(freeList, 0, heapSize); 
        
        WriteWord(freeList, heapSize);   // all memory is in this single free list record
        WriteWord(freeList+2, 0);        // next = null
        WriteWord(freeList+4, 0);        // prev = null
    }
    
    uint Allocate(uint size)
    {
        uint address;
        loop
        {
            if (0 == size)
            {
                Error = 0x0C;
                break;
            }
            uint best;
            uint bestSize;
            uint bestNext; 
            uint bestPrev;
            uint current = freeList;
            size++; size++; // +2 space for the size field
            if (size < 6)
            {   
                size = 6; // minimum size for participation in free list
            }
            loop
            {
                if (0 == current)
                {
                    break;
                }
                // read current freeList
                uint currentSize = ReadWord(current);
                uint currentNext = ReadWord(current + 2);
                uint currentPrev = ReadWord(current + 4);
                if ((currentSize >= size) && 
                    ((0 == bestSize) || // first available block
                    (currentSize < bestSize)) // better than what we've seen so far in terms of fit
                    )
                {
                    best = current;
                    bestSize = currentSize;
                    bestNext = currentNext;
                    bestPrev = currentPrev;
                }
                if (bestSize == size)
                {
                    break; // can't get better than that
                }
                current = currentNext;
            }
            address = best + 2;
            if (bestSize >= size + 6)
            {
                // so we now how much to free later
                // block size includes the size of the size field itself
                WriteWord(best, size); 
                
                // enough extra to make a new freelist record from the balance
                uint newHole = best + size;
                uint newHoleSize = bestSize - size;
                WriteWord(newHole, newHoleSize);
                
                if (0 == bestPrev)
                {
                    freeList = newHole;
                    WriteWord(newHole+2,  bestNext); 
                    WriteWord(newHole+4,  0); // start of list now
                    if (0 != bestNext)
                    {
                        WriteWord(bestNext+4, newHole);
                    }
                }
                else
                {
                    WriteWord(newHole+2,  bestNext);
                    WriteWord(newHole+4,  bestPrev);
                    WriteWord(bestPrev+2, newHole);
                    
                    if (0 != bestNext)
                    {
                        WriteWord(bestNext+4, newHole);
                    }
                }
            }
            else if (bestSize >= size)
            {
                // just link the freelist past the new hole
                // and give allocate the entire slot (more than was asked)
               
                // so we now how much to free later
                // block size includes the size of the size field itself
                WriteWord(best, bestSize); 
                
                if (0 == bestPrev)
                {
                    // best was the old freeList
                    freeList = bestNext;
                    if (0 != bestNext)
                    {
                        WriteWord(freeList+4, 0); // start of list now so no previous
                    }
                }
                else
                {
                    WriteWord(bestPrev+2, bestNext);
                    if (0 != bestNext)
                    {
                        WriteWord(bestNext+4, bestPrev);
                    }
                }
            }
            else
            {
                Error = 0x0C;
                address = 0; // failed to allocate
            }
            break;
        }
        return address;
    }
    
    Free(uint address)
    {
        loop
        {
            if (0x0000 == address)
            {
                ErrorDump(1);
                Error = 0x0B;
                break;
            }
            uint blockAddress = address - 2;
            uint size  = ReadWord(blockAddress);
            
            uint current  = freeList;
            uint previous = 0;
            loop
            {
                if (0 == current)
                {
                    break;
                }
                // walk current freeList till next record beyond address
                if (current > address)
                {
                    break;
                }
                previous = current;
                uint currentNext = ReadWord(current + 2);
                current = currentNext;
            }
            uint currentPrev = previous;
            uint currentSize = 0;
            uint currentNext = 0;
            if (0 != current)
            {
                currentSize = ReadWord(current);
                currentNext = ReadWord(current + 2);
                //currentPrev = ReadWord(current + 4); // already set above
            }
            
            uint freeSlot = address-2;
            if (0 == currentPrev)
            {
                // current is front of freelist, insert in front of it
                WriteWord(freeSlot+2, current);
                WriteWord(freeSlot+4, 0);
                WriteWord(current+ 4, freeSlot);
                uint gapFront = freeList - (freeSlot+size);
                if (0 == gapFront)
                {
                    uint nextSize = ReadWord(freeList);
                    uint nextNext = ReadWord(freeList+2);
                    // no gap between freeSlot and freeList so absorb it into freeSlot block
                    WriteWord(freeSlot, size+nextSize);
                    WriteWord(freeSlot+2, nextNext);
                    if (0 != nextNext)
                    {
                        WriteWord(nextNext+4, freeSlot);
                    }
                }
                freeList = freeSlot;
            }
            else if (0 == current)
            {
                // currentPrev != 0 means we are at the end of the freeList
                // append to end of freelist (after currentPrev)
                WriteWord(currentPrev+2, freeSlot);
                WriteWord(freeSlot   +4, currentPrev);
                WriteWord(freeSlot   +2, 0);
                uint prevSize = ReadWord(currentPrev);
                uint gapBack = freeSlot - (currentPrev+prevSize);
                if (0 == gapBack)
                {
                    // no gap between freeSlot and previous so absorb it into previous block
                    WriteWord(currentPrev, prevSize+size);
                    WriteWord(currentPrev+2, 0); // nothing beyond freeSlot, tail of freeList
                }
            }
            else
            {
                // insert into freelist before current
                WriteWord(currentPrev+2, freeSlot);
                WriteWord(freeSlot   +4, currentPrev);
                WriteWord(freeSlot   +2, current);
                WriteWord(current    +4, freeSlot);
                uint prevSize = ReadWord(currentPrev);
                uint gapBack = freeSlot - (currentPrev+prevSize);
                if (0 == gapBack)
                {
                    // no gap between freeSlot and previous so absorb it into previous block
                    WriteWord(currentPrev, prevSize+size);
                    WriteWord(currentPrev+2, current);
                    WriteWord(current+4, currentPrev);
                    freeSlot = currentPrev;
                    size = prevSize+size;
                }
                uint gapNext = current - (freeSlot+size);
                if (0 == gapNext)
                {
                    // no gap between freeSlot and current so absorb it into freeSlot block
                    WriteWord(freeSlot, size+currentSize);
                    WriteWord(freeSlot+2, currentNext);
                    if (0 != currentNext)
                    {
                        WriteWord(currentNext+4, freeSlot);
                    }
                }
            }
            break;
        }
    }
    uint Available()
    {
        uint available;
        uint current = freeList;
        loop
        {
            if (0 == current)
            {
                break;
            }
            available = available + ReadWord(current) - 2;
            current = ReadWord(current + 2);
        }
        return available;
    }
    uint Maximum()
    {
        uint available;
        uint current = freeList;
        loop
        {
            if (0 == current)
            {
                break;
            }
            uint size = ReadWord(current + 0);
            if (size > available)
            {
                available = size;
            }
            current = ReadWord(current + 2);
        }
        if (available > 0)
        {
            available--; available--;
        }
        return available;
    }
    
    Set(uint memory, byte value, uint size)
    {
        for (uint i = 0; i < size; i++)
        {
            WriteByte(memory+i, value);
        }
    }
    
}
