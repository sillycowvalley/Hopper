unit Memory
{
    byte ReadByte(uint address) system;
    WriteByte(uint address, byte value) system;
    uint ReadWord(uint address) system;
    WriteWord(uint address, uint value) system;
    
    const uint cHeapStart         = 0xFF07; // 1 byte
    const uint cHeapSize          = 0xFF08; // 1 byte
    const uint cFreeList          = 0xFF09; // 2 bytes
    
    uint Available()
    {
        uint available;
        uint current = ReadWord(cFreeList);
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
        uint size;
        uint current = ReadWord(cFreeList);
        loop
        {
            if (0 == current)
            {
                break;
            }
            size = ReadWord(current + 0);
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
    
    uint Allocate(uint size)
    {
        uint address;
        uint best;
        uint bestSize;
        uint bestNext; 
        uint bestPrev;
        uint current;
        uint currentSize;
        uint currentNext;
        uint currentPrev;
        uint newHole; 
        uint newHoleSize;
        
        loop
        {
            if (0 == size)
            {
                Die(0x0C);
                break;
            }
            current = ReadWord(cFreeList);
            size += 2; // +2 space for the size field
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
                currentSize = ReadWord(current);
                currentNext = ReadWord(current + 2);
                currentPrev = ReadWord(current + 4);
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
                newHole = best + size;
                newHoleSize = bestSize - size;
                WriteWord(newHole, newHoleSize);
                
                if (0 == bestPrev)
                {
                    WriteWord(cFreeList, newHole);
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
                    WriteWord(cFreeList, bestNext);
                    if (0 != bestNext)
                    {
                        WriteWord(ReadWord(cFreeList)+4, 0); // start of list now so no previous
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
                Die(0x0C);
                address = 0; // failed to allocate
            }
            break;
        }
        return address;
    }
    Free(uint address)
    {
        uint blockAddress;
        uint size;
        uint current;
        uint previous;
        uint currentPrev;
        uint currentSize;
        uint currentNext;
        uint freeSlot;
        uint gapFront;
        uint nextSize;
        uint nextNext;
        uint prevSize;
        uint gapBack;
        uint gapNext;
        
        loop
        {
            if (0x0000 == address)
            {
                Die(0x0B);
                break;
            }
            blockAddress = address - 2;
            size  = ReadWord(blockAddress);
            
            current  = ReadWord(cFreeList);
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
                current = ReadWord(current + 2);
            }
            currentPrev = previous;
            currentSize = 0;
            currentNext = 0;
            if (0 != current)
            {
                currentSize = ReadWord(current);
                currentNext = ReadWord(current + 2);
                //currentPrev = ReadWord(current + 4); // already set above
            }
            
            freeSlot = address-2;
            if (0 == currentPrev)
            {
                // current is front of freelist, insert in front of it
                WriteWord(freeSlot+2, current);
                WriteWord(freeSlot+4, 0);
                WriteWord(current+ 4, freeSlot);
                gapFront = ReadWord(cFreeList) - (freeSlot+size);
                if (0 == gapFront)
                {
                    nextSize = ReadWord(ReadWord(cFreeList));
                    nextNext = ReadWord(ReadWord(cFreeList)+2);
                    // no gap between freeSlot and freeList so absorb it into freeSlot block
                    WriteWord(freeSlot, size+nextSize);
                    WriteWord(freeSlot+2, nextNext);
                    if (0 != nextNext)
                    {
                        WriteWord(nextNext+4, freeSlot);
                    }
                }
                WriteWord(cFreeList, freeSlot);
            }
            else if (0 == current)
            {
                // currentPrev != 0 means we are at the end of the freeList
                // append to end of freelist (after currentPrev)
                WriteWord(currentPrev+2, freeSlot);
                WriteWord(freeSlot   +4, currentPrev);
                WriteWord(freeSlot   +2, 0);
                prevSize = ReadWord(currentPrev);
                gapBack = freeSlot - (currentPrev+prevSize);
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
                prevSize = ReadWord(currentPrev);
                gapBack = freeSlot - (currentPrev+prevSize);
                if (0 == gapBack)
                {
                    // no gap between freeSlot and previous so absorb it into previous block
                    WriteWord(currentPrev, prevSize+size);
                    WriteWord(currentPrev+2, current);
                    WriteWord(current+4, currentPrev);
                    freeSlot = currentPrev;
                    size = prevSize+size;
                }
                gapNext = current - (freeSlot+size);
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

}
