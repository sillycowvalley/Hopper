program Memory
{
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Char"
    uses "/Source/System/Int"
    uses "/Source/System/UInt"
    uses "/Source/System/Long"
    uses "/Source/System/String"
    uses "/Source/System/List"
    
    const uint heapStart = 0x1000;
    const uint heapSize  = 0x4000;
    byte[heapSize] memory; // bottom 0x1000 is unused to simulate not starting at 0x0000 on Zooper
    
    uint freeList = 0x0000;
    uint totalAllocated = 0;
    long allocations = 0;
    long failures = 0;
    
    uint ReadWord(uint address)
    {
        uint word = memory[address-heapStart] + (memory[address-heapStart+1] << 8);
        return word;
    }
    WriteWord(uint address, uint word)
    {
        uint lsb = (word & 0xFF);
        uint msb = (word >> 8);
        memory[address-heapStart] = byte(lsb);
        memory[address-heapStart+1] = byte(msb);
    }
    MemorySet(uint address, byte value, uint size)
    {
        loop
        {
            if (size == 0)
            {
                break;
            }
            memory[address-heapStart] = value;
            address++;
            size--;
        }
    }
    MemoryMove(uint toAddress, uint fromAddress, uint size)
    {
        loop
        {
            memory[toAddress-heapStart] = memory[fromAddress-heapStart];
            toAddress++;
            fromAddress++;
            size--;
        }
    }
    bool DiagnosticsOn()
    {
        bool on = false;
        if (totalAllocated > heapSize)
        {
            on = true;
        }
        if (freeList < heapStart)
        {
            on = true;
        }
        if (freeList > heapStart + heapSize)
        {
            on = true;
        }
        if (!on)
        {
            uint available = ReadWord(freeList + 0);
            if (available > heapSize)
            {
                on = true;
            }
        }
        return on; 
    }
    Initialize(uint startAddress, uint blockSize)
    {
        freeList = startAddress;
        
        // Zero initialize
        MemorySet(freeList, 0x00, blockSize);
        
        WriteWord(freeList+0, blockSize); // all memory is in this single free list record
        WriteWord(freeList+2, 0);         // next = null
        WriteWord(freeList+4, 0);         // prev = null
    }
    uint Allocate(uint size)
    {
        return Allocate(size, false);
    }
    uint Allocate(uint size, bool diagnostics)
    {
        if (!diagnostics)
        {
            diagnostics = DiagnosticsOn();
        }
        if (diagnostics)
        {
            DumpFreeList();
        }
        uint address;
        loop
        {
            if (size == 0)
            {
                break;
            }
            uint best;
            uint bestSize;
            uint bestNext; 
            uint bestPrev;
            uint current = freeList;
            size = size + 2; // space for the size field
            if (size < 6)
            {   
                size = 6; // minimum size for participation in free list
            }
            loop
            {
                if (current == 0)
                {
                    break;
                }
                // read current freeList
                uint currentSize = ReadWord(current + 0);
                uint currentNext = ReadWord(current + 2);
                uint currentPrev = ReadWord(current + 4);
                if ((currentSize >= size) && 
                    ((bestSize == 0) || // first available block
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
            if (diagnostics)
            {
                PrintLn("Best:");
                PrintLn("  0x" + best.ToHexString(4) + ": bestPrev=0x" + bestPrev.ToHexString(4) + " bestNext=0x" + bestNext.ToHexString(4) + " (" + bestSize.ToString() + " bytes)");
                PrintLn("    bestSize=" + bestSize.ToString() + ", size=" + size.ToString());
            }   
            if (bestSize >= 6 + size)
            {
                // so we now how much to free later
                // block size includes the size of the size field itself
                WriteWord(best+0, size); 
                totalAllocated = totalAllocated + size;
                allocations = allocations + 1;
            
                // enough extra to make a new freelist record from the balance
                uint newHole = best + size;
                uint newHoleSize = bestSize - size;
                WriteWord(newHole, newHoleSize);
                if (bestPrev == 0)
                {
                    if (diagnostics)
                    {
                        PrintLn("      New Hole 1: newHole=0x" + newHole.ToHexString(4) +", newHoleSize=" + newHoleSize.ToString());
                    }
                    freeList = newHole;
                    WriteWord(newHole+2,  bestNext); 
                    WriteWord(newHole+4,  0); // start of list now
                    if (bestNext != 0)
                    {
                        WriteWord(bestNext+4, newHole);
                    }
                }
                else
                {
                    if (diagnostics)
                    {
                        PrintLn("      New Hole 2: newHole=0x" + newHole.ToHexString(4) +", newHoleSize=" + newHoleSize.ToString());
                    }
                    WriteWord(newHole+2,  bestNext);
                    WriteWord(newHole+4,  bestPrev);
                    WriteWord(bestPrev+2, newHole);
                    if (bestNext != 0)
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
                WriteWord(best+0, bestSize); 
               
                totalAllocated = totalAllocated + bestSize;
                allocations = allocations + 1;
            
                
                if (bestPrev == 0)
                {
                    // best was the old freeList
                    if (diagnostics)
                    {
                        PrintLn("New 3");
                    }
                    freeList = bestNext;
                    if (bestNext != 0)
                    {
                        WriteWord(freeList+4, 0); // start of list now so no previous
                    }
                }
                else
                {
                    
                    if (diagnostics)
                    {
                        PrintLn("New 4");
                    }
                    WriteWord(bestPrev+2, bestNext);
                    if (bestNext != 0)
                    {
                        WriteWord(bestNext+4, bestPrev);
                    }
                }
            }
            else
            {
                address = 0; // failed to allocate
                //DumpFreeList("Failed to Allocate " + size.ToString() + " bytes", Colour.MatrixRed, Colour.Black);
                //PrintLn("Best:");
                //PrintLn("  0x" + best.ToHexString(4) + ": bestPrev=0x" + bestPrev.ToHexString(4) + " bestNext=0x" + bestNext.ToHexString(4) + " (" + bestSize.ToString() + " bytes)");
                //PrintLn("    bestSize=" + bestSize.ToString() + ", size=" + size.ToString());
                //Key key = ReadKey();
                failures = failures + 1;
            }
            if (diagnostics)
            {
                PrintLn("Allocate: 0x" + best.ToHexString(4)+ " (" + size.ToString() + " bytes)");
                DumpFreeList();
                if (DiagnosticsOn())
                {
                    Key key = ReadKey();
                }
            }
            break;
        }
        
        return address;
    }
    
    Free(uint address)
    {
        Free(address, false);
    }
    Free(uint address, bool diagnostics)
    {
        if (!diagnostics)
        {
            diagnostics = DiagnosticsOn();
        }
        if (diagnostics)
        {
            DumpFreeList();
        }
        loop
        {
            if (address == 0)
            {
                break; // NOP like K&R C
            }
            uint blockAddress = address - 2;
            uint size  = ReadWord(blockAddress + 0);
            totalAllocated = totalAllocated - size;
            if (diagnostics)
            {
                PrintLn("Free: 0x" + blockAddress.ToHexString(4)+ " (" + size.ToString() + " bytes)");
            }
            
            uint current  = freeList;
            uint previous = 0;
            loop
            {
                if (current == 0)
                {
                    if (diagnostics)
                    {
                        PrintLn("Free 0");
                    }
                    break;
                }
                // walk current freeList till next record beyond address
                if (current > address)
                {
                    if (diagnostics)
                    {
                        PrintLn("Free 1");
                    }
                    break;
                }
                previous = current;
                uint currentNext = ReadWord(current + 2);
                current = currentNext;
            }
            if ((current == 0) && (previous == 0))
            {
                DumpFreeList("Free: no free list?", Colour.MatrixRed, Colour.Black);
                
                PrintLn("    freeList=" + freeList.ToHexString(4), Colour.MatrixRed, Colour.Black);
                PrintLn("    totalAllocated=" + totalAllocated.ToString(), Colour.MatrixRed, Colour.Black);
                PrintLn("    allocations=" + allocations.ToString(), Colour.MatrixRed, Colour.Black);
                PrintLn("    allocations=" + allocations.ToString(), Colour.MatrixRed, Colour.Black);
                
                diagnostics = true;
            }
            
            uint currentPrev = previous;
            uint currentSize = 0;
            uint currentNext = 0;
            if (current != 0)
            {
                currentSize = ReadWord(current + 0);
                currentNext = ReadWord(current + 2);
                currentPrev = ReadWord(current + 4);
                if (currentPrev != previous)
                {
                    PrintLn("currentPrev != previous  : WTF?!", Colour.MatrixRed, Colour.Black);
                }
            }
            if (diagnostics)
            {
                PrintLn("Next Free:");  
                PrintLn("  0x" + current.ToHexString(4) + ": prev=0x" + currentPrev.ToHexString(4) + " next=0x" + currentNext.ToHexString(4) + " (" + currentSize.ToString() + " bytes)");
            }
            uint freeSlot = address-2;
            
            if (currentPrev == 0)
            {
                if (diagnostics)
                {
                    PrintLn("Free 2");
                }
                // current is front of freelist, insert in front of it
                WriteWord(freeSlot+2, current);
                WriteWord(freeSlot+4, 0);
                WriteWord(current+ 4, freeSlot);
                uint gapFront = freeList -  (freeSlot+size);
                if (diagnostics)
                {
                    PrintLn("  gapFront=" + gapFront.ToHexString(4));
                }
                if (gapFront == 0)
                {
                    uint nextSize = ReadWord(freeList);
                    uint nextNext = ReadWord(freeList+2);
                    if (diagnostics)
                    {
                        PrintLn("    Free Merge 2 -> " + freeSlot.ToHexString(4));
                        PrintLn("      size=" + size.ToString() + " + nextSize=" + nextSize.ToString());
                    }
                    // no gap between freeSlot and freeList so absorb it into freeSlot block
                    WriteWord(freeSlot+0, size+nextSize);
                    WriteWord(freeSlot+2, nextNext);
                    if (nextNext != 0)
                    {
                        WriteWord(nextNext+4, freeSlot);
                    }
                }
                freeList = freeSlot;
            }
            else if (current == 0)
            {
                // currentPrev != 0 means we are at the end of the freeList
                if (diagnostics)
                {
                    PrintLn("Free 4");
                    PrintLn("    Free Slot 0x" + freeSlot.ToHexString(4));
                }
                // append to end of freelist (after currentPrev)
                WriteWord(currentPrev+2, freeSlot);
                WriteWord(freeSlot   +4, currentPrev);
                WriteWord(freeSlot   +2, 0);
                uint prevSize = ReadWord(currentPrev);
                uint gapBack = freeSlot - (currentPrev+prevSize);
                if (diagnostics)
                {
                    PrintLn("  gapBack=" + gapBack.ToHexString(4) + " (" + freeSlot.ToHexString(4) + "-(" + currentPrev.ToHexString(4) + "+" +prevSize.ToHexString(4) +")");
                }
                if (gapBack == 0)
                {
                    if (diagnostics)
                    {
                        PrintLn("    Free Merge 6 -> " + currentPrev.ToHexString(4));
                        PrintLn("      prevSize=" + prevSize.ToString() + " + size=" + size.ToString());
                    }
                    // no gap between freeSlot and previous so absorb it into previous block
                    WriteWord(currentPrev+0, prevSize+size);
                    WriteWord(currentPrev+2, 0); // nothing beyond freeSlot, tail of freeList
                }
                
            }
            else
            {
                if (diagnostics)
                {
                    PrintLn("Free 3");
                }
                // insert into freelist before current
                WriteWord(currentPrev+2, freeSlot);
                WriteWord(freeSlot   +4, currentPrev);
                WriteWord(freeSlot   +2, current);
                WriteWord(current    +4, freeSlot);
                uint prevSize = ReadWord(currentPrev);
                uint gapBack = freeSlot - (currentPrev+prevSize);
                if (diagnostics)
                {
                    PrintLn("  gapBack=" + gapBack.ToHexString(4) + " (" + freeSlot.ToHexString(4) + "-(" + currentPrev.ToHexString(4) + "+" +prevSize.ToHexString(4) +")");
                }
                if (gapBack == 0)
                {
                    if (diagnostics)
                    {
                        PrintLn("    Free Merge 4 -> " + currentPrev.ToHexString(4));
                        PrintLn("      prevSize=" + prevSize.ToString() + " + size=" + size.ToString());
                    }
                    // no gap between freeSlot and previous so absorb it into previous block
                    WriteWord(currentPrev+0, prevSize+size);
                    WriteWord(currentPrev+2, current);
                    WriteWord(current+4, currentPrev);
                    freeSlot = currentPrev;
                    size = prevSize+size;
                }
                uint gapNext = current - (freeSlot+size);
                if (diagnostics)
                {
                    PrintLn("  gapNext=" + gapNext.ToHexString(4));
                }
                if (gapNext == 0)
                {
                    if (diagnostics)
                    {
                        PrintLn("    Free Merge 5 -> " + freeSlot.ToHexString(4));
                        PrintLn("      size=" + size.ToString() + " + currentSize=" + currentSize.ToString());
                    }
                    // no gap between freeSlot and current so absorb it into freeSlot block
                    WriteWord(freeSlot+0, size+currentSize);
                    WriteWord(freeSlot+2, currentNext);
                    if (currentNext != 0)
                    {
                        WriteWord(currentNext+4, freeSlot);
                    }
                }
            }
            break;
        } // loop
        if (diagnostics)
        {
            DumpFreeList();
            if (DiagnosticsOn())
            {
                Key key = ReadKey();
            }
        }
    }
    uint AvailableTotal()
    {
        uint available;
        
        uint current = freeList;
        loop
        {
            if (current == 0)
            {
                break;
            }
            available = available + ReadWord(current + 0) - 2;
            current = ReadWord(current + 2);
        }
        return available;
    }
    uint AvailableMax()
    {
        uint available;
        
        uint current = freeList;
        loop
        {
            if (current == 0)
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
            available = available - 2;
        }
        return available;
    }
    DumpFreeList()
    {
        DumpFreeList("", Colour.MatrixGreen, Colour.Black);
    }
    DumpFreeList(string message, uint fore, uint back)
    {
        uint current = freeList;
        PrintLn();
        uint extentPrev = 0;
        loop
        {
            if (current == 0)
            {
                break;
            }
            // read current freeList
            uint currentSize = ReadWord(current + 0);
            uint currentNext = ReadWord(current + 2);
            uint currentPrev = ReadWord(current + 4); 
            
            
            Print("  0x" + current.ToHexString(4) + ": prev=0x" + currentPrev.ToHexString(4) 
                                                     + " next=0x" + currentNext.ToHexString(4) 
                                                     + " (" + currentSize.ToString() + " bytes)", fore, back);
            if (extentPrev > 0)
            {
                uint gapBack = current - extentPrev;
                Print(" [gapback=" + gapBack.ToString() + " bytes]", fore, back);    
            }
            PrintLn();
            extentPrev = current + currentSize;
            current = currentNext;
        }
        uint total = AvailableTotal();
        uint   max = AvailableMax();
        PrintLn("Free List: " + message + " (currently allocated: " + totalAllocated.ToString() + " bytes, " + allocations.ToString() + " allocations, " + failures.ToString() + " failures)", fore, back);  
        PrintLn("           availableTotal=" + total.ToString()  + " bytes,  availableMax=" + max.ToString()  + " bytes");
    }
    {
        Initialize(heapStart, heapSize);
        
        <uint> allocations3;
        <uint> allocations4;
        uint iterations = 0;
        loop
        {
            uint ptr1 = Allocate(64);
            uint ptr4 = Allocate(iterations % 256);
            allocations4.Append(ptr4);

            if (ptr4 == 0)
            {
                foreach (var ptrh in allocations4)    {       Free(ptrh);        }
                allocations4.Clear();
            }
            <uint> allocations;
            for (uint i=0; i < 3; i++)
            {
                uint ptr = Allocate(31 + 10-i);     allocations.Append(ptr);

            }
            <uint> allocations2;
            for (uint i=0; i < 5; i++)
            {
                uint ptr = Allocate(13 + 10-i);     allocations2.Append(ptr);
            }
            foreach (var ptr in allocations)      {     Free(ptr);      }
            for (uint i=0; i < 7; i++)
            {
                uint ptr = Allocate(73 + 10-i);     allocations3.Append(ptr);
                if (ptr == 0)
                {
                    foreach (var ptrg in allocations4)    {       Free(ptrg);        }
                    allocations4.Clear();
                }
            }
            uint ptr0 = Allocate(64);
            Free(ptr1);
            foreach (var ptr in allocations2)     {    Free(ptr);      }
            Free(ptr0);
            iterations++;
            if (iterations == 8000)
            {
                break;
            }
            if (iterations % 24 == 0)
            {
                foreach (var ptr in allocations3)     {      Free(ptr);       }
                allocations3.Clear();
            }
            if (iterations % 1000 == 0)
            {
                DumpFreeList("Lap " + iterations.ToString(), Colour.MatrixGreen Colour.Black);
            }
        }
        foreach (var ptr in allocations3)    {       Free(ptr);        }
        foreach (var ptr in allocations4)    {       Free(ptr);        }
        DumpFreeList();
    }
}