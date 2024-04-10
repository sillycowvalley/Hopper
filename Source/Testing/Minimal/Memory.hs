program Memory
{
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/Memory"
    uses "/Source/Minimal/Diagnostics"
    
    writeDigit(uint uthis)
    {
        byte digit = byte(uthis % 10);
        char c = digit.ToDigit();
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Serial.WriteChar(c);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    WriteHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        byte lsn = b & 0xF;
        Serial.WriteChar(msn.ToHex());
        Serial.WriteChar(lsn.ToHex());
    }
    WriteHex(uint u)
    {
        byte msb = byte(u >> 8);
        byte lsb = byte(u & 0xFF);
        WriteHex(msb);
        WriteHex(lsb);
    }
    
    
    HeapWalk()
    {
        uint heapStart = ReadByte(0x0008) << 8;
        uint heapSize  = ReadByte(0x0009) << 8;
        uint freeList  = ReadWord(0x0006);
        Serial.WriteChar(Char.EOL); Serial.WriteChar('H'); Serial.WriteChar(':');  WriteHex(heapStart);
        Serial.WriteChar(Char.EOL); Serial.WriteChar('S'); Serial.WriteChar(':');  WriteHex(heapSize);Serial.WriteChar(' ');WriteUInt(heapSize);
        Serial.WriteChar(Char.EOL); Serial.WriteChar('F'); Serial.WriteChar(':');  WriteHex(freeList);
        
        // walk the heap
        uint current = heapStart;
        uint total = 0;
        loop
        {
            uint blockSize = ReadWord(current);
            total += blockSize;
            Serial.WriteChar(Char.EOL); Serial.WriteChar(' '); Serial.WriteChar(' ');    
            WriteHex(current); Serial.WriteChar(' '); WriteHex(blockSize);
            Serial.WriteChar(' '); Serial.WriteChar('S'); WriteUInt(blockSize-2);
            Serial.WriteChar(' '); Serial.WriteChar('T'); WriteUInt(total);
            
            current += blockSize;
            if (current >= heapStart + heapSize) { break; }
        }
        
        // walk the freelist
        Serial.WriteChar(Char.EOL); 
        current = freeList;
        total = 0;
        uint biggest;
        loop
        {
            uint blockSize = ReadWord(current);
            uint nextBlock = ReadWord(current+2);
            uint prevBlock = ReadWord(current+4);
            uint userSize =      blockSize-2;
            total += userSize;
            if (userSize > biggest)
            {
                biggest = userSize;
            }
            
            Serial.WriteChar(Char.EOL); Serial.WriteChar(' '); Serial.WriteChar(' ');    
            WriteHex(current); Serial.WriteChar(' '); WriteHex(blockSize); 
            Serial.WriteChar(' '); WriteHex(nextBlock);
            Serial.WriteChar(' '); WriteHex(prevBlock);
            
            Serial.WriteChar(' '); Serial.WriteChar('S'); WriteUInt(userSize);
            Serial.WriteChar(' '); Serial.WriteChar('T'); WriteUInt(total);
            Serial.WriteChar(' '); Serial.WriteChar('M'); WriteUInt(biggest);
            
            if (nextBlock == 0) { break; }
            current = nextBlock;
        }
    }
    
    Failure(uint instance)
    {    
        HeapWalk();
        Serial.WriteChar(Char.EOL);Serial.WriteChar('D');Serial.WriteChar('A');Serial.WriteChar('N');Serial.WriteChar('G');Serial.WriteChar(':');
        WriteUInt(instance);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    
    
    HeapAllocation()
    {
        uint memoryMax = Memory.Maximum();
        uint used;
        if (Memory.Available() != memoryMax)
        {
            Failure(1);
        }
        HeapWalk();
        
        uint address0 = Memory.Allocate(256); used += 258;
        if (Memory.Maximum() != memoryMax - used)
        {
            Failure(2);
        }
        if (Memory.Available() != memoryMax - used)
        {
            Failure(3);
        }
        
        uint address1 = Memory.Allocate(17);   used += 19;
        uint address2 = Memory.Allocate(23);   used += 25;
        uint address3 = Memory.Allocate(127);  used += 129;
        uint address4 = Memory.Allocate(40);   used += 42;
        uint address5 = Memory.Allocate(1000); used += 1002;
        uint address6 = Memory.Allocate(3);    used += 6;
        
        if (Memory.Maximum() != memoryMax - used)
        {
            Failure(4);
        }
        if (Memory.Available() != memoryMax - used)
        {
            Failure(5);
        }
        
        Memory.Free(address1); used -= 19;
        Memory.Free(address4); used -= 42;
        Memory.Free(address3); used -= 129;
        Memory.Free(address6); used -= 6;
        
        uint frag = 17 + 42 + 129 - 2 + 4;
        if (Memory.Maximum() != memoryMax - used - frag)
        {
            Serial.WriteChar(' '); Serial.WriteChar('X'); WriteUInt(Memory.Available());
            Serial.WriteChar(' '); Serial.WriteChar('M'); WriteUInt(memoryMax);
            Serial.WriteChar(' '); Serial.WriteChar('U'); WriteUInt(used);
            Serial.WriteChar(' '); Serial.WriteChar('F'); WriteUInt(frag);
            Serial.WriteChar(' '); Serial.WriteChar('?'); WriteUInt(memoryMax - used - frag);
            Failure(6);
        }
        if (Memory.Available() != memoryMax - used - 4) // 3 slots in the freelist, one accounted for in memoryMax
        {
            Serial.WriteChar(' '); Serial.WriteChar('X'); WriteUInt(Memory.Available());
            Serial.WriteChar(' '); Serial.WriteChar('M'); WriteUInt(memoryMax);
            Serial.WriteChar(' '); Serial.WriteChar('U'); WriteUInt(used);
            Serial.WriteChar(' '); Serial.WriteChar('?'); WriteUInt(memoryMax - used - 6 + 4);
            Failure(7);
        }
        
        
        
        uint address7 = Memory.Allocate(1270);
        uint address8 = Memory.Allocate(401);
        uint address9 = Memory.Allocate(101);

        Memory.Free(address9);
        Memory.Free(address0);
        Memory.Free(address7);
        Memory.Free(address5);
        Memory.Free(address2);
        Memory.Free(address8);

        if (Memory.Maximum() != memoryMax)
        {
            Failure(8);
        }
        if (Memory.Available() != memoryMax)
        {
            Failure(9);
        }
        HeapWalk();
    }
    
    Hopper()
    {
        HeapAllocation();
        Serial.WriteChar(Char.EOL); Serial.WriteChar('O'); Serial.WriteChar('K');Serial.WriteChar('!');
    }
}
