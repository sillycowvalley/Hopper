program rZ80
{
    #define CPU_Z80
    #define ROM_16K
    #define CDECL
    
    //#define GENERATING
    
    uses "/Source/Minimal/System"

    uses "/Source/Minimal/Memory"
#ifdef GENERATING
    uses "/Source/Minimal/GC"
#endif    
    
    writeDigit(uint uthis)
    {
        char c;
        byte digit = byte(uthis % 10);
        c = digit.ToDigit();
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Serial.WriteChar(c);
    }
    WriteInt(int this)
    {
        uint uthis;
        if (this < 0)
        {
            Write('-');
            this = 0 - this;
        }
        uthis = uint(this);
        writeDigit(uthis);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    
    WriteHex(byte b)
    {
        byte lsn;
        byte msn = ((b >> 4) & 0xF);
        lsn = b & 0xF;
        Serial.WriteChar(msn.ToHex());
        Serial.WriteChar(lsn.ToHex());
    }
    WriteHex(uint u)
    {
        byte lsb;
        byte msb = byte(u >> 8);
        lsb = byte(u & 0xFF);
        WriteHex(msb);
        WriteHex(lsb);
    }
    
    uint Fibo(uint n)
    {
        if ( n <= 1 ) 
        { return n; }
        else
        { return Fibo(n-1) + Fibo(n-2); }
    }
    
    Failure(uint instance)
    {    
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
        uint address7;
        uint address8;
        uint address9;
        
        
        uint frag;
        
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
        
        frag = 17 + 42 + 129 - 2 + 4;
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
        
        address7 = Memory.Allocate(1270);
        address8 = Memory.Allocate(401);
        address9 = Memory.Allocate(101);

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
    }
    
    //string testGlobal;
    byte [10] arrg;
    
    
    
    Method(ref byte[10] arg)
    {
        arg = arrg;
    }
    Method2(byte[10] arg)
    {
        Method(ref arg);
    }
    
    Hopper()
    {
        byte [10] arr;
        //string test;
        
        arrg = arr;
        
        //arr = arrg;
        
        //Method(ref arr);
        
            
        //Serial.WriteChar(Char.EOL); WriteUInt(c);
        //Serial.WriteChar(Char.EOL); WriteUInt(test.Length);
        
        //uint i = testGlobal.Length;
        //Serial.WriteChar(Char.EOL); WriteUInt(i);
        //Serial.WriteChar(Char.EOL); WriteUInt(arr3.Count);
        
        //testGlobal = test;
        //arr[1] = 10;
        //WriteUInt(arr[1]);
        
        //uint result = Fibo(10);
        //WriteUInt(result);
        
        //HeapAllocation();
        
#ifdef GENERATING        
        HeapAllocation();
        uint str = String.New();
        str = String.NewFromConstant(0, 0);
        uint str2 = GC.Clone(str);
        GC.Release(str);
        
        uint length = String.GetLength(str);
        uint arr2 = Array.New(10, Type.UInt);
        arr2 = Array.NewFromConstant(0, 10, Type.UInt);
        uint count = Array.GetCount(arr2);
#endif        
        //Serial.WriteChar(Char.EOL); Serial.WriteChar('O'); Serial.WriteChar('K');Serial.WriteChar('!');
    }
}
