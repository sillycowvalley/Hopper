unit HopperVM
{
    uses "/Source/Runtime/Emulation/Minimal"
    uses "/Source/Runtime/Emulation/Memory"
    
    uses "/Source/Runtime/Platform/OpCodes"
    uses "/Source/Runtime/Platform/Types"
    uses "/Source/Runtime/Platform/SysCalls"
    
    uses "/Source/Runtime/Platform/GC"
    uses "/Source/Runtime/Platform/Array"
    uses "/Source/Runtime/Platform/Dictionary"
    uses "/Source/Runtime/Platform/Int"
    uses "/Source/Runtime/Platform/List"
    uses "/Source/Runtime/Platform/Long"
    uses "/Source/Runtime/Platform/Pair"
    uses "/Source/Runtime/Platform/String"
    uses "/Source/Runtime/Platform/UInt"
    uses "/Source/Runtime/Platform/Variant"
    
    uses "/Source/Runtime/Platform/External"
    uses "/Source/Runtime/Platform/Instructions"
    
    
    const uint stackSize     = 512; // size of value stack in byte (each stack slot is 2 bytes)
    const uint callStackSize = 512; // size of callstack in bytes (4 bytes per call)
    
    const uint dataMemoryStart = 0x0000; // data memory magically exists from 0x0000 to 0xFFFF
    
    
#ifdef SERIALCONSOLE
    const uint jumpTableSize   = 512; // 4 byte function pointer slots on Pi Pico
#else    
    const uint jumpTableSize   = 256; // 2 byte delegate slots, highest OpCode is currently 0x6A (256 will do until 0x7F)
#endif
    
    uint binaryAddress;
    uint constAddress;
    uint methodTable;
    
    uint valueStack; // 2 byte slots
    uint typeStack;  // 2 byte slots (but we only use the LSB, just to be able to use the same 'sp' as valueStack)
    uint callStack;  // 2 byte slots (either return address PC or BP for stack from)
    
    uint dataMemory; // start of free memory (changes if a new program is loaded)
    
    uint breakpoints;
    bool breakpointExists;
    
    uint pc;
    uint sp;
    uint bp;
    uint csp;
    bool cnp;
    
    uint PC  { get { return pc; }  set { pc = value; } }
    uint SP  { get { return sp; }  set { sp = value; } }
    uint CSP { get { return csp; } set { csp = value; } }
    bool CNP { get { return cnp; } set { cnp = value; } }
    uint BP  { get { return bp; }  set { bp = value; } }
    
    OpCode opCode;
    OpCode CurrentOpCode { get { return opCode; } }
    
    uint ValueStack { get { return valueStack; } }
    uint TypeStack  { get { return typeStack; } }
    uint CallStack  { get { return callStack; } }
    
    bool BreakpointExists { get { return breakpointExists; } }
    
    uint jumpTable; // 2 byte delegate slots for Instruction jumps
    
    
    ClearBreakpoints(bool includingZero)
    {
        for (byte i=2; i < 32; i = i + 2)
        {
            WriteWord(breakpoints + i, 0);
        }
        if (includingZero)
        {
            WriteWord(breakpoints, 0);
            breakpointExists = false;
        }
        else
        {
            breakpointExists = ReadWord(breakpoints) != 0; // single step breakpoint set?
        }
    }
    uint GetBreakpoint(byte n)
    {
        return ReadWord(breakpoints + n * 2);
    }
    
    SetBreakpoint(byte n, uint address)
    {
        WriteWord(breakpoints + n * 2,  address);
        if (address != 0)
        {
            breakpointExists = true;
        }
        else
        {
            breakpointExists = false;
            for (byte i=0; i < 32; i = i + 2)
            {
                if (ReadWord(breakpoints+i) != 0)
                {
                    breakpointExists = true;
                    break;
                }
            }
        }
    }
    
    Initialize(uint loadedAddress, uint codeLength)
    {
        binaryAddress      = loadedAddress;
        constAddress       = ReadCodeWord(binaryAddress + 0x0002);
        methodTable        = binaryAddress + 0x0006;
    }
    
    DataMemoryReset()
    {
        HRArray.Release();  // in case we were already called
        
        uint nextAddress   = dataMemoryStart;
        callStack          = nextAddress;
        nextAddress        = nextAddress + callStackSize;
        
        valueStack         = nextAddress;
        nextAddress        = nextAddress + stackSize;
        
        typeStack          = nextAddress;
        nextAddress        = nextAddress + stackSize;
        
        jumpTable          = nextAddress;
        nextAddress        = nextAddress + jumpTableSize;
        
        Instructions.PopulateJumpTable(jumpTable);
        
        dataMemory         = nextAddress;
        
#ifdef SERIALCONSOLE        
        if (dataMemory < 0x0800)
        {
            dataMemory = 0x0800; // after our 'fake' stack pages
        }
#endif
        
#ifdef SERIALCONSOLE
        // currently we have 64K on the Pi Pico (0xFFFC is 64K (0x10000) - 4 
        // to avoid any boundary condition issues in the heap allocator)
        // For Wemos D1 Mini, 32K segments so 0x7FFC
        Memory.Initialize(dataMemory, ((External.GetSegmentSizes()-2) << 1) - dataMemory);
#else
        Memory.Initialize(dataMemory, 0xFFFC - dataMemory);
#endif

        breakpoints   = Memory.Allocate(32);
        ClearBreakpoints(true);
        HRArray.Initialize();
    }
    Release()
    {
        HRArray.Release();
        Memory.Free(breakpoints);
        breakpoints = 0;
    }
    
    Restart()
    {
        DataMemoryReset();
        
        sp = 0;
        bp = 0;
        csp = 0;
        Error = 0;
        cnp = false;
        pc = ReadCodeWord(binaryAddress + 0x0004);   
    }
    
    
    uint LookupMethod(uint methodIndex)
    {
        methodIndex = (methodIndex & 0x3FFF);
        uint address = methodTable;
        loop
        {
            uint entry = ReadCodeWord(address);
            if (entry == methodIndex)
            {
                address = ReadCodeWord(address+2);
                break;
            }
            address = address + 4;
        } // loop
        return address;
    }
#ifdef CHECKED
    AssertChar(Type htype, uint value)
    {
        switch (htype)
        {
            case Type.Byte:
            case Type.UInt:
            case Type.Int:
            case Type.Char:
            {
                if (value > 255)
                {
                    ErrorDump(38);
                    Error = 0x0B; // system failure (internal error)
                }
            }
            default:
            {
                ErrorDump(37);
                Error= 0x0B; // system failure (internal error)
            }
        }
    }
    AssertBool(Type htype, uint value)
    {
        if (htype != Type.Bool)
        {
            AssertUInt(htype, value);
        }
        if (value > 1)
        {
            ErrorDump(36);
            Error = 0x0B; // system failure (internal error)
        }
    }
    AssertLong(Type htype)
    {
        if (htype != Type.Long)
        {
            ErrorDump(74);
            Error = 0x0B; // system failure (internal error)
        }
    }
    AssertFloat(Type htype)
    {
        if (htype != Type.Float)
        {
            ErrorDump(99);
            Error = 0x0B; // system failure (internal error)
        }
    }
    AssertUInt(Type htype, uint value)
    {
        switch (htype)
        {
            case Type.Byte:
            case Type.Char:
            case Type.Bool:
            case Type.UInt:
            {
            }
            case Type.Int:
            {
                if (value > 0x7FFF) // +int is ok, -int is not
                {
                    ErrorDump(35);
                    Error = 0x0D; // numeric type out of range / overflow
                }
            }
            default:
            {
                Write(' ');WriteHex(byte(htype)); Write(' ');ErrorDump(34);
                Error= 0x0B; // system failure (internal error)
            }
        }
    }
    AssertInt(Type htype)
    {
        AssertInt(htype, 0);
    }
    AssertInt(Type htype, uint value)
    {
        switch (htype)
        {
            case Type.Byte:
            case Type.Char:
            case Type.Bool:
            case Type.UInt:
            {
                if (value > 0x7FFF) // won't fit
                {
                    ErrorDump(61);
                    Error = 0x0D; // numeric type out of range / overflow
                }
            }
            case Type.Int:
            {
                
            }
            default:
            {
                ErrorDump(33);
                Error= 0x0B; // system failure (internal error)
            }
        }
    }
    AssertReference(Type htype, uint value)
    {
        switch (htype)
        {
            case Type.Byte:
            case Type.UInt:
            case Type.Reference:
            {
            }
            default:
            {
                ErrorDump(32);
                Error= 0x0B; // system failure (internal error)
            }
        }
    }
#endif    
    bool ExecuteSysCall(byte iSysCall, uint iOverload)
    {
        switch (SysCall(iSysCall))
        {
            case SysCall.ScreenPrintLn:
            {
                IO.WriteLn();
            }
            case SysCall.ScreenClear:
            {
                IO.Clear();
            }
            case SysCall.ScreenPrint:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint bc = Pop(ref atype);
                        Type btype;
                        uint fc = Pop(ref btype);
                        Type ctype;
                        uint ch = Pop(ref ctype);
#ifdef CHECKED
                        AssertUInt(atype, bc);
                        AssertUInt(btype, fc);
                        AssertChar(ctype, ch);
#endif
                        IO.Write(char(ch));
                    }
                    case 1:
                    {
                        Type atype;
                        uint bc = Pop(ref atype);
                        Type btype;
                        uint fc = Pop(ref btype);
                        Type stype;
                        uint str = Pop(ref stype);
#ifdef CHECKED
                        AssertUInt(atype, bc);
                        AssertUInt(btype, fc);
                        if (stype != Type.String)
                        {
                            ErrorDump(31);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif
                        uint length = HRString.GetLength(str);
                        for (uint i=0; i < length; i++)
                        {
                            char ch = HRString.GetChar(str, i);
                            IO.Write(ch);
                        }
                        GC.Release(str);
                    }
                    default:
                    {
                        ErrorDump(6);
                        Error = 0x0A; // iOverload
                    }
                }
            }
                  
            case SysCall.SerialWriteChar:
            {
                Type atype;
                uint ch = Pop(ref atype);
#ifdef CHECKED
                AssertChar(atype, ch);
#endif
                Serial.WriteChar(char(ch));
            }
            
            case SysCall.LongNewFromConstant:
            {
                Type atype;
                uint location = Pop(ref atype);
#ifdef CHECKED
                AssertUInt(atype, location);
#endif
                uint address = HRLong.NewFromConstant(constAddress + location);
                Push(address, Type.Long);
            }
            
            case SysCall.FloatNewFromConstant:
            {
                Type atype;
                uint location = Pop(ref atype);
#ifdef CHECKED
                AssertUInt(atype, location);
#endif
                uint address = HRFloat.NewFromConstant(constAddress + location);
                Push(address, Type.Float);
            }
        
            case SysCall.StringNewFromConstant:
            {
                switch(iOverload)
                {
                    case 0:
                    {
                        Type ltype;
                        uint length  = Pop(ref ltype);
                        Type atype;
                        uint location = Pop(ref atype);
#ifdef CHECKED
                        AssertUInt(ltype, length);
                        AssertUInt(atype, location);
#endif
                        uint address = HRString.NewFromConstant0(constAddress + location, length);
                        Push(address, Type.String);
                    }
                    case 1:
                    {
                        Type utype;
                        uint doubleChar = Pop(ref utype);
#ifdef CHECKED
                        AssertUInt(utype, doubleChar);
#endif
                        uint address = HRString.NewFromConstant1(doubleChar);
                        Push(address, Type.String);
                    }
                    default:
                    {
                        ErrorDump(5);
                        Error = 0x0A; // iOverload
                    }
                }
            }
            case SysCall.StringNew:
            {
                uint address = HRString.New();
                Push(address, Type.String);
            }
            case SysCall.StringLengthGet:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.String)
                {
                    ErrorDump(30);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                uint length = HRString.GetLength(this);
                GC.Release(this);
                Push(length, Type.UInt);
            }
            case SysCall.StringGetChar:
            {
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.String)
                {
                    ErrorDump(29);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                char ch = HRString.GetChar(this, index);
                GC.Release(this);
                Push(uint(ch), Type.Char);
            }
            case SysCall.StringInsertChar:
            {
                Type atype;
                uint ch = Pop(ref atype);
                Type itype;
                uint index  = Pop(ref itype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertChar(atype, ch);
                AssertUInt(itype, index);
                if (ttype != Type.String)
                {
                    ErrorDump(28);
                    Error = 0x0B; // system failure (internal error)
                }
#endif             
                uint result = HRString.InsertChar(this, index, char(ch));
                GC.Release(this);
                Push(result, Type.String);                
            }
            case SysCall.StringEndsWith:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint with = Pop(ref atype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        AssertChar(atype, with);
                        if (ttype != Type.String)
                        {
                            ErrorDump(27);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = uint(HRString.EndsWith(this, char(with)));
                        GC.Release(this);
                        Push(result, Type.Bool);
                    }
                    case 1:
                    {
                        Type atype;
                        uint with = Pop(ref atype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        if ((atype != Type.String) || (ttype != Type.String))
                        {
                            ErrorDump(26);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = uint(HRString.EndsWith(this, with));
                        GC.Release(this);
                        GC.Release(with);
                        Push(result, Type.Bool);
                    }
                    
                }  
            }
            case SysCall.StringCompare:
            {
                Type atype;
                uint right = Pop(ref atype);
                Type btype;
                uint left  = Pop(ref btype);
#ifdef CHECKED
                if ((atype != Type.String) || (btype != Type.String))
                {
                    ErrorDump(77);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                int result = HRString.Compare(left, right);
                GC.Release(right);
                GC.Release(left);
                PushI(result);
            }
            case SysCall.StringReplace:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint to     = Pop(ref atype);
                        Type btype;
                        uint from   = Pop(ref btype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        if ((atype != Type.String) || (btype != Type.String) || (ttype != Type.String))
                        {
                            ErrorDump(25);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Replace(this, from, to);
                        GC.Release(this);
                        GC.Release(to);
                        GC.Release(from);
                        Push(result, Type.String);
                    }
                    case 1:
                    {
                        Type atype;
                        uint to   = Pop(ref atype);
                        Type btype;
                        uint from   = Pop(ref btype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        AssertChar(atype, to);
                        AssertChar(btype, from);
                        if (ttype != Type.String)
                        {
                            ErrorDump(24);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Replace(this, char(from), char(to));
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                }    
            }
            case SysCall.StringAppend:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint append = Pop(ref atype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        if ((atype != Type.String) || (ttype != Type.String))
                        {
                            ErrorDump(23);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Append(this, append);
                        GC.Release(this);
                        GC.Release(append);
                        Push(result, Type.String);
                    }
                    case 1:
                    {
                        Type atype;
                        uint append = Pop(ref atype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        AssertChar(atype, append);
                        if (ttype != Type.String)
                        {
                            ErrorDump(22);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Append(this, char(append));
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                }   
            }
            case SysCall.StringSubstring:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type stype;
                        uint start = Pop(ref stype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        AssertUInt(stype, start);
                        if (ttype != Type.String)
                        {
                            ErrorDump(21);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Substring(this, start);
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                    case 1:
                    {
                        Type ltype;
                        uint limit = Pop(ref ltype);
                        Type stype;
                        uint start = Pop(ref stype);
                        Type ttype;
                        uint this = Pop(ref ttype);
#ifdef CHECKED
                        AssertUInt(ltype, limit);
                        AssertUInt(stype, start);
                        if (ttype != Type.String)
                        {
                            ErrorDump(20);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Substring(this, start, limit);
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                    case 2:
                    {
                        Type stype;
                        uint start = Pop(ref stype);
                        Type htype;
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        AssertUInt(stype, start);
                        if (htype != Type.String)
                        {
                            ErrorDump(19);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.Substring(ref str, start);
                        Put(address, str, Type.String);
                    }
                    default:
                    {
                        ErrorDump(18);
                        Error = 0x0B;
                    }
                }   
            }
            case SysCall.StringBuild:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint append = Pop(ref atype);
#ifdef CHECKED
                        if (atype != Type.String)
                        {
                            ErrorDump(43);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif
                        Type htype;
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(17);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.Build(ref str, append);
                        Put(address, str, Type.String);
                        GC.Release(append);
                    }
                    case 1:
                    {
                        Type htype;
                        char ch = char(Pop(ref htype));
#ifdef CHECKED
                        AssertChar(htype, byte(ch));
#endif                
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(17);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.Build(ref str, ch);
                        Put(address, str, Type.String);
                    }
                    case 2:
                    {
                        Type htype;
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(17);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.Build(ref str);
                        Put(address, str, Type.String);                        
                    }
                    default:
                    {
                        ErrorDump(42);
                        Error = 0x0B;
                    }
                }
            }
            case SysCall.StringBuildFront:
            {
                Type htype;
                char ch = char(Pop(ref htype));
#ifdef CHECKED
                AssertChar(htype, byte(ch));
#endif                
                uint address = Pop(ref htype);
                uint str = Get(address, ref htype);
#ifdef CHECKED
                if (htype != Type.String)
                {
                    ErrorDump(16);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                HRString.BuildFront(ref str, ch);
                Put(address, str, Type.String);
            }
            case SysCall.StringTrim:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type htype;
                        uint this = Pop(ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(14);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.Trim(this);
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                    case 1:
                    {
                        Type htype;
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(15);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.TrimRight(ref str);
                        HRString.TrimLeft(ref str);
                        Put(address, str, Type.String);
                    }
                    default:
                    {
                        ErrorDump(4);
                        Error = 0x0A; // iOverload
                    }
                }
            }
            case SysCall.StringTrimLeft:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type htype;
                        uint this = Pop(ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(14);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        uint result = HRString.TrimLeft(this);
                        GC.Release(this);
                        Push(result, Type.String);
                    }
                    case 1:
                    {
                        Type htype;
                        uint address = Pop(ref htype);
                        uint str = Get(address, ref htype);
#ifdef CHECKED
                        if (htype != Type.String)
                        {
                            ErrorDump(14);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif        
                        HRString.TrimLeft(ref str);
                        Put(address, str, Type.String);
                    }
                    default:
                    {
                        ErrorDump(3);
                        Error = 0x0A; // iOverload
                    }
                }
            }
            case SysCall.StringTrimRight:
            {
                Type htype;
                uint address = Pop(ref htype);
                uint str = Get(address, ref htype);
#ifdef CHECKED
                if (htype != Type.String)
                {
                    ErrorDump(68);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                HRString.TrimRight(ref str);
                Put(address, str, Type.String);
            }
            case SysCall.ArrayNew:
            {   
                Type stype;
                Type htype = Type(Pop(ref stype));
#ifdef CHECKED
                if (IsReferenceType(htype))
                {
                    ErrorDump(12);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint count = Pop(ref stype);
#ifdef CHECKED
                AssertUInt(htype, count);
#endif
                uint address = HRArray.New(htype, count);
                Push(address, Type.Array);
            }
            
            case SysCall.ArrayGetItem:
            {
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.Array)
                {
                    ErrorDump(11);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                Type etype;
                uint item = HRArray.GetItem(this, index, ref etype);
                GC.Release(this);
                Push(item, etype);
            }
            case SysCall.ArraySetItem:
            {
                Type itype;
                uint item = Pop(ref itype);
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if ((ttype != Type.Array) || IsReferenceType(itype))
                {
                    ErrorDump(10);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                HRArray.SetItem(this, index, item);
                GC.Release(this);
            }
            case SysCall.ArrayCountGet:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Array)
                {
                    ErrorDump(53);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                uint length = HRArray.GetCount(this);
                GC.Release(this);
                Push(length, Type.UInt);
            }
          
            
            case SysCall.ListNew:
            {   
                Type stype;
                Type htype = Type(Pop(ref stype));
                uint address = HRList.New(htype);
                Push(address, Type.List);
            }
            case SysCall.ListLengthGet:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.List)
                {
                    ErrorDump(53);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                uint length = HRList.GetLength(this);
                GC.Release(this);
                Push(length, Type.UInt);
            }
            case SysCall.ListAppend:
            {
                Type itype;
                uint item = Pop(ref itype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.List)
                {
                    ErrorDump(54);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                HRList.Append(this, item, itype);
                if (IsReferenceType(itype))
                {
                    GC.Release(item);    
                }
                GC.Release(this);
            }
            case SysCall.ListSetItem:
            {
                Type itype;
                uint item  = Pop(ref itype);
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.List)
                {
                    ErrorDump(57);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                    
                HRList.SetItem(this, index, item, itype);
                if (IsReferenceType(itype))
                {
                    GC.Release(item);
                }
                GC.Release(this);
            }
            case SysCall.ListInsert:
            {
                Type itype;
                uint item  = Pop(ref itype);
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.List)
                {
                    ErrorDump(55);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                    
                HRList.Insert(this, index, item, itype);
                if (IsReferenceType(itype))
                {
                    GC.Release(item);
                }
                GC.Release(this);
            }
            case SysCall.ListGetItem:
            {
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.List)
                {
                    ErrorDump(56);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                Type itype;
                uint item = HRList.GetItem(this, index, ref itype);
                GC.Release(this);
                Push(item, itype);
            }
            
            case SysCall.ListClear:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.List)
                {
                    ErrorDump(58);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                    
                HRList.Clear(this);
                GC.Release(this);
            }
            case SysCall.ListRemove:
            {
                Type atype;
                uint index = Pop(ref atype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(atype, index);
                if (ttype != Type.List)
                {
                    ErrorDump(72);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                HRList.Remove(this, index);
                GC.Release(this);
            }
            case SysCall.ListContains:
            {
                Type itype;
                uint item = Pop(ref itype);
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.List)
                {
                    ErrorDump(60);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                bool contains = HRList.Contains(this, item, itype);
                if (IsReferenceType(itype))
                {
                    GC.Release(item);    
                }
                GC.Release(this);
                Push(contains ? 1 : 0, Type.Bool);
            }
            
            case SysCall.PairNew:
            {
                Type vtype;
                uint value = Pop(ref vtype);
                Type ktype;
                uint key = Pop(ref ktype);
#ifdef CHECKED
                if (IsReferenceType(ktype) && (ktype != Type.String))
                {
                    ErrorDump(85);
                    Error = 0x0B; // system failure (internal error) - only reference type allowed for key is String
                }
#endif
                uint address = HRPair.New(ktype, key, vtype, value);
                Push(address, Type.Pair);
            }
            
            case SysCall.VariantBox:
            {
                Type vvtype;
                Type vtype = Type(Pop(ref vvtype));
                Type vttype;
                uint value = Pop(ref vttype);
                uint address = HRVariant.New(value, vtype);
                Push(address, Type.Variant);
            }
            
            case SysCall.PairValue:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Pair)
                {
                    ErrorDump(81);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                Type vtype;
                uint value = HRPair.GetValue(this, ref vtype);
                GC.Release(this);
                Push(value, vtype);
            }
            case SysCall.PairKey:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Pair)
                {
                    ErrorDump(81);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                Type ktype;
                uint key = HRPair.GetKey(this, ref ktype);
                GC.Release(this);
                Push(key, ktype);
            }
            
            case SysCall.TypesTypeOf:
            {
                Type ttype;
                uint this = Pop(ref ttype);
                if (IsReferenceType(ttype))
                {
                    GC.Release(this);
                }
                Push(byte(ttype), Type.Type);
            }
            
            case SysCall.DictionaryNew:
            {   
                Type stype;
                Type vtype = Type(Pop(ref stype));
                Type ktype = Type(Pop(ref stype));
#ifdef CHECKED
                if (IsReferenceType(ktype) && (ktype != Type.String))
                {
                    ErrorDump(9);
                    Error = 0x0B; // system failure (internal error) - only reference type allowed for key is String
                }
#endif
                uint address = HRDictionary.New(ktype, vtype);
                Push(address, Type.Dictionary);
            }
            
            case SysCall.DictionaryCountGet:
            {
                Type ttype;
                uint this = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(81);
                    Error = 0x0B; // system failure (internal error)
                }
#endif    
                uint count = HRDictionary.GetCount(this);
                GC.Release(this);
                Push(count, Type.UInt);
            }
            
            case SysCall.DictionarySet:
            {
                Type vtype;
                uint value  = Pop(ref vtype);
                Type ktype;
                uint key = Pop(ref ktype);
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(82);
                    Error = 0x0B; // system failure (internal error)
                    return false;
                }
#endif                    
                HRDictionary.Set(this, key, ktype, value, vtype);
                if (IsReferenceType(ktype))
                {
                    GC.Release(key);
                }
                if (IsReferenceType(vtype))
                {
                    GC.Release(value);
                }
                GC.Release(this);
            }
            
            case SysCall.DictionaryNext:
            {
                Type htype;
                uint iterator = Pop(ref htype);
                
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                AssertUInt(htype, iterator);
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(89);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                  
                uint hrpair;
                uint found = HRDictionary.Next(this, ref iterator, ref hrpair) ? 1 : 0;
                
                GC.Release(this);
                Push(found, Type.Bool);
                Push(hrpair, Type.Pair);    
                Push(iterator, Type.UInt);    
            }
            
            case SysCall.DictionaryContains:
            {
                Type ktype;
                uint key = Pop(ref ktype);
                
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                if (ktype != Type.String)
                {
                    AssertUInt(ktype, key);
                }
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(87);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                  
                uint found = HRDictionary.Contains(this, key) ? 1 : 0;
                if (ktype == Type.String)
                {
                    GC.Release(key);
                }
                GC.Release(this);
                Push(found, Type.Bool);
            }
            
            case SysCall.DictionaryGet:
            {
                Type ktype;
                uint key = Pop(ref ktype);
                
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                if (ktype != Type.String)
                {
                    AssertUInt(ktype, key);
                }
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(88);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                  
                Type vtype;
                uint result = HRDictionary.Get(this, key, ref vtype);
                if (ktype == Type.String)
                {
                    GC.Release(key);
                }
                GC.Release(this);
                Push(result, vtype);
            }
            case SysCall.DictionaryClear:
            {
                Type ttype;
                uint this  = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Dictionary)
                {
                    ErrorDump(90);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                  
                HRDictionary.Clear(this);
                GC.Release(this);
            }
            
            case SysCall.CharToDigit:
            {
                Type htype;
                uint b = byte(Pop(ref htype));
#ifdef CHECKED
                if (b > 9) // 0..9 ok
                {
                    ErrorDump(8);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                Push(uint(b+48), Type.Char);
            }
            case SysCall.UIntToLong:
            {
                Type htype;
                uint value = Pop(ref htype);
#ifdef CHECKED
                AssertUInt(htype, value);
#endif
                uint lng = HRUInt.ToLong(value);
                Push(lng, Type.Long);
            }
            
            case SysCall.IntToLong:
            {
                Type htype;
                uint ichunk = Pop(ref htype);
#ifdef CHECKED
                AssertInt(htype, ichunk);
#endif
                uint lng = HRInt.ToLong(ichunk);
                Push(lng, Type.Long);
            }
            case SysCall.IntToFloat:
            {
                Type htype;
                int ichunk = PopI(ref htype);
                uint f = External.IntToFloat(ichunk);
                Push(f, Type.Float);
            }
            case SysCall.UIntToFloat:
            {
                Type htype;
                uint ichunk = Pop(ref htype);
#ifdef CHECKED
                AssertUInt(htype, ichunk);
#endif
                uint f = External.UIntToFloat(ichunk);
                Push(f, Type.Float);
            }
            case SysCall.IntToBytes:
            {
                Type htype;
                uint ichunk = Pop(ref htype);
#ifdef CHECKED
                AssertInt(htype, ichunk);
#endif
                uint lst = HRInt.ToBytes(ichunk);
                Push(lst, Type.List);
            }
            case SysCall.LongToBytes:
            {
                Type htype;
                uint l = Pop(ref htype);
#ifdef CHECKED
                AssertLong(htype);
#endif
                uint lst = HRLong.ToBytes(l);
                Push(lst, Type.List);                
            }
            case SysCall.LongGetByte:
            {
                uint index = Pop();
                Type htype;
                uint l = Pop(ref htype);
#ifdef CHECKED
                AssertLong(htype);
#endif
                byte b = HRLong.GetByte(l, index);
                Push(b, Type.Byte);                
            }
            case SysCall.LongFromBytes:
            {
                byte b3 = byte(Pop());
                byte b2 = byte(Pop());
                byte b1 = byte(Pop());
                byte b0 = byte(Pop());
                
                uint l = HRLong.FromBytes(b0, b1, b2, b3);
                Push(l, Type.Long);                
            }
            case SysCall.IntGetByte:
            {
                uint index = Pop();
                Type htype;
                uint i = Pop();
                byte b = HRInt.GetByte(i, index);
                Push(b, Type.Byte);                
            }
            case SysCall.IntFromBytes:
            {
                byte b1 = byte(Pop());
                byte b0 = byte(Pop());
                
                uint i = HRInt.FromBytes(b0, b1);
                Push(i, Type.Int);  
            }
            
            case SysCall.FloatToBytes:
            {
                Type htype;
                uint l = Pop(ref htype);
#ifdef CHECKED
                AssertFloat(htype);
#endif
                uint lst = HRFloat.ToBytes(l);
                Push(lst, Type.List);                
            }
            case SysCall.FloatToString:
            {
                Type htype;
                uint l = Pop(ref htype);
#ifdef CHECKED
                AssertFloat(htype);
#endif
                uint str = External.FloatToString(l);
                Push(str, Type.String);                
            }
            case SysCall.FloatGetByte:
            {
                uint index = Pop();
                Type htype;
                uint f = Pop(ref htype);
#ifdef CHECKED
                AssertFloat(htype);
#endif
                byte b = HRFloat.GetByte(f, index);
                Push(b, Type.Byte);                
            }
            case SysCall.FloatFromBytes:
            {
                byte b3 = byte(Pop());
                byte b2 = byte(Pop());
                byte b1 = byte(Pop());
                byte b0 = byte(Pop());
                
                uint f = HRFloat.FromBytes(b0, b1, b2, b3);
                Push(f, Type.Float);                
            }
            case SysCall.LongToUInt:
            {
                Type htype;
                uint this = Pop(ref htype);
#ifdef CHECKED
                if (htype != Type.Long)
                {
                    ErrorDump(7);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint ui = HRLong.ToUInt(this);
                Push(ui, Type.UInt);
                GC.Release(this);
            }
            case SysCall.LongToFloat:
            {
                Type htype;
                uint this = Pop(ref htype);
#ifdef CHECKED
                if (htype != Type.Long)
                {
                    ErrorDump(7);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint f = External.LongToFloat(this);
                Push(f, Type.Float);
                GC.Release(this);
            }
            
            case SysCall.LongNegate:
            {
                Type ttype;
                uint top = Pop(ref ttype);
#ifdef CHECKED
                if (ttype != Type.Long)
                {
                    ErrorDump(64);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint result;
                Type rtype = Type.Long;
                result = HRLong.LongNegate(top);
                Push(result, rtype);        
                GC.Release(top);
            }
            
            
            case SysCall.LongAdd:
            case SysCall.LongSub:
            case SysCall.LongDiv:
            case SysCall.LongMul:
            case SysCall.LongMod:
            case SysCall.LongEQ:
            case SysCall.LongLT:
            case SysCall.LongLE:
            case SysCall.LongGT:
            case SysCall.LongGE:
            {
                Type ttype;
                uint top = Pop(ref ttype);
                Type ntype;
                uint next = Pop(ref ntype);
#ifdef CHECKED
                if ((ttype != Type.Long) || (ntype != Type.Long))
                {
                    ErrorDump(6);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint result;
                Type rtype = Type.Long;
                switch (SysCall(iSysCall))
                {
                    case SysCall.LongAdd:
                    {
                        result = External.LongAdd(next, top);
                    }
                    case SysCall.LongSub:
                    {
                        result = External.LongSub(next, top);
                    }
                    case SysCall.LongDiv:
                    {
                        result = External.LongDiv(next, top);
                    }
                    case SysCall.LongMul:
                    {
                        result = External.LongMul(next, top);
                    }
                    case SysCall.LongMod:
                    {
                        result = External.LongMod(next, top);
                    }   
                    case SysCall.LongEQ:
                    {
                        result = External.LongEQ(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.LongLT:
                    {
                        result = External.LongLT(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.LongLE:
                    {
                        result = External.LongLE(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.LongGT:
                    {
                        result = External.LongGT(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.LongGE:
                    {
                        result = External.LongGE(next, top);
                        rtype = Type.Bool;
                    }   
                    
                }
                Push(result, rtype);        
                GC.Release(top);
                GC.Release(next);
            }
            
            case SysCall.FloatAdd:
            case SysCall.FloatSub:
            case SysCall.FloatDiv:
            case SysCall.FloatMul:
            case SysCall.FloatEQ:
            case SysCall.FloatLT:
            case SysCall.FloatLE:
            case SysCall.FloatGT:
            case SysCall.FloatGE:
            {
                Type ttype;
                uint top = Pop(ref ttype);
                Type ntype;
                uint next = Pop(ref ntype);
#ifdef CHECKED
                if ((ttype != Type.Float) || (ntype != Type.Float))
                {
                    ErrorDump(6);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint result;
                Type rtype = Type.Float;
                switch (SysCall(iSysCall))
                {
                    case SysCall.FloatAdd:
                    {
                        result = External.FloatAdd(next, top);
                    }
                    case SysCall.FloatSub:
                    {
                        result = External.FloatSub(next, top);
                    }
                    case SysCall.FloatDiv:
                    {
                        result = External.FloatDiv(next, top);
                    }
                    case SysCall.FloatMul:
                    {
                        result = External.FloatMul(next, top);
                    }
                    case SysCall.FloatEQ:
                    {
                        result = External.FloatEQ(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.FloatLT:
                    {
                        result = External.FloatLT(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.FloatLE:
                    {
                        result = External.FloatLE(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.FloatGT:
                    {
                        result = External.FloatGT(next, top);
                        rtype = Type.Bool;
                    }   
                    case SysCall.FloatGE:
                    {
                        result = External.FloatGE(next, top);
                        rtype = Type.Bool;
                    }   
                    
                }
                Push(result, rtype);        
                GC.Release(top);
                GC.Release(next);
            }
            case SysCall.LongNew:
            {
                uint address = HRLong.New();
                Push(address, Type.Long);
            }
            case SysCall.FloatNew:
            {
                uint address = HRFloat.New();
                Push(address, Type.Float);
            }
            
            
            case SysCall.TimeMillis:
            {
                uint address = External.GetMillis();
                Push(address, Type.Long);
            }
            case SysCall.TimeDelay:
            {
                External.Delay(Pop());
            }
            case SysCall.MCUPinMode:
            {
                byte mode  = byte(Pop());
                byte pin   = byte(Pop());
                External.PinMode(pin, mode);
            }
            case SysCall.MCUDigitalWrite:
            {
                byte value = byte(Pop());
                byte pin   = byte(Pop());
                External.DigitalWrite(pin, value);
            }
            case SysCall.MCUDigitalRead:
            {
                byte pin   = byte(Pop());
                byte value = External.DigitalRead(pin);
                Push(value, Type.Byte);
            }
            
            default:
            {
                Runtime.Out4Hex(PC);
                Serial.WriteChar(':');
                Serial.WriteChar('S');
                Runtime.Out2Hex(iSysCall);
                Serial.WriteChar(' ');
                WriteHex(PC); Write(':'); Write('S'); WriteHex(iSysCall); Write(' '); ErrorDump(2);
                Error = 0x0A; // not implemented
            }
        }
        return Error == 0;
    }
    Put(uint address, uint value, Type htype)
    {
        WriteWord(valueStack + address, value);
        WriteWord(typeStack  + address, byte(htype));
    }
    uint Get(uint address, ref Type htype)
    {
        uint value  = ReadWord(valueStack + address);
        htype  = Type(ReadWord(typeStack + address));
        return value;
    }
    uint Get(uint address)
    {
        return ReadWord(valueStack + address);
    }
    PushI(int ivalue)
    {
        uint value = External.IntToUInt(ivalue);
#ifdef CHECKED        
        if (sp == stackSize)
        {
            Error = 0x07; // stack overflow
            return;
        }
#endif
        WriteWord(valueStack + sp, value);
        WriteWord(typeStack + sp, byte(Type.Int));
        sp = sp + 2;
    }
    PutI(uint address, int ivalue, Type htype)
    {
        uint value = External.IntToUInt(ivalue);
        WriteWord(valueStack + address, value);
        WriteWord(typeStack  + address, byte(htype));
    }
    
    int PopI(ref Type htype)
    {
#ifdef CHECKED
        if (sp == 0)
        {
            Error = 0x07; // stack underflow
            return 0;
        }
#endif
        sp = sp - 2;
        uint value  = ReadWord(valueStack + sp);
        htype  = Type(ReadWord(typeStack + sp));
        return External.UIntToInt(value);
    }
    int PopI()
    {
#ifdef CHECKED
        if (sp == 0)
        {
            Error = 0x07; // stack underflow
            return 0;
        }
#endif
        sp = sp - 2;
        return External.UIntToInt(ReadWord(valueStack + sp));
    }
    int GetI(uint address, ref Type htype)
    {
        uint value  = ReadWord(valueStack + address);
        htype  = Type(ReadWord(typeStack + address));
        return External.UIntToInt(value);
    }
    
    
    Push(uint value, Type htype)
    {
#ifdef CHECKED        
        if (sp == stackSize)
        {
            Error = 0x07; // stack overflow
            return;
        }
#endif
        WriteWord(valueStack + sp, value);
        WriteWord(typeStack + sp, byte(htype));
        sp = sp + 2;
    }
    uint Pop(ref Type htype)
    {
#ifdef CHECKED
        if (sp == 0)
        {
            Error = 0x07; // stack underflow
            return 0;
        }
#endif
        sp = sp - 2;
        uint value  = ReadWord(valueStack + sp);
        htype  = Type(ReadWord(typeStack + sp));
        return value;
    }
    uint Pop()
    {
#ifdef CHECKED
        if (sp == 0)
        {
            Error = 0x07; // stack underflow
            return 0;
        }
#endif
        sp = sp - 2;
        return ReadWord(valueStack + sp);
    }
    uint GetCS(uint address)
    {
        return ReadWord(callStack + address);
    }
    PushCS(uint value)
    {
#ifdef CHECKED
        if (csp == callStackSize)
        {
            Error = 0x06; // call stack overflow
            return;
        }
#endif
        WriteWord(callStack + csp, value);
        csp = csp + 2;
    }
    uint PopCS()
    {
#ifdef CHECKED
        if (csp == 0)
        {
            Error = 0x06; // call stack underflow
            return 0;
        }
#endif
        csp = csp - 2;
        return ReadWord(callStack + csp);
    }
    
    
    uint ReadWordOperand()
    {
        uint operand = ReadCodeWord(pc); 
        pc++; pc++;
        return operand;
    }
    byte ReadByteOperand()
    {
        byte operand = ReadCodeByte(pc); 
        pc++;
        return operand;
    }
    int ReadByteOffsetOperand()
    {
        int offset = int(ReadCodeByte(pc)); 
        pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }
        return offset;
    }
    int ReadWordOffsetOperand()
    {
        int offset = External.UIntToInt(ReadCodeWord(pc)); 
        pc++; pc++;
        return offset;
    }
    ShowCurrent()
    {
        IO.WriteHex(pc); IO.Write(' ');  IO.WriteHex(ReadCodeByte(pc)); IO.Write(' ');
        IO.WriteLn();
    }
    
    DumpHeap(bool display, uint accountedFor)
    {
        bool verboseDisplay = false;
        if (display)
        {
            verboseDisplay = true;
        }
        if (verboseDisplay)
        {
            IO.WriteLn();
            for (uint s = 0; s < sp; s = s + 2)
            {
                uint value  = ReadWord(valueStack + s);
                uint address = valueStack + s;
                byte htype  = byte(ReadWord(typeStack + s));
                if (IsReferenceType(Type(htype)))
                {
                    byte count  = byte(ReadByte(value + 1));
                    IO.WriteHex(value); IO.Write(':'); IO.WriteHex(htype);IO.Write(' ');IO.WriteHex(count); IO.Write(' '); 
                    GC.Dump(value, 0);
                    IO.WriteLn();
                }
            }
        }
        
        
        
        if (display)
        {
            IO.WriteLn();
            IO.Write('P');IO.Write('C');IO.Write(':');IO.WriteHex(PC);
            IO.WriteLn();
            IO.Write('F');IO.Write(':');
        }
        
        uint pCurrent = Memory.FreeList;
        uint freeSize = 0;
        uint allocatedSize = 0;
        loop
        {
            if (0 == pCurrent)
            {
                break;
            }
            uint size  = ReadWord(pCurrent);    
            uint pNext = ReadWord(pCurrent+2);    
            uint pPrev = ReadWord(pCurrent+4);   
            if (verboseDisplay) // free list items
            { 
                IO.WriteLn();
                IO.WriteHex(pCurrent);IO.Write(' ');
                IO.WriteHex(size); IO.Write(' ');
                IO.WriteHex(pNext);IO.Write('>');IO.Write(' ');IO.Write('<');IO.WriteHex(pPrev);
            }
            pCurrent = pNext;
            freeSize = freeSize + size;
        }
        if (display)
        {
            IO.WriteLn();
            IO.Write('H');IO.Write(':');
            IO.WriteHex(HeapStart+HeapSize);IO.Write('-');
            IO.WriteHex(HeapStart);IO.Write('=');IO.WriteHex(HeapSize);
        }
        pCurrent = HeapStart;
        uint pLimit   = HeapStart + HeapSize;
        uint count = 0;
        loop
        {
            count ++;
            if (pCurrent >= pLimit)
            {
                break;
            }
            uint size  = ReadWord(pCurrent);    
            if (count > 50)
            {
                break;
            }
                
            if (!IsOnFreeList(pCurrent))
            {
                if (verboseDisplay) // heap items
                {
                    IO.WriteLn();
                    IO.WriteHex(pCurrent); Write(' ');
                    IO.WriteHex(size); Write(' ');
                    byte tp = ReadByte(pCurrent+2);
                    byte rf = ReadByte(pCurrent+3);
                    IO.WriteHex(tp); Write(' ');IO.WriteHex(rf);
                }
                allocatedSize = allocatedSize + size;
            }
            else if (verboseDisplay)
            {
                // free list items
                IO.WriteLn();
                IO.WriteHex(pCurrent); Write(' ');
                IO.WriteHex(size); Write(' ');
            }
            if (size == 0)
            {
                break;
            }
            pCurrent = pCurrent + size; // this is why we limit ourselves to 0xFFFC (not 0x10000, actual 64K)
        }
        bool reportAndStop = (HeapSize != (allocatedSize + freeSize));
        if (!reportAndStop && (accountedFor > 0))
        {
            reportAndStop = (accountedFor != allocatedSize);    
        }
        if (reportAndStop)
        {
            if (!display)
            {
                DumpHeap(true, accountedFor);
                ErrorDump(91);
                Error = 0x0B;
            }
            else
            {
                IO.WriteLn();
                Write('A');IO.WriteHex(allocatedSize);Write(' ');Write('F');IO.WriteHex(freeSize);
                Write(' ');Write('L');IO.WriteHex(HeapSize - (allocatedSize + freeSize));
            }
        }
    }
    bool IsOnFreeList(uint pCandidate)
    {
        uint pCurrent = Memory.FreeList;
        loop
        {
            if (0 == pCurrent)
            {
                break;
            }
            if (pCurrent == pCandidate)
            {
                return true;
            }
            pCurrent = ReadWord(pCurrent+2);  
        }
        return false;
    }
    
    DumpStack()
    {
        return; // TODO REMOVE
        IO.WriteLn();
        for (uint s = 0; s < sp; s = s + 2)
        {
            if (s == bp)
            {
                IO.Write('B'); IO.Write('P');
            }
            else
            {
                IO.Write(' '); IO.Write(' ');
            }
            IO.Write(' '); IO.Write(' ');
            uint value  = ReadWord(valueStack + s);
            uint address = valueStack + s;
            byte htype  = byte(ReadWord(typeStack + s));
            IO.WriteHex(s); IO.Write(' '); IO.WriteHex(address); IO.Write(' ');
            IO.WriteHex(value); IO.Write(':'); IO.WriteHex(htype);
            if (IsReferenceType(Type(htype)))
            {
                byte count  = byte(ReadByte(value + 1));
                IO.Write(' '); IO.WriteHex(count); IO.Write(' '); 
                GC.Dump(value);
            }
            else
            {
                switch (Type(htype))
                {
                    case Type.Byte:
                    case Type.UInt:
                    {
                        IO.Write(' ');
                        IO.WriteUInt(value);
                    }
                    case Type.Bool:
                    {
                        IO.Write(' ');
                        IO.Write(((value != 0) ? 't' : 'f'));
                    }
                    case Type.Char:
                    {
                        IO.Write(char(0x27)); // single quote
                        IO.Write(char(value));
                        IO.Write(char(0x27)); // single quote
                    }
                    case Type.Int:
                    {
                        IO.Write(' ');
                        int iv = External.UIntToInt(value);
                        IO.WriteInt(iv);
                    }
                } 
            }
            IO.WriteLn();
            
        }
    }  
    
    ExecuteStepTo()
    {
        uint messagePC = pc;
        bool doNext = ExecuteOpCode();
        if (Error != 0)
        {
#ifndef SERIALCONSOLE                
            DumpStack();
            IO.WriteLn();
            IO.WriteHex(messagePC);
            IO.Write(' '); IO.Write('E'); IO.Write('r'); IO.Write('r'); IO.Write('o'); IO.Write('r'); IO.Write(':'); 
            byte berror = Error;
            IO.WriteHex(berror);
            IO.WriteLn();
#endif
        }
        else if (pc == 0) // returned from "main"
        {
            Restart(); // this restart causes the Profiler to hang for MSU (since 0 is legit start address)
        }
    }
    
    
    Execute()
    {
        uint messagePC;
        loop
        {
            messagePC = pc;
            bool doNext = ExecuteOpCode();
            if (Error != 0)
            {
#ifndef SERIALCONSOLE                
                DumpStack();
                IO.WriteLn();
                IO.WriteHex(messagePC);
                IO.Write(' '); IO.Write('E'); IO.Write('r'); IO.Write('r'); IO.Write('o'); IO.Write('r'); IO.Write(':'); 
                byte berror = Error;
                IO.WriteHex(berror);
                IO.WriteLn();
#endif
                break;
            }
            if (pc == 0) // returned from "main"
            {
                Restart(); // this restart causes the Profiler to hang for MSU (since 0 is legit start address)
                break;     // clean exit of "main"
            }
            if (IsBreak())
            {
                IO.WriteLn();IO.Write('B');IO.Write('R');IO.Write('E');IO.Write('A');IO.Write('K');IO.WriteLn();
                break;
            }
            if (BreakpointExists)
            {
                // pc is never zero here (see above)
                byte iBreak;
                bool atBreakpoint;
                for (iBreak = 0; iBreak<16; iBreak++)
                {
                    if (GetBreakpoint(iBreak) == pc)
                    {
                        if (iBreak == 0)
                        {
                            SetBreakpoint(0, 0); // clear single step breakpoint
                        }
                        return; // return control to Monitor
                    }
                }
            }
            WatchDog();
        } // loop
    }
    
    bool ExecuteOpCode()
    {
        bool doNext;
        opCode = OpCode(ReadCodeByte(pc));
        pc++;

#ifndef SERIALCONSOLE                
        uint jump = ReadWord(jumpTable + (byte(opCode) << 1));
#endif        
#ifdef CHECKED
        if (0 == jump)
        {
            if (Instructions.Undefined()) {}
            return false;
        }
#endif
#ifdef SERIALCONSOLE // on MCU
        doNext = External.FunctionCall(jumpTable, byte(opCode));
#else
        InstructionDelegate instructionDelegate = InstructionDelegate(jump);
        doNext = instructionDelegate();
#endif
        return doNext;
    }
    
    ExecuteWarp()
    {
#ifdef CHECKED
        uint messagePC;
#endif
        bool doNext;
        uint watchDog = 2500;
        loop
        {
#ifdef CHECKED
            messagePC = PC;
#endif
            opCode = OpCode(ReadCodeByte(pc));
#ifndef SERIALCONSOLE            
            uint jump = ReadWord(jumpTable + (byte(opCode) << 1));
#endif
            pc++;
            
#ifdef CHECKED
            if (0 == jump)
            {
                if (Instructions.Undefined()) {}
                return;
            }
#endif
#ifdef SERIALCONSOLE // on MCU
            doNext = External.FunctionCall(jumpTable, byte(opCode));
#else
            InstructionDelegate instructionDelegate = InstructionDelegate(jump);
            doNext = instructionDelegate();
#endif
            watchDog--;
            if (watchDog == 0)
            {
                WatchDog(); // yield() on devices like Wemos D1 Mini so WDT is not tripped
                watchDog = 2500;
            }
            if (doNext) { continue; }
            
            if (Error != 0)
            {
#ifndef SERIALCONSOLE                
                DumpStack();
                IO.WriteLn();
#ifdef CHECKED                
                IO.WriteHex(messagePC);
#endif
                IO.Write(' '); IO.Write('E'); IO.Write('r'); IO.Write('r'); IO.Write('o'); IO.Write('r'); IO.Write(':'); 
                byte berror = Error;
                IO.WriteHex(berror);
                IO.WriteLn();
#endif
                break;
            }
            if (PC == 0) // returned from "main"
            {
                Restart(); // this restart causes the Profiler to hang for MSU (since 0 is legit start address)
                break;     // clean exit of "main"
            }
            
        } // loop
    }

}
