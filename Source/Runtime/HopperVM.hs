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
    
    
    const uint stackSize     = 512; // size of value stack in byte (each stack slot is 2 bytes)
    const uint callStackSize = 512; // size of callstack in bytes (4 bytes per call)
    
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
    
    uint PC  { get { return pc; } }
    uint SP  { get { return sp; } }
    uint CSP { get { return csp; } }
    uint BP  { get { return bp; } }
    
    bool BreakpointExists { get { return breakpointExists; } }
    
    
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
        constAddress       = ReadWord(binaryAddress + 0x0002);
        methodTable        = binaryAddress + 0x0006;
        
        uint nextAddress   = binaryAddress + codeLength;
        callStack          = nextAddress;
        nextAddress        = nextAddress + callStackSize;
        
        valueStack         = nextAddress;
        nextAddress        = nextAddress + stackSize;
        
        typeStack          = nextAddress;
        nextAddress        = nextAddress + valueStack;
        
        dataMemory         = nextAddress;
#ifdef SERIALCONSOLE        
        if (dataMemory < 0x0800)
        {
            dataMemory = 0x0800; // after our 'fake' stack pages
        }
#endif
     
        MemoryReset();
    }
    MemoryReset()
    {
        // assumes dataMemory has been set after loading program
        HRArray.Release();  
#ifdef SERIALCONSOLE
        // currently we have 16K on the MCU
        Memory.Initialize(dataMemory, 0x4000 - dataMemory);
#else
        if (dataMemory < 0x1000)
        {
            dataMemory = 0x1000; // TODO REMOVE to make debugging easier for now
        }
        Memory.Initialize(dataMemory, 0xC000 - dataMemory);
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
        sp = 0;
        bp = 0;
        csp = 0;
        Error = 0;
        cnp = false;
        pc = ReadWord(binaryAddress + 0x0004);   
    }
    
    
    uint LookupMethod(uint methodIndex)
    {
        methodIndex = (methodIndex & 0x3FFF);
        uint address = methodTable;
        loop
        {
            uint entry = ReadWord(address);
            if (entry == methodIndex)
            {
                address = ReadWord(address+2);
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
    ExecuteSysCall(byte iSysCall, uint iOverload)
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
                    return;
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
            
            case SysCall.FloatToBytes:
            {
                Type htype;
                uint l = Pop(ref htype);
#ifdef CHECKED
                AssertLong(htype);
#endif
                uint lst = HRFloat.ToBytes(l);
                Push(lst, Type.Float);                
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
        
            case SysCall.LongNew:
            {
                uint address = HRLong.New();
                Push(address, Type.Long);
            }
            case SysCall.FloatNew:
            {
                uint address = HRLong.New();
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
        sp++; sp++;
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
        sp--;sp--;
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
        sp--;sp--;
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
        sp++; sp++;
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
        sp--;sp--;
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
        sp--;sp--;
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
        csp++; csp++;
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
        csp--;csp--;
        return ReadWord(callStack + csp);
    }
    
    
    uint ReadWordOperand()
    {
        uint operand = ReadWord(pc); 
        pc++; pc++;
        return operand;
    }
    byte ReadByteOperand()
    {
        byte operand = ReadByte(pc); 
        pc++;
        return operand;
    }
    int ReadByteOffsetOperand()
    {
        int offset = int(ReadByte(pc)); 
        pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }
        return offset;
    }
    int ReadWordOffsetOperand()
    {
        int offset = External.UIntToInt(ReadWord(pc)); 
        pc++; pc++;
        return offset;
    }
    ShowCurrent()
    {
        IO.WriteHex(pc); IO.Write(' ');  IO.WriteHex(ReadByte(pc)); IO.Write(' ');
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
            uint size  = ReadWord(pCurrent);    
            if (pCurrent >= pLimit)
            {
                break;
            }
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
            pCurrent = pCurrent + size;    
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
    ExecuteOpCode()
    {
#ifdef CHECKED        
        //DumpHeap(false, 0);
#endif
        OpCode opCode = OpCode(ReadByte(pc));
        pc++;
        loop
        {
            switch (opCode)
            {
                case OpCode.BOOLNOT:
                {
#ifdef CHECKED                
                    Type ttype;
                    uint top = Pop(ref ttype);
                    AssertBool(ttype, top);
#else
                    uint top = Pop();
#endif              
                    Push((top == 0) ? 1 : 0, Type.Bool); 
                }
                case OpCode.BITNOT:
                {
#ifdef CHECKED                
                    Type ttype;
                    uint top = Pop(ref ttype);
                    AssertUInt(ttype, top);
#else
                    uint top = Pop();
#endif              
                    Push(~top, Type.UInt); 
                }
                case OpCode.BOOLAND:
                case OpCode.BOOLOR:
                case OpCode.BITAND:
                case OpCode.BITOR:
                case OpCode.BITXOR:
                case OpCode.BITSHL:
                case OpCode.BITSHR:
                {
    #ifdef CHECKED                
                    Type ttype;
                    uint top = Pop(ref ttype);
                    Type ntype;
                    uint next = Pop(ref ntype);
                    switch (opCode)
                    {
                        case OpCode.BOOLAND:
                        case OpCode.BOOLOR:
                        {
                            AssertBool(ttype, top);
                            AssertBool(ntype, next);
                        }
                        case OpCode.BITAND:
                        case OpCode.BITOR:
                        case OpCode.BITXOR:
                        case OpCode.BITSHL:
                        case OpCode.BITSHR:
                        {
                            AssertUInt(ttype, top);
                            AssertUInt(ntype, next);
                        }
                    }
    #else
                    uint top = Pop();
                    uint next = Pop();
    #endif              
                    uint result;
                    Type rtype = Type.UInt;
                    switch (opCode)
                    {
                        case OpCode.BOOLAND:
                        {
                            result = ((next != 0) && (top != 0)) ? 1 : 0;
                            rtype = Type.Bool;
                        }
                        case OpCode.BOOLOR:
                        {
                            result = ((next != 0) || (top != 0)) ? 1 : 0;
                            rtype = Type.Bool;
                        }
                        case OpCode.BITAND:
                        {
                            result = (next & top);
                        }
                        case OpCode.BITOR:
                        {
                            result = (next | top);
                        }
                        case OpCode.BITXOR:
                        {
                            result = (next | top) & (~(next & top));
                        }
                        case OpCode.BITSHL:
                        {
                            switch (top)
                            {
                                case 1:  {result = (next << 1); }
                                case 2:  {result = (next << 2); }
                                case 3:  {result = (next << 3); }
                                case 4:  {result = (next << 4); }
                                case 5:  {result = (next << 5); }
                                case 6:  {result = (next << 6); }
                                case 7:  {result = (next << 7); }
                                case 8:  {result = (next << 8); }
                                case 9:  {result = (next << 9); }
                                case 10: {result = (next << 10); }
                                case 11: {result = (next << 11); }
                                case 12: {result = (next << 12); }
                                case 13: {result = (next << 13); }
                                case 14: {result = (next << 14); }
                                case 15: {result = (next << 15); }
                                default: { Error = 0x0B; ErrorDump(79); }
                            }
                            
                        }
                        case OpCode.BITSHR:
                        {
                            switch (top)
                            {
                                case 1:  {result = (next >> 1); }
                                case 2:  {result = (next >> 2); }
                                case 3:  {result = (next >> 3); }
                                case 4:  {result = (next >> 4); }
                                case 5:  {result = (next >> 5); }
                                case 6:  {result = (next >> 6); }
                                case 7:  {result = (next >> 7); }
                                case 8:  {result = (next >> 8); }
                                case 9:  {result = (next >> 9); }
                                case 10: {result = (next >> 10); }
                                case 11: {result = (next >> 11); }
                                case 12: {result = (next >> 12); }
                                case 13: {result = (next >> 13); }
                                case 14: {result = (next >> 14); }
                                case 15: {result = (next >> 15); }
                                default: { Error = 0x0B; ErrorDump(80); }
                            }
                        }
                    }
                    Push(result, rtype); 
                }
                case OpCode.ADD:
                case OpCode.SUB:
                case OpCode.DIV:
                case OpCode.MUL:
                case OpCode.MOD:
                {
    #ifdef CHECKED                
                    Type ttype;
                    uint top = Pop(ref ttype);
                    Type ntype;
                    uint next = Pop(ref ntype);
                    AssertUInt(ttype, top);
                    AssertUInt(ntype, next);
    #else
                    uint top = Pop();
                    uint next = Pop();
    #endif
                    uint result;
                    switch (opCode)
                    {
                        case OpCode.ADD:
                        {
                            result = next + top;
                        }
                        case OpCode.SUB:
                        {
                            result = next - top;
                        }
                        case OpCode.DIV:
                        {
                            if (top == 0)
                            {
                                Error = 0x04; // division by zero attempted
                            }
                            else
                            {
                                result = next / top;
                            }
                        }
                        case OpCode.MUL:
                        {
                            result = next * top;
                        }
                        case OpCode.MOD:
                        {
                            if (top == 0)
                            {
                                Error = 0x04; // division by zero attempted
                            }
                            else
                            {
                                result = next % top;
                            }
                        }   
                    }
                    Push(result, Type.UInt); 
                }
                case OpCode.ADDI:
                case OpCode.SUBI:
                case OpCode.DIVI:
                case OpCode.MULI:
                case OpCode.MODI:
                {
    #ifdef CHECKED                
                    Type ttype;
                    int top = PopI(ref ttype);
                    Type ntype;
                    int next = PopI(ref ntype);
                    AssertInt(ttype);
                    AssertInt(ntype);
    #else
                    int top = PopI();
                    int next = PopI();
    #endif
                    int result;
                    switch (opCode)
                    {
                        case OpCode.ADDI:
                        {
                            result = next + top;
                        }
                        case OpCode.SUBI:
                        {
                            result = next - top;
                        }
                        case OpCode.DIVI:
                        {
                            if (top == 0)
                            {
                                Error = 0x04; // division by zero attempted
                            }
                            else
                            {
                                result = next / top;
                            }
                        }
                        case OpCode.MULI:
                        {
                            result = next * top;
                        }
                        case OpCode.MODI:
                        {
                            if (top == 0)
                            {
                                Error = 0x04; // division by zero attempted
                            }
                            else
                            {
                                result = next % top;
                            }
                        }   
                    }
                    
                    PushI(result); 
                }
                
                case OpCode.PUSHDW:
                case OpCode.PUSHIW:
                {
                    Push(ReadWordOperand(), Type.UInt);
                }
                case OpCode.PUSHDB:
                case OpCode.PUSHIB:
                {
                    Push(ReadByteOperand(), Type.Byte);
                }
                case OpCode.PUSHGP:
                {
                    Push(0, Type.UInt);
                }
                case OpCode.PUSHI0:
                {
                    Push(0, Type.Byte);
                }
                case OpCode.PUSHI1:
                {
                    Push(1, Type.Byte);
                }
                case OpCode.PUSHIM1:
                {
                    PushI(-1);
                }
                case OpCode.INCLOCALB:
                {
                    int offset     = ReadByteOffsetOperand();
                    
                    // INCLOCALB is an optimization of "i = i + 1":
                    // If it were done using ADDI or ADD, then the result pushed on the stack
                    // would be tInt or tUInt, even if i was a tByte.
                    // POPLOCALB would then supply the type for the resulting expression.
                    //
                    // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
                    // (we need to avoid munting the type if it is currently a -ve tInt)
                    
                    Type itype;
                    uint address = uint(int(bp) + offset);
                    uint value = Get(address, ref itype);
                    if (itype == Type.Byte)
                    {
                        itype = Type.UInt;
                    }
                    Put(address, value+1, itype);
                }
                case OpCode.DECLOCALB:
                {
                    int offset     = ReadByteOffsetOperand();
                    Type itype;
                    uint address = uint(int(bp) + offset);
                    uint value = Get(address, ref itype);
                    Put(address, value-1, itype);
                }
                case OpCode.INCLOCALBB:
                {
                    int offset0    = ReadByteOffsetOperand();
                    int offset1    = ReadByteOffsetOperand();
                    uint address0 = uint(int(valueStack) + int(bp) + offset0);
                    uint address1 = uint(int(valueStack) + int(bp) + offset1);
                    WriteWord(address0, ReadWord(address0) + ReadWord(address1));
                }
                
                case OpCode.CAST:
                {
                    uint operand = ReadByteOperand();
                    Type htype;
                    Put(sp-2, Get(sp-2, ref htype), Type(operand));
                }
                
                
                
                
                
                case OpCode.JZB:
                {
    #ifdef CHECKED  
                    int offset = ReadByteOffsetOperand();              
                    Type htype;
                    uint choice = Pop(ref htype);
                    if (IsReferenceType(htype)) // != 0 is not the same as UInt
                    {
                        ErrorDump(40);
                        Error = 0x0B;
                    }
                    
                    if (choice == 0)
                    {
                        pc = uint(int(pc-2) + offset);
                    }
    #else
                    if (Pop() == 0)
                    {
                        pc = uint(ReadByteOffsetOperand() + int(pc-2));
                    }
                    else
                    {
                        pc++;
                    }
    #endif   
                    
                }
                case OpCode.JNZB:
                {
                    
    #ifdef CHECKED
                    int offset = ReadByteOffsetOperand();
                    Type htype;
                    uint choice = Pop(ref htype);
                    if (IsReferenceType(htype)) // != 0 is not the same as UInt
                    {
                        ErrorDump(38);
                        Error = 0x0B;
                    }
                    if (choice != 0)
                    {
                        pc = uint(int(pc-2) + offset);
                    }
    #else
                    if (Pop() != 0)
                    {
                        pc = uint(ReadByteOffsetOperand() + int(pc-2));
                    }
                    else
                    {
                        pc++;
                    }
    #endif   
                    
                }
                case OpCode.JB:
                {
                    pc = uint(ReadByteOffsetOperand() + int(pc-2));
                }
                case OpCode.JZW:
                {
                    
    #ifdef CHECKED
                    int offset = ReadWordOffsetOperand();
                    Type htype;
                    uint choice = Pop(ref htype);
                    if (IsReferenceType(htype)) // != 0 is not the same as UInt
                    {
                        ErrorDump(39);
                        Error = 0x0B;
                    }
                    
                    if (choice == 0)
                    {
                        pc = uint(int(pc-3) + offset);
                    }
    #else
                    if (Pop() == 0)
                    {
                        pc = uint(ReadWordOffsetOperand() + int(pc-3));
                    }
                    else
                    {
                        pc++;pc++;
                    }
    #endif   
                    
                }
                case OpCode.JNZW:
                {
    #ifdef CHECKED
                    int offset = ReadWordOffsetOperand();
                    Type htype;
                    uint choice = Pop(ref htype);
                    if (IsReferenceType(htype)) // != 0 is not the same as UInt
                    {
                        ErrorDump(41);
                        Error = 0x0B;
                    }
                    
                    if (choice != 0)
                    {
                        pc = uint(int(pc-3) + offset);
                    }
    #else
                    if (Pop() != 0)
                    {
                        pc = uint(ReadWordOffsetOperand() + int(pc-3));
                    }
                    else
                    {
                        pc++;pc++;
                    }
    #endif   
                    
                }
                case OpCode.JW:
                {
                    pc = uint(ReadWordOffsetOperand() + int(pc-3));
                }
                
                case OpCode.PUSHIWLT:
                {
                    uint top = ReadWordOperand();    
                    Type ntype;
                    uint next = Pop(ref ntype);
    #ifdef CHECKED
                    AssertUInt(ntype, next);
    #endif  
                    Push((next < top) ? 1 : 0, Type.Bool);
                }
                
                case OpCode.PUSHIWLE:
                {
                    uint top = ReadWordOperand();    
                    Type ntype;
                    uint next = Pop(ref ntype);
    #ifdef CHECKED
                    AssertUInt(ntype, next);
    #endif  
                    Push((next <= top) ? 1 : 0, Type.Bool);
                }
                
                
                case OpCode.EQ:
                case OpCode.NE:
                case OpCode.GT:
                case OpCode.LT:
                case OpCode.GE:
                case OpCode.LE:
                {
                    Type ttype;
                    uint top  = Pop(ref ttype);
                    Type ntype;
                    uint next = Pop(ref ntype);
    #ifdef CHECKED
                    if ((OpCode.EQ != opCode) && (OpCode.NE != opCode))
                    {
                        AssertUInt(ttype, top);
                        AssertUInt(ntype, next);
                    }
    #endif                
                    switch (opCode)
                    {
                        case OpCode.EQ:
                        {    
                            Push((next == top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.NE:
                        {    
                            Push((next != top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.GT:
                        {    
                            Push((next > top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.LT:
                        {    
                            Push((next < top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.GE:
                        {    
                            Push((next >= top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.LE:
                        {    
                            Push((next <= top) ? 1 : 0, Type.Bool);
                        }
                        default:
                        {
                            ErrorDump(5);
                            Error = 0x0B;
                        }
                    }
                }
                case OpCode.PUSHIWLEI:
                {
                    Push(ReadWordOperand(), Type.UInt);    
                    Type ntype;
                    Type ttype;
                    int top  = PopI(ref ttype);
                    Type ntype;
                    int next = PopI(ref ntype);
    #ifdef CHECKED
                    AssertInt(ttype);
                    AssertInt(ntype);
    #endif  
                    Push((next <= top) ? 1 : 0, Type.Bool);
                }
                case OpCode.GTI:
                case OpCode.LTI:
                case OpCode.GEI:
                case OpCode.LEI:
                {
                    Type ttype;
                    int top  = PopI(ref ttype);
                    Type ntype;
                    int next = PopI(ref ntype);
    #ifdef CHECKED
                    AssertInt(ttype);
                    AssertInt(ntype);
    #endif                
                    switch (opCode)
                    {
                        case OpCode.GTI:
                        {    
                            Push((next > top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.LTI:
                        {    
                            Push((next < top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.GEI:
                        {    
                            Push((next >= top) ? 1 : 0, Type.Bool);
                        }
                        case OpCode.LEI:
                        {    
                            Push((next <= top) ? 1 : 0, Type.Bool);
                        }
                        default:
                        {
                            ErrorDump(4);
                            Error = 0x0B;
                        }
                    }
                }
                
                
                
                case OpCode.PUSHRELB:
                {
                    int  offset = ReadByteOffsetOperand();
                    uint referenceAddress = uint(int(bp) + offset);
                    Type rtype;
                    uint localAddress = Get(referenceAddress, ref rtype);
    #ifdef CHECKED
                    AssertReference(rtype, localAddress);
    #endif
                    uint value = Get(localAddress, ref rtype);
                    Push(value, rtype);
                    if (IsReferenceType(rtype))
                    {
                        GC.AddReference(value);
                    }
                }
                case OpCode.PUSHSTACKADDRB:
                {
                    int  offset = ReadByteOffsetOperand();
                    uint address = uint(int(bp) + offset);
                    Push(address, Type.Reference);
                }
                
                case OpCode.SWAP:
                {
                    uint topValue  = ReadWord(valueStack + sp - 2);
                    uint nextValue = ReadWord(valueStack + sp - 4);
                    WriteWord(valueStack + sp - 2, nextValue);
                    WriteWord(valueStack + sp - 4, topValue);
                    uint topType  = ReadWord(typeStack + sp - 2);
                    uint nextType = ReadWord(typeStack + sp - 4);
                    WriteWord(typeStack + sp - 2, nextType);
                    WriteWord(typeStack + sp - 4, topType);
                }
                case OpCode.DUP: // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
                {
                    byte  offset  = ReadByteOperand();
                    uint address = sp - 2 - offset;
                    uint value = ReadWord(valueStack     + address);
                    Type htype = Type(ReadWord(typeStack + address));
                    Push(value, htype);
                    if (IsReferenceType(htype))
                    {
                        GC.AddReference(value);
                    }
                }
                case OpCode.PUSHLOCALB00:
                case OpCode.PUSHLOCALB02:
                case OpCode.PUSHLOCALB:
                {
                    int offset;
                    switch (opCode)
                    {
                        case OpCode.PUSHLOCALB00:
                        {
                            offset = 0;
                        }
                        case OpCode.PUSHLOCALB02:
                        {
                            offset = 2;
                        }
                        case OpCode.PUSHLOCALB:
                        {
                            offset     = ReadByteOffsetOperand();
                        }
                    }
                    uint value = ReadWord(uint(int(valueStack)     + int(bp) + offset));
                    Type htype = Type(ReadWord(uint(int(typeStack) + int(bp) + offset)));
                    Push(value, htype);
                    if (IsReferenceType(htype))
                    {
                        GC.AddReference(value);
                    }
                }
                case OpCode.PUSHLOCALBB:
                {
                    // LSB
                    int offset     = ReadByteOffsetOperand();
                    uint value = ReadWord(uint(int(valueStack)     + int(bp) + offset));
                    Type htype = Type(ReadWord(uint(int(typeStack) + int(bp) + offset)));
                    Push(value, htype);
                    if (IsReferenceType(htype))
                    {
                        GC.AddReference(value);
                    }
                    
                    // MSB
                    offset     = ReadByteOffsetOperand();
                    value = ReadWord(uint(int(valueStack)     + int(bp) + offset));
                    htype = Type(ReadWord(uint(int(typeStack) + int(bp) + offset)));
                    Push(value, htype);
                    if (IsReferenceType(htype))
                    {
                        GC.AddReference(value);
                    }
                }
                case OpCode.PUSHGLOBALB:
                {
                    byte offset     = ReadByteOperand();
                    uint value = ReadWord(valueStack + offset);
                    Type htype = Type(ReadWord(typeStack + offset));
                    Push(value, htype);
                    if (IsReferenceType(htype))
                    {
                        GC.AddReference(value);
                    }
                }
                
                
                
                case OpCode.SYSCALL0:
                {
                    byte iSysCall = ReadByteOperand();  
                    ExecuteSysCall(iSysCall, 0);
                }
                case OpCode.SYSCALL1:
                {
                    byte iSysCall = ReadByteOperand();  
                    ExecuteSysCall(iSysCall, 1);
                }
                case OpCode.SYSCALL:
                {
                    Type htype;
                    uint iOverload = Pop(ref htype);
                    byte iSysCall  = ReadByteOperand();  
                    ExecuteSysCall(iSysCall, iOverload);
                }
                case OpCode.CALLB:
                {
                    uint methodIndex = ReadByteOperand();
                    PushCS(pc);
                    pc = LookupMethod(methodIndex);
                }
                case OpCode.CALLW:
                {
                    uint methodIndex = ReadWordOperand();
                    PushCS(pc);
                    pc = LookupMethod(methodIndex);
                }
                case OpCode.CALLREL:
                {
    #ifdef CHECKED
                    Type rtype;
                    uint methodIndex = Pop(ref rtype);
                    AssertUInt(rtype, methodIndex);
                    if (methodIndex == 0)
                    {
                        Error = 0x0D; // invalid or uninitialized delegate
                    }
    #else
                    uint methodIndex = Pop();                
    #endif          
                    PushCS(pc);
                    pc = LookupMethod(methodIndex);
                }
                
                case OpCode.DECSP:
                {
                    uint popBytes = ReadByteOperand();
                    while (popBytes != 0)
                    {
                        Type htype;
                        uint address = Pop(ref htype);
                        if (IsReferenceType(htype))
                        {
                            GC.Release(address);
                        }
                        popBytes = popBytes - 2;
                    }
                }
                case OpCode.RETFAST:
                {
                    pc = PopCS();
                }
                case OpCode.RET0:
                case OpCode.RETB:
                case OpCode.RETRETB:
                {
                    uint value;
                    Type rtype;
                    if (opCode == OpCode.RETRETB)
                    {
                        value = Pop(ref rtype);
                    }
                    uint popBytes;
                    if (opCode != OpCode.RET0)
                    {
                        popBytes = ReadByteOperand();
                    }
                    while (popBytes != 0)
                    {
                        Type htype;
                        uint address = Pop(ref htype);
                        if (IsReferenceType(htype))
                        {
                            GC.Release(address);
                        }
                        popBytes = popBytes - 2;
                    }
                    if (opCode == OpCode.RETRETB)
                    {
                        Push(value, rtype);
                    }
                    bp = PopCS();
                    if (csp == 0)
                    {
                        pc = 0; // exit program
                    }
                    else
                    {
                        pc = PopCS();
                    }
                }
                
                case OpCode.ENTER:
                case OpCode.ENTERB:
                {
                    PushCS(bp);
                    bp = sp;
                    
                    uint zeros = 0;
                    Type htype;
                    if (opCode == OpCode.ENTERB)
                    {
                        zeros = ReadByteOperand();
                    }
                    for (uint i = 0; i < zeros; i++)
                    {
                        Push(0, Type.Byte);
                    }
                }
                
                case OpCode.COPYNEXTPOP:
                {
                    cnp = true;
                }
                
                
                case OpCode.POPLOCALB00:
                case OpCode.POPLOCALB02:
                case OpCode.POPLOCALB:
                {
                    int offset;
                    switch (opCode)
                    {
                        case OpCode.POPLOCALB00:
                        {
                            if (cnp) { opCode = OpCode.POPCOPYLOCALB00; cnp=false; continue; }
                            offset = 0;
                        }
                        case OpCode.POPLOCALB02:
                        {
                            if (cnp) { opCode = OpCode.POPCOPYLOCALB02; cnp=false; continue; }
                            offset = 2;
                        }
                        case OpCode.POPLOCALB:
                        {
                            if (cnp) { opCode = OpCode.POPCOPYLOCALB; cnp=false; continue; }
                            offset     = ReadByteOffsetOperand();
                        }
                    }
                
                    // this is the slot we are about to overwrite: decrease reference count if reference type
                    Type htype = Type(ReadWord(uint(int(typeStack) + int(bp) + offset)));
                    uint value;
                    if (IsReferenceType(htype))
                    {
                        value = ReadWord(uint(int(valueStack)     + int(bp) + offset));
                        GC.Release(value);
                    }
                    
                    value = Pop(ref htype);
                    WriteWord(uint(int(valueStack) + int(bp) + offset), value);
                    WriteWord(uint(int(typeStack)  + int(bp) + offset), uint(htype));
                }
                
                case OpCode.POPGLOBALB:
                {
                    if (cnp) { opCode = OpCode.POPCOPYGLOBALB; cnp=false; continue; }
                    byte offset     = ReadByteOperand();
                    
                    // this is the slot we are about to overwrite: decrease reference count if reference type
                    Type htype = Type(ReadWord(typeStack  + offset));
                    uint value;
                    if (IsReferenceType(htype))
                    {
                        value = ReadWord(valueStack  + offset);
                        GC.Release(value);
                    }
                    
                    value = Pop(ref htype);
                    WriteWord(valueStack + offset, value);
                    WriteWord(typeStack  + offset, uint(htype));
                }
                case OpCode.POPRELB:
                {
                    if (cnp) { opCode = OpCode.POPCOPYRELB; cnp=false; continue; }
                    
                    int offset = ReadByteOffsetOperand();
                    uint referenceAddress = uint(int(bp) + offset);
                    
                    Type rtype;
                    uint localAddress = Get(referenceAddress, ref rtype);
                    
                    uint existing = Get(localAddress, ref rtype);
                    if (IsReferenceType(rtype))
                    {
                        GC.Release(existing);
                    }
                    
                    Type vtype;
                    uint value = Pop(ref vtype);
                    Put(localAddress, value, vtype);
                }
                
                case OpCode.POPCOPYRELB:
                {
                    int  offset = ReadByteOffsetOperand();
                    uint referenceAddress = uint(int(bp) + offset);
                    Type rtype;
                    uint localAddress = Get(referenceAddress, ref rtype);
    #ifdef CHECKED
                    AssertReference(rtype, localAddress);
    #endif
                    // this is the slot we are about to overwrite: decrease reference count if reference type
                    uint oldvalue = Get(localAddress, ref rtype);
                    if (IsReferenceType(rtype))
                    {
                        GC.Release(oldvalue);
                    }
                    uint value = Pop(ref rtype);
                    if (value == oldvalue)
                    {
                        // nothing more to do
                    }
                    else
                    {
                        // clone self, release the original
                        uint newvalue = GC.Clone(value);
                        GC.Release(value);
                        Put(localAddress, newvalue, rtype);
                    }
                }
                
                case OpCode.POPCOPYLOCALB00:
                case OpCode.POPCOPYLOCALB02:
                case OpCode.POPCOPYLOCALB:
                {
                    int offset;
                    switch (opCode)
                    {
                        case OpCode.POPCOPYLOCALB00:
                        {
                            offset = 0;
                        }
                        case OpCode.POPCOPYLOCALB02:
                        {
                            offset = 2;
                        }
                        case OpCode.POPCOPYLOCALB:
                        {
                            offset     = ReadByteOffsetOperand();
                        }
                    }
                    // this is the slot we are about to overwrite: decrease reference count if reference type
                    Type htype;
                    uint localAddress = uint(int(bp) + offset);
                    uint oldvalue = Get(localAddress, ref htype);
                    if (IsReferenceType(htype))
                    {
                        GC.Release(oldvalue);
                    }
                    uint value = Pop(ref htype);
                    if (value == oldvalue)
                    {
                        // overwriting self - no more to do
                    }
                    else
                    {
                        // clone self, release the original
                        uint newvalue = GC.Clone(value);
                        GC.Release(value);
                        Put(localAddress, newvalue, htype); 
                    }
                }
                
                case OpCode.POPCOPYGLOBALB:
                {
                    byte offset     = ReadByteOperand();
                    
                    // this is the slot we are about to overwrite: decrease reference count if reference type
                    
                    Type htype;
                    uint oldvalue = Get(offset, ref htype);
                    if (IsReferenceType(htype))
                    {
                        GC.Release(oldvalue);
                    }
                    uint value = Pop(ref htype);
                    if (value == oldvalue)
                    {
                        // overwriting self - no more to do
                    }
                    else
                    {
                        // clone self, release the original
                        uint newvalue = GC.Clone(value);
                        GC.Release(value);
                        Put(offset, newvalue, htype); 
                    }
                }
                case OpCode.JIXB:
                case OpCode.JIXW:
                {
                    uint switchCase = Pop();

                    byte minRange = ReadByteOperand();
                    byte maxRange = ReadByteOperand();
                    
                    byte lsb = ReadByteOperand();
                    byte msb = ReadByteOperand();
                    
                    int jumpBackOffset = int(lsb + (msb << 8));
                    
                    uint tpc = pc;
                    
                    pc = uint(int(pc) - jumpBackOffset - 5);
                    
                    uint tableSize = uint(maxRange) - uint(minRange) + 1;
                    if (opCode == OpCode.JIXW)
                    {
                        tableSize = tableSize << 1;
                    }
                    
                    uint offset = 0;
                    if ((switchCase >= minRange) && (switchCase <= maxRange))
                    {
                        // in the table
                        if (opCode == OpCode.JIXW)
                        {
                            uint index = tpc + (switchCase - minRange)*2;
                            offset = ReadByte(index) + ReadByte(index+1) << 8;
                        }
                        else
                        {
                            uint index = tpc + switchCase - minRange;
                            offset = ReadByte(index);
                        }
                    }
                    
                    if (offset == 0)
                    {
                        // default
                        pc = tpc + tableSize;
                    }
                    else
                    {
                        pc = pc + offset;
                    }
                }
                
                
                default:
                {
                    Runtime.Out4Hex(PC);
                    Serial.WriteChar(':');
                    Serial.WriteChar('O');
                    Runtime.Out2Hex(byte(opCode));
                    Serial.WriteChar(' ');
                    ErrorDump(1);
                    Error = 0x0A; // not implemented
                }   
            } // switch (opCode)
            break;
        } // loop
    } 
    
    ExecuteStepTo()
    {
        uint messagePC = pc;
        ExecuteOpCode();
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
    
    ExecuteWarp()
    {
        uint messagePC;
        loop
        {
            messagePC = pc;
            ExecuteOpCode();
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
            WatchDog();
        } // loop
    }
    Execute()
    {
        uint messagePC;
        loop
        {
            messagePC = pc;
            ExecuteOpCode();
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
}
