unit Runtime
{
    uses "/Source/6502/Samples/GIBL/HopperCode"  // code related to Hopper VM byte code
    
    // #### globals at the top of the file so we can keep track of them:

    int vA; int vB; int vC; int vD; int vE; int vF; int vG; int vH;  
    int vI; int vJ; int vK; int vL; int vM; int vN; int vO; int vP;  
    int vQ; int vR; int vS; int vT; int vU; int vV; int vW; int vX;  
    int vY; int vZ; 
    uint rnd;
    
    // GOSUB -> RETURN stack
#ifdef DYNAMICSTACK    
    const uint           callStackLimit     = 64;  // arbitrary but we do need some limit (~6 byte overhead per list item)
    <uint>               callStack;                // prefer a list rather than yet another array of wasted space
#else
    const byte           zeroPageGOSUBCSP   = 0xCC;
    const uint           callStackLimit     = 1024; // same size as above but much deeper and faster (always wastes 2K though
    uint[callStackLimit] callStack;             
#endif    

    const byte    zeroPageHopperFlags = 0xBB;
    
    Conditions gCondition; // runtime state
    
    // #### end of globals
    
    delegate   bool IsBreakDelegate();
    delegate        RuntimePrintDelegate(string str);
    delegate        RuntimePrintRefDelegate(uint address);
    delegate string IntToStringDelegate(int i);
    delegate string UIntToHexStringDelegate(uint i, byte w);
    delegate        RuntimeErrorDelegate(uint number);
    delegate        HopperCodePatchNextJumpDelegate(uint ln);
    delegate uint   RAMReadWordDelegate(uint addresss);
    delegate        RAMWriteWordDelegate(uint addresss, uint word);
    delegate int    RuntimeGetDelegate();
    delegate int    RuntimeRndDelegate();
    delegate        RuntimeSeedDelegate(int seed);
    delegate bool   RuntimeInputDelegate(byte iVariable, bool isString);
    delegate bool   RuntimePushReturnDelegate(uint address);
    delegate uint   RuntimePopReturnDelegate();
    
    // runtime function pointers accessed from the inlined VM opcodes:
    IntToStringDelegate             IntToStringPtr     { get { return Int.ToString; } }
    UIntToHexStringDelegate         UIntToStringHexPtr { get { return UInt.ToHexString; } } 
    IsBreakDelegate                 IsBreakPtr         { get { return Runtime.isBreak; } }
    RuntimeErrorDelegate            ErrorPtr           { get { return Runtime.error; } }
    HopperCodePatchNextJumpDelegate HopperCodePatchPtr { get { return HopperCode.patchNextJump; } }
    RAMReadWordDelegate             RAMReadWordPtr     { get { return RAM.ReadWord;  } }
    RAMWriteWordDelegate            RAMWriteWordPtr    { get { return RAM.WriteWord; } }
    RuntimeGetDelegate              GetPtr             { get { return Runtime.Get; } }
    RuntimeRndDelegate              RndPtr             { get { return Runtime.random; } }
    RuntimeSeedDelegate             SeedPtr            { get { return Runtime.seedRnd; } }
    RuntimeInputDelegate            InputPtr           { get { return Runtime.input; } }
    RuntimePushReturnDelegate       PushReturnPtr      { get { return Runtime.pushReturn; } }
    RuntimePopReturnDelegate        PopReturnPtr       { get { return Runtime.popReturn; } }
    RuntimePrintDelegate            PrintPtr           { get { return Runtime.print; } }
    RuntimePrintRefDelegate         PrintRefPtr        { get { return Runtime.printRef; } }
    
    flags Conditions
    {
        None  = 0x00,
        Jump  = 0x01,
        Break = 0x02,
        Error = 0x04,
    }
    
    Conditions Condition  { get { return gCondition; }  set { gCondition = value; } }
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
        TraceOn        = 0x01,
        WarpSpeed      = 0x02, // on 6502, built without checks for <Ctrl><C>
        StackSlot32Bit = 0x02, // on MCUs, 'float' and 'long' are value types
        CheckedBuild   = 0x04,
        SP8Bit         = 0x08,
        ProfileBuild   = 0x10,
        BreakpointsSet = 0x20,
        SingleStep     = 0x40,
        MCUPlatform    = 0x80,
    }
    HopperFlags Flags 
    { 
        get 
        { 
            byte hf = Memory.ReadByte(zeroPageHopperFlags); // zero page
            return HopperFlags(hf); 
        } 
    }
    
    // pointer to the array of variables or index of first global:
    byte       Variables          { get { return byte(&vA);                                    } }
    byte       CallStack          { get { return byte(&callStack);                             } }
    
    
    
    bool pushReturn(uint address)
    {
#ifdef DYNAMICSTACK
        if (callStack.Length == callStackLimit)
        {
            error(7); // out of memory for call stack
            return false;
        }
        callStack.Append(address);
#else
        byte csp = Memory.ReadByte(zeroPageGOSUBCSP); // zero page
        if (csp == callStackLimit)
        {
            error(7); // out of memory for call stack
            return false;
        }
        callStack[csp] = address;
        csp++;
        Memory.WriteByte(zeroPageGOSUBCSP, csp);
#endif
        return true;
    }
    uint popReturn()
    {
#ifdef DYNAMICSTACK
        if (callStack.Length == 0)
        {
            error(8); // RETURN without GOSUB
            return 0;
        }
        uint address = callStack[callStack.Length-1];
        callStack.Remove(callStack.Length-1);
#else
        byte csp = Memory.ReadByte(zeroPageGOSUBCSP); // zero page
        if (csp == 0)
        {
            error(8); // RETURN without GOSUB
            return 0;
        }
        csp--;
        uint address = callStack[csp];
        Memory.WriteByte(zeroPageGOSUBCSP, csp);
#endif
        return address;
    }
    
    error(uint number) // only one overload here
    { 
        uint ln = HopperCode.GetCurrentLineNumber();
        Errors.Error(number, char(0), ln);
    }
    
    bool Is8BitStack { get { return (HopperFlags.Stack8Bit == (Runtime.Flags & HopperFlags.Stack8Bit)); } }
    Clear() // called by "CLEAR" and before each "RUN"
    {
        // clears the variables
        byte i;
        byte lpVariables = Variables;
        byte typeStackWidth = 1;
        uint local0 = 0;
        uint local2 = 2;
        byte intType = byte(type(int));
        uint typeAddress  = 0x0500 + (lpVariables >> 1);
        uint valueAddress = 0x0600 + lpVariables;
        if (Is8BitStack)
        {
            typeStackWidth = 2;
            typeAddress = 0x0500 + lpVariables;
        }
        for (i=0; i < 26; i++)
        {
            Memory.WriteByte(typeAddress, intType);
            typeAddress = typeAddress + typeStackWidth;
            RAM.WriteWord(valueAddress, local0);
            valueAddress = valueAddress + local2;
        }
        loop // just to avoid zero
        {
            rnd = (RAM.TimeForSeed & 0x7FFF);
            if (rnd != 0) { break; }
        }
#ifdef DYNAMICSTACK 
        callStack.Clear();
#else
        Memory.WriteByte(zeroPageGOSUBCSP, 0);
#endif        
    }
    
    print(string str)
    {
        Write(str);
    }
    printRef(uint address)
    {
        loop
        {
            byte b = ReadByte(address);
            if (b == 0)
            {
                break;
            }
            Write(char(b));
            address++;
        } 
    }
    int random()
    {
        // PRNG from here: 
        // https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
        rnd = rnd ^ (rnd << 7);
        rnd = rnd ^ (rnd >> 9);
        rnd = rnd ^ (rnd << 8);
        return int(rnd & 0x7FFF);
    }
    seedRnd(int seed)
    {
        if (seed == 0) // bad for the PRND
        {
            seed = 1;
        }
        if (seed < 0)
        {
            seed = -seed;
        }
        rnd = uint(seed);
    }
    int Get()
    {
        char ch;
        loop
        {
            if (IO.IsAvailable)
            {
                if (IO.IsBreak()) 
                {
                    Condition = Conditions.Break;
                    ch = char(0);
                    break;
                }
                ch = IO.Read();
                break;
            }
        } // loop
        return int(ch);
    }
    bool input(byte variableIndex, bool isString)
    {
        int value;
        string content;
        bool ok;
        loop
        {
            if (IO.IsAvailable)
            {
                if (IO.IsBreak()) 
                {
                    Condition = Conditions.Break;
                    ok = false;
                    break;
                }
                char ch = IO.Read();
                byte b = byte(ch);
                uint clength = content.Length;
                if (ch.IsDigit())
                {
                    content = content + ch;
                    ok = isString || Int.TryParse(content, ref value);
                    Write(ch);
                }
                else if ((clength == 0) && ((ch == '-') || (ch == '+')))
                {
                    content = content + ch;
                }
                else if ((ch == char(0x0D)) && ok)
                {
                    break;
                }
                else if ((ch == char(0x08)) && (clength != 0))
                {
                    content = content.Substring(0, clength-1);
                    clength--;
                    // backspace
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                    ok = isString || Int.TryParse(content, ref value);
                }
                else if (ch == char(0x1B))
                {
                    while (clength != 0)
                    {
                        content = content.Substring(0, clength-1);
                        clength--;
                        // backspace
                        Write(char(0x08));
                        Write(' ');
                        Write(char(0x08));
                    }
                    ok = false;
                }
                else if (isString && (byte(ch) >= 32) && (byte(ch) < 127))
                {
                    content = content + ch; // any printable ASCII character
                    ok = true;
                    Write(ch);
                }
            }
        } // loop
        WriteLn();
        if (ok)
        {   
            if (isString)
            {
                SetString(variableIndex, content);
            }
            else
            {
                SetVariable(variableIndex, value);
            }
        }
        return ok; // ok
    }
    bool isBreak()
    {
        // avoid yet another method call to IsBreak()
        //    Serial.IsAvailable compiles to a SYSCALL0
        if (Serial.IsAvailable) 
        {
            if (IO.IsBreak()) 
            { 
                Condition = Conditions.Break;
                return true;
            } // check for <ctrl><X>
        }
        return false;
    }
    
    SetVariable(byte variableIndex, int value)
    {
        uint w = value.GetByte(0) + (value.GetByte(1) << 8);
        byte lpVariables = Variables;
        RAM.WriteWord(0x0600 + lpVariables + variableIndex*2, w);
    }
    SetString(byte variableIndex, string content)
    {
        byte lpVariables = Variables;
        uint variableAddress = 0x0600 + lpVariables + variableIndex*2;  
        uint stringAddress = RAM.ReadWord(variableAddress);
        uint i = 0;
        loop
        {
            if (i == content.Length)
            {
                //WriteLn("SetString: 0x" + stringAddress.ToHexString(4) + "char(0)");
                Memory.WriteByte(stringAddress, 0); // null terminator
                break;
            }
            char ch = content[i];
            //WriteLn("SetString: 0x" + stringAddress.ToHexString(4) + "'" + ch + "'");
            Memory.WriteByte(stringAddress, byte(ch));
            i++;
            stringAddress++;
        }
    }

}
